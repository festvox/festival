/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                 (University of Edinburgh, UK) and                     */
/*                           Korin Richmond                              */
/*                         Copyright (c) 2004                            */
/*                         All Rights Reserved.                          */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
/*                                                                       */
/*   1. The code must retain the above copyright notice, this list of    */
/*      conditions and the following disclaimer.                         */
/*   2. Any modifications must be clearly marked as such.                */
/*   3. Original authors' names are not deleted.                         */
/*   4. The authors' names are not used to endorse or promote products   */
/*      derived from this software without specific prior written        */
/*      permission.                                                      */
/*                                                                       */
/*  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        */
/*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT   */
/*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*                          Author: Korin Richmond                       */
/*                            Date: January 2004                         */
/*                                                                       */
/*************************************************************************/

#include "EST_JoinCostCache.h"
#include "EST_JoinCost.h"
#include "EST_error.h"
#include "safety.h"
#include <iostream>
#include <cmath>

EST_JoinCostCache::EST_JoinCostCache( unsigned int id )
  : numInstances(0),
    _id(id),
    cache(0),
    cachelen(0)

{

}

EST_JoinCostCache::EST_JoinCostCache( unsigned int id, unsigned int n )
  : numInstances(n),
    _id(id),
    cachelen((n*n)/2-n),
    deleteMemoryOnDeath(true)
{
  cache = new unsigned char [cachelen];
  CHECK_PTR( cache );
}
  
EST_JoinCostCache::EST_JoinCostCache( unsigned int id, unsigned char *mem, unsigned int n, bool del )
  : numInstances(n),
    _id(id),
    cache(mem),
    cachelen((n*n)/2-n),
    deleteMemoryOnDeath(del)
{

}

EST_JoinCostCache::~EST_JoinCostCache()
{
  if( cache != 0 )
    if( deleteMemoryOnDeath )
      delete [] cache;
}

unsigned char EST_JoinCostCache::val( unsigned int a, unsigned int b ) const
{
  if( a>numInstances || b>numInstances )
    EST_error( "Requested index greater than cache size" );

  if( a == b )
    return minVal;
  else if( b > a )
    return cache[(b*(b-1)>>1)+a];
  else
    return cache[(a*(a-1)>>1)+b];
  
  return defVal;
}


bool EST_JoinCostCache::setval( unsigned int a, unsigned int b, unsigned char v )
{
  if( a>numInstances || b>numInstances )
    EST_error( "Requested index greater than cache size" );

  if( a == b ){
    return true;
  }
  else if( b > a ){
    cache[(b*(b-1)>>1)+a] = v;
    return true;
  }
  else{
    cache[(a*(a-1)>>1)+b] = v;
    return true;
  }
  
  return false;
}


ostream& EST_JoinCostCache::write( ostream &os ) const
{
  os << cachelen;
  //  os.write( cache, cachelen );
  return os;
}

bool EST_JoinCostCache::computeAndCache( const EST_TList<EST_Item*> &list,
					 const EST_JoinCost &jc, 
					 bool verbose )
{
  unsigned char qcost; // quantized cost
  
  unsigned int qleveln = maxVal-minVal;

  float ulimit = 1.0-1/(float)(qleveln);
  float llimit = 0.0+1/(float)(qleveln);
  
  unsigned int i=0;
  EST_warning("EST_JoinCostCache::computeAndCache");
  for( EST_Litem *it=list.head(); it; it=it->next(), ++i ){
    
    unsigned int j=i+1;    
    for( EST_Litem *jt=it->next(); jt; jt=jt->next(), ++j ){
      float cost = jc( list(it), list(jt) );
      
      if( cost >= ulimit )
	qcost = maxVal;
      else if( cost <= llimit )
	qcost = minVal;
      else
	qcost = static_cast<unsigned char>(rint(cost*(float)qleveln));

      setval( i, j, qcost );
    }

    // yet to be convinced this is the best place for this...
    list(it)->set( "jccid", (int)this->id() );
    list(it)->set( "jccindex", (int) i );
  }

  return true;
}




