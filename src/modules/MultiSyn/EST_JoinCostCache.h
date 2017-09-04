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
/* --------------------------------------------------------------------- */
/* Data type intended to handle a table of n^2 measures relating to      */
/* n distinct entities (e.g. join costs for a phone type).  We assume    */
/* the measure is symmetric and hence we don't actually store n^2 values */
/*                                                                       */
/*************************************************************************/


#ifndef __EST_JOINCOSTCACHE_H__
#define __EST_JOINCOSTCACHE_H__

/**@name Caching of join cost computations (need for speed)
   We assume the distance measure is symmetric and hence we don't actually
   store n^2 values for the n instances of any given phone.
*/ 

//@{

/** Object oriented approach for better and for worse... 
*/

#include "EST_TList.h"
#include "ling_class/EST_Item.h"
#include <iostream>

using namespace std;

class EST_JoinCost;

class EST_JoinCostCache {
public:

  EST_JoinCostCache( unsigned int id );   
  EST_JoinCostCache( unsigned int id, unsigned int n );
  EST_JoinCostCache( unsigned int id, unsigned char *mem, unsigned int n, bool del=false );
  ~EST_JoinCostCache();

  unsigned int id() const {return _id;}
  
  ostream& write( ostream &os ) const;
  unsigned char val( unsigned int a, unsigned int b ) const;
  bool setval( unsigned int a, unsigned int b, unsigned char v );

  bool computeAndCache( const EST_TList<EST_Item*> &list,
			const EST_JoinCost &jc, 
			bool verbose=false );
  
private: 
  EST_JoinCostCache( EST_JoinCostCache &);
  EST_JoinCostCache& operator=( EST_JoinCostCache &);

private:
  unsigned int numInstances;
  unsigned int _id;
  unsigned char* cache;
  static const unsigned char minVal =  0x0;
  static const unsigned char maxVal = 0xff;
  static const unsigned char defVal = 0xff;
  unsigned int cachelen;
  bool deleteMemoryOnDeath;
};

#endif // __EST_JOINCOSTCACHE_H__
