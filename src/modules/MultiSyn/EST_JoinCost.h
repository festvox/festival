/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                 (University of Edinburgh, UK) and                     */
/*                           Korin Richmond                              */
/*                         Copyright (c) 2002                            */
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
/*                            Date: October 2002                         */
/* --------------------------------------------------------------------- */
/* Interface for family of join cost function objects which              */
/* calculate a join score for given candidates                           */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*************************************************************************/


#ifndef __EST_JOINCOST_H__
#define __EST_JOINCOST_H__

class EST_Item;
class EST_JoinCostCache;

///////////////////////////////
//because of the inline 
#include "EST_JoinCostCache.h"
#include "DiphoneVoiceModule.h"
#include "safety.h"
#include "ling_class/EST_Item.h"
#include "EST_FMatrix.h"
///////////////////////////////

/**@name Interface for Join Cost function object
*/ 

//@{

/** Object oriented approach for better or for worse... 
*/

class EST_JoinCost {
 public:
  
  EST_JoinCost() 
    : defCost(1),
    f0_weight (1.0),
    power_weight(1.0),
    spectral_weight(1.0)
    {};
  
  ~EST_JoinCost();

  bool computeAndCache( const EST_TList<EST_Item*> &list, bool verbose=true );

  // join cost which avoids information lookup (or faster lookup), to
  // be used in Viterbi search
  inline float operator()( const DiphoneCandidate* left, const DiphoneCandidate* right ) const;
  
  // original join cost, retained because it's still used by cost calculations for
  // caching (and for posterity, and comparison)
  inline float operator()( const EST_Item *left, const EST_Item *right ) const;

  inline void set_f0_weight(float val) { f0_weight = val; }
  inline void set_power_weight(float val) { power_weight = val; }
  inline void set_spectral_weight(float val) { spectral_weight = val; }

 private: 
  // member data all used by the older implementation (taking EST_Item* not
  // DiphoneCandidate*)
  float defCost;
  mutable const EST_Item *cachedItem;
  mutable const EST_FVector *cachedItemVector;
  mutable unsigned int cached_jccid;
  mutable unsigned int cached_jccindex;
  mutable bool costIsCached;
  mutable bool diphoneJoin; //would definitely be better somewhere else...
  float f0_weight;
  float power_weight;
  float spectral_weight;


  EST_TSimpleVector<EST_JoinCostCache *> costCaches;
  
  // function which computes the actual cost between two vectors
  // of join cost coefficients
  inline float calcDistance( const EST_FVector *l, const EST_FVector *r ) const;
};


//////////////////////////////////////////////////////////////////////////////////
// experiment to see if this is sensible or not
// // for now, the left and right edges of a join are represented by EST_Items,
// // which is particular to the implementation of the DiphoneUnitVoice.
// // It would be desirable in the future though to abstract the interface to 
// // something like concatUnit::right_edge() and concatUnit::left_edge() since
// // we would like a given join cost to be able to be generally applied to units of
// // any length or form.
inline float EST_JoinCost::operator()( const EST_Item* left, const EST_Item* right ) const
{
  float d_overall;

  //default zero cost if units contiguous in database
  // (i.e. this is the cost between a phone and *itself*
  if( left == iprev(right) )
    return 0.0;

  // An "infinite" join cost for bad units. The idea here is that if
  // you use a unit marked as bad (either for duration or
  // pitchmarks, then the units either side of it must also be used.
  //  if( left->f_present("bad") || right->f_present("bad")
  //      || left->f_present("bad_dur") || right->f_present("bad_dur") )
  //    return 100.0;

  // else...

  // since the Viterbi class takes each path at time t and tries to
  // extend with all candidates at time t+1, it probably makes sense
  // to cache the feature vector for the "left" item to avoid looking
  // it up every time this function is called...
  if( cachedItem != left ){
    cachedItem = left;

    if( left->f_present( "jccid" ) ){
      costIsCached = true;
      cached_jccid = left->features().val( "jccid" ).Int();
      cached_jccindex = left->features().val( "jccindex" ).Int();
    }
    else{
      costIsCached = false;

      if( left->f_present( "extendRight") ){
        diphoneJoin = false;
        cachedItemVector = fvector( left->features().val( "endcoef" ) );
      }
      else{
        diphoneJoin = true;
        cachedItemVector = fvector( left->features().val( "midcoef" ) );
      }

    }
  }

  if( costIsCached && right->f_present( "jccid" ) ){
    unsigned int right_jccid = right->features().val( "jccid" ).Int();
    unsigned int right_jccindex = right->features().val( "jccindex" ).Int();

    if( cached_jccid == right_jccid )
      d_overall = (float)(costCaches(cached_jccid)->val(cached_jccindex, right_jccindex))/255;
    else{
      EST_warning( "JoinCost: inconsistent cache ids, setting max join cost" );
      d_overall = 1.0;
    }
  }
  else{
    const EST_FVector *l = cachedItemVector;
    const EST_FVector *r;
    if( diphoneJoin )
      r = fvector( right->features().val( "midcoef" ) );
    else
      r = fvector( right->features().val( "startcoef" ) );

    d_overall = calcDistance( l, r );
  }
  return d_overall;
}

inline float EST_JoinCost::operator()( const DiphoneCandidate* left, const DiphoneCandidate* right ) const
{
  float dist;
  
  //default zero cost if units contiguous in database
  // (i.e. this is the cost between a phone and *itself*
  if( inext(left->ph1) == right->ph1 )
    return 0.0;     

  //use cached costs in preference to calculating
  if( left->ph2_jccid >= 0 )
    if( left->ph2_jccid == right->ph1_jccid ) 
      dist = (float)(costCaches(left->ph2_jccid)->val(left->ph2_jccindex, right->ph1_jccindex))/255;
    else{
      EST_warning( "JoinCost: inconsistent cache ids, setting max join cost" );
      dist = 1.0;
    }
  else{
    dist = calcDistance( left->r_coef, right->l_coef );
  }
  return dist; 
}

inline float EST_JoinCost::calcDistance( const EST_FVector *l, const EST_FVector *r ) const
{
  float d_spectral, d_f0, d_power, d_overall;
  
  int l_length = l->length();
  if (l_length != r->length())
    EST_error("Can't compare vectors of differing length\n");
    
  ////////////////////////////////////////////////////////////////////////////
  // f0 distance
  //
  // because unvoiced is represented as -1.0, we need to take
  // special measures when calculating f0 distances, to avoid
  // situation where something low in the speaker's range is closer
  // to 0.0 than other higher voiced speech.  (this could especially
  // be problematic where bad pitchmarking or labelling occurs)
  float l_f0 = l->a_no_check( l_length-1 );
  float r_f0 = r->a_no_check( l_length-1 );
  
  if( l_f0 != -1.0 ){
    if( r_f0 != -1.0 ){
      d_f0 = pow((float)( l_f0 - r_f0 ), (float)2.0);
      d_f0 = sqrt( d_f0 );
    }
    else
      d_f0 = 1.0;
  }
  else if( r_f0 != -1.0 )
    d_f0 = 1.0;
  else
    d_f0 = 0.0;
  
  ////////////////////////////////////////////////////////////////////////////
  // power distance
  d_power = pow((float)((l->a_no_check(l_length-2) - r->a_no_check(l_length-2))), (float)2.0);
  d_power = sqrt( d_power );
  
  ////////////////////////////////////////////////////////////////////////////
  // spectral distance
  float d = 0.0;
  l_length -= 2; // don't include f0 and power
  for(int i = 0; i < l_length; i++)
    d += pow((float)(l->a_no_check(i) - r->a_no_check(i)), (float)2.0);
  d_spectral = sqrt( d );
  
  // equal weighting by default
  d_overall = (d_f0*f0_weight + d_power*power_weight + d_spectral*spectral_weight) / 3;

  
  return d_overall;
}

#endif // __EST_JOINCOST_H__





