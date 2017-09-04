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
/* Interface for family of target cost function objects which            */
/* calculate a target score for given candidate                          */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*************************************************************************/


#ifndef __EST_TARGETCOST_H__
#define __EST_TARGETCOST_H__

#include "siod.h" // for gc_protect( obj**)
class EST_Item;

/**@name Interface for Target Cost function object
*/ 

//@{

/** Object oriented approach for better or for worse... 
*/

/* Positional enum */

enum tcpos_t {
  TCPOS_INITIAL,
  TCPOS_MEDIAL,
  TCPOS_FINAL,
  TCPOS_INTER,
  TCPOS_POSITIONS
};


/*
 *  BASE CLASS:  EST_TargetCost
 */
class EST_TargetCost {
 public:
  
  EST_TargetCost() : defScore(0.0){};
  virtual ~EST_TargetCost() {};

  // Base class operator() doesn't do much, but it will work.
  virtual float operator()( const EST_Item* targp, const EST_Item* candp ) const
    { return defScore; }

  // Allow flatpacking
  virtual const bool is_flatpack() const {return false;}

 protected: 
  float defScore;
  // Temp variables for use while calculating cost (here for speed)
  mutable float score;
  mutable float weight_sum;
  mutable const EST_Item *cand;
  mutable const EST_Item *targ;

  inline void set_cand(const EST_Item* seg) const 
    { cand = seg; }
  
  inline void set_targ(const EST_Item* seg) const 
    { targ = seg; }
  
  inline void set_targ_and_cand(const EST_Item* tseg, const EST_Item* cseg) const
    { set_targ(tseg); set_cand(cseg); }

  inline float add_weight(float w) const 
    { weight_sum += w ; return w; }
  
  // General cost functions that derived classes may want to use.
  float apml_accent_cost() const;
  float stress_cost() const;
  float position_in_syllable_cost() const;
  float position_in_word_cost() const;
  float position_in_phrase_cost() const;
  float partofspeech_cost() const;
  float punctuation_cost() const;
  float left_context_cost() const;
  float right_context_cost() const;
  float bad_duration_cost() const;
  float out_of_lex_cost() const;
  float bad_f0_cost() const;
};


/*
 *  DERIVED CLASS: EST_DefaultTargetCost
 */
class EST_DefaultTargetCost : public EST_TargetCost {

 public:
  float operator()(const EST_Item* targ, const EST_Item* cand) const;

};

/*
 *  DERIVED CLASS: EST_APMLTargetCost
 */
class EST_APMLTargetCost : public EST_TargetCost {

 public:
  float operator()(const EST_Item* targ, const EST_Item* cand) const;

};

/*
 *  DERIVED CLASS: EST_SingingTargetCost
 */
class EST_SingingTargetCost : public EST_TargetCost {

 public:
  float operator()(const EST_Item* targ, const EST_Item* cand) const;

 protected:
  float pitch_cost() const;
  float duration_cost() const;

};





/*
 *  DERIVED CLASS: EST_SchemeTargetCost
 */
class EST_SchemeTargetCost : public EST_TargetCost {

 private:
  LISP tc;

 public:
  EST_SchemeTargetCost( LISP scheme_targetcost )
    : EST_TargetCost(), tc(scheme_targetcost) 
  { gc_protect( &tc ); }
  
  ~EST_SchemeTargetCost() 
  { gc_unprotect( &tc ); }
  
  float operator()(const EST_Item* targ, const EST_Item* cand) const;
};


#endif // __EST_TARGETCOST_H__





