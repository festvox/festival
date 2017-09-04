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
/*                            Date: June 2006                            */
/* --------------------------------------------------------------------- */
/*                                                                       */
/* Code to allow the possibility of changing the costs (thus ranking)    */
/* assigned to candidates units by the target cost function, typically   */
/* based on what those original costs are...                             */
/*                                                                       */
/*                                                                       */
/*************************************************************************/

#ifndef __TARGETCOSTRESCORING_H__
#define __TARGETCOSTRESCORING_H__

#include <iostream>
#include "EST_viterbi.h"

// do some additional target cost, based on the acoustic properties of
// the units found matching this target unit.  This function assumes the 
// viterbi candidates linked list is in the correct format, with the 
// correct information available.

// toplevel function to call (Changes the candidate list in place.)
// beam_width specifies which candidates are included in the
// computation (i.e. only those candidates with target costs within
// "beam_width" of the best candidate's score).
// mult specifies an arbitrary weighting factor to be applied to the penalties
// added to the candidates' target cost scores by this function (e.g. a mult of 
// 0.0 will stop the target cost scores being changed at all, whereas a mult of 10.0
// will make it have a large effect).
void rescoreCandidates( EST_VTCandidate *candidates, float beam_width, float mult );

// our internal class for doing the work
class ScorePair {
public:
  ScorePair( )
    : _score(0.0),
      _dur(0.0),
      _cand(0)
  {};

  ScorePair( float score, float duration, EST_VTCandidate* cand )
    : _score(score),
      _dur(duration),
      _cand(cand)
  {};

public:
  float _score;
  float _dur;
  EST_VTCandidate* _cand;
};

inline bool operator > ( const ScorePair &A, const ScorePair &B )
{
  return ( A._score > B._score ) ? true : false; 
}

inline bool operator < ( const ScorePair &A, const ScorePair &B )
{
  return ( A._score < B._score ) ? true : false;
}

inline bool operator == ( const ScorePair &A, const ScorePair &B )
{
  return ( A._score == B._score ) ? true : false;
}

ostream& operator<<( ostream& out, const ScorePair &sp );


#endif // __TARGETCOSTRESCORING_H__
