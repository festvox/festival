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
/*                          Author: Korin Richmond                       */
/*                            Date: June 2006                            */
/* --------------------------------------------------------------------- */
/*                                                                       */
/* Code to allow the possibility of changing the costs (thus ranking)    */
/* assigned to candidates units by the target cost function, typically   */
/* based on what those original costs are...                             */
/*                                                                       */
/*************************************************************************/

#include "TargetCostRescoring.h"
#include "ling_class/EST_Item.h"
#include "EST_TList.h"
#include "EST_viterbi.h"

#include "DiphoneVoiceModule.h" //for getJoinTime(const EST_Item *it)

// Do some additional target cost here, based on the acoustic
// properties of the units found matching this target unit. (Changes
// the candidate list "candidates" in place.)  This currently is
// limited to looking at the durations of the top scoring candidates,
// but also could be extended to include components for the pitch and
// amplitude at various points for example...

void rescoreCandidates( EST_VTCandidate *candidates, float beam_width, float mult )
{
    // first calculate the stats for the "model"
    float dur = 0.0;  
    EST_Item *ph1 = 0;
    EST_Item *ph2 = 0;
    //EST_FVector *ph1_mid = 0;
    //EST_FVector *ph2_mid = 0;
  
    EST_TList<ScorePair> scores;
  
    // get all scores to work out what durations are "suitable"
    for( EST_VTCandidate *it = candidates; it != 0; it=it->next ){
        ph1 = it->s;
        ph2 = inext(ph1);
        //       ph1_mid = fvector( ph1->f( "midcoef" ) );
        //       ph2_mid = fvector( ph2->f( "midcoef" ) );
    
        dur = getJoinTime(ph2) - getJoinTime(ph1); // duration of diphone unit
        scores.append( ScorePair(it->score,dur, it)  );
    }
  
    sort( scores );
    //cerr << scores << endl;
  
    // calculate simple mean duration of some or all of candidates
    float meandur = 0.0;
    int n = 0;

    if( beam_width < 0 ){ // just average all of them
        for( EST_Litem *li = scores.head(); li != 0; li = li->next() ){
            meandur += scores(li)._dur;
            n++;
        }
    }
    else{
        float score_cutoff = scores.first()._score + beam_width;
        for( EST_Litem *li = scores.head(); li != 0; li = li->next() ){
            if( scores(li)._score > score_cutoff )
                break;
            else{
                meandur += scores(li)._dur;
                n++;
            }
        }    
    }
  
    meandur /= n;

    // then tweak the scores based on that
    for( EST_Litem *li = scores.head(); li != 0; li = li->next() ){
        float cand_dur = scores(li)._dur; 
        //    cerr << scores(li)._cand->score << " ";
        scores(li)._cand->score += (mult * abs( cand_dur - meandur ) );
        //    cerr << scores(li)._cand->score << endl;
    }
}

ostream& operator << ( ostream& out, const ScorePair &sp )
{
  out << sp._score << " " << sp._dur << "\n";
  return out;
}
