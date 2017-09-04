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
/*                        Author: Korin Richmond                         */
/*                          Date: October 2002                           */
/* --------------------------------------------------------------------- */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*************************************************************************/

#include <iostream>
#include "festival.h"
#include "ling_class/EST_Item.h"
#include "EST_TargetCost.h"
#include "siod.h"

static const EST_String simple_pos(const EST_String &s);
static const EST_Utterance *tc_get_utt(const EST_Item *seg);
static const EST_Item* tc_get_syl(const EST_Item *seg);
static const EST_Item* tc_get_word(const EST_Item *seg);
static EST_String ff_tobi_accent(const EST_Item *s);
static EST_String ff_tobi_endtone(const EST_Item *s);
static bool threshold_equal(float a, float b, float threshold);

/*
 *  BASE CLASS:  EST_TargetCost
 */


/* Individual cost functions */


// This is really designed only for apml!
float EST_TargetCost::apml_accent_cost() const
{
    // Check if target is an apml utterance. If not return 0 as we don't
    // trust its accent specification.

    if( !tc_get_utt(targ)->relation_present("SemStructure"))
        return 0.0;

    // Check if candidate is an apml utterance. If not return 1
    // (as we want to use apml if available)

    if( !tc_get_utt(cand)->relation_present("SemStructure"))
        return 1.0;

    // As they are both apml match accents.

    const EST_Item *tsyl, *csyl;
    EST_String targ_accent, cand_accent, targ_boundary, cand_boundary;


    if( ph_is_vowel(targ->features().val("name").String()) && 
        !ph_is_silence(targ->features().val("name").String()) )
    {
        tsyl = tc_get_syl(targ);
        csyl = tc_get_syl(cand);
      
        // Can't assume candidate and target identities are the same
        // (because of backoff to a silence for example)
        if( csyl == 0 )
            return 1.0;

        targ_accent = ff_tobi_accent(tsyl);
        cand_accent = ff_tobi_accent(csyl);
        targ_boundary = ff_tobi_endtone(tsyl);
        cand_boundary = ff_tobi_endtone(csyl);

        if( (cand_accent != targ_accent) || (cand_boundary != targ_boundary) )
            return 1.0;
    }

    if( ph_is_vowel(inext(targ)->features().val("name").String()) && 
        !ph_is_silence(inext(targ)->features().val("name").String()) )
    {
        tsyl = tc_get_syl(inext(targ));
        csyl = tc_get_syl(inext(cand));
      
        // Can't assume candidate and target identities are the same
        // (because of backoff to a silence for example)
        if( csyl == 0 )
            return 1.0;

        targ_accent = ff_tobi_accent(tsyl);
        cand_accent = ff_tobi_accent(csyl);
        targ_boundary = ff_tobi_endtone(tsyl);
        cand_boundary = ff_tobi_endtone(csyl);

        if( (cand_accent != targ_accent) || (cand_boundary != targ_boundary) )
            return 1.0;
    }

    return 0.0;

}


float EST_TargetCost::stress_cost() const
{
    int cand_stress;
    int targ_stress;
    const EST_Item *tsyl, *csyl;

    if( ph_is_vowel(targ->features().val("name").String()) && 
        !ph_is_silence(targ->features().val("name").String()) )
    {
        tsyl = tc_get_syl(targ);
        csyl = tc_get_syl(cand);
      
        // Can't assume candidate and target identities are the same
        // (because of backoff to a silence for example)
        if( csyl == 0 )
	{
            //cout << "SC: 1 returning 1\n";
            return 1;
	}

        targ_stress = (tsyl->I("stress") > 0) ? 1 : 0;
        cand_stress = (csyl->I("stress") > 0) ? 1 : 0;

        if( cand_stress != targ_stress)
	{
            //cout << "SC: 2 returning 1\n";
            return 1;
	}
    }
  
    if( ph_is_vowel(inext(targ)->features().val("name").String()) &&
        !ph_is_silence(inext(targ)->features().val("name").String()) )
    {
        tsyl = tc_get_syl(inext(targ));
        csyl = tc_get_syl(inext(cand));

        // Can't assume candidate and target identities are the same
        // (because of backoff to a silence for example)
        if( csyl == 0 )
	{
            //cout << "SC: 3 returning 1\n";
            return 1;
	}

        targ_stress = (tsyl->I("stress") > 0) ? 1 : 0;
        cand_stress = (csyl->I("stress") > 0) ? 1 : 0;
        if( cand_stress != targ_stress)
	{
            //cout << "SC: 4 returning 1\n";
            return 1;
	}
    }
  
    //cout << "SC: 5 returning 0\n";
    return 0;
}

float EST_TargetCost::position_in_syllable_cost() const
{
    tcpos_t targ_pos = TCPOS_MEDIAL;
    tcpos_t cand_pos = TCPOS_MEDIAL;

    const EST_Item *targ_syl = tc_get_syl(targ);
    const EST_Item *targ_next_syl = tc_get_syl(inext(targ));
    const EST_Item *targ_next_next_syl = tc_get_syl(inext(inext(targ)));
    const EST_Item *targ_prev_syl = tc_get_syl(iprev(targ));
    const EST_Item *cand_syl = tc_get_syl(cand);
    const EST_Item *cand_next_syl = tc_get_syl(inext(cand));
    const EST_Item *cand_next_next_syl = tc_get_syl(inext(inext(cand)));
    const EST_Item *cand_prev_syl = tc_get_syl(iprev(cand));
   
    if( targ_syl != targ_next_syl )
        targ_pos = TCPOS_INTER;
    else if( targ_syl != targ_prev_syl)
        targ_pos = TCPOS_INITIAL;
    else if( targ_next_syl != targ_next_next_syl)
        targ_pos = TCPOS_FINAL;
   
    if( cand_syl != cand_next_syl )
        cand_pos = TCPOS_INTER;
    else if( cand_syl != cand_prev_syl)
        cand_pos = TCPOS_INITIAL;
    else if( cand_next_syl != cand_next_next_syl)
        cand_pos = TCPOS_FINAL;
   
    return (targ_pos == cand_pos) ? 0 : 1;
}

float EST_TargetCost::position_in_word_cost() const
{
    tcpos_t targ_pos = TCPOS_MEDIAL;
    tcpos_t cand_pos = TCPOS_MEDIAL;
  
    const EST_Item *targ_word = tc_get_word(targ);
    const EST_Item *targ_next_word = tc_get_word(inext(targ));
    const EST_Item *targ_next_next_word = tc_get_word(inext(inext(targ)));
    const EST_Item *targ_prev_word = tc_get_word(iprev(targ));
    const EST_Item *cand_word = tc_get_word(cand);
    const EST_Item *cand_next_word = tc_get_word(inext(cand));
    const EST_Item *cand_next_next_word = tc_get_word(inext(inext(cand)));
    const EST_Item *cand_prev_word = tc_get_word(iprev(cand));
  
    if( targ_word != targ_next_word )
        targ_pos = TCPOS_INTER;
    else if( targ_word != targ_prev_word)
        targ_pos = TCPOS_INITIAL;
    else if( targ_next_word != targ_next_next_word)
        targ_pos = TCPOS_FINAL;
  
    if( cand_word != cand_next_word )
        cand_pos = TCPOS_INTER;
    else if( cand_word != cand_prev_word)
        cand_pos = TCPOS_INITIAL;
    else if( cand_next_word != cand_next_next_word)
        cand_pos = TCPOS_FINAL;
  
    return (targ_pos == cand_pos) ? 0 : 1;
}


float EST_TargetCost::position_in_phrase_cost() const
{
  
    const EST_Item *targ_word = tc_get_word(targ);
    const EST_Item *cand_word = tc_get_word(cand);
  
    if (!targ_word && !cand_word)
        return 0;
    if (!targ_word || !cand_word)
        return 1;

    return (targ_word->features().val("pbreak").String() == cand_word->features().val("pbreak").String()) ? 0 : 1;
}

float EST_TargetCost::punctuation_cost() const
{
    const EST_Item *targ_word = tc_get_word(targ);
    const EST_Item *cand_word = tc_get_word(cand);
    const EST_Item *next_targ_word = tc_get_word(inext(targ));
    const EST_Item *next_cand_word = tc_get_word(inext(cand));

    float score = 0.0;

    if ( (targ_word && !cand_word) || (!targ_word && cand_word) )
        score += 0.5;
    else
        if (targ_word && cand_word)
            if ( parent(targ_word,"Token")->features().val("punc","NONE").String()
                 != parent(cand_word,"Token")->features().val("punc","NONE").String() )
                score += 0.5;
  

    if ( (next_targ_word && !next_cand_word) || (!next_targ_word && next_cand_word) )
        score += 0.5;
    else
        if(next_targ_word && next_cand_word)
            if ( parent(next_targ_word,"Token")->features().val("punc","NONE").String()
                 != parent(next_cand_word,"Token")->features().val("punc","NONE").String() )
                score += 0.5;
  

    return score;

}


float EST_TargetCost::partofspeech_cost() const
{
    // Compare left phone half of diphone
    const EST_Item *targ_left_word = tc_get_word(targ);
    const EST_Item *cand_left_word = tc_get_word(cand);
  
    if(!targ_left_word && !cand_left_word)
        return 0;
    if(!targ_left_word || !cand_left_word)
        return 1;

    const EST_String targ_left_pos( simple_pos(targ_left_word->features().val("pos").String()) );
    const EST_String cand_left_pos( simple_pos(cand_left_word->features().val("pos").String()) );

    if( targ_left_pos != cand_left_pos )
        return 1;

    // Compare right phone half of diphone
    const EST_Item *targ_right_word = tc_get_word(inext(targ));
    const EST_Item *cand_right_word = tc_get_word(inext(cand));

    if(!targ_right_word && !cand_right_word)
        return 0;
    if(!targ_right_word || !cand_right_word)
        return 1;

    const EST_String targ_right_pos( simple_pos(targ_right_word->features().val("pos").String()) );
    const EST_String cand_right_pos( simple_pos(cand_right_word->features().val("pos").String()) );

    if( targ_right_pos != cand_right_pos )
        return 1;

    return 0;
}

float EST_TargetCost::left_context_cost() const
{
    EST_Item *targ_context = iprev(targ);
    EST_Item *cand_context = iprev(cand);
  
    if ( !targ_context && !cand_context)
        return 0;
    if ( !targ_context  || !cand_context)
        return 1;

    return (targ_context->features().val("name").String() == cand_context->features().val("name").String()) ? 0 : 1;
}

float EST_TargetCost::right_context_cost() const
{
  
    EST_Item *targ_context = inext(inext(targ));
    EST_Item *cand_context = inext(inext(cand));
  
    if ( !targ_context && !cand_context)
        return 0;
    if ( !targ_context  || !cand_context)
        return 1;
  
    return (targ_context->features().val("name").String() == cand_context->features().val("name").String()) ? 0 : 1;
}

float EST_TargetCost::out_of_lex_cost() const
{
    static const EST_String ool_feat("bad_lex");
  
    // bad_dur may at some stage be set on a target for resynthesis purposes.
    if( cand->f_present(ool_feat) 
        != targ->f_present(ool_feat) )
        return 1.0;
  
    if( inext(cand)->f_present(ool_feat) 
        != inext(targ)->f_present(ool_feat) )
        return 1.0;

    return 0.0;
}

float EST_TargetCost::bad_duration_cost() const
{
    static const EST_String bad_dur_feat("bad_dur");
  
    // bad_dur may at some stage be set on a target for resynthesis purposes.
    if( cand->f_present(bad_dur_feat) 
        != targ->f_present(bad_dur_feat) )
        return 1.0;
  
    if( inext(cand)->f_present(bad_dur_feat) 
        != inext(targ)->f_present(bad_dur_feat) )
        return 1.0;
    // If the segments next to these segments are bad, then these ones are probably wrong too!
    if( iprev(cand) && iprev(targ) && ( iprev(cand)->f_present(bad_dur_feat) 
                                        != iprev(targ)->f_present(bad_dur_feat) ) )
        return 1.0;
  
    if( inext(inext(cand)) && inext(inext(targ)) &&
        ( inext(inext(cand))->f_present(bad_dur_feat)
          != inext(inext(targ))->f_present(bad_dur_feat) ) )
        return 1.0;

  
    return 0.0;
}

float EST_TargetCost::bad_f0_cost() const
{
    // by default, the last element of join cost coef vector is
    // the f0 (i.e. fv->a_no_check( fv->n()-1 ) )

    const EST_Item *cand_left = cand;
    const EST_Item *cand_right = inext(cand_left);

    const EST_String &left_phone(  cand_left->features().val("name").String()  );
    const EST_String &right_phone( cand_right->features().val("name").String() );  

    EST_FVector *fv = 0;
    float penalty = 0.0;

    if( ph_is_vowel( left_phone )
        || ph_is_approximant( left_phone )
        || ph_is_liquid( left_phone )
        || ph_is_nasal( left_phone ) ){
        fv = fvector( cand_left->f("midcoef") );
        if( fv->a_no_check(fv->n()-1) == -1.0 ) // means unvoiced
            penalty += 0.5;
    }
  
    if( ph_is_vowel( right_phone )
        || ph_is_approximant( right_phone )
        || ph_is_liquid( right_phone )
        || ph_is_nasal( right_phone ) ){
        fv = fvector( cand_right->f("midcoef") );
        if( fv->a_no_check(fv->n()-1) == -1.0 ) // means unvoiced 
            penalty += 0.5;
    }

    return penalty; 
}


/*
 *  DERIVED CLASS: EST_DefaultTargetCost
 *
 *  This is CSTR's proposed default target cost. Nothing special, if you think you can
 *  do better derive your own class.
 */

float EST_DefaultTargetCost::operator()(const EST_Item* targ, const EST_Item* cand) const 
{ 
    set_targ_and_cand(targ,cand);
    score = 0.0;
    weight_sum = 0.0;

    score += add_weight(10.0)*stress_cost();
    score += add_weight(5.0)*position_in_syllable_cost();
    score += add_weight(5.0)*position_in_word_cost();
    score += add_weight(6.0)*partofspeech_cost();
    score += add_weight(15.0)*position_in_phrase_cost();
    score += add_weight(4.0)*left_context_cost();
    score += add_weight(3.0)*right_context_cost();

    score /= weight_sum;

    // These are considered really bad, and will result in a score > 1.
    score += 10.0*bad_duration_cost(); // see also join cost.
    score += 10.0*bad_f0_cost();
    score += 10.0*punctuation_cost();
    score += 10.0*out_of_lex_cost();

    return score ;
}

/*
 *  DERIVED CLASS: EST_APMLTargetCost
 *
 */

float EST_APMLTargetCost::operator()(const EST_Item* targ, const EST_Item* cand) const 
{ 
    set_targ_and_cand(targ,cand);
    score = 0.0;
    weight_sum = 0.0;

    score += add_weight(10.0)*stress_cost();
    score += add_weight(20.0)*apml_accent_cost(); // APML only!
    score += add_weight(5.0)*position_in_syllable_cost();
    score += add_weight(5.0)*position_in_word_cost();
    score += add_weight(6.0)*partofspeech_cost();
    score += add_weight(4.0)*position_in_phrase_cost();
    score += add_weight(4.0)*left_context_cost();
    score += add_weight(3.0)*right_context_cost();

    score /= weight_sum;

    score += 10.0*bad_duration_cost(); // see also join cost.
    score += 10.0*bad_f0_cost();
    score += 10.0*punctuation_cost();
    score += 10.0*out_of_lex_cost();

    return score;

}

/*
 *  DERIVED CLASS: EST_SingingTargetCost
 *
 *  Mostly default stuff, but tries to match pitch and duration
 *  specified on Tokens from the xxml
 *  
 */

float EST_SingingTargetCost::pitch_cost() const
{
    const EST_Item *targ_word = tc_get_word(targ);
    const EST_Item *cand_word = tc_get_word(cand);
    const EST_Item *next_targ_word = tc_get_word(inext(targ));
    const EST_Item *next_cand_word = tc_get_word(inext(cand));
    const float threshold = 0.1;
    float targ_pitch, cand_pitch;
    LISP l_tmp;

    float score = 0.0;

    if ( (targ_word && !cand_word) || (!targ_word && cand_word) )
    {
        cout << "PITCH PENALTY WORD NON-WORD MISMATCH\n";		    
        score += 0.5;
    }
    else
        if (targ_word && cand_word)
        {
	
            l_tmp = lisp_val(parent(targ_word,"Token")->f("freq",est_val(0)));

            // This currently assumes one syllable words, need to process
            // the list more for multiple syllable words, or move the info
            // to the syllable.
            if(CONSP(l_tmp))
                targ_pitch = get_c_float(car(l_tmp));
            else
                targ_pitch = get_c_float(l_tmp);
            cand_pitch = parent(cand_word,"Token")->F("freq",0.0);

            if ( ! threshold_equal(targ_pitch,cand_pitch,threshold))
            {
                cout << "PP: " << targ_pitch << " " << cand_pitch << endl;
                score += 0.5;
            }
        }

    if ( (next_targ_word && !next_cand_word) || (!next_targ_word && next_cand_word) )
    {
        cout << "PITCH PENALTY NEXT WORD NON-WORD MISMATCH\n";		    
        score += 0.5;
    }
    else
        if(next_targ_word && next_cand_word)
        {
            l_tmp = lisp_val(parent(next_targ_word,"Token")->f("freq",est_val(0)));
            if(CONSP(l_tmp))
                targ_pitch = get_c_float(car(l_tmp));
            else
                targ_pitch = get_c_float(l_tmp);
            cand_pitch = parent(next_cand_word,"Token")->F("freq",0.0);

            if ( ! threshold_equal(targ_pitch,cand_pitch,threshold))
            {
                cout << "NP: "  << targ_pitch << " " << cand_pitch << endl;
                score += 0.5;
            }
        }
  
    if (score == 0.0)
        cout << "NO PITCH PENALTY\n";

    return score;
}

float EST_SingingTargetCost::duration_cost() const
{
    const EST_Item *targ_word = tc_get_word(targ);
    const EST_Item *cand_word = tc_get_word(cand);
    const EST_Item *next_targ_word = tc_get_word(inext(targ));
    const EST_Item *next_cand_word = tc_get_word(inext(cand));
    float targ_dur, cand_dur;
    LISP l_tmp;

    float score = 0.0;

    if ( (targ_word && !cand_word) || (!targ_word && cand_word) )
        score += 0.5;
    else
        if (targ_word && cand_word)
        {
            l_tmp = lisp_val(parent(targ_word,"Token")->f("dur",est_val(0)));
            if(CONSP(l_tmp))
                targ_dur = get_c_float(car(l_tmp));
            else
                targ_dur = get_c_float(l_tmp);

            cand_dur = parent(cand_word,"Token")->F("dur",0.0);

            if ( targ_dur != cand_dur )
                score += 0.5;
        }

    if ( (next_targ_word && !next_cand_word) || (!next_targ_word && next_cand_word) )
        score += 0.5;
    else
        if(next_targ_word && next_cand_word)
        {
            l_tmp = lisp_val(parent(next_targ_word,"Token")->f("dur",est_val(0)));
            if(CONSP(l_tmp))
                targ_dur = get_c_float(car(l_tmp));
            else
                targ_dur = get_c_float(l_tmp);
            cand_dur = parent(next_cand_word,"Token")->F("dur",0.0);

            if ( targ_dur != cand_dur )
                score += 0.5;
        }
  
    return score;
}



float EST_SingingTargetCost::operator()(const EST_Item* targ, const EST_Item* cand) const 
{ 
    set_targ_and_cand(targ,cand);
    score = 0.0;
    weight_sum = 0.0;

    score += add_weight(50.0)*pitch_cost();
    score += add_weight(50.0)*duration_cost();
    score += add_weight(5.0)*stress_cost();
    score += add_weight(5.0)*position_in_syllable_cost();
    score += add_weight(5.0)*position_in_word_cost();
    score += add_weight(5.0)*partofspeech_cost();
    score += add_weight(5.0)*position_in_phrase_cost();
    score += add_weight(5.0)*punctuation_cost();
    score += add_weight(4.0)*left_context_cost();
    score += add_weight(3.0)*right_context_cost();
    score += add_weight(2.0)*bad_duration_cost(); // see also join cost.

    return score / weight_sum;
}



/*
 *  DERIVED CLASS: EST_SchemeTargetCost
 *
 *  This lets you implement your target cost in scheme, so you can
 *  change it on the fly. Great for developement, but about 5 times as slow.
 *  
 */


float EST_SchemeTargetCost::operator()( const EST_Item* targ, const EST_Item* cand ) const
 { 
   LISP r,l;

   l = cons(tc,
 	   cons( siod(targ), cons( siod(cand), NIL) ));
   r = leval(l,NIL);
   if ((consp(r)) || (r == NIL) || !(numberp(r)))
     {
       cerr << "Lisp function: " << tc << 
 	" did not return float score" << endl;
       festival_error();
     }
   else
     score = get_c_float(r);
  
   return score;  
 }



/* 
 *   Auxillary target cost functions
 */


static const EST_String simple_pos(const EST_String &s)
{
  if( s == "nn" || s == "nnp" || s == "nns" || s == "nnps" || s == "fw" || s == "sym" || s == "ls")
    return "n";
  if( s == "vbd" || s == "vb" || s == "vbn" || s == "vbz" || s == "vbp" || s == "vbg")
    return "v";
  if( s == "jj" || s == "jjr" || s == "jjs" || s == "1" || s == "2" || s == "rb" || 
      s == "rp" || s == "rbr" || s == "rbs")
    return "other";
  return "func";
 }
 
static const EST_Utterance *tc_get_utt(const EST_Item *seg)
{
  return seg->relation()->utt();
}

static const EST_Item *tc_get_syl(const EST_Item *seg)
{
  //  if(!seg)
  //  return 0;
  
  return parent(seg,"SylStructure");
}

static const EST_Item *tc_get_word(const EST_Item *seg)
 {
   // if(!seg)
   //  return 0;
   const EST_Item *syl = tc_get_syl(seg);
   
   if(syl)
     return parent(syl,"SylStructure");
   else
     return 0;
 }


/* adapted from base/ff.cc */
static EST_String ff_tobi_accent(const EST_Item *s)
{
    // First tobi accent related to syllable
    EST_Item *nn = as(s,"Intonation");
    EST_Item *p;

    for (p=daughter1(nn); p; p=inext(p))
	if (p->name().contains("*"))
	    return p->name();
    return "NONE";
}

static EST_String ff_tobi_endtone(const EST_Item *s)
{
    // First tobi endtone (phrase accent or boundary tone)
    EST_Item *nn = as(s,"Intonation");
    EST_Item *p;

    for (p=daughter1(nn); p; p=inext(p))
    {
	EST_String l = p->name();
	if ((l.contains("%")) || (l.contains("-")))
	    return p->name();
    }

    return "NONE";
}

static bool threshold_equal(float a, float b, float threshold)
{
  if ( ( (a-b) < threshold ) && ( (a-b) > -threshold ) )
    return true;
  else
    return false;
}
