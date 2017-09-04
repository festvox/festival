/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                         Copyright (c) 1998                            */
/*                        All Rights Reserved.                           */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
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
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   */
/*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*             Author :  Alan W Black and Paul Taylor                    */
/*             Date   :  February 1998                                   */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*         An implementation of Metrical Tree Phonology                  */
/*                                                                       */
/*=======================================================================*/

#ifndef __US_FEATURES_H__
#define __US_FEATURES_H__

#include "festival.h"

EST_Val usf_vowel_start(EST_Item *s);

void add_feature_function(EST_Relation &r, const EST_String &fname,
			  const EST_String &f);

EST_Item *syl_nucleus(EST_Item *syl_struct);
EST_Item *named_daughter(EST_Item *syl, const EST_String &fname, 
			 const EST_String &fval);

EST_Val vowel_start_time(EST_Item *s);

bool verify_utterance_relations(EST_Utterance &u, const EST_String &names, 
				int err);
bool verify_relation_features(EST_Relation &r, const EST_String &names, 
			      int err);
bool verify_relation_features(EST_Utterance &u, const EST_String rname, 
			      const EST_String &names, int err);

EST_Item *nth_leaf(EST_Item *r, int n);
EST_Item *nth(EST_Relation &r, int n);


#endif 
