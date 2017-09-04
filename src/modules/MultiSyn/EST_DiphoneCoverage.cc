/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                 (University of Edinburgh, UK)                         */
/*                                                                       */
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
/*                        Author: Rob Clark                              */
/*                          Date: April 2005                             */
/* --------------------------------------------------------------------- */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*************************************************************************/
#include <iostream>
#include "EST_rw_status.h"
#include "festival.h"
#include "ling_class/EST_Item.h"
#include "EST_TargetCost.h"
#include "EST_DiphoneCoverage.h"

/* 
 * This file contains some functions which may be useful for diphone
 * selection. Not quite sure exactly how yet...
 * Target cost should probably be rewritten in terms of these functions.
 */



static EST_String pos_map[4] =
  { "INI", "MED", "FIN", "INT" } ;


static EST_String stress_map[4] = 
  { "UU" , "US" , "SU" , "SS"} ;

static EST_String get_diphone_name(const EST_Item *seg1);
static int get_stress_index(const EST_Item *seg1);
static EST_String get_stress_name(const int index);
static int get_syl_pos_index(const EST_Item *seg1);
static EST_String get_syl_pos_name(const int index);


void EST_DiphoneCoverage::add_stats(const EST_Utterance *utt)
{
    EST_Relation *segs = utt->relation("Segment");
    EST_Item *it=segs->head();

    for( ; inext(it); it=inext(it) )
    {
        if(inext(it))
        {
            EST_String key =
                EST_String::cat(get_diphone_name(it),"-",
                                get_stress_name(get_stress_index(it)),"-",
                                get_syl_pos_name(get_syl_pos_index(it)));
            int val = 0;
            if (strhash.present(key))
            {
                val = strhash.val(key);
                strhash.remove_item(key);
            }
            ++val;
            strhash.add_item(key,val);
        }
    }
}
	
void EST_DiphoneCoverage::print_stats(const EST_String filename)
{
    ostream *outf;
    
    if (filename == "-")
        outf = &cout;
    else
        outf = new ofstream(filename);
    
    EST_THash<EST_String, int>::Entries them;

    for (them.begin(strhash); them; them++)
    {
        *outf << them->k << " " << them->v << "\n";
    }
    
    if (outf != &cout)
	delete outf;
}





static EST_String get_diphone_name(const EST_Item *seg1)
{
    return EST_String::cat(seg1->S("name"),"_",inext(seg1)->S("name"));
}


static int get_stress_index(const EST_Item *seg1)
{
    int i1 = 0, i2=0;

    if( ph_is_vowel(seg1->S("name")) && 
        !ph_is_silence(seg1->S("name")) )
        i1 = (parent(seg1,"SylStructure")->I("stress") > 0) ? 1 : 0;

    if( ph_is_vowel(inext(seg1)->S("name")) && 
        !ph_is_silence(inext(seg1)->S("name")) )
        i2 = (parent(inext(seg1),"SylStructure")->I("stress") > 0) ? 1 : 0;

    return i2+2*i1;
}

static EST_String get_stress_name(const int index)
{ 
  return stress_map[index] ;
}

static int get_syl_pos_index(const EST_Item *seg1)
{
  int pos = TCPOS_MEDIAL;

  const EST_Item *syl = parent(seg1,"SylStructure");
  const EST_Item *next_syl = parent(inext(seg1),"SylStructure");
  const EST_Item *next_next_syl = parent(inext(inext(seg1)),"SylStructure");
  const EST_Item *prev_syl = parent(iprev(seg1),"SylStructure");

  if( syl != next_syl )
    pos = TCPOS_INTER;
   else if( syl != prev_syl)
     pos = TCPOS_INITIAL;
   else if( next_syl != next_next_syl)
     pos = TCPOS_FINAL;

  return pos;
}

static EST_String get_syl_pos_name(const int index)
{
  return pos_map[index] ;
}
  

