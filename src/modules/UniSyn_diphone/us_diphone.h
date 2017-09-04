/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                       Copyright (c) 1996,1997                         */
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
/*                                                                       */
/*                 Author: Paul Taylor                                   */
/*                   Date: 6 Jan 1998                                    */
/* --------------------------------------------------------------------- */
/*            LPC residual synthesis alternative version                 */
/*                                                                       */
/*************************************************************************/

#ifndef __US_DIPHONE_H__
#define __US_DIPHONE_H__

#include "EST_FileType.h"
#include "EST_TVector.h"
#include "EST_THash.h"
#include "EST_Token.h"
#include "ling_class/EST_Relation.h"

SIOD_REGISTER_CLASS_DCLS(us_db,USDiphIndex)
VAL_REGISTER_CLASS_DCLS(us_db,USDiphIndex)

class USDiphIndex {
public:
    USDiphIndex();
    ~USDiphIndex();

    EST_String name;
    EST_String index_file;
    EST_String group_file;
    EST_String track_file_format;
    EST_String sig_file_format;
    bool grouped;
    int index_offset;
    EST_TokenStream ts; // for grouped diphones

    EST_String coef_dir;
    EST_String sig_dir;

    EST_String coef_ext;
    EST_String sig_ext;
    LISP params;

    EST_TVector<EST_Item> diphone;
    EST_TStringHash<int> dihash;
};

int read_diphone_database(const EST_String &filename, USDiphIndex &index);
void check_us_db();

void load_separate_diphone(int unit, bool keep_full, 
			   const EST_String &cut_type="all");

int find_diphone_index(const EST_Item &d);

void parse_diphone_times(EST_Relation &diphone_stream, 
				EST_Relation &source_lab);

void us_get_diphones(EST_Utterance &utt);

void us_full_cut(EST_Relation &unit);

LISP us_check_diphone_presence(LISP name);


#endif // __US_DIPHONE_H__

