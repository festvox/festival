/*************************************************************************/
/*                                                                       */
/*                   Carnegie Mellon University and                      */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                       Copyright (c) 1998-2000                         */
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
/*  THE UNIVERSITY OF EDINBURGH, CARNEGIE MELLON UNIVERSITY AND THE      */
/*  CONTRIBUTORS TO THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO     */
/*  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY   */
/*  AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF EDINBURGH, CARNEGIE */
/*  MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE FOR ANY SPECIAL,    */
/*  INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER          */
/*  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  AN ACTION   */
/*  OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF     */
/*  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.       */
/*                                                                       */
/*************************************************************************/

#ifndef __CLUNITS_H__
#define __CLUNITS_H__

#include "EST_StringTrie.h"

class CLunit {
  public:
    CLunit();
    ~CLunit();
    
    EST_String fileid;
    EST_String name;
    EST_String base_name;
    float start;
    float mid;
    float end;
    class CLunit *prev_unit;
    class CLunit *next_unit;
    int samp_start;
    int samp_end;
    int middle_frame;
    EST_Track *join_coeffs;
    EST_Track *coefs;
    EST_Wave *sig;
};

class CLfile {
  public:
    CLfile();
    ~CLfile();
    
    EST_Track *join_coeffs;
    EST_Track *coefs;
    EST_Wave *sig;
};

class CLDB {
  public:
    CLDB();
    ~CLDB();

    LISP params;
    EST_StringTrie index;
    EST_StringTrie fileindex;
    EST_FVector cweights;

    CLunit *get_unit(const EST_String &name)
	{ return (CLunit *)index.lookup(name); }
    CLfile *get_fileitem(const EST_String &name)
	{ return (CLfile *)fileindex.lookup(name); }
    void load_coefs_sig(EST_Item *unit);
    CLfile *get_file_coefs_sig(const EST_String &fileid);
    void load_join_coefs(CLunit *unit);
    CLfile *get_file_join_coefs(const EST_String &fileid);
};


LISP cl_load_db(LISP params);
LISP acost_utt_load_coeffs(LISP utt, LISP params);
LISP make_unit_distance_tables(LISP unittypes, LISP params);
LISP ac_distance_tracks(LISP filename1, LISP filename2, LISP lweights);
void acost_dt_params(LISP params);
float ac_unit_distance(const EST_Track &unit1,
		       const EST_Track &unit2,
		       const EST_FVector wghts);
float frame_distance(const EST_Track &a, int ai,
		     const EST_Track &b, int bi,
		     const EST_FVector &wghts,
		     float f0_weight);
void cl_maybe_fix_pitch_c0(EST_Track *c);

CLDB *check_cldb();
LISP cldb_list(void);
LISP cldb_select(LISP dbname);
LISP l_cl_mapping(LISP utt, LISP method);

#endif
