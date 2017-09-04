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
/*                   Date: February 1998                                 */
/* --------------------------------------------------------------------- */
/*             Waveform Generation Scheme Interface File                 */
/*                                                                       */
/*************************************************************************/

#include "EST.h"
#include "us_diphone.h"
#include "festival.h"

extern USDiphIndex *diph_index;
LISP us_dbs = NIL;
LISP us_make_group_file(LISP lname, LISP params);

int read_diphone_index(const EST_String &filename, 
			  USDiphIndex &di);
void us_check_db();


void us_add_diphonedb(USDiphIndex *db);

LISP FT_us_get_diphones(LISP lutt)
{
    EST_Utterance *utt = get_c_utt(lutt);

    us_get_diphones(*utt);

    return lutt;
}

LISP us_select_db(LISP name)
{
    // Select diphone set
    LISP lpair;
    
    lpair = siod_assoc_str(get_c_string(name),us_dbs);
    
    if (lpair == NIL)
	EST_error("US DB: no diphone database named \"%s\" defined\n",
		  (const char *) get_c_string(name));
    else
	diph_index = us_db(car(cdr(lpair)));

    return name;
}

LISP us_list_dbs(void)
{
    // List names of currently loaded dbs 
    LISP names,n;

    for (names=NIL,n=us_dbs; n != NIL; n=cdr(n))
	names = cons(car(car(n)),names);
    return reverse(names);
}

LISP us_db_params(void)
{
    // Return parameters of current db

    us_check_db();
    return diph_index->params;
}

LISP us_diphone_init(LISP args)
{
    EST_String x;
    USDiphIndex *d_index = new USDiphIndex;
    d_index->grouped = false;
    d_index->params = args;  
    d_index->name = get_param_str("name",args,"name");
    d_index->index_file = get_param_str("index_file",args,"");

    read_diphone_index(d_index->index_file, *d_index);

    // This is needed because there is no get_param_EST_String function
    x = get_param_str("grouped",args,"");
    if (x == "true")
    {
	d_index->grouped = true;
	if (d_index->ts.open(d_index->index_file) != 0)
	{
	    cerr << "US DB: can't open grouped diphone file " 
		<< d_index->index_file << endl;
	    festival_error();
	}
	// set up the character constant values for this stream
	d_index->ts.set_SingleCharSymbols(";");
    }
    else
    {
	*cdebug << ":" << get_param_str("grouped",args,"") << ":" << endl;
	*cdebug << "index grouped:" << d_index->grouped << endl;
	*cdebug << "true:" << true << endl;
	*cdebug << "false:" << false << endl;
	
	d_index->coef_dir = get_param_str("coef_dir",args,"");
	d_index->sig_dir = get_param_str("sig_dir",args,"");
	
	d_index->coef_ext = get_param_str("coef_ext",args,"");
	d_index->sig_ext = get_param_str("sig_ext",args,"");
    }

    us_add_diphonedb(d_index);

    return rintern(d_index->name);
}

LISP FT_us_full_cut(LISP lutt, LISP lrname)
{
    EST_Utterance *utt = get_c_utt(lutt);
    EST_String rname = get_c_string(lrname);

    us_full_cut(*utt->relation(rname));

//    parse_diphone_times(*(utt->relation(rname)), 
//			*(utt->relation("SourceSegments")));

    return lutt;
}

void festival_UniSyn_diphone_init(void)
{
    proclaim_module("UniSyn_diphone");

    init_subr_0("us_list_dbs", us_list_dbs,
    "(us_list_dbs)\n\
    List names of UniSyn databases currently loaded.");

    init_subr_0("us_db_params", us_db_params,
    "(us_db_params)\n\
    Return parameters of current UniSyn database.");

    init_subr_1("us_db_select", us_select_db,
    "(us_db_select NAME)\n\
    Select named UniSyn database.");

    init_subr_1("us_get_diphones", FT_us_get_diphones,
    "(us_get_synthesis UTT)\n\
    Construct a unit stream in UTT comprising suitable diphones. The unit \n\
     stream produced is suitable for immediate use in us_ps_synthesis.");

    init_subr_2("us_make_group_file", us_make_group_file,
    "(us_make_group_file FILENAME PARAMS)\n\
    Make a group file from the currently specified diphone set.  PARAMS \n\
    is an optional assoc list and allows specification of the \n\
    track_file_format (default est_binary), sig_file_format (default \n\
    snd) and sig_sample_format (default mulaw).  This is recommended \n\
    for LPC databases.  For PSOLA based databases the sig_sample_format \n\
    should probably be set to short.");

    init_subr_2("us_full_cut", FT_us_full_cut,
    "(us_ps_synthesis UTT SIGPR)\n\
    Synthesize utterance UTT using signal processing technique SIGPR \n\
    for the UniSyn pitch-synchronous synthesizer.");

    init_subr_1("us_diphone_init", us_diphone_init,
    "(us_diphone_init DIPHONE_NAME)\n\
    Initialise UniSyn diphone synthesizer with database DIPHONE_NAME.");

    init_subr_1("diphone_present", us_check_diphone_presence,
    "(diphone_present? STR)\n\
  Checks whether the given STRing corresponds to any diphone in the\n\
  current database.");
    
}
