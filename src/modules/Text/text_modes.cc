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
/*             Author :  Alan W Black                                    */
/*             Date   :  November 1996                                   */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*  Support for general user tts modes                                   */
/*  Each mode consists of user definable parameters for (at least) the   */
/*  the following                                                        */
/*    filter   external Unix program filter                              */
/*    utterance chunk tree:  decision tree to determine end of utterance */
/*    punctuation                                                        */
/*    whitespace                                                         */
/*    token analysis rule                                                */
/*    init function          run before mode is applied                  */
/*    exit function        run after mode is applied                     */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "EST_unix.h"
#include "festival.h"
#include "text.h"
#include "lexicon.h"

static void um_apply_filter(const EST_String &filtername,
			    const EST_String &infile,
			    const EST_String &outname);

void tts_file_user_mode(LISP filename, LISP params)
{

    volatile EST_String tmpname = make_tmp_filename();
    volatile EST_String inname = (EST_String)get_c_string(filename);
    volatile EST_String filter;
    volatile EST_TokenStream ts;
    volatile LISP func;
    jmp_buf *old_errjmp = est_errjmp;
    int old_errjmp_ok = errjmp_ok;

    func = get_param_lisp("init_func",params,NIL);
    if (func != NIL) 
	leval(cons(func,NIL),NIL);   // run initial function if specified

    errjmp_ok = 1;
    est_errjmp = walloc(jmp_buf,1);

    if (setjmp(*est_errjmp)) 
    {
	cerr << "festival: text modes, caught error and tidying up\n";
	if (siod_ctrl_c == TRUE)
	{
	    wfree(est_errjmp); 
	    est_errjmp = old_errjmp;  
	    errjmp_ok = old_errjmp_ok;
	    err("forwarded ctrl_c",NIL);
	}
    }
    else
    {

	filter.ignore_volatile() = get_param_str("filter",params,"");
	um_apply_filter(filter.ignore_volatile(),inname.ignore_volatile(),tmpname.ignore_volatile());

	if (streq("xxml",get_param_str("analysis_type",params,"")))
	  tts_file_xxml(strintern(tmpname.ignore_volatile()));
	else if (streq("xml",get_param_str("analysis_type",params,"")))
	{
	    // As xml support is optional we call it through a LISP
	    // function which wont be defined if its not in this installation
	  leval(cons(rintern("tts_file_xml"),cons(strintern(tmpname.ignore_volatile()),NIL)),NIL);
	}
	else
	    tts_file_raw(strintern(tmpname.ignore_volatile()));
	}
    wfree(est_errjmp); 
    est_errjmp = old_errjmp;  
    errjmp_ok = old_errjmp_ok;

    unlink(tmpname.ignore_volatile());

    func = get_param_lisp("exit_func",params,NIL);
    if (func != NIL)
	leval(cons(func,NIL),NIL);   // run end function if specified
}

void um_apply_filter(const EST_String &filtername,
			    const EST_String &infile,
			    const EST_String &outfile)
{
    // filter the file into standard form
    EST_String command;

    if (access(infile,R_OK) != 0)
    {
	cerr << "TTS user mode: \"" << infile << "\" cannot be accessed" <<
	    endl;
	festival_error();
    }

    if (filtername == "")
    {   // don't bother forking 
	FILE *fdin, *fdout;
	char buff[256];
	int n;
	if ((fdin = fopen(infile,"rb")) == NULL)
	{
	    cerr << "TTS user mode: \"" << infile << "\" cannot be read from" 
		<< endl;
	    festival_error();
	}
	if ((fdout = fopen(outfile,"wb")) == NULL)
	{
	    cerr << "TTS user mode: \"" << outfile << "\" cannot be written to"
		<< endl;
	    festival_error();
	}

	while ((n = fread(buff,1,256,fdin)) > 0)
	    fwrite(buff,1,n,fdout);
	fclose(fdin);
	fclose(fdout);
    }
    else
    {
	command = filtername + " '" + infile + "' > " + outfile;
	system(command);  // should test if this is successful or not
    }
}

