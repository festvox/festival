/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                       Copyright (c) 1996-1998                         */
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
/*             Author :  Alan W Black, Paul Taylor, Richard Caley        */
/*                       and others                                      */
/*             Date   :  April 1996                                      */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Top level file for synthesizer                                        */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>

using namespace std;

#include "festival.h"

static void festival_main(int argc, char **argv);
static int festival_check_script_mode(int argc, char **argv);
static void festival_script_mode(int argc, char **argv);

void awb_free_diph_index();

extern "C" {
    void mtrace();
    void muntrace();
}
int main(int argc, char **argv)
{

/*    putenv("MALLOC_TRACE=mallfile");
      mtrace(); */
    festival_main(argc,argv);

/*    awb_free_diph_index();

muntrace(); */
    return 0;
}

static void festival_main(int argc, char **argv)
{
    EST_Option al;
    int stdin_input,interactive;
    EST_Litem *p;
    EST_StrList files;
    int real_number_of_files = 0;
    int heap_size = FESTIVAL_HEAP_SIZE;

    if (festival_check_script_mode(argc,argv) == TRUE)
    {   // Need to check this directly as in script mode args are 
	// passed for analysis in the script itself
	return;
    }

    parse_command_line(argc, argv, 
	EST_String("Usage:\n")+
	"festival  <options> <file0> <file1> ...\n"+
        "In evaluation mode \"filenames\" starting with ( are evaluated inline\n"+
	"Festival Speech Synthesis System: "+ festival_version +"\n"+
	"-q            Load no default setup files\n"+
	"--libdir <string>\n"+
        "              Set library directory pathname\n"+
        "-b            Run in batch mode (no interaction)\n"+
	"--batch       Run in batch mode (no interaction)\n"+
	"--tts         Synthesize text in files as speech\n"+
	"              no files means read from stdin\n"+
	"              (implies no interaction by default)\n"+
	"-i            Run in interactive mode (default)\n"+
	"--interactive\n"+
        "              Run in interactive mode (default)\n"+
	"--pipe        Run in pipe mode, reading commands from\n"+
	"              stdin, but no prompt or return values\n"+
	"              are printed (default if stdin not a tty)\n"+
	"--language <string>\n"+
        "              Run in named language, default is\n"+
	"              english, spanish and welsh are available\n"+
	"--server      Run in server mode waiting for clients\n"+
	"              of server_port (1314)\n"+
	"--script <ifile>\n"+
        "              Used in #! scripts, runs in batch mode on\n"+
	"              file and passes all other args to Scheme\n"+
	"--heap <int> {10000000}\n"+
        "              Set size of Lisp heap, should not normally need\n"+
        "              to be changed from its default\n"+
	"-v            Display version number and exit\n"+
	"--version     Display version number and exit\n",
			files, al);

    if ((al.present("-v")) || (al.present("--version")))
    {
	printf("%s: Festival Speech Synthesis System: %s\n",
	       argv[0],festival_version);
	exit(0);
    }

    if (al.present("--libdir"))
	festival_libdir = wstrdup(al.val("--libdir"));
    else if (getenv("FESTLIBDIR") != 0)
	festival_libdir = getenv("FESTLIBDIR");
    if (al.present("--heap"))
	heap_size = al.ival("--heap");

    festival_initialize(!al.present("-q"),heap_size);

    if (al.present("--language"))
	festival_init_lang(al.val("--language"));
    
    // File processing
    for (p=files.head(); p != 0; p=p->next())
    {
	if (files(p) == "-")  // paul thinks I want the "-" -- I don't
	    continue;
	real_number_of_files++;
	if (al.present("--tts"))
	{
	    if (!festival_say_file(files(p)))
		festival_error();
	}
	else if (files(p).matches(make_regex("^(.*")))
	{
	    if (!festival_eval_command(files(p)))
		festival_error(); // fail if it fails
	}
	else if (!festival_load_file(files(p)))
	    festival_error();
    }

    // What to do about standard input and producing prompts etc.
    if ((al.present("-i")) || (al.present("--interactive")))
    {
	interactive = TRUE;
	stdin_input = TRUE;
    }
    else if ((al.present("--pipe")))
    {
	interactive=FALSE;
	stdin_input = TRUE;
    }
    else if ((al.present("-b")) || (al.present("--batch")) ||
	     (al.present("--tts")))
    {
	interactive=FALSE;
	stdin_input=FALSE;
    }
    else if (isatty(0))  // if stdin is a terminal assume interactive
    {   
	interactive = TRUE;
	stdin_input = TRUE;
    }
    else                     // else assume pipe mode
    {   
	interactive = FALSE;
	stdin_input = TRUE;
    }

    if (al.present("--server"))
	festival_server_mode();        // server mode
    else if ((al.present("--tts")) && (real_number_of_files == 0))
	festival_say_file("-");        // text to speech from files
    else if (stdin_input)
	festival_repl(interactive);    // expect input from stdin

    if (al.present("--tts"))
	festival_wait_for_spooler();   // wait for end of audio output

    return;
}

static int festival_check_script_mode(int argc, char **argv)
{
    // Checks if we are in script mode, i.e. if --script exists
    // which may be possibily be preceeded by --libdir  (and heap ?)
    //

    if (argc == 0)
	return FALSE;
    else if ((argc > 2) && (streq("--script",argv[1])))
    {
	if (getenv("FESTLIBDIR") != 0)
	    festival_libdir = getenv("FESTLIBDIR");
	festival_script_mode(argc,argv);
	return TRUE;
    }
    else if ((argc > 4) && (streq("--script",argv[3]))
	     && (streq("--libdir",argv[1])))
    {
	festival_libdir = wstrdup(argv[2]);
	festival_script_mode(argc,argv);
	return TRUE;
    }
    else
	return FALSE;
}

static void festival_script_mode(int argc, char **argv)
{
    // In script mode the first file arg after -script is interpreted and
    // the remainder are set in the variable argv so the script
    // itself may do what ever it wants
    LISP args;
    const char *siodheapsize;
    int i;

    if (argc < 2)
    {   
	cerr << "festival: script_mode has no file to interpret" << endl;
	return;
    }

    // initialize without loading init files
    siodheapsize = getenv("SIODHEAPSIZE");
    if (siodheapsize)
	festival_initialize(FALSE,atoi(siodheapsize));
    else
	festival_initialize(FALSE,FESTIVAL_HEAP_SIZE);
    
    for (args=NIL,i=3; i < argc; i++)
	args = cons(rintern(argv[i]),args);

    siod_set_lval("argv",reverse(args));
    siod_set_lval("argc",flocons(argc));

    festival_load_file(argv[2]);

    return;
}



