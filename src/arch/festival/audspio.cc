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
/*                Author :  Alan W Black                                 */
/*                Date   :  September 1996                               */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Interface with the audio spooler                                      */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "EST_unix.h"
#include <cstdlib>
#include "festival.h"
#include "festivalP.h"

#ifdef NO_SPOOLER
void audsp_play_wave(EST_Wave *w) { cerr << "no spooler available\n"; }
LISP l_audio_mode(LISP mode) { return NIL; }
#else

#include <sys/types.h>
#include <sys/wait.h>

static int start_sub_process(int *fds,int argc,char **argv);
static char **enargen(const char *command,int *argc);
static void audsp_send(const char *c);
static int *pipe_open(const char *command);
static void pipe_close(int *fds);

static int *audfds;
static int audsp_num=0;
static int audsp_pid = 0;

void audsp_play_wave(EST_Wave *w)
{
    EST_String tpref = make_tmp_filename();
    char *tmpfilename = walloc(char,tpref.length()+20);
    sprintf(tmpfilename,"%s_aud_%05d",(const char *)tpref,audsp_num++);
    w->save(tmpfilename,"nist");
    audsp_send(EST_String("play ")+tmpfilename+EST_String(" ")+
	       itoString(w->sample_rate()));
    wfree(tmpfilename);
}

static void audsp_send(const char *c)
{
    char reply[4];
    int  pid;
    int statusp;

    pid = waitpid((pid_t)audsp_pid,&statusp,WNOHANG);
    if (pid != 0)
    {
	cerr << "Audio spooler has died unexpectedly" << endl;
	audsp_mode = FALSE;
	festival_error();
    }
	
    write(audfds[0],c,strlen(c));
    write(audfds[0],"\n",1);
    read(audfds[1],reply,3);  /* confirmation */
}

LISP l_audio_mode(LISP mode)
{
    // Switch audio mode
    LISP audio=NIL;
    LISP command=NIL;
    
    if (mode == NIL)
    {
	cerr << "audio_mode: nil is not a valid mode\n";
	festival_error();
    }
    else if (streq("async",get_c_string(mode)))
    {   // Asynchronous mode using the audio spooler.
	if (audsp_mode == FALSE)
	{
	    audio = ft_get_param("Audio_Method");
	    command = ft_get_param("Audio_Command");
	    audfds = pipe_open("audsp");
	    if (audio != NIL)
		audsp_send(EST_String("method ")+get_c_string(audio));
	    if (command != NIL)
	    {
		// command needs to be a single line so delete an newlines
		EST_String flattened = get_c_string(command);
		flattened.gsub("\\\n"," ");
		flattened.gsub("\n"," ");
		audsp_send(EST_String("command ")+flattened);
	    }
	    if ((audio = ft_get_param("Audio_Required_Rate")) != NIL)
		audsp_send(EST_String("rate ")+get_c_string(audio));
	    if ((audio = ft_get_param("Audio_Required_Format")) != NIL)
		audsp_send(EST_String("otype ")+get_c_string(audio));
	    if ((audio = ft_get_param("Audio_Device")) != NIL)
		audsp_send(EST_String("device ")+get_c_string(audio));
	    audsp_mode = TRUE;
	}
    }
    else if (streq("sync",get_c_string(mode)))
    {
	// Synchronous mode
	if (audsp_mode)
	    pipe_close(audfds);
	audsp_mode = FALSE;
    }
    else if (streq("shutup",get_c_string(mode)))
    {
	if (audsp_mode)
	    audsp_send("shutup");
	else
	{
	    cerr << "audio_mode: not in async mode, can't shutup\n";
	    festival_error();
	}
    }
    else if (streq("close",get_c_string(mode)))
    {   // return only when queue is empty
	if (audsp_mode)
	    audsp_send("close");
    }
    else if (streq("query",get_c_string(mode)))
    {
	if (audsp_mode)
	    audsp_send("query");
	else
	{
	    cerr << "audio_mode: not in async mode, can't query\n";
	    festival_error();
	}
    }
    else
    {
	cerr << "audio_mode: unknown mode \"" << get_c_string(mode) <<
	    "\"\n";
	festival_error();
    }

    return mode;
}

static void pipe_close(int *fds)
{
    // Close down the pipes
    close(fds[0]);
    close(fds[1]);
}

static int *pipe_open(const char *command)
{
    // Starts a subprocess with its stdin and stdout bounad to pipes
    // the ends of which are returned in an array
    int argc;
    char **argv;
    int *fds;

    argv = enargen(command,&argc);
    fds = walloc(int,2);

    if (start_sub_process(fds,argc,argv) != 0)
    {
	cerr << "pipe_open: failed to start subprocess: \n" << endl;
	cerr << "pipe_open: \"" << command << "\"\n";
	festival_error();
    }

    return fds;
}

static int start_sub_process(int *fds, int argc, char **argv)
{
    // start sub_process with stdin and stdout bound to pipes whose ends
    // are in fds[0] and fds[1]
    int pid;
    int in[2];
    int out[2];
    (void)argc;

    if ((pipe(in) != 0) ||
	(pipe(out) != 0))
    {
	cerr << "pipe_open: failed to open pipes\n";
	festival_error();
    }

    switch (pid=fork())
    {
      case 0:              /* child */
	close(in[1]);          /* close the end child isn't using */
	dup2(in[0],0);         /* reassign stdin to the pipe */
	close(out[0]);
	dup2(out[1],1);        /* reassign stdout to the pipe */
	execvp(argv[0],argv);
	cerr << "pipe_open: failed to start " << argv[0] << endl;
	exit(-1);      /* should only get here on failure */
      case -1:
	cerr << "pipe_open: fork failed\n";
	festival_error();
      default:             /* parent */
	close(in[0]);          /* Close unused sides of the pipes */
	close(out[1]);
	fds[0] = in[1];
	fds[1] = out[0];
    }

    audsp_pid = pid;
    return 0;
}

static char **enargen(const char *command,int *argc)
{
    EST_TokenStream ts;
    char **argv;
    int i;
    
    ts.open_string(command);
    for (i=0; ts.get() != ""; i++);
    ts.close();
    *argc = i;

    argv = walloc(char *,i+1);
    ts.open_string(command);
    for (i=0; i < *argc; i++)
	argv[i] = wstrdup(ts.get().string());
    argv[i] = 0;

    return argv;
}

#endif
