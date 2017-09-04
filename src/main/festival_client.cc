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
/*             Date   :  December 1996                                   */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Client program used to send comands/data to a festival server         */
/*                                                                       */
/*=======================================================================*/

#include <cstdio>

using namespace std;

#include "EST_unix.h"
#include "festival.h"

#ifdef WIN32
typedef HANDLE SERVER_FD;
#else
typedef FILE *SERVER_FD;
#endif

static void festival_client_main(int argc, char **argv);
static void copy_to_server(FILE *fdin,SERVER_FD serverfd);
static void ttw_file(SERVER_FD serverfd, const EST_String &file);
static void client_accept_waveform(SERVER_FD fd);
static void client_accept_s_expr(SERVER_FD fd);
static void new_state(int c, int &state, int &bdepth);

static EST_String output_filename = "-";
static EST_String output_type = "riff";
static EST_String tts_mode = "nil";
static EST_String prolog = "";
static int withlisp = FALSE;
static EST_String aucommand = "";
static int async_mode = FALSE;

// So that festival_error works (and I don't need the whole of libFestival.a)
void festival_tidy_up() { return; }

int main(int argc, char **argv)
{

    festival_client_main(argc,argv);

    return 0;
}

static void festival_client_main(int argc, char **argv)
{
    EST_Option al;
    EST_StrList files;
    EST_String server;
    int port;
    FILE *infd;

    parse_command_line(argc, argv, 
	EST_String("Usage:\n")+
        "festival_client <options> <file0> <file1> ...\n"+
	"Access to festival server process\n"+
	"--server <string>   hostname (or IP number) of server\n"+
	"--port <int> {1314} port number of server process (1314)\n"+
	"--output <string>   file to save output waveform to\n"+
	"--otype <string> {riff}\n"
        "                    output type for waveform\n"+
	"--passwd <string>   server passwd in plain text (optional)\n"+
	"--prolog <string>   filename containing commands to be sent\n"+
        "                    to the server before standard commands\n"+
        "                    (useful when using --ttw)\n"+
	"--async             Asynchronous mode, server may send back\n"+
        "                    multiple waveforms per text file\n"+
	"--ttw               Text to waveform: take text from first\n"+
        "                    arg or stdin get server to return\n"+
        "                    waveform(s) stored in output or operated\n"+
        "                    on by aucommand.\n"+
        "--withlisp          Output lisp replies from server.\n"+
        "--tts_mode <string> TTS mode for file (default is fundamental).\n"+
        "--aucommand <string>\n"+
        "                    command to be applied to each\n"+
        "                    waveform retruned from server.  Use $FILE\n"+
        "                    in string to refer to waveform file\n",
			files, al);

    if (al.present("--server"))
	server = al.val("--server");
    else
	server = "localhost";

    if (al.present("--port"))
	port = al.ival("--port");
    else
	port = FESTIVAL_DEFAULT_PORT;

    if (al.present("--tts_mode"))
	tts_mode = al.val("--tts_mode");

    if (al.present("--output"))
	output_filename = al.val("--output");
    if (al.present("--otype"))
    {
	output_type = al.val("--otype");
	if (!output_type.matches(RXalphanum))
	{
	    cerr << "festival_client: invalid output type \"" 
		<< output_type << "\"" << endl;
	    exit(-1);
	}
    }

    // Specify what to do with received waveform
    if (al.present("--aucommand"))
	aucommand = al.val("--aucommand");

    if (al.present("--withlisp"))
	withlisp = TRUE;

    if (al.present("--async"))
	async_mode = TRUE;
    else
	async_mode = FALSE;

    int fd = festival_socket_client(server,port);
#ifdef WIN32
    HANDLE serverfd = (HANDLE)fd;
#else
    FILE *serverfd = fdopen(fd,"wb");
#endif

    if (al.present("--passwd"))
#ifdef WIN32
	{
		DWORD bytes_written;
		DWORD passwdlen = (DWORD)strlen(al.val("--passwd"))+1;
		char *buffer = new char[passwdlen];
		sprintf(buffer,"%s\n",al.val("--passwd"));
		bytes_written = send((SOCKET)serverfd,buffer,passwdlen,0);
		if (SOCKET_ERROR == bytes_written || bytes_written < passwdlen)
		{
			GetLastError();  // at least get the error
			cerr << "festival_client:  can't send password to server\n";
		}
		delete [] buffer;
	}
#else
	fprintf(serverfd,"%s\n",(const char *)al.val("--passwd"));
#endif

    if (al.present("--prolog"))
    {
	FILE *pfd = fopen(al.val("--prolog"),"rb");
	if (pfd == NULL)
	{
	    cerr << "festival_client: can't open prolog file \"" 
		<< al.val("--prolog") << "\"" << endl;
	    exit(-1);
	}
	copy_to_server(pfd,serverfd);
	fclose(pfd);
    }

    if (al.present("--ttw"))
	ttw_file(serverfd,files.nth(0));
    else
    {
	if ((files.length() == 0) || (files.nth(0) == "-"))
	    copy_to_server(stdin,serverfd);
	else 
	{
	    if ((infd=fopen(files.nth(0),"rb")) == NULL)
	    {
		cerr << "festival_client: can't open \"" <<
		    files.nth(0) << "\"\n";
		exit(-1);
	    }
	    copy_to_server(infd,serverfd);
	}
    }

    return;
}

static void ttw_file(SERVER_FD serverfd, const EST_String &file)
{
    // text to waveform file.  This includes the tts wraparounds for
    // the text in file and outputs a waveform in output_filename
    // This is done as *one* waveform.  This is designed for short
    // dialog type examples.  If you need spooling this isn't the
    // way to do it
    EST_String tmpfile = make_tmp_filename();
    FILE *fd, *tfd;
    int c;
    
    if ((fd=fopen(tmpfile,"wb")) == NULL)
    {
	cerr << "festival_client: can't open tmpfile \"" <<
	    tmpfile << "\"\n";
	exit(-1);
    }
    // Here we ask for NIST because its a byte order aware headered format
    // the eventual desired format might be unheadered and if we asked the
    // the server for that we wouldn't know if it required byte swap or 
    // not.  The returned wave data from the server is actually saved
    // to a file and read in by EST_Wave::load so NIST is a safe option
    // Of course when the wave is saved by the client the requested 
    // format is respected.
    fprintf(fd,"(Parameter.set 'Wavefiletype 'nist)\n");
    if (async_mode)
    {   // In async mode we need to set up tts_hooks to send back the waves
	fprintf(fd,"(tts_return_to_client)\n");
	fprintf(fd,"(tts_text \"\n");
    }
    else   // do it in one go
	fprintf(fd,"(tts_textall \"\n");
    if (file == "-")
	tfd = stdin;
    else if ((tfd=fopen(file,"rb")) == NULL)
    {
	cerr << "festival_client: can't open text file \"" <<
	    file << "\"\n";
	exit(-1);
    }

    while ((c=getc(tfd)) != EOF)
    {
	if ((c == '"') || (c == '\\'))
	    putc('\\',fd);
	putc(c,fd);
    }
    if (file != "-")
	fclose(tfd);

    fprintf(fd,"\" \"%s\")\n",(const char *)tts_mode);

    fclose(fd);

    // Now send the file to the server
    if ((fd=fopen(tmpfile,"rb")) == NULL)
    {
	cerr << "festival_client: tmpfile \"" <<
	    tmpfile << "\" mysteriously disappeared\n";
	exit(-1);
    }
    copy_to_server(fd,serverfd);
    fclose(fd);
    unlink(tmpfile);
}

static void copy_to_server(FILE *fdin,SERVER_FD serverfd)
{
    // Open a connection and copy everything from stdin to 
    // server
    int c,n;
    int state=0;
    int bdepth=0;
    char ack[4];

    while((c=getc(fdin)) != EOF)
    {
#ifdef WIN32
	DWORD bytes_pending;
	n = send((SOCKET)serverfd,(const char *)&c,1,0);
	if (SOCKET_ERROR == n || 0 == n)
	{
		if (SOCKET_ERROR == n)
			GetLastError();
		cerr << "festival_client: couldn't copy to server\n";
	}
#else
	putc(c,serverfd);
#endif
	new_state(c,state,bdepth);

	if (state == 1)
	{
	    state = 0;
#ifndef WIN32
	    fflush(serverfd);
#endif
	    do {
#ifdef WIN32
		{
			for (n=0; n < 3; )
			{
				int bytes_read = recv((SOCKET)serverfd,ack+n,3-n,0);
				if (SOCKET_ERROR == bytes_read)
				{
					GetLastError();
					cerr << "festival_client: error reading from server\n";
				}
				else n+= bytes_read;
			}
		}
#else
		for (n=0; n < 3; )
		    n += read(fileno(serverfd),ack+n,3-n);
#endif
		ack[3] = '\0';
		if (streq(ack,"WV\n"))    // I've been sent a waveform
		    client_accept_waveform(serverfd);
		else if (streq(ack,"LP\n"))    // I've been sent an s-expr
		{
		    client_accept_s_expr(serverfd);
		}
		else if (streq(ack,"ER\n"))
		{
		    cerr << "festival server error: reset to top level\n";
		    break;
		}
	    } while (!streq(ack,"OK\n"));
	}
    }
}

static void new_state(int c, int &state, int &bdepth)
{
    // FSM (plus depth) to detect end of s-expr

    if (state == 0)
    {
	if ((c == ' ') || (c == '\t') || (c == '\n') || (c == '\r'))
	    state = 0;
	else if (c == '\\')   // escaped character
	    state = 2;
	else if (c == ';')
	    state = 3;  // comment
	else if (c == '"')
	    state = 4;  // quoted string
	else if (c == '(')
	{
	    bdepth++;
	    state = 5;
	}
	else
	    state = 5;  // in s-expr
    }
    else if (state == 2)
	state = 5;      // escaped character
    else if (state == 3)
    {
	if (c == '\n')
	    state = 5;
	else
	    state = 3;
    }
    else if (state == 4)
    {
	if (c == '\\')
	    state = 6;
	else if (c == '"')
	    state = 5;
	else
	    state = 4;
    }
    else if (state == 6)
	state = 4;
    else if (state == 5)
    {
	if ((c == ' ') || (c == '\t') || (c == '\n') || (c == '\r'))
	{
	    if (bdepth == 0)
		state = 1;
	    else
		state = 5;
	}
	else if (c == '\\')   // escaped character
	    state = 2;
	else if (c == ';')
	    state = 3;  // comment
	else if (c == '"')
	    state = 4;  // quoted string
	else if (c == '(')
	{
	    bdepth++;
	    state = 5;
	}
	else if (c == ')')
	{
	    bdepth--;
	    state = 5;
	}
	else
	    state = 5;  // in s-expr
    }
    else  // shouldn't get here
	state = 5;
}

static void client_accept_waveform(SERVER_FD fd)
{
    // Read a waveform from fd.  The waveform will be passed
    // using 
    EST_String tmpfile = make_tmp_filename();
    EST_Wave sig;

    // Have to copy this to a temporary file, then load it.
#ifdef WIN32
    socket_receive_file((SOCKET)fd,tmpfile);
#else
    socket_receive_file(fileno(fd),tmpfile);
#endif
    sig.load(tmpfile);
    if (aucommand != "")
    {
	// apply the command to this file 
	EST_String tmpfile2 = make_tmp_filename();
	sig.save(tmpfile2,output_type);
	char *command = walloc(char,1024+tmpfile2.length()+aucommand.length());
	sprintf(command,"FILE=\"%s\"; %s",(const char *)tmpfile2,
		(const char *)aucommand);
	system(command);
	unlink(tmpfile2);
    }
    else if (output_filename == "")
	cerr << "festival_client: ignoring received waveform, no output file"
	    << endl;
    else
	sig.save(output_filename,output_type);
    unlink(tmpfile);
}

static void client_accept_s_expr(SERVER_FD fd)
{
    // Read an s-expression.  Inefficeintly into a file
    EST_String tmpfile = make_tmp_filename();
    FILE *tf;
    int c;
    
    // Have to copy this to a temporary file, then load it.
#ifdef WIN32
    socket_receive_file((SOCKET)fd,tmpfile);
#else
    socket_receive_file(fileno(fd),tmpfile);
#endif
    
    if (withlisp)
    {
	if (( tf = fopen(tmpfile,"rb")) == NULL)
	{
	    cerr << "festival_client: lost an s_expr tmp file" << endl;
	}
	else
	{
	    while ((c=getc(tf)) != EOF)
		putc(c,stdout);
	    fclose(tf);
	}
    }
    
    unlink(tmpfile);
}

