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
/*             Date   :  September 1996                                  */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* An audio file spooler, like lpd.  Reads in commands about files to    */
/* to play, and queues them until any previous requests are finished.    */
/* This allows the synthesizer to get on with synthesizing the next      */
/* utterance.                                                            */
/*                                                                       */
/* Actually this doesn't use anything in Festival, only the speech_tools */
/*=======================================================================*/
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <csignal>

using namespace std;

#include "EST.h"
#include "EST_unix.h"

#ifdef NO_SPOOLER

int main(int argc, char **argv)
{

  printf("Audio spooler not supported\n");
  return 0;
}

#else

class Command {
  private:
    EST_String p_file;
    int p_rate;
  public:
    Command(const EST_String &f, int rate) { p_file=f; p_rate=rate; }
    int rate(void) const { return p_rate; }
    const EST_String &file(void) const { return p_file; }
};

class CQueue_Item {
  public:
    Command *c;
    CQueue_Item *next;
    CQueue_Item(Command *com) { c=com; next=0; }
    ~CQueue_Item() { delete c; if (next != 0) delete next; }
};

class CQueue {
  private:
    CQueue_Item *head;
    CQueue_Item *tail;
  public:
    CQueue() { head = tail = 0; }
    ~CQueue() { delete head; }
    void push(Command *c);
    Command *pop(void);
    void display(void) const;
    int length(void) const;
    void clear(void);
};

static void auspl_main(int argc, char **argv);
static void check_new_input(void);
static char *read_a_line(void);
static void process_command(char *line);
static void check_new_output(void);
static int execute_command(Command *c);
static void load_play_file(Command *c);
static int sp_terminate(void);
static void tidy_up(void);

void CQueue::push(Command *c)
{
    // Put this item on tail
    CQueue_Item *n = new CQueue_Item(c);
    
    if (head == 0)
    {   // first one
	head = n;
	tail = n;
    }
    else
    {
	tail->next = n;
	tail = n;
    }
}    

Command *CQueue::pop(void)
{
    // Pop top from the queue

    if (head == 0)
	return 0;
    else
    {
	Command *c = head->c;
	CQueue_Item *h;
	h = head;
	h->c = 0;
	head = head->next;
	h->next = 0;
	delete h;
	return c;
    }
}

void CQueue::display(void) const
{
    CQueue_Item *t;
    int i;

    cerr << "Command_queue: " << length() << endl;
    for (i=0,t=head; t != 0; t=t->next,i++)
	cerr << " " << i << ": " << t->c->file() << endl;
}

int CQueue::length(void) const
{
    // length of queue
    CQueue_Item *t;
    int i;

    for (i=0,t=head; t != 0; t=t->next)
	i++;

    return i;
}

void CQueue::clear(void)
{
    // Remove all memebers in the queue
    CQueue_Item *t;

    // Somebody has to do it ...
    for (t=head; t != 0; t=t->next)
	unlink(t->c->file());

    delete head;
    head = 0;
    tail = 0;
}

static int no_more_input = FALSE;
static CQueue command_queue;
static int child_pid = 0;
static EST_String current_file;
static EST_Option play_wave_options;
static int maxqueue = 5;
static int pending_close = FALSE;
static int kids = 0;

int main(int argc, char **argv)
{

    auspl_main(argc,argv);

    return 0;
}

static void auspl_main(int argc, char **argv)
{
    EST_Option al;
    EST_StrList files;

    parse_command_line(argc, argv, 
	 EST_String("Usage: audio spooler \n")+
	 "auspl  <options> <file0> <file1> ...\n"+
	 "--method <string>    audio play method\n"+
	 "--command <string>   Unix command to play file, used when\n"+
	 "              method is audio_command\n"+
	 "--maxqueue <int> {5} Maximum number of files in queue\n",
			files, al);

    if (al.present("--method"))
	play_wave_options.add_item("-p",al.val("--method"));
    if (al.present("--command"))
	play_wave_options.add_item("-command",al.val("--command"));
    play_wave_options.add_item("-quality","HIGH");

    if (al.present("--maxqueue"))
	maxqueue = al.ival("--maxqueue");

    while (!sp_terminate())
    {
	check_new_input();
	check_new_output();
    }

    tidy_up();
}

static int sp_terminate(void)
{
    // I'm never very sure of all the conditions necessary to terminate
    
    if (no_more_input && (command_queue.length() == 0))
	return TRUE;
    else
	return FALSE;
}

static void tidy_up(void)
{
    // should wait for any remaining children if I've been
    // requested to.
    int pid;
    int statusp;

    if (pending_close == TRUE)
    {
	while (kids > 0)
	{
	    pid = waitpid(0,&statusp,0);
	    kids--;
	}
	fprintf(stdout,"OK\n");   // give an acknowledgement
	fflush(stdout);
    }

    return;
}

static void check_new_input(void)
{
    // Do a select on stdin to find out if there is any new
    // commands to process
    fd_set inset;
    fd_set outset;
    fd_set exset;
    struct timeval t;
    int sv;

    t.tv_sec = 0;
    t.tv_usec = 1000;  // 0.1 seconds

    FD_ZERO(&inset);
    FD_ZERO(&outset);
    FD_ZERO(&exset);

    if ((command_queue.length() >= maxqueue) ||
	no_more_input)
    {
	// wait a bit for the queue to go down a bit
	// not we're selecting on no fds at all, just for the delay
	sv = select(0,&inset,&outset,&exset,&t);
	return;   
    }

    FD_SET(0,&inset);

    sv = select(1,&inset,&outset,&exset,&t);

    if (sv == 1)
	process_command(read_a_line());
    else if (sv == -1)
	no_more_input = TRUE;
}

static int getc_unbuffered(int fd)
{
    // An attempted to get rid of the buffering
    char c;
    int n;

    n = read(fd,&c,1);

    if (n == 0)
	return EOF;
    else
	return c;
}

static char *read_a_line(void)
{
    // read upto \n on stdin -- wonder if I should read instead
    int maxsize = 1024;
    char *line = walloc(char,maxsize+2);
    int i,c;

    for (i=0; 
	 (((c=getc_unbuffered(0)) != '\n') &&
	  (c != EOF));
	 i++)
    {
	if (i == maxsize)
	{
	    char *nline = walloc(char,maxsize*2);
	    memcpy(nline,line,maxsize);
	    maxsize = maxsize*2;
	    wfree(line);
	    line = nline;
	}
	line[i] = c;
    }

    line[i] = '\n';
    line[i+1] = '\0';
    if (c == EOF)
	no_more_input = TRUE;

    if (strncmp(line,"close",5) != 0)
    {
	fprintf(stdout,"OK\n");   // give an acknowledgement
	fflush(stdout);
    }

    return line;
}

static void process_command(char *line)
{
    // Process command, some are immediate
    EST_TokenStream ts;
    ts.open_string(line);
    EST_String comm = ts.get().string();

    if ((comm == "quit") || (comm == ""))
    {
	no_more_input = TRUE;
    }
    else if (comm == "play")
    {
	EST_String file = ts.get().string();
	int rate = atoi(ts.get().string());
	Command *c = new Command(file,rate);
	command_queue.push(c);
    }
    else if (comm == "method")
    {
	play_wave_options.add_item("-p",ts.get().string());
    }
    else if (comm == "command")
    {
	play_wave_options.add_item("-command",ts.get_upto_eoln().string());
    }
    else if (comm == "rate")
    {
	play_wave_options.add_item("-rate",ts.get().string());
    }
    else if (comm == "otype")
    {
	play_wave_options.add_item("-otype",ts.get().string());
    }
    else if (comm == "device")
    {
	play_wave_options.add_item("-audiodevice",ts.get().string());
    }
    else if (comm == "close")
    {
	pending_close = TRUE;
	no_more_input = TRUE;
    }
    else if (comm == "shutup")
    {
	// clear queue and kill and child currently playing 
	command_queue.clear();
	if (child_pid != 0)
	{
	    kill(child_pid,SIGKILL);
	    unlink(current_file);
	}
    }
    else if (comm == "query")
	command_queue.display();
    else if (comm != "")
    {
	cerr << "audsp: unknown command \"" << comm << "\"\n";
    }

    ts.close();
    wfree(line);
}    

static void check_new_output(void)
{
    // If we are not waiting on any children lauch next command
    int pid;
    int statusp;

    if (kids > 0)
    {
	pid = waitpid(0,&statusp,WNOHANG);
	if (pid != 0)
	{
	    kids--;  
	    child_pid = 0;
	}
    }
    else if (command_queue.length() != 0)
    {
	Command *c = command_queue.pop();
	if (execute_command(c) == 0)
	    kids++;
	delete c;
    }

    // else do nothing
}

static int execute_command(Command *c)
{
    // Execute the command as a child process
    int pid;
    
    current_file = c->file();

    if ((pid=fork()) == 0)
    {   // child process
	load_play_file(c);
	_exit(0);  // don't close any files on exit
	return 0;  // can't get here
    }
    else if (pid > 0)
    {   // parent process
	child_pid = pid;
	return 0;
    }
    else
    {
	cerr << "auspd: fork failed, \"" << c->file() << "\"\n";
	return -1;
    }
}

static void load_play_file(Command *c)
{
    // Load in wave file and play it

    EST_Wave w;

    w.load(c->file());
    play_wave(w,play_wave_options);
    unlink(c->file());   // delete it afterwards
}    
	
#endif
