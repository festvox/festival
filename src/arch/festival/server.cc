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
/*                Author :  Alan Black and Richard Tobin                 */
/*                Date   :  December 1996                                */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Socket support to run Festival as a server process                    */
/*                                                                       */
/* The original socket code is derived Richard Tobin's scokpipe/pipesock */
/* examples, but have be substantially changed to be more general        */
/* for Festival                                                          */
/*                                                                       */
/* The logging, access control, file transfer stuff are all new          */
/*                                                                       */
/*=======================================================================*/
#include <cstdlib>
#include <cstdio>
#include <cerrno>
#include <sys/types.h>
#include <cstring>
#include <ctime>
#include "EST_unix.h"
#include "EST_socket.h"
#include "festival.h"
#include "festivalP.h"

#define DEFAULT_MAX_CLIENTS 10

/* The folloing gives a server that never forks */
/* and only accepts one client at a time.  This is good for */
/* OSs with an expensive implementation of fork and or waitpid (e.g. NT) */
#ifdef WIN32
#define SINGLE_CLIENT 1
#endif


static int client_access_check(int fd,int client);
static EST_String log_time_stamp(int client);
static void log_message(int client,const char *message);

int ft_server_socket = -1;
ostream *cslog = NULL;

int festival_start_server(int port)
{
    // Never exits except by signals
    struct sockaddr_in serv_addr;
    int fd, fd1;
    int statusp;
    int client_name=0;
    int max_clients, num_clients, pid;
    LISP lmax_clients, llog_file;

    lmax_clients = siod_get_lval("server_max_client",NULL);
    if (lmax_clients != NULL)
	max_clients = get_c_int(lmax_clients);
    else
	max_clients = DEFAULT_MAX_CLIENTS;
    num_clients = 0;
    llog_file = siod_get_lval("server_log_file",NULL);
    if (llog_file == NIL)
	cslog = cdebug;
    else if (llog_file == siod_get_lval("t",NULL))
	cslog = &cout;
    else 
	cslog = new ofstream(get_c_string(llog_file),ios::app);
    
    if (!socket_initialise())
    {
	festival_error();
      }
    
    fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

   if (NOT_A_SOCKET(fd))
    {
      int n = socket_error();
      cerr << "socket: socket failed (" << n << ")\n";
	
	festival_error();
    }
   int one = 1;

   if (setsockopt(fd, SOL_SOCKET,SO_REUSEADDR,(char *)&one,sizeof(int)) < 0) 
     {
       cerr << "socket: SO_REUSEADDR failed" << endl;
	festival_error();
     }

    memset(&serv_addr, 0, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    if (bind(fd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) != 0)
    {
	cerr << "socket: bind failed" << endl;
	festival_error();
    }
    
    if (listen(fd, 5) != 0)
    {
	cerr << "socket: listen failed" << endl;
	festival_error();
    }

#if SINGLE_CLIENT
    log_message(0,EST_String("Festival server (non-forking) started on port ")+
		itoString(port));
#else
    log_message(0,EST_String("Festival server started on port ")+
		itoString(port));
#endif

    fflush(stdout);
    fflush(stderr);
    fflush(stdin);

    while(1)                          // never exits except by signals
    {
	if((fd1 = accept(fd, 0, 0)) < 0)
	{
	    cerr << "socket: accept failed";
	    festival_error();
	}

	client_name++;
	if (client_access_check(fd1,client_name) == FALSE)
	{
	    close(fd1);
	    continue;
	}
#ifdef SINGLE_CLIENT
	ft_server_socket = fd1;
	repl_from_socket(fd1);
	log_message(client_name,"disconnected");
#else
	num_clients++;

	// Fork new image of festival and call interpreter
	if (num_clients > max_clients)
	{
	    log_message(client_name,"failed: too many clients");
	    num_clients--;
	}
	else if ((pid=fork()) == 0)
	{
	    ft_server_socket = fd1;
	    repl_from_socket(fd1);
	    log_message(client_name,"disconnected");
	    exit(0);
	}
	else if (pid < 0)
	{
	    log_message(client_name,"failed to fork new client");
	    num_clients--;
	}

	while ((num_clients > 0) &&  (waitpid(0,&statusp,WNOHANG) != 0))
	    num_clients--;
#endif

	close(fd1);
    }

    return 0;
}

static int client_access_check(int fd,int client)
{
    // Check client against various possible checks to see if they
    // are allowed access to the server
    LISP passwd, access_list, deny_list;
    int client_access = TRUE;
    struct sockaddr_in peer;
    socklen_t addrlen=sizeof(peer);
    struct hostent *clienthost;
    const char *client_hostname;
    const char *client_hostnum;
    const char *reason = "";
    
    getpeername(fd,(struct sockaddr *)&peer,&addrlen);
    clienthost = gethostbyaddr((char *)&peer.sin_addr,
			       sizeof(peer.sin_addr),AF_INET);
    client_hostnum = inet_ntoa(peer.sin_addr);
    if (streq(client_hostnum,"0.0.0.0") || streq(client_hostnum,"127.0.0.1")) // its me !
	client_hostname = "localhost";
    else if (clienthost == 0)	                       // failed to get a name
	client_hostname = client_hostnum;
    else
	client_hostname = clienthost->h_name;
    
    if (((deny_list = siod_get_lval("server_deny_list",NULL)) != NIL) &&
	(siod_regex_member_str(client_hostname,deny_list) != NIL))
    {
	client_access = FALSE;
	reason = "in deny list";
    }
    else if ((access_list = siod_get_lval("server_access_list",NULL)) != NIL)
    {
	client_access = FALSE;	                       // by default now
	reason = "not in access list";
	if (siod_regex_member_str(client_hostname,access_list) != NIL)
	{
	    reason = "";
	    client_access = TRUE;
	}
    }
    
    passwd = siod_get_lval("server_passwd",NULL);
    if ((client_access == TRUE) && (passwd != NULL))
    {
	char *client_passwd = walloc(char,strlen(get_c_string(passwd))+1);
	read(fd,client_passwd,strlen(get_c_string(passwd)));
	client_passwd[strlen(get_c_string(passwd))] = '\0';
	if (streq(get_c_string(passwd),client_passwd))
	    client_access = TRUE;
	else
	{
	    client_access = FALSE;
	    reason = "bad passwd";
	}
	wfree(client_passwd);
    }
    char *message = walloc(char,20+strlen(client_hostname)+strlen(reason));
    
    if (client_access == TRUE)
    {
	sprintf(message,"accepted from %s",client_hostname);
	log_message(client,message);
    }
    else
    {
	sprintf(message,"rejected from %s %s",client_hostname,reason);
	log_message(client,message);
    }
    
    wfree(message);
    
    return client_access;
    
}

static void log_message(int client, const char *message)
{
    // log the message in log file
    
    *cslog << log_time_stamp(client) << message << endl;
}

static EST_String log_time_stamp(int client)
{
    // returns a string with client id, time and date
    char lst[1024];
    time_t thetime = time(0);
    char *cthetime = ctime(&thetime);
    cthetime[24] = '\0';	// get rid of \n
    
    if (client == 0)
	sprintf(lst,"server    %s : ",cthetime);
    else
	sprintf(lst,"client(%d) %s : ",client,cthetime);
    
    return lst;
}



