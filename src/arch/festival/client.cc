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
/*                Author :  Alan Black (with Richard Tobin)              */
/*                Date   :  December 1996                                */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Low level client functions, separated from the other scoket functions */
/* so things that link with these don't need the whole system            */
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

static EST_Regex ipnum("[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+");

int festival_socket_client(const char *host,int port)
{
    // Return an FD to a remote server
    struct sockaddr_in serv_addr;
    struct hostent *serverhost;
    EST_String shost;
    int fd;

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
    memset(&serv_addr, 0, sizeof(serv_addr));
    shost = host;
    if (shost.matches(ipnum))
	serv_addr.sin_addr.s_addr = inet_addr(host);
    else
    {
	serverhost = gethostbyname(host);
	if (serverhost == (struct hostent *)0)
	{
	    cerr << "socket: gethostbyname failed" << endl;
	    festival_error();
	}
	memmove(&serv_addr.sin_addr,serverhost->h_addr, serverhost->h_length);
    }
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);

    if (connect(fd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) != 0)
    {
	cerr << "socket: connect failed" << endl;
	festival_error();
    }

    return fd;
}


