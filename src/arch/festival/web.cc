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
/*                 Authors:  Alan W Black                                */
/*                 Date   :  November 1996                               */
/*-----------------------------------------------------------------------*/
/*  Some basic functions for dealing with the web and urls               */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include "EST_unix.h"
#include <cstdlib>
#include "festival.h"
#include "festivalP.h"
#include "EST_String.h"

#if 0
static int getc_unbuffered(int fd,int *c);
#endif

LISP parse_url(EST_String url)
{
  EST_String protocol, host, port, path;
  
  if (!parse_url(url, protocol, host, port, path))
    err("can't parse URL", url);

  return cons(strintern(protocol),
	      cons(strintern(host),
		   cons(strintern(port),
			cons(strintern(path), NIL))));
}

LISP lisp_parse_url(LISP l_url)
{
  EST_String url(get_c_string(l_url));

  return parse_url(url);
}


#if 0
LISP lisp_get_url(LISP url,LISP filename)
{
    // Copy file identified by URL and copy it to filename
    // Current only file:/.../... and http:/.../... are supported
    EST_TokenStream us;
    EST_String host,file,port;
    char *comm = walloc(char,32+strlen(get_c_string(url)));
    char *getstr = walloc(char,32+strlen(get_c_string(filename)));
    int wwwserver=-1,c;
    FILE *fd, *fin, *fout;

    // first parse the url
    us.open_string(get_c_string(url));
    us.set_WhiteSpaceChars("");
    us.set_SingleCharSymbols(":/");

    if (us.peek() == "http")
    {
	us.get();
	if ((us.get() != ":") ||
	    (us.get() != "/") ||
	    (us.get() != "/"))
	{
	    cerr << "url_get: malformed url" << endl;
	    festival_error();
	}
	host = us.get().string();  // upto next /
	if (us.peek() == ":")      // a port is specified
	{
	    us.get();
	    port = us.get().string();
	}
	else
	    port = "80";           // standard port for http servers
	file = us.get_upto_eoln();
	sprintf(comm,"telnet %s %s",(const char *)host,(const char *)port);
	wwwserver = festival_socket_client(host,atoi(port));
	if (wwwserver < 0)
	{
	    cerr << "get_url: can't access server\n";
	    festival_error();
	}
	fout = fdopen(wwwserver,"wb");
	fprintf(fout,"GET %s\n",(const char *)file);
	fflush(fout);

	if ((fd=fopen(get_c_string(filename),"wb")) == NULL)
	{
	    cerr << "get_url: can't open outputfile \"" << 
		get_c_string(filename) << "\"\n";
	    festival_error();
	}
	else
	{
	    while (getc_unbuffered(wwwserver,&c) != EOF)
		putc(c,fd);
	    fclose(fd);
	}

	close(wwwserver);
	wfree(comm);
	wfree(getstr);
    }
    else if (us.peek() == "file")
    {
	us.get();
	if (us.get() != ":")
	{
	    cerr << "url_get: malformed url" << endl;
	    festival_error();
	}
	file = us.get_upto_eoln();
	if ((fin = fopen(file,"rb")) == NULL)
	{
	    cerr << "get_url: unable to access file url \"" << 
		get_c_string(url) << "\"\n";
	    festival_error();
	}
	if ((fd=fopen(get_c_string(filename),"wb")) == NULL)
	{
	    cerr << "get_url: can't open outputfile \"" << 
		get_c_string(filename) << "\"\n";
	    fclose(fin);
	    festival_error();
	}
	else
	{
	    while ((c=getc(fin)) != EOF)
		putc(c,fd);
	    fclose(fd);
	    fclose(fin);
	}
    }
    else
    {
	cerr << "get_url: unrecognizable url \"" << 
	    get_c_string(url) << "\"\n";
	festival_error();
    }	
    
    return NIL;
}

LISP l_open_socket(LISP host, LISP port, LISP how)
{
    // Open socket to remote server
    int fd;
    char *how_c;

    fd = festival_socket_client(get_c_string(host),get_c_int(port));
    if (streq(get_c_string(how),"rw"))
    {   // return list of r and w FILEDESCRIPTORS
	return cons(siod_fdopen_c(fd,
				  EST_String(get_c_string(host))+
                                     ":"+get_c_string(port),
				  "rb"),
		    cons(siod_fdopen_c(fd,
				       EST_String(get_c_string(host))+
				          ":"+get_c_string(port),
				       "wb"),NIL));
    }
    else
    {
	if (how == NIL)
	    how_c = "wb";
	else
	    how_c = get_c_string(how);
	return siod_fdopen_c(fd,
			     EST_String(get_c_string(host))+":"+get_c_string(port),
			     how_c);
    }
}

static int getc_unbuffered(int fd,int *rc)
{
    // An attempted to get rid of the buffering
    char c;
    int n;

    n = read(fd,&c,1);
    *rc = c;
    if (n == 0)
	return EOF;
    else
	return 0;
}

#endif
