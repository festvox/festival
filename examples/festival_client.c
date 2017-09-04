/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                        Copyright (c) 1999                             */
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
/*             Author :  Alan W Black (awb@cstr.ed.ac.uk)                */
/*             Date   :  March 1999                                      */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* Client end of Festival server API in C designed specifically for      */
/* Galaxy Communicator use though might be of use for other things       */
/*                                                                       */
/* This is a standalone C client, no other Festival or Speech Tools      */
/* libraries need be link with this.  Thus is very small.                */
/*                                                                       */
/* Compile with (plus socket libraries if required)                      */
/*    cc -o festival_client -DSTANDALONE festival_client.c               */
/*                                                                       */
/* Run as                                                                */
/*    festival_client -text "hello there" -o hello.snd                   */
/*                                                                       */
/*                                                                       */
/* This is provided as an example, it is quite limited in what it does   */
/* but is functional compiling without -DSTANDALONE gives you a simple   */
/* API                                                                   */
/*                                                                       */
/*=======================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "festival_client.h"

/* For testing endianness */
int fapi_endian_loc = 1;

static char *socket_receive_file_to_buff(int fd,int *size);

void delete_FT_Wave(FT_Wave *wave)
{
    if (wave != 0)
    {
	if (wave->samples != 0)
	    free(wave->samples);
	free(wave);
    }
}

int save_FT_Wave_snd(FT_Wave *wave, const char *filename)
{
    FILE *fd;
    struct {
	unsigned int    magic;	/* magic number */
	unsigned int    hdr_size;	/* size of this header */
	int    data_size;	        /* length of data (optional) */
	unsigned int    encoding;	/* data encoding format */
	unsigned int    sample_rate; /* samples per second */
	unsigned int    channels;	 /* number of interleaved channels */
    } header;
    short sw_short;
    int i;

    if ((filename == 0) ||
	(strcmp(filename,"stdout") == 0) ||
	(strcmp(filename,"-") == 0))
	fd = stdout;
    else if ((fd = fopen(filename,"wb")) == NULL)
    {
	fprintf(stderr,"save_FT_Wave: can't open file \"%s\" for writing\n",
		filename);
	return -1;
    }
    
    header.magic = (unsigned int)0x2e736e64;
    header.hdr_size = sizeof(header);
    header.data_size = 2 * wave->num_samples;
    header.encoding = 3; /* short */
    header.sample_rate = wave->sample_rate;
    header.channels = 1;
    if (FAPI_LITTLE_ENDIAN)
    {   /* snd is always sparc/68000 byte order */
	header.magic = SWAPINT(header.magic);
	header.hdr_size = SWAPINT(header.hdr_size);
	header.data_size = SWAPINT(header.data_size);
	header.encoding = SWAPINT(header.encoding);
	header.sample_rate = SWAPINT(header.sample_rate);
	header.channels = SWAPINT(header.channels);
    }
    /* write header */
    if (fwrite(&header, sizeof(header), 1, fd) != 1)
	return -1;
    if (FAPI_BIG_ENDIAN)
	fwrite(wave->samples,sizeof(short),wave->num_samples,fd);
    else
    {  /* have to swap */
	for (i=0; i < wave->num_samples; i++)
	{
	    sw_short = SWAPSHORT(wave->samples[i]);
	    fwrite(&sw_short,sizeof(short),1,fd);
	}
    }

    if (fd != stdout)
	fclose(fd);
    return 0;
}

void delete_FT_Info(FT_Info *info)
{
    if (info != 0)
	free(info);
}

static FT_Info *festival_default_info()
{
    FT_Info *info;
    info = (FT_Info *)malloc(1 * sizeof(FT_Info));
    
    info->server_host = FESTIVAL_DEFAULT_SERVER_HOST;
    info->server_port = FESTIVAL_DEFAULT_SERVER_PORT;
    info->text_mode = FESTIVAL_DEFAULT_TEXT_MODE;

    info->server_fd = -1;
    
    return info;
}

static int festival_socket_open(const char *host, int port)
{   
    /* Return an FD to a remote server */
    struct sockaddr_in serv_addr;
    struct hostent *serverhost;
    int fd;

    fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

    if (fd < 0)  
    {
	fprintf(stderr,"festival_client: can't get socket\n");
	return -1;
    }
    memset(&serv_addr, 0, sizeof(serv_addr));
    if ((serv_addr.sin_addr.s_addr = inet_addr(host)) == -1)
    {
	/* its a name rather than an ipnum */
	serverhost = gethostbyname(host);
	if (serverhost == (struct hostent *)0)
	{
	    fprintf(stderr,"festival_client: gethostbyname failed\n");
	    return -1;
	}
	memmove(&serv_addr.sin_addr,serverhost->h_addr, serverhost->h_length);
    }
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);

    if (connect(fd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) != 0)
    {
	fprintf(stderr,"festival_client: connect to server failed\n");
	return -1;
    }

    return fd;
}

static int nist_get_param_int(char *hdr, char *field, int def_val)
{
    char *p;
    int val;

    if (((p=strstr(hdr,field)) != NULL) &&
	(strncmp(" -i ",p+strlen(field),4) == 0))
    {
	sscanf(p+strlen(field)+4,"%d",&val);
	return val;
    }
    else
	return def_val;
}

static int nist_require_swap(char *hdr)
{
    char *p;
    char *field = "sample_byte_format";

    if ((p=strstr(hdr,field)) != NULL)
    {
	if (((strncmp(" -s2 01",p+strlen(field),7) == 0) && FAPI_BIG_ENDIAN) ||
	    ((strncmp(" -s2 10",p+strlen(field),7) == 0) 
	     && FAPI_LITTLE_ENDIAN))
	    return 1;
    }
    return 0; /* if unknown assume native byte order */
}

static char *client_accept_s_expr(int fd)
{
    /* Read s-expression from server, as a char * */
    char *expr;
    int filesize;

    expr = socket_receive_file_to_buff(fd,&filesize);
    expr[filesize] = '\0';
    return expr;
}

static FT_Wave *client_accept_waveform(int fd)
{
    /* Read waveform from server */
    char *wavefile;
    int filesize;
    int num_samples, sample_rate, i;
    FT_Wave *wave;

    wavefile = socket_receive_file_to_buff(fd,&filesize);
    wave = 0;
    
    /* I know this is NIST file and its an error if it isn't */
    if (filesize >= 1024)
    {
	num_samples = nist_get_param_int(wavefile,"sample_count",0);
	sample_rate = nist_get_param_int(wavefile,"sample_rate",16000);
	if ((num_samples*sizeof(short))+1024 == filesize)
	{
	    wave = (FT_Wave *)malloc(sizeof(FT_Wave));
	    wave->num_samples = num_samples;
	    wave->sample_rate = sample_rate;
	    wave->samples = (short *)malloc(num_samples*sizeof(short));
	    memmove(wave->samples,wavefile+1024,num_samples*sizeof(short));
	    if (nist_require_swap(wavefile))
		for (i=0; i < num_samples; i++)
		    wave->samples[i] = SWAPSHORT(wave->samples[i]);
	}
    }

    if (wavefile != 0)  /* just in case we've got an ancient free() */
	free(wavefile);
    return wave;
}

static char *socket_receive_file_to_buff(int fd,int *size)
{
    /* Receive file (probably a waveform file) from socket using   */
    /* Festival key stuff technique, but long winded I know, sorry */
    /* but will receive any file without closeing the stream or    */
    /* using OOB data                                              */
    static char *file_stuff_key = "ft_StUfF_key"; /* must == Festival's key */
    char *buff;
    int bufflen;
    int n,k,i;
    char c;

    bufflen = 1024;
    buff = (char *)malloc(bufflen);
    *size=0;

    for (k=0; file_stuff_key[k] != '\0';)
    {
	n = read(fd,&c,1);
	if (n==0) break;  /* hit stream eof before end of file */
	if ((*size)+k+1 >= bufflen)
	{   /* +1 so you can add a NULL if you want */
	    bufflen += bufflen/4;
	    buff = (char *)realloc(buff,bufflen);
	}
	if (file_stuff_key[k] == c)
	    k++;
	else if ((c == 'X') && (file_stuff_key[k+1] == '\0'))
	{   /* It looked like the key but wasn't */
	    for (i=0; i < k; i++,(*size)++) 
		buff[*size] = file_stuff_key[i];
	    k=0;
	    /* omit the stuffed 'X' */
	}
	else
	{
	    for (i=0; i < k; i++,(*size)++)
		buff[*size] = file_stuff_key[i];
	    k=0;
	    buff[*size] = c;
	    (*size)++;
	}

    }

    return buff;
}

/***********************************************************************/
/* Public Functions to this API                                        */
/***********************************************************************/

FT_Info *festivalOpen(FT_Info *info)
{
    /* Open socket to server */

    if (info == 0)
	info = festival_default_info();

    info->server_fd = 
	festival_socket_open(info->server_host, info->server_port);
    if (info->server_fd == -1)
	return NULL;

    return info;
}

FT_Wave *festivalStringToWave(FT_Info *info,char *text)
{
    FT_Wave *wave;
    FILE *fd;
    char *p;
    char ack[4];
    int n;

    if (info == 0)
	return 0;

    if (info->server_fd == -1)
    {
	fprintf(stderr,"festival_client: server connection unopened\n");
	return 0;
    }
    fd = fdopen(dup(info->server_fd),"wb");
    
    /* Copy text over to server, escaping any quotes */
    fprintf(fd,"(tts_textall \"\n");
    for (p=text; p && (*p != '\0'); p++)
    {
	if ((*p == '"') || (*p == '\\'))
	    putc('\\',fd);
	putc(*p,fd);
    }
    fprintf(fd,"\" \"%s\")\n",info->text_mode);
    fclose(fd);

    /* Read back info from server */
    /* This assumes only one waveform will come back, also LP is unlikely */
    wave = 0;
    do {
	for (n=0; n < 3; )
	    n += read(info->server_fd,ack+n,3-n);
	ack[3] = '\0';
	if (strcmp(ack,"WV\n") == 0)         /* receive a waveform */
	    wave = client_accept_waveform(info->server_fd);
	else if (strcmp(ack,"LP\n") == 0)    /* receive an s-expr */
	    client_accept_s_expr(info->server_fd);
	else if (strcmp(ack,"ER\n") == 0)    /* server got an error */
	{
	    fprintf(stderr,"festival_client: server returned error\n");
	    break;
	}
    } while (strcmp(ack,"OK\n") != 0);
    
    return wave;
}


int festivalClose(FT_Info *info)
{
    if (info == 0)
	return 0;

    if (info->server_fd != -1)
	close(info->server_fd);

    return 0;
}

#ifdef STANDALONE
int main(int argc, char **argv)
{
    char *server=0;
    int port=-1;
    char *text=0;
    char *output=0;
    char *mode=0;
    int i;
    FT_Info *info;
    FT_Wave *wave;

    for (i=1; i < argc; i++)
    {
	if (strcmp(argv[i],"-server") == 0)
	    server = argv[++i];
	else if (strcmp(argv[i],"-port") == 0)
	    port = atoi(argv[++i]);
	else if (strcmp(argv[i],"-text") == 0)
	    text = argv[++i];
	else if (strcmp(argv[i],"-mode") == 0)
	    mode = argv[++i];
	else if (strcmp(argv[i],"-o") == 0)
	    output = argv[++i];
    }
    if (i > argc) 
    {
	fprintf(stderr,"missing argument\n");
	exit(1);
    }

    info = festival_default_info();
    if (server != 0)
	info->server_host = server;
    if (port != -1)
	info->server_port = port;
    if (mode != 0)
	info->text_mode = mode;

    info = festivalOpen(info);
    wave = festivalStringToWave(info,text);

    if (wave != 0)
	save_FT_Wave_snd(wave,output);
    
    festivalClose(info);

    return 0;
}
#endif
