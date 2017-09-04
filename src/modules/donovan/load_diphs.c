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
/*             Author :  Alistair Conkie                                 */
/*             Date   :  1996                                            */
/*  This version was gifted by Alistair for this new                     */
/*  copyright, the original retains their original copyright             */
/*                                                                       */
/*************************************************************************/
#include <stdio.h>
#include <string.h>
#include "EST_cutils.h"
#include "t2s.h"

static int load_index(CONFIG *config);
static int load_diphs(CONFIG *config);

/* awb added */
int nindex = 0;  
static ENTRY *indx = 0;
static FRAME *dico = 0;

int load_speech(CONFIG *config)
{

    if (load_index(config) != 0)  /* in alphabetical order  */
	return -1;
    if (load_diphs(config) != 0)
	return -1;
    return 0;
}

static int load_index(CONFIG *config)
{
	char s[100];
	int i;

	if (indx == 0)
	    indx = walloc(ENTRY,NDIPHS);

	if((config->xfd=fopen(config->index_file,"rb")) == NULL) {
		(void)fprintf(stderr,"Can't open file %s\n",config->index_file);
		return -1;
	}

	for(i=0;(fgets(s,100,config->xfd) != NULL) && (i < NDIPHS);i++) {
		sscanf(s,"%s %d %d %d",indx[i].diph,&indx[i].beg,&indx[i].mid,&indx[i].end);
	}
	nindex = i;

	fclose(config->xfd);
	return 0;
}

static int load_diphs(CONFIG *config)
{
	int i,j;

	if (dico == 0)
	    dico = walloc(FRAME,NFRAMES);

	if((config->dfd=fopen(config->diphone_file,"rb")) == NULL) {
	    fprintf(stderr,"Can't open file %s\n",config->diphone_file);
	    return -1;
	}

	/* zero the first one... */
	for(i=0;i<FR_DATA;i++)
	    dico[0].frame[i] = 0;
	dico[0].frame[2] = FR_SZ;

	/* note the use of 1 to tie in with indexing  */
	for(i=1;(fread((char *)&dico[i],sizeof(FRAME),1,config->dfd) != 0) && 
	    (i < NFRAMES);i++) 
	{
		;
	}

	/* check the first little bit is as we expect...  */
	if ((dico[1].frame[0] != 181) || (dico[1].frame[1] != 176))
	{
	    if ((SWAPSHORT(dico[1].frame[0]) == 181) &&
		(SWAPSHORT(dico[1].frame[1]) == 176))
	    {			/* Its bytes swapped */
		for (j=1;j<i;j++)
		    swap_bytes_short(dico[j].frame,FR_DATA);
	    }
	    else
	    {
		fprintf(stderr,"File %s apparently corrupted\n",
			config->diphone_file);
		fclose(config->dfd);
		return -1;
	    }
	}

	fclose(config->dfd);
	return 0;
}

int lookup(char *diph)
{
	int low, high, mid;


	low = 0; 
	high = nindex-1;
	while(low <= high) {
		mid = (low+high) / 2;
		if(strcmp(diph,indx[mid].diph)<0) 
			high = mid-1;
		else if(strcmp(diph,indx[mid].diph)>0) 
			low = mid+1;
		else
			return(mid);
	}
	return(-1);
}

void phonstoframes(SPN *ps, ACOUSTIC *as)
{
	int i,j;
	int ref;

	as->f_sz = 0;
	
	for(i=0;i<ps->p_sz-1;i++) 
		sprintf(ps->diphs[i],"%s-%s",ps->phons[i],ps->phons[i+1]);

	ps->pb[0] = 0;	/* Gets treated a little bit specially  */

	/* insert zero frame  */
	as->mcebuf[as->f_sz++] = &dico[0];

	for(i=0;i<ps->p_sz-1;i++) {
		ref = lookup(ps->diphs[i]);	/* gives back the reference no.  */
		if(ref == -1) {
			(void)fprintf(stderr,"Diphone not found -  %s\n",ps->diphs[i]);
			ref = 0;
		}
		if(as->f_sz+50 > as->f_max) {
			as_realloc(as->f_max*2,as->p_max,as);
		}
		for(j=indx[ref].beg;j<=indx[ref].end;j++) {
			if(j==indx[ref].mid)
				ps->pb[i+1] = as->f_sz;
			as->mcebuf[as->f_sz++] = &dico[j];
		}
	}
	as->mcebuf[as->f_sz++] = &dico[0];
	as->mcebuf[as->f_sz++] = &dico[0];
	as->mcebuf[as->f_sz++] = &dico[0];

	ps->pb[ps->p_sz] = as->f_sz-1;

}

