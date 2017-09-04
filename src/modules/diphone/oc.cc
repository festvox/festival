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
/*                      Author :  Alan W Black                           */
/*                      Date   :  December 1996                          */
/*-----------------------------------------------------------------------*/
/*   Optimal Coupling for two pieces of speech                           */
/*                                                                       */
/*   Given two Tracks find the best point (minimised weighted euclidean    */
/*   distance between two vectors).                                      */
/*                                                                       */
/*=======================================================================*/
#include <iostream>
#include "festival.h"

#define LEFT_PHONE(x) (car(x))
#define RIGHT_PHONE(x) (car(cdr(x)))
#define FILEID(x) (car(cdr(cdr(x))))
#define START(x) (car(cdr(cdr(cdr(x)))))
#define MID(x) (car(cdr(cdr(cdr(cdr(x))))))
#define END(x) (car(cdr(cdr(cdr(cdr(cdr(x)))))))

static int get_track_for_phone(EST_Track &a,const EST_String &fileid,float st,float end);
static LISP before_ds(LISP d1, LISP ds);
static LISP after_ds(LISP d1, LISP ds);
static float find_best_left(LISP d,LISP ds,LISP weights);
static float find_best_right(LISP d,LISP ds,LISP weights);
static float frametoms(int frame,float frame_shift);
static int mstoframe(float ms,float frame_shift);
static float frame_distance(EST_Track &a, int fa, 
			    EST_Track &b, int fb,
			    int size, double *weights);

static EST_String coeffs_dir = "coeffs/";
static EST_String coeffs_ext = ".cep";

LISP find_optimal_coupling(LISP table, LISP weights)
{
    // For each diphone description in table find the best overall
    // join point between it and all other diphone sthat can join with it
    LISP d,newtab,newent;
    float best_left,best_right;

    newtab = NIL;
    gc_protect(&newtab);
    coeffs_dir = get_c_string(siod_get_lval("oc_coeffs_dir","no coeffs dir"));
    coeffs_ext = get_c_string(siod_get_lval("oc_coeffs_ext","no coeffs ext"));
    
    for (d=table; d != NIL; d=cdr(d))
    {
	pprint(car(d));
	if (ph_is_silence(get_c_string(LEFT_PHONE(car(d)))))
	    best_left = get_c_float(START(car(d)));
	else
	    best_left=find_best_left(car(d),before_ds(car(d),table),weights);
	if (ph_is_silence(get_c_string(RIGHT_PHONE(car(d)))))
	    best_right = get_c_float(END(car(d)));
	else
	    best_right=find_best_right(car(d),after_ds(car(d),table),weights);
	newent = cons(LEFT_PHONE(car(d)),              // left phone
		 cons(RIGHT_PHONE(car(d)),             // right phone
		 cons(FILEID(car(d)),                  // file_id
   	         cons(flocons(best_left),              // left cut point
		 cons(MID(car(d)),                     // mid point
		 cons(flocons(best_right),NIL))))));   // right cut point
	newtab = cons(newent,newtab);
    }
    newtab = reverse(newtab);
    gc_unprotect(&newtab);
    return newtab;
}

static int get_track_for_phone(EST_Track &a, const EST_String &fileid, float st, float end)
{
    // Load EST_Track from fileid from st to end (in ms)
    EST_Track whole;
    int start_frame, end_frame, i;

    if (whole.load(coeffs_dir+fileid+coeffs_ext) != 0)
	return -1;
    start_frame = mstoframe(st-12.8,whole.shift());
    end_frame = mstoframe(end-12.8,whole.shift())+1;
    a.resize(end_frame-start_frame,whole.num_channels());
    for (i=start_frame; i < end_frame; i++)
	a.copy_frame_in(i-start_frame, 
			whole, i, 0, 
			0, whole.num_channels());
    a.fill_time(whole.shift());

    return 0;
}

static LISP before_ds(LISP d1, LISP ds)
{
    // Return all entries in ds whose cadr equals d1's car
    // i.e. all diphones which match d1's left phone
    LISP m=NIL,l;

    for (l=ds; l != NIL; l=cdr(l))
	if (streq(get_c_string(car(d1)),get_c_string(car(cdr(car(l))))))
	    m=cons(car(l),m);
    return m;
}

static LISP after_ds(LISP d1, LISP ds)
{
    // Return all entries in ds whose car equals d1's cadr
    // i..e all diphones which match d1's right phone
    LISP m=NIL,l;

    for (l=ds; l != NIL; l=cdr(l))
	if (streq(get_c_string(car(cdr(d1))),get_c_string(car(car(l)))))
	    m=cons(car(l),m);
    return m;
}

static float find_best_left(LISP d,LISP ds,LISP weights)
{
    // Find the best join point with each of phones described
    // in d
    EST_Track a,b;
    LISP l;
    int i,j,best,bestj;;
    double b_dist,dist;
    float best_pos;

    get_track_for_phone(a,get_c_string(FILEID(d)),
			get_c_float(START(d)),get_c_float(MID(d)));

    // Cummulate the costs for each possible cut point
    double *counts = new double[a.num_frames()];
    for (i=0; i < a.num_frames(); i++)
	counts[i] = 0;
    double *w = new double[siod_llength(weights)];
    for (l=weights,i=0; i < siod_llength(weights); i++,l=cdr(l))
	w[i] = get_c_float(car(l));
    
    for (l=ds; l != NIL; l=cdr(l))
    {   // for each matching phone
	get_track_for_phone(b,get_c_string(FILEID(car(l))),
			  get_c_float(MID(car(l))),get_c_float(END(car(l))));
	best=1;

	b_dist = frame_distance(a, 1, b, 0, a.num_channels(),w);

	for (i=1; i < a.num_frames()-1; i++)
	{
	    for (j=0; j < b.num_frames(); j++)
	    {
		dist = frame_distance(a, i, b, j, a.num_channels(),w);
		if (dist < b_dist)
		{
		    b_dist = dist;
		    best = i;
		    bestj = j;
		}
	    }
	}
	// You should probably find minimise the std
//	printf("best pos %d %s-%s %s-%s\n",best,
//	       get_c_string(LEFT_PHONE(d)),get_c_string(RIGHT_PHONE(d)),
//	       get_c_string(LEFT_PHONE(car(l))),get_c_string(RIGHT_PHONE(car(l))));
	counts[best] += 1;  // sum the best possible
//	counts[best] += b_dist;  // sum the best possible
    }

    // Now find out the best position
    best = 0;
    for (i=0; i < a.num_frames(); i++)
    {
	if (counts[i] > counts[best])
	    best = i;
    }

    // Change frame number back to ms offset
    best_pos = frametoms(mstoframe(get_c_float(START(d)),a.shift())
			 + best,a.shift());
    delete counts;
    delete w;
    return best_pos;
}

static float find_best_right(LISP d,LISP ds,LISP weights)
{
    // Find the best join point with each of phones described
    // in d
    EST_Track a,b;
    LISP l;
    int i,j,best,bestj;;
    double b_dist,dist;
    float best_pos;

    get_track_for_phone(a,get_c_string(FILEID(d)),
			get_c_float(MID(d)),get_c_float(END(d)));

    // Cummulate the costs for each possible cut point
    double *counts = new double[a.num_frames()];
    for (i=0; i < a.num_frames(); i++)
	counts[i] = 0;
    double *w = new double[siod_llength(weights)];
    for (l=weights,i=0; i < siod_llength(weights); i++,l=cdr(l))
	w[i] = get_c_float(car(l));
    
    for (l=ds; l != NIL; l=cdr(l))
    {   // for each matching phone
	get_track_for_phone(b,get_c_string(FILEID(car(l))),
			    get_c_float(START(car(l))),
			    get_c_float(MID(car(l))));
	best=1;
	b_dist = frame_distance( a, 1,  b, 0, a.num_channels(),w);
	for (i=1; i < a.num_frames()-1; i++)
	{
	    for (j=0; j < b.num_frames(); j++)
	    {
		dist = frame_distance( a, i,  b, j, a.num_channels(),w);
		if (dist < b_dist)
		{
		    b_dist = dist;
		    best = i;
		    bestj = j;
		}
	    }
	}
	// You should probably find minimise the std
	counts[best] += 1;  // sum the best possible
//	counts[best] += b_dist;  // sum the best possible
    }

    // Now find out the best position
    best = 0;
    for (i=0; i < a.num_frames(); i++)
    {
	if (counts[i] > counts[best])
	    best = i;
    }

    // Change frame number back to ms offset
    best_pos = frametoms(mstoframe(get_c_float(MID(d)),a.shift())
			 + best,a.shift());
    delete counts;
    delete w;
    return best_pos;
}

static float frametoms(int frame,float frame_shift)
{
    return (frame*frame_shift)*1000.0;
}

static int mstoframe(float ms,float frame_shift)
{
    return (int)((ms/1000.0)/frame_shift);
}

// RJC - change for Track reorg.

static float frame_distance(EST_Track &a, int fa, 
			    EST_Track &b, int fb,
			    int size, double *weights)
{
    float cost = 0.0,diff;
    int i;

    for (i=0; i < size; i++)
    {
	if (weights[i] != 0.0)
	{
	    diff = (a(fa,i)-b(fb,i));
	    cost += diff*diff*weights[i];
	}
    }

    return cost;
}

