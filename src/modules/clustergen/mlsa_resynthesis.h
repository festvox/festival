/*********************************************************************/
/*                                                                   */
/*            Nagoya Institute of Technology, Aichi, Japan,          */
/*       Nara Institute of Science and Technology, Nara, Japan       */
/*                                and                                */
/*             Carnegie Mellon University, Pittsburgh, PA            */
/*                      Copyright (c) 2003-2004                      */
/*                        All Rights Reserved.                       */
/*                                                                   */
/*  Permission is hereby granted, free of charge, to use and         */
/*  distribute this software and its documentation without           */
/*  restriction, including without limitation the rights to use,     */
/*  copy, modify, merge, publish, distribute, sublicense, and/or     */
/*  sell copies of this work, and to permit persons to whom this     */
/*  work is furnished to do so, subject to the following conditions: */
/*                                                                   */
/*    1. The code must retain the above copyright notice, this list  */
/*       of conditions and the following disclaimer.                 */
/*    2. Any modifications must be clearly marked as such.           */
/*    3. Original authors' names are not deleted.                    */
/*                                                                   */    
/*  NAGOYA INSTITUTE OF TECHNOLOGY, NARA INSTITUTE OF SCIENCE AND    */
/*  TECHNOLOGY, CARNEGIE MELLON UNIVERSITY, AND THE CONTRIBUTORS TO  */
/*  THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  */
/*  INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, */
/*  IN NO EVENT SHALL NAGOYA INSTITUTE OF TECHNOLOGY, NARA           */
/*  INSTITUTE OF SCIENCE AND TECHNOLOGY, CARNEGIE MELLON UNIVERSITY, */
/*  NOR THE CONTRIBUTORS BE LIABLE FOR ANY SPECIAL, INDIRECT OR      */
/*  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM   */
/*  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,  */
/*  NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN        */
/*  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.         */
/*                                                                   */
/*********************************************************************/
/*                                                                   */
/*          Author :  Tomoki Toda (tomoki@ics.nitech.ac.jp)          */
/*          Date   :  June 2004                                      */
/*                                                                   */
/*          Modified by Alan W Black (awb@cs.cmu.edu) Jan 2006       */
/*          taken from festvox/src/vc/ back into Festival            */
/*-------------------------------------------------------------------*/
/*                                                                   */
/*  Subroutine for Speech Synthesis                                  */
/*                                                                   */
/*-------------------------------------------------------------------*/

#ifndef __MLSA_RESYNTHESIS_H
#define __MLSA_RESYNTHESIS_H

typedef struct DVECTOR_STRUCT {
    long length;
    double *data;
    double *imag;
} *DVECTOR;

typedef struct DMATRIX_STRUCT {
    long row;
    long col;
    double **data;
    double **imag;
} *DMATRIX;

#define XBOOL int
#define XTRUE 1
#define XFALSE 0

#define NODATA NULL

#define FABS(x) ((x) >= 0.0 ? (x) : -(x))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

DVECTOR synthesis_body(DMATRIX mcep, DVECTOR f0v, 
                       EST_Track *str, EST_Track *filtrack,
                       double fs, double framem,
                       double alpha, double beta);
#define RANDMAX 32767 
#define   B0         0x00000001
#define   B28        0x10000000
#define   B31        0x80000000
#define   B31_       0x7fffffff
#define   Z          0x00000000

typedef enum {MFALSE, MTRUE} Boolean;

typedef struct _VocoderSetup {
   
   int fprd;
   int iprd;
   int seed;
   int pd;
   unsigned long next;
   Boolean gauss;
   double p1;
   double pc;
   double pj;
   double pade[21];
   double *ppade;
   double *c, *cc, *cinc, *d1;
   double rate;
   
   int sw;
   double r1, r2, s;
   
   int x;
   
   /* for postfiltering */
   int size;
   double *d; 
   double *g;
   double *mc;
   double *cep;
   double *ir;
   int o;
   int irleng;

    /* for MIXED EXCITATION */
    int ME_order;
    int ME_num;
    double *hpulse;
    double *hnoise;

    double *xpulsesig;
    double *xnoisesig;

    double **h;
   
} VocoderSetup;

#endif /* __RESYNTHESIS_SUB_H */
