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
/*             Date   :  August 1996                                     */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/* As CNET (France Telecom) claims a patent on the PSOLA/TD(*) algorithm */
/* we have decided not to distrubute PSOLA code as part of the basic     */
/* system.                                                               */
/*                                                                       */
/* We have, of course, implemented PSOLA as part of our own research.    */
/* Depending on the installation, our PSOLA impelementation may or may   */
/* be included with your distribution.  If you do have it then           */
/* define HAVE_DI_PSOLA_TM (in config/config_make_rules).  If defined    */
/* the file di_psolaTM.C is included and PSOLA is available otherwise    */
/* an error function is compiled.                                        */
/*                                                                       */
/* if PSOLA is included "di_psolaTM" is added to the proclaimed module   */
/* list so that its existence may be tested in the LISP domain, as used  */
/* in lib/gsw_diphone.scm                                                */
/*                                                                       */
/* (*) PSOLA/TD is a Trade Mark of France Telecom                        */
/*                                                                       */
/*=======================================================================*/
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include "festival.h"
#include "diphone.h"

#ifdef SUPPORT_PSOLA_TM
#include "di_psolaTM.cc"
#else
void di_psola_tm(DIPHONE_DATABASE *db, DIPHONE_ACOUSTIC *as, DIPHONE_OUTPUT *output)
{
    (void)db;
    (void)as;
    (void)output;
    
    cerr << "Diphone: di_psola is not available in this installation" << endl;
    festival_error();
}
#endif    
