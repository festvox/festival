/*************************************************************************/
/*                                                                       */
/*                   Language Technologies Institute                     */
/*                     Carnegie Mellon University                        */
/*                       Copyright (c) 2005-2017                         */
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
/*  CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK         */
/*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   */
/*  SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE      */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*  Most of the key functions in this directory are derived from HTS     */
/*  and are only provided with Scheme wraparounds for Clustergen         */
/*                                                                       */
/*************************************************************************/

/*  Standard C Libraries  */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cctype>
#include "festival.h"

LISP mlsa_resynthesis(LISP ltrack, LISP strtrack, LISP filtertrack);
LISP mlpg(LISP ltrack);

LISP me_mlsa(LISP ltrack, LISP strtrack) {
  return mlsa_resynthesis(ltrack, strtrack, NULL);
}

void festival_clustergen_init(void)
{
    proclaim_module("clustergen_engine",
                    "Copyright (C) Carnegie Mellon University 2005-2017\n");

    init_subr_3("mlsa_resynthesis", mlsa_resynthesis,
                "(mlsa_resynthesis TRACK STRTRACK FILTERTRACK)\n\
  Return a WAVE synthesized from the F0/MCEP TRACK, STRTRACK is non-nil, use mixed excitation.\n\
  If FILTERTRACK is non-nil, it has filters for excitation");

    init_subr_1("mlpg",mlpg,
                "(mlpg TRACK)\n\
  Return a track suitable for mlsa from a TRACK with dynamics in it.");

    init_subr_2("me_mlsa",me_mlsa,
                "(me_mlsa TRACK STRTRACK)\n\
  Return a WAVE synthesized from the F0/MCEP TRACK, STRTRACK is non-nil, use mixed excitation.\n\
  Deprecated! Use mlsa_resynthesis instead");


}

