/*
 * Copyright (C)1997 Jacques H. de Villiers <jacques@cse.ogi.edu>
 * Copyright (C)1997 Center for Spoken Language Understanding,
 *                   Oregon Graduate Institute of Science & Technology
 *
 * The authors hereby grant permission to use, copy, modify, distribute,
 * and license this software and its documentation for any purpose, provided
 * that existing copyright notices are retained in all copies and that this
 * notice is included verbatim in any distributions. No written agreement,
 * license, or royalty fee is required for any of the authorized uses.
 * Modifications to this software may be copyrighted by their authors
 * and need not follow the licensing terms described here, provided that
 * the new terms are clearly indicated on the first page of each file where
 * they apply.
 * 
 * IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
 * DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
 * IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
 * NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 * MODIFICATIONS.
 * 
 * GOVERNMENT USE: If you are acquiring this software on behalf of the
 * U.S. government, the Government shall have only "Restricted Rights"
 * in the software and related documentation as defined in the Federal 
 * Acquisition Regulations (FARs) in Clause 52.227.19 (c) (2).  If you
 * are acquiring the software on behalf of the Department of Defense, the
 * software shall be classified as "Commercial Computer Software" and the
 * Government shall have only "Restricted Rights" as defined in Clause
 * 252.227-7013 (c) (1) of DFARs.  Notwithstanding the foregoing, the
 * authors grant the U.S. Government and others acting in its behalf
 * permission to use and distribute the software in accordance with the
 * terms specified in this license. 
 * 
 *
 * * Sat May 17 13:43:47 BST 1997 *
 *   minor modifications to make it work in 1.1.4  -- awb@cstr.ed.ac.uk
 *
 *---------------------------------------------------------------------------
 * This module adds a Tcl interpreter to Scheme, Scheme commands to
 *  retrieve wave and phoneme info.  A Scheme command to invoke Tcl and a
 *  Tcl command to invoke Scheme.  We'll load CSLUsh packages into the Tcl
 *  interpreter to implement a client/server architecture.
 *---------------------------------------------------------------------------
 */
#include <cstdio>
#include <cstring>
#include "festival.h"
#include "festivalP.h"

#if SUPPORT_TCL
#include <tcl.h>

static int festCmd(ClientData d, Tcl_Interp *interp, int argc, char *argv []);

/* Scheme: (cslush TCL_COMMAND_STRING)
 *
 * Invoke Tcl interpreter from within Festival's scheme runtime.
 * Returns a LISP string, throws an error if the Tcl command fails.
 * For the time being a single, global Tcl interp will suffice.
 */
static Tcl_Interp *tcl_interpreter=NULL;
static LISP tcl_eval(LISP tcl_command)
{
    char *cmd=get_c_string(tcl_command);

    if (!tcl_interpreter) 
    {
	tcl_interpreter=Tcl_CreateInterp();
	Tcl_Init(tcl_interpreter);
	Tcl_CreateCommand(tcl_interpreter,"festival",festCmd,NULL,NULL);
    }
    if (Tcl_Eval(tcl_interpreter,cmd)!=TCL_OK) 
    {
	cerr << tcl_interpreter->result << endl;
	festival_error();
    }
    return strintern(tcl_interpreter->result);
}


/* TCL: festival scheme_command
 *
 * Invoke the scheme interpreter that wraps our Tcl interp.
 * Result is a Tcl string
 *  (The Scheme interpreter aborts the cslush command if a scheme error
 *   occurs.  So much for it being reentrant.)
 */
static int festCmd(ClientData d, Tcl_Interp *interp, int argc, char *argv [])
{
  LISP cmd;
  if(argc!=2) {
    Tcl_AppendResult(interp,"wrong # args: should be \"",argv[0],
		     " scheme_command\"",NULL);
    return TCL_ERROR;
  }
  d=d; /* stop compiler warning */
  cmd=strintern(argv[1]);
  cmd=read_from_string(cmd);
  cmd=leval(cmd,NIL);
  Tcl_SetResult(interp,get_c_string(cmd),TCL_VOLATILE);
  return TCL_OK;
}


/* register our new Scheme functions with Festival
 */
void festival_tcl_init(void)
{
    init_subr_1("tcl_eval",tcl_eval,
    "(tcl_eval STRING)\n\
    Evaluate STRING as a Tcl command, return the result as a LISP string");

    proclaim_module("tcl");
}
#else  /* no TCL */
void festival_tcl_init(void)
{
    // nothing to do 
}
#endif
