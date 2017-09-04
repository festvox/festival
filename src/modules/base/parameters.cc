 /************************************************************************/
 /*                                                                      */
 /*                Centre for Speech Technology Research                 */
 /*                     University of Edinburgh, UK                      */
 /*                       Copyright (c) 1996,1997                        */
 /*                        All Rights Reserved.                          */
 /*                                                                      */
 /*  Permission is hereby granted, free of charge, to use and distribute */
 /*  this software and its documentation without restriction, including  */
 /*  without limitation the rights to use, copy, modify, merge, publish, */
 /*  distribute, sublicense, and/or sell copies of this work, and to     */
 /*  permit persons to whom this work is furnished to do so, subject to  */
 /*  the following conditions:                                           */
 /*   1. The code must retain the above copyright notice, this list of   */
 /*      conditions and the following disclaimer.                        */
 /*   2. Any modifications must be clearly marked as such.               */
 /*   3. Original authors' names are not deleted.                        */
 /*   4. The authors' names are not used to endorse or promote products  */
 /*      derived from this software without specific prior written       */
 /*      permission.                                                     */
 /*                                                                      */
 /*  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK       */
 /*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     */
 /*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  */
 /*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE    */
 /*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   */
 /*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  */
 /*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         */
 /*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      */
 /*  THIS SOFTWARE.                                                      */
 /*                                                                      */
 /*************************************************************************/
 /*                                                                       */
 /*                 Author: Richard Caley (rjc@cstr.ed.ac.uk)             */
 /*                   Date: Tue Aug 26 1997                               */
 /* -------------------------------------------------------------------   */
 /* Utility routines for easy access to parameters from C++ modules.      */
 /*                                                                       */
 /*************************************************************************/

#include "module_support.h"

// implemented as a call to scheme so that redefineing how parameters
// are accessed in scheme will affect things as required. Of course
// this isn't as efficient as it might be, but we aren't going to be
// doing this inside any loops

LISP lisp_parameter_get(const EST_String parameter_name)
{
  LISP parameter_get = siod_get_lval("Parameter.get", "Parameter.get not defined");
  LISP parameter = rintern(parameter_name);
  LISP sexp = cons(parameter_get, cons(quote(parameter), NIL));
  LISP val=NIL;

  gc_protect(&sexp);

  CATCH_ERRORS()
    {
      cerr << "error getting parameter " << parameter_name << "\n";
      siod_reset_prompt();
      gc_unprotect(&sexp);
      return NIL;
    }
  val = leval(sexp, NIL);
  END_CATCH_ERRORS();

  gc_unprotect(&sexp);
  return val;
}

int int_parameter_get(const EST_String parameter, int def)
{
    LISP lval = lisp_parameter_get(parameter);

    if (lval == NIL)
	return def;
    if (!FLONUMP(lval))
    {
	cerr << "non numeric value for parameter " << parameter << "\n";
	return 0;
    }
      
    return get_c_int(lval);
}

float float_parameter_get(const EST_String parameter, float def)
{
    LISP lval = lisp_parameter_get(parameter);

    if (lval == NIL)
	return def;
    if (!FLONUMP(lval))
    {
	cerr << "non numeric value for parameter " << parameter << "\n";
	return 0.0;
    }
      
    return get_c_float(lval);
}

bool bool_parameter_get(const EST_String parameter)
{
  LISP lval = lisp_parameter_get(parameter);

  return lval != NIL;
}

EST_String string_parameter_get(const EST_String parameter, EST_String def)
{
  LISP lval = lisp_parameter_get(parameter);

  if (lval == NIL)
    return def;
  if (!SYMBOLP(lval) && !STRINGP(lval))
    {
      cerr << "non string value for parameter " << parameter << "\n";
      return 0;
    }
      
  return get_c_string(lval);
}





