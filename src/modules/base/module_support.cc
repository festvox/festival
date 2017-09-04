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
 /*                   Date: Tue Jul 29 1997                               */
 /* --------------------------------------------------------------------  */
 /* Some things useful in modules.                                        */
 /*                                                                       */
 /*************************************************************************/

#include "module_support.h"

#define CAR6(x) CAR(CDR5(x))
#define CDR6(x) CDR(CDR5(x))
#define CAR7(x) CAR(CDR6(x))
#define CDR7(x) CDR(CDR6(x))

#define CDR_to1(X) ((X!=NIL)&&CDR1(X))
#define CDR_to2(X) (CDR_to1(X)&&CDR2(X))
#define CDR_to3(X) (CDR_to2(X)&&CDR3(X))
#define CDR_to4(X) (CDR_to3(X)&&CDR4(X))
#define CDR_to5(X) (CDR_to4(X)&&CDR5(X))
#define CDR_to6(X) (CDR_to5(X)&&CDR6(X))
#define CDR_to7(X) (CDR_to6(X)&&CDR7(X))


void unpack_multiple_args(LISP args, LISP &v1, LISP &v2, LISP &v3, LISP &v4)
{
  if (args)
    {
      v1 = CAR1(args);
      if (CDR1(args))
	{
	  v2 = CAR2(args);
	  if (CDR2(args))
	    {
	      v3 = CAR3(args);
	      if (CDR3(args))
		v4 = CAR4(args);
	    }
	}
    }
}

void unpack_multiple_args(LISP args, LISP &v1, LISP &v2, LISP &v3, LISP &v4, LISP &v5)
{
  unpack_multiple_args(args, v1, v2, v3, v4);

  if (CDR4(args))
    v5 = CAR5(args);
}

void unpack_relation_arg(EST_Utterance *utt, 
			 LISP lrel_name, 
			 EST_String &relation_name,
			 EST_Relation *&relation, 
			 RelArgType type)
{
    if (lrel_name)
	relation_name = get_c_string(lrel_name);

    if(utt->relation(relation_name))
	relation = utt->relation(relation_name);

    if (type==sat_existing)
    {
	if(!relation)
	    err("no relation", relation_name);	
    }
    else if (type==sat_new || type==sat_replace)
    {
	if (relation)
	    if (type==sat_new)
		err("relation exists", relation_name);
	utt->create_relation(relation_name);

	relation = &(*(utt->relation(relation_name)));
    }
}

void unpack_module_args(LISP args, EST_Utterance *&utt)
{
  if (args)
    {
      LISP lutt = CAR1(args);

      utt = get_c_utt(lutt);
      return;
    }
  err("no utterance given", NIL);
}


void unpack_module_args(LISP args, 
			EST_Utterance *&utt,
			EST_String &relation1_name, EST_Relation *&relation1, RelArgType type1)
{
  unpack_module_args(args, utt);

  unpack_relation_arg(utt, CDR_to1(args)?CAR2(args):NIL, relation1_name, relation1, type1);
}

void unpack_module_args(LISP args, 
			EST_Utterance *&utt,
			EST_String &relation1_name, EST_Relation *&relation1, RelArgType type1,
			EST_String &relation2_name, EST_Relation *&relation2, RelArgType type2
			)
{
  unpack_module_args(args, utt);

  unpack_relation_arg(utt, CDR_to1(args)?CAR2(args):NIL, relation1_name, relation1, type1);
  unpack_relation_arg(utt, CDR_to2(args)?CAR3(args):NIL, relation2_name, relation2, type2);
}

void unpack_module_args(LISP args, 
			EST_Utterance *&utt,
			EST_String &relation1_name, EST_Relation *&relation1, RelArgType type1,
			EST_String &relation2_name, EST_Relation *&relation2, RelArgType type2,
			EST_String &relation3_name, EST_Relation *&relation3, RelArgType type3
			)
{
  unpack_module_args(args, utt);

  unpack_relation_arg(utt, CDR_to1(args)?CAR2(args):NIL, relation1_name, relation1, type1);
  unpack_relation_arg(utt, CDR_to2(args)?CAR3(args):NIL, relation2_name, relation2, type2);
  unpack_relation_arg(utt, CDR_to3(args)?CAR4(args):NIL, relation3_name, relation3, type3);
}

void unpack_module_args(LISP args, 
			EST_Utterance *&utt,
			EST_String &relation1_name, EST_Relation *&relation1, RelArgType type1,
			EST_String &relation2_name, EST_Relation *&relation2, RelArgType type2,
			EST_String &relation3_name, EST_Relation *&relation3, RelArgType type3,
			EST_String &relation4_name, EST_Relation *&relation4, RelArgType type4
			)
{
  unpack_module_args(args, utt);

  unpack_relation_arg(utt, CDR_to1(args)?CAR2(args):NIL, relation1_name, relation1, type1);
  unpack_relation_arg(utt, CDR_to2(args)?CAR3(args):NIL, relation2_name, relation2, type2);
  unpack_relation_arg(utt, CDR_to3(args)?CAR4(args):NIL, relation3_name, relation3, type3);
  unpack_relation_arg(utt, CDR_to4(args)?CAR5(args):NIL, relation4_name, relation4, type4);
}

void unpack_module_args(LISP args, 
			EST_Utterance *&utt,
			EST_String &relation1_name, EST_Relation *&relation1, RelArgType type1,
			EST_String &relation2_name, EST_Relation *&relation2, RelArgType type2,
			EST_String &relation3_name, EST_Relation *&relation3, RelArgType type3,
			EST_String &relation4_name, EST_Relation *&relation4, RelArgType type4,
			EST_String &relation5_name, EST_Relation *&relation5, RelArgType type5
			)
{
  unpack_module_args(args, utt);

  unpack_relation_arg(utt, CDR_to1(args)?CAR2(args):NIL, relation1_name, relation1, type1);
  unpack_relation_arg(utt, CDR_to2(args)?CAR3(args):NIL, relation2_name, relation2, type2);
  unpack_relation_arg(utt, CDR_to3(args)?CAR4(args):NIL, relation3_name, relation3, type3);
  unpack_relation_arg(utt, CDR_to4(args)?CAR5(args):NIL, relation4_name, relation4, type4);
  unpack_relation_arg(utt, CDR_to5(args)?CAR6(args):NIL, relation5_name, relation5, type5);
}

