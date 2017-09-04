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
 /* --------------------------------------------------------------------  */
 /* Machine readable description of a module.                             */
 /*                                                                       */
 /*************************************************************************/

#include <iostream>
#include "siod.h"
#include "ModuleDescription.h"

// to make life easier
static inline EST_String S(const char *s) { return EST_String(s); }

struct ModuleDescription *ModuleDescription::create()
{
  struct ModuleDescription *desc = new ModuleDescription;

  desc->name = "";
  desc->version = 0.0;
  desc->organisation = "";
  desc->author = "";
  desc->description[0] = NULL;
  desc->input_streams[0].name = NULL;
  desc->optional_streams[0].name = NULL;
  desc->output_streams[0].name = NULL;
  desc->parameters[0].name = NULL;

  return desc;
}

EST_String ModuleDescription::to_string(const ModuleDescription &desc)
{
    EST_String s;
    char buf[10];
    int i;

    if (desc.input_streams[0].name 
	|| desc.optional_streams[0].name
	|| desc.output_streams[0].name)
    {
	s += S("(") + desc.name + S(" UTT");
	for(i=0; i<MD_MAX_INPUT_STREAMS && desc.input_streams[i].name; i++)
	    s += S(" \"") + desc.input_streams[i].name + S("StreamName\"");
	for(i=0; i<MD_MAX_OPTIONAL_STREAMS && desc.optional_streams[i].name; i++)
	    s += S(" \"") + desc.optional_streams[i].name + S("StreamName\"");
	for(i=0; i<MD_MAX_OUTPUT_STREAMS && desc.output_streams[i].name; i++)
	    s += S(" \"") + desc.output_streams[i].name + S("StreamName\"");
	s += S(")\n\n");
    }

    sprintf(buf, "%.2f", desc.version);
    s += S("Module: ") + desc.name + S(" version: ") + buf + S("\n\n");
    s += S("From: ") + desc.organisation + S("\n\n");
    s += S("By: ") + desc.author + S("\n");
    s += S("\n");
    for(i=0; i<MD_MAX_DESCRIPTION_LINES && desc.description[i]; i++)
	s += S("    ") + desc.description[i] + S("\n");
    s += S("\n");

    if (desc.input_streams[0].name)
    {
	s += S("Inputs Streams.\n");
	for(i=0; i<MD_MAX_INPUT_STREAMS && desc.input_streams[i].name; i++)
	    s += S("    ") + desc.input_streams[i].name + 
		S(":\n\t") + desc.input_streams[i].description + S("\n");
	s += S("\n");
    }

    if (desc.optional_streams[0].name)
    {
	s += S("Optional Streams.\n");
	for(i=0; i<MD_MAX_OPTIONAL_STREAMS && desc.optional_streams[i].name;
	    i++)
	    s += S("    ") + desc.optional_streams[i].name + S(":\n\t") + 
		desc.optional_streams[i].description + S("\n");
	s += S("\n");
    }

    if (desc.output_streams[0].name)
    {
	s += S("Output Streams.\n");
	for(i=0; i<MD_MAX_OUTPUT_STREAMS && desc.output_streams[i].name; i++)
	    s += S("    ") + desc.output_streams[i].name + S(":\n\t") 
		+ desc.output_streams[i].description + S("\n");
	s += S("\n");
    }

    if (desc.parameters[0].name)
    {
	s += S("Parameters.\n");
	for(i=0; i<MD_MAX_PARAMETERS && desc.parameters[i].name; i++)
	    s += S("    ") + desc.parameters[i].name + S(" (") + 
		(desc.parameters[i].type?desc.parameters[i].type:"UNTYPED") + 
		    S(")\t") 
		    + S(" [") + (desc.parameters[i].default_val?desc.parameters[i].default_val:"NO DEFAULT") + S("]:\n\t") 
			+ (desc.parameters[i].description?desc.parameters[i].description:"NO DESCRIPTION") + S("\n");
	s += S("\n");
    }

    return s;
}

int ModuleDescription::print(FILE *s, const ModuleDescription &desc)
{
  return fputs(ModuleDescription::to_string(desc), s);
}

ostream &print(ostream &s, const ModuleDescription &desc)
{
  return s << ModuleDescription::to_string(desc);
}

ostream &operator << (ostream &s, const ModuleDescription &desc)
{
  return print(s, desc);
}

//VAL_REGISTER_CLASS(moddesc,ModuleDescription)  // clang/llvm complains about this (Rob)
//SIOD_REGISTER_CLASS(moddesc,ModuleDescription)
VAL_REGISTER_TYPE(moddesc,ModuleDescription)
SIOD_REGISTER_TYPE(moddesc,ModuleDescription)
