 ###########################################################################
 ##                                                                       ##
 ##                Centre for Speech Technology Research                  ##
 ##                     University of Edinburgh, UK                       ##
 ##                    Copyright (c) 1994,1995,1996                       ##
 ##                        All Rights Reserved.                           ##
 ##                                                                       ##
 ##  Permission is hereby granted, free of charge, to use and distribute  ##
 ##  this software and its documentation without restriction, including   ##
 ##  without limitation the rights to use, copy, modify, merge, publish,  ##
 ##  distribute, sublicense, and/or sell copies of this work, and to      ##
 ##  permit persons to whom this work is furnished to do so, subject to   ##
 ##  the following conditions:                                            ##
 ##   1. The code must retain the above copyright notice, this list of    ##
 ##      conditions and the following disclaimer.                         ##
 ##   2. Any modifications must be clearly marked as such.                ##
 ##   3. Original authors' names are not deleted.                         ##
 ##   4. The authors' names are not used to endorse or promote products   ##
 ##      derived from this software without specific prior written        ##
 ##      permission.                                                      ##
 ##                                                                       ##
 ##  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ##
 ##  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ##
 ##  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ##
 ##  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ##
 ##  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ##
 ##  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ##
 ##  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ##
 ##  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ##
 ##  THIS SOFTWARE.                                                       ##
 ##                                                                       ##
 ###########################################################################
 ##                                                                       ##
 ##                 Author: Richard Caley (rjc@cstr.ed.ac.uk)             ##
 ##                   Date: June 1997                                     ##
 ## --------------------------------------------------------------------- ##
 ## Makefile for bin directory.                                           ##
 ##                                                                       ##
 ## If we are staticly linked, we link to things in main, otherwise we    ##
 ## write scripts which set LD_LIBRARY_PATH.                              ##
 ##                                                                       ##
 ## Things in scripts are preprocessed.                                   ##
 ##                                                                       ##
 ###########################################################################

TOP=..
DIRNAME=bin
FILES= Makefile VCLocalRules

ALL = .remove_links .link_main .process_scripts text2wave

include $(TOP)/config/common_make_rules
include $(EST)/config/rules/bin_process.mak

text2wave:
	@ cp -p $(TOP)/examples/text2wave .

