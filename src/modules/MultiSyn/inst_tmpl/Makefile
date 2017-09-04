###########################################################################
##                                                                       ##
##                Centre for Speech Technology Research                  ##
##                 (University of Edinburgh, UK) and                     ##
##                           Korin Richmond                              ##
##                         Copyright (c) 2002                            ##
##                        All Rights Reserved.                           ##
##                                                                       ##
##  Permission is hereby granted, free of charge, to use and distribute  ##
##  this software and its documentation without restriction, including   ##
##  without limitation the rights to use, copy, modify, merge, publish,  ##
##  distribute, sublicense, and/or sell copies of this work, and to      ##
##  permit persons to whom this work is furnished to do so, subject to   ##
##  the following conditions:                                            ##
##                                                                       ##
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
##  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT   ##
##  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ##
##  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ##
##  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ##
##  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ##
##  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ##
##  THIS SOFTWARE.                                                       ##
##                                                                       ##
###########################################################################
#              Makefile for base class instantiations                     #
#                                                                         #
#  Author: Korin Richmond (korin@cstr.ed.ac.uk) Aug 2002                  #
###########################################################################

TOP=../../../..
DIRNAME=src/modules/MultiSyn/inst_tmpl

LOCAL_INCLUDES = -I$(TOP)/src/modules/MultiSyn

TSRCS = hash_s_itemlistp_t.cc \
	hash_itemp_tcdatap_t.cc \
        list_uttp_t.cc \
        list_itemp_t.cc \
	list_voicemodulep_t.cc \
        list_strlist_t.cc \
	vector_jccp_t.cc \
	list_scorepair_t.cc

CPPSRCS = $(TSRCS)

SRCS = $(CPPSRCS)
OBJS = $(CPPSRCS:.cc=.o)
FILES = $(SRCS) Makefile 

ALL = .buildlib $(BUILD_DIRS)

include $(TOP)/config/common_make_rules

