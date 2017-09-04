###########################################################################
##                                                                       ##
##                Centre for Speech Technology Research                  ##
##                     University of Edinburgh, UK                       ##
##                       Copyright (c) 1996,1997                         ##
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
#                                                                         #
# Makefile for festival directory                                         #
#                                                                         #
# Contains basic interface functions, initialization, and functions for   #
# using various models                                                    #
#                                                                         #
###########################################################################
TOP=../../..
DIRNAME=src/arch/festival
H = festivalP.h
SRCS =  festival.cc  Phone.cc utterance.cc features.cc \
       wave.cc wagon_interp.cc linreg.cc \
       audspio.cc server.cc client.cc web.cc tcl.cc \
       wfst.cc ngram.cc viterbi.cc \
       ModuleDescription.cc $(TSRCS)

OBJS = $(SRCS:.cc=.o)

FILES = Makefile $(SRCS) $(H)

#LOCAL_TEMPLATES = -pti.

INLIB = $(TOP)/src/lib/libFestival.a

ALL = .buildlib

include $(TOP)/config/common_make_rules

LOCAL_DEFINES += $(FESTIVAL_DEFINES)
LOCAL_INCLUDES += $(FESTIVAL_INCLUDES)

festival.o: festival.cc
	$(CXX_COMMAND_TEMPLATES) -DFTNAME='$(PROJECT_NAME)' -DFTLIBDIRC='$(FTLIBDIR)' -DFTVERSION='$(PROJECT_VERSION)' -DFTSTATE='$(PROJECT_STATE)'  -DFTDATE='$(PROJECT_DATE)' -DFTOSTYPE=\"$(SYSTEM_TYPE)\" festival.cc


