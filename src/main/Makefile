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
TOP=../..
DIRNAME=src/main

SRCS = festival_main.cc audsp.cc festival_client.cc
OBJS = $(SRCS:.cc=.o)
FILES=Makefile $(SRCS)
LOCAL_CLEAN = $(ETCDIR)/audsp $(ETCDIR)/.made

ETCDIR=$(TOP)/lib/etc/$(SYSTEM_TYPE)

ALL_EXECS = festival  festival_client

ALL = $(ALL_EXECS) make_audiosp

include $(TOP)/config/common_make_rules
include $(EST)/config/rules/bin_process.mak

ETCDIR=$(TOP)/lib/etc/$(SYSTEM_TYPE)

ifeq (SIOD_PYTHON, $(findstring SIOD_PYTHON, $(INCLUDE_MODULES)))
	LIBS += $(shell python3-config --ldflags) -ldl -lutil -lm
endif

festival: festival_main.o $(LIBDEPS)
	$(LINK_COMMAND) -o festival festival_main.o $(LIBS)

festival_client: festival_client.o $(REQUIRED_LIBDEPS)
	$(LINK_COMMAND) -o festival_client festival_client.o $(LIBS)

$(ETCDIR)/audsp: $(ETCDIR)/.made audsp.o $(LIBDEPS)
	$(LINK_COMMAND) -o $(ETCDIR)/audsp audsp.o $(LIBS)

# Can't just rely on the dir as it gets updated with new files
# check for the data of a file created in etcdir

make_audiosp: $(ETCDIR)/audsp
	@$(DO_NOTHING)

$(ETCDIR)/.made:
	@ if [ ! -d $(ETCDIR) ] ; \
          then mkdir -p $(ETCDIR); fi
	@ if [ ! -f $(ETCDIR)/.made ] ; \
          then touch $(ETCDIR)/.made ; fi

