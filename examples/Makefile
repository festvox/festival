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
TOP=..
DIRNAME=examples
BUILD_DIRS=
ALL_DIRS=$(BUILD_DIRS) songs

EXTEXTS = intro.text spintro.text benchmark.text
EXAMPLES = webdemo.scm ex1.email ex1.ogi example.sable example2.sable \
           tobi.stml example.th example.apml
SCRIPTS = saytime.sh text2pos.sh latest.sh \
           scfg_parse_text.sh text2wave.sh make_utts.sh dumpfeats.sh \
           durmeanstd.sh powmeanstd.sh run-festival-script.sh text2utt.sh
SHELL_SCRIPTS = benchmark festival_client.pl
SCMS = toksearch.scm th-mode.scm addr-mode.scm
SAMPLEC = festival_client.c festival_client.h
OTHERS = apml.dtd
FILES=Makefile $(EXAMPLES) $(EXTEXTS) $(SCRIPTS) $(SHELL_SCRIPTS) $(SCMS) $(SAMPLEC) $(OTHERS) speech_pm_1.0.tar

ALL = $(SCRIPTS:.sh=)
LOCAL_CLEAN = $(SCRIPTS:.sh=)

include $(TOP)/config/common_make_rules

$(ALL) : % : %.sh
	rm -f $@
	@echo "#!/bin/sh" >$@
	@echo "\"true\" ; exec "$(FESTIVAL_HOME)/bin/festival --script '$$0 $$*' >>$@
	cat $< >>$@
	chmod +x $@

festival_client: festival_client.o festival_client.h
	$(LINK_COMMAND) -o festival_client festival_client.o $(LIBS)

festival_client.o: festival_client.c festival_client.h
	$(CC_COMMAND) -DSTANDALONE festival_client.c -o festival_client.o

# Do this manually to make sure Festival.tar (perl module) is up to date.

speech_pm_1.0.tar: $(wildcard speech_pm_1.0/*) $(wildcard  speech_pm_1.0/*/*) $(wildcard  speech_pm_1.0/*/*/*)
	-chmod +w speech_pm_1.0.tar
	tar cvf speech_pm_1.0.tar `cat speech_pm_1.0/MANIFEST|sed -e 's/^/speech_pm_1.0\//'`
