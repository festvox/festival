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
DIRNAME=doc

DOCNAME=festival

FESTIVAL=$(TOP)/bin/festival --libdir $(TOP)/lib

MANPAGES = festival.head festival.tail \
           festival_client.head festival_client.tail \
           text2wave.head text2wave.tail

#SGMLFILES = festival.sgml \
#	introductory.sgml basics.sgml core.sgml advanced.sgml programming.sgml

FILES=Makefile $(MANPAGES) festival.texi $(SGMLFILES) refcard.tex  $(DOCXXFILES)
LOCAL_CLEAN = *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr

ALL = festival.1 festival_client.1 text2wave.1

include $(TOP)/config/common_make_rules

%.1 : %.head %.options %.tail 
	cat $^ >$@
%.options : $(TOP)/src/main/%
	$(TOP)/bin/$* -man_options >$@
%.options : $(TOP)/src/main/%.exe
	$(TOP)/bin/$* -man_options >$@

festival.info: festival.texi festfunc.texi festvars.texi festfeat.texi
	@ if [ ! -d info ] ; \
          then mkdir -p info ; fi
	@ sed 's/@url{/@file{/g' <festival.texi >info/festival.texi
	@ cp festfunc.texi info/festfunc.texi
	@ cp festvars.texi info/festvars.texi
	@ cp festfeat.texi info/festfeat.texi
	( cd info; makeinfo --output festival.info festival.texi )
	@ rm info/*.texi

festival.html: festival.texi festfunc.texi festvars.texi
	@ if [ ! -d html ] ; \
          then mkdir -p html ; fi
	(cd html; makeinfo --html --ifinfo  ../festival.texi)
# give the html files background color of white
	#@ for i in html/*.html ; \
	#  do \
	#    sed 's/<BODY>/<BODY bgcolor="#ffffff">/' $$i >ttt.html; \
	#    mv ttt.html $$i ; \
	#  done
festival.ps: festival.dvi
	dvips -f festival.dvi >festival.ps
festival.dvi: festival.texi festfunc.texi festvars.texi
	tex festival.texi
	texindex festival.cp
	tex festival.texi
doc:	festival.ps festival.html festival.info refcard.ps

festfunc.texi festvars.texi festfeat.texi: $(TOP)/src/main/festival doc.scm
	$(FESTIVAL) < doc.scm

refcard.dvi:	refcard.tex
	latex refcard.tex
refcard.ps:	refcard.dvi
	dvips -f -t landscape refcard.dvi >refcard.ps

