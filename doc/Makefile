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
DSSL_SANE_DB=$(EST)/doc/sane_to_docbook.dsl
DSSSL_HTML=$(EST)/doc/cstr.dssl
DSSSL=$(DSSSL_HTML)

# Temporarilly we explicitly list the programs which have been documented

MAIN_TO_DOCUMENT=

EXAMPLE_TO_DOCUMENT=

FESTIVAL=$(TOP)/bin/festival --libdir $(TOP)/lib

# Include some of EST documentation.
DOCXX_EXTRA_FILES = 

DOCXXIMAGES = edcrest.gif cstr.gif \
	est.jpg est_small.jpg \
	festival.jpg festival_small.jpg festival_tiny.jpg
DOCXXFILES=  classHeader.inc hierHeader.inc indexHeader.inc \
	     banner.inc $(DOCXXIMAGES)

MANPAGES = festival.head festival.tail \
           festival_client.head festival_client.tail

#SGMLFILES = festival.sgml \
#	introductory.sgml basics.sgml core.sgml advanced.sgml programming.sgml

FILES=Makefile $(MANPAGES) festival.texi $(SGMLFILES) refcard.tex  $(DOCXXFILES)
LOCAL_CLEAN = *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr

ALL = festival.1 festival_client.1

include $(TOP)/config/common_make_rules
include $(EST)/config/rules/doc.mak

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
	( cd info; makeinfo festival.texi )
	@ rm info/*.texi
# texi2html is available from http://wwwcn.cern.ch/dci/texi2html/
festival.html: festival.texi festfunc.texi festvars.texi
	@ if [ ! -d html ] ; \
          then mkdir -p html ; fi
	(cd html; texi2html -number -split_chapter ../festival.texi)
# give the html files background color of white
	@ for i in html/*.html ; \
	  do \
	    sed 's/<BODY>/<BODY bgcolor="#ffffff">/' $$i >ttt.html; \
	    mv ttt.html $$i ; \
	  done
festival.ps: festival.dvi
	dvips -f festival.dvi >festival.ps
festival.dvi: festival.texi festfunc.texi festvars.texi
	tex festival.texi
	texindex festival.cp
	tex festival.texi
doc:	festival.ps festival.html festival.info

festfunc.texi festvars.texi festfeat.texi: $(TOP)/src/main/festival
	echo "(load_library \"festdoc.scm\") (make-doc)" | $(FESTIVAL)

refcard.dvi:	refcard.tex
	latex refcard.tex
refcard.ps:	refcard.dvi
	dvips -f -t landscape refcard.dvi >refcard.ps

