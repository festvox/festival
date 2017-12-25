###########################################################################
##                                                                       ##
##                Centre for Speech Technology Research                  ##
##                     University of Edinburgh, UK                       ##
##                      Copyright (c) 1996-2017                          ##
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
##                The Festival Speech Synthesis System                   ##
##                                                                       ##
##       Authors:  Alan W Black, Paul Taylor, Richard Caley and others   ##
##          Date:  December 2017                                         ##
##                                                                       ## 
###########################################################################
TOP=.
DIRNAME=.
BUILD_DIRS = src lib examples bin doc
ALL_DIRS=config $(BUILD_DIRS) testsuite
CONFIG=configure configure.in config.sub config.guess \
       missing install-sh mkinstalldirs
FILES = Makefile README.md ACKNOWLEDGMENTS NEWS COPYING INSTALL $(CONFIG)
VERSION=$(PROJECT_VERSION)

LOCAL_CLEAN= Templates.DB

ALL = .config_error $(BUILD_DIRS)

# Try and say if config hasn't been created
config_dummy := $(shell test -f config/config || ( echo '*** '; echo '*** Making default config file ***'; echo '*** '; ./configure; )  >&2)

# force a check on the system file
system_dummy := $(shell $(MAKE) -C $(TOP)/config -f make_system.mak TOP=.. system.mak)

include $(TOP)/config/common_make_rules

default_voices:
	./src/scripts/default_voices.sh

backup:  time-stamp
	 @ $(RM) -f $(TOP)/FileList
	 @ $(MAKE) file-list
	 @ sed 's/^\.\///' <FileList | sed 's/^/festival\//' >.file-list-all
	 @ (cd ..; tar cvf - `cat festival/.file-list-all` festival/.time-stamp | gzip > festival/festival-$(VERSION)-$(PROJECT_STATE).tar.gz )
	 @ $(RM) -f $(TOP)/.file-list-all
	 @ ls -l festival-$(VERSION)-$(PROJECT_STATE).tar.gz

time-stamp :
	@ echo festival $(VERSION) >.time-stamp
#    Too many randomly different unices out there ....
#	@ echo `id -u -n`@`hostname`.`domainname` >>.time-stamp
	@ date >>.time-stamp

test:
	@ $(MAKE) --no-print-directory -C testsuite test

config/config: config/config.in config.status
	./config.status

configure: configure.in
	autoconf

include $(EST)/config/rules/top_level.mak
include $(EST)/config/rules/install.mak
