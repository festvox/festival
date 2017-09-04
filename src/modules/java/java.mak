 ###########################################################################
 ##                                                                       ##
 ##                Centre for Speech Technology Research                  ##
 ##                     University of Edinburgh, UK                       ##
 ##                         Copyright (c) 1996                            ##
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
 ##                   Date: Wed May 27 1998                               ##
 ## --------------------------------------------------------------------  ##
 ## Make rules for Java module                                            ##
 ##                                                                       ##
 ###########################################################################

INCLUDE_JAVA=1

MOD_DESC_JAVA=Festival Java Support

# PROJECT_LIBRARIES := festjava $(PROJECT_LIBRARIES)
PROJECT_LIBRARY_DIR_festjava = $(TOP)/src/lib
# PROJECT_SHARED_LIBRARIES := festjava $(PROJECT_SHARED_LIBRARIES)

EST_JAVA_VERSION=basic

ifeq ($(DIRNAME),src/modules)
    LIB_BUILD_DIRS := java $(LIB_BUILD_DIRS)
endif

ifeq ($(DIRNAME),src/scripts)
	SCRIPTS := festival_client_java.sh $(SCRIPTS)
endif


include $(EST)/config/compilers/$(JAVA_COMPILER).mak

ifndef SHARED
.config_error:: FORCE
	@echo "+--------------------------------------------------"
	@echo "| Must compile speech tools shared for Java support"
	@echo "+--------------------------------------------------"
	@exit 1
endif
