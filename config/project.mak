 ########################################################-*-mode:Makefile-*-
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
 ##                   Date: Tue Oct  7 1997                               ##
 ## --------------------------------------------------------------------  ##
 ## Description of festival.                                              ##
 ##                                                                       ##
 ###########################################################################

PROJECT_NAME = Festival Speech Synthesis System
PROJECT_PREFIX = FESTIVAL
PROJECT_VERSION = 2.5.0
PROJECT_DATE = February 2017
PROJECT_STATE = current

# config files of projects we depend on

PROJECT_OTHER_CONFIGS = $(EST)/config/config

# Place to find the optional modules for this project.

MODULE_DIRECTORY = $(TOP)/src/modules

DISTRIBUTED_MODULES = \
	JAVA

DEVELOPMENT_MODULES = RJC_SYNTHESIS \
	UNISYN_PHONOLOGY

UTILITY_MODULES =

ALL_REAL_MODULES = \
	$(DISTRIBUTED_MODULES) \
	$(DEVELOPMENT_MODULES)

ALL_MODULES = \
	$(ALL_REAL_MODULES) \
	$(UTILITY_MODULES)

# Place where programs are compiled

PROJECT_MAIN_DIR=$(FESTIVAL_HOME)/src/main
PROJECT_SCRIPTS_DIR=$(FESTIVAL_HOME)/src/scripts

# Where the main RCS tree is, probably only used within CSTR

CENTRAL_DIR = $(LOCAL_REPOSITORY)/festival/code_base/festival

# Libraries defined in this project

PROJECT_LIBRARIES = Festival
PROJECT_LIBRARY_DIR_Festival = $(TOP)/src/lib
PROJECT_DEFAULT_LIBRARY = Festival

# Libraries used from other projects

REQUIRED_LIBRARIES = estools estbase eststring
REQUIRED_LIBRARY_DIR_estools = $(EST)/lib
REQUIRED_LIBRARY_DIR_estbase = $(EST)/lib
REQUIRED_LIBRARY_DIR_eststring = $(EST)/lib

REQUIRED_MAKE_INCLUDE = $(EST)/make.include

# Includes for this and related projects

PROJECT_INCLUDES = -I$(TOP)/src/include -I$(EST)/include

PROJECT_TEMPLATE_DIRS = src/arch/festival
PROJECT_TEMPLATE_DBS  = $(TOP) $(EST)

LIBRARY_TEMPLATE_DIRS_estools = $(LIBRARY_TEMPLATE_DIRS:%=$(EST)/%)

JAVA_CLASS_LIBRARY = $(TOP)/src/lib/festival.jar

JAVA_CLASSPATH= $(TOP)/lib/festival.jar:$(EST_HOME)/lib/est_$(EST_JAVA_VERSION).jar:$(SYSTEM_JAVA_CLASSPATH)

PROJECT_JAVA_ROOT=$(TOP)/src/modules/java

# Places to look for documentation

DOCXX_DIRS = $(TOP)/src 
MODULE_TO_DOCXX = perl $(TOP)/src/modules/utilities/extract_module_doc++.prl

FTLIBDIR = $(FESTIVAL_HOME)/lib 


