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
 ## --------------------------------------------------------------------  ##
 ## Very common settings to avoid repetition.                             ##
 ##                                                                       ##
 ###########################################################################

###########################################################################
## Installation directories

INSTALL_PREFIX=/usr/local

BINDIR=$(INSTALL_PREFIX)/bin
LIBDIR=$(INSTALL_PREFIX)/lib
INCDIR=$(INSTALL_PREFIX)/include
MANDIR=$(INSTALL_PREFIX)/man

###########################################################################
## Where the central RCS masters are stored.
## 
## Used for development at CSTR, you can probably ignore it.

LOCAL_REPOSITORY =

###########################################################################
## Where to find Network Audio

NAS_INCLUDE = /usr/X11R6/include
NAS_LIB = /usr/X11R6/lib

###########################################################################
## Where to find Enlightenment Speech Demon

ESD_INCLUDE = /usr/local/include
ESD_LIB = /usr/local/lib

###########################################################################
## Where to find X11

X11_INCLUDE = /usr/X11R6/include
X11_LIB     = /usr/X11R6/lib

###########################################################################
## TCL support

TCL_INCLUDE = /usr/local/include
TCL_LIB     = /usr/local/lib
TCL_LIBRARY = -ltcl7.6

###########################################################################
## Efence library for malloc debugging

EFENCE_LIB = /usr/local/lib

###########################################################################
## Commands.

## Must support -nt
GNUTEST = gnutest

## 
INSTALL_PROG = install

## Used to index libraries
RANLIB = ranlib

## echo without a newline
ECHO_N = echo -n

## make depend for when we haven't specified a compiler
MAKE_DEPEND = makedepend $(INCLUDES) $(TEMPLATES) $(TEMPLATE_SPECIFIC)

## Generic library building
BUILD_LIB =$(AR) cruv

## generic library indexing
INDEX_LIB = $(RANLIB)

## shrink executables
STRIP = strip

## Useful sloth
DO_NOTHING = true
DO_NOTHING_ARGS = :

## different types of awk. For our purposes gawk can be used for nawk
AWK = awk
NAWK = nawk

## Perl. Not used in build, but we have some perl scripts.
PERL=/usr/bin/perl

## Just in case someone has a broken test
TEST = test

## Must understand -nt
GNUTEST = gnutest

## Avoid clever RMs people may have on their path
RM = /bin/rm

###########################################################################
## Arguments for DOC++ for creating documentation

DOCXX = doc++_sane
DOCXX_ARGS = -a -f -B banner.inc -M sane -D 'SYSTEM "$(EST_HOME)/doc/sane.dtd"'


COMPILER_VERSION_COMMAND=true
JAVA_COMPILER_VERSION_COMMAND=true

JAVA_SYSTEM_INCLUDES  = -I$(JAVA_HOME)/include/genunix
