#!/bin/sh
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
 ## Guess what kind of system we are on.                                  ##
 ##                                                                       ##
 ###########################################################################

# Where the Makefile fragments live
SYSTEMS=$1

# Drop and _xxx from the end 
OSTYPE=`uname -s |
	sed -e '/^\([^_]*\).*/s//\1/' -e '/\//s///g'`

# CPU, downcased, /s and some uninteresting details eliminated
MACHINETYPE=`{ mach || uname -m || echo unknown ; } 2>/dev/null |
	tr ABCDEFGHIJKLMNOPQRSTUVWXYZ/ abcdefghijklmnopqrstuvwxyz_ | 
	sed -e 's/i[0-9]86/ix86/' \
	    -e 's/sun4/sparc/' \
	    -e 's/ip[0-9]*/ip/'\
	    -e 's/ /_/g'\
	    -e 's/9000_7../hp9000/'
	    `

# OR revision, only take first two numbers.
OSREV=`{ uname -r || echo ""; } 2> /dev/null |
	sed -e 's/^\([^.]*\)\(\.[^-. ]*\).*/\1\2/'`

# Sort out various flavours of Linux
if [ "$OSTYPE" = Linux ]
    then
    if [ -f "/etc/redhat-release" ]
	then
	OSTYPE=RedHatLinux
	OSREV=`cat /etc/redhat-release | sed -e 's/[^0-9]*\([0-9.]*\).*/\1/'`
    elif [ -f "/etc/debian_version" ]
	then
	OSTYPE=DebianGNULinux
	OSREV=`cat /etc/debian_version`
    else
	# Generic unknown GNU/Linux system.
	OSTYPE=Linux
    fi
fi

# Make sure we actually have a .mak file for it, otherwise fall back
# to sensible defaults (for example, kernel version and architecture
# are completely irrelevant on Linux)
if [ ! -f "${SYSTEMS}/${MACHINETYPE}_${OSTYPE}${OSREV}.mak" ]; then
    if [ -f "${SYSTEMS}/${OSTYPE}${OSREV}.mak" ]; then
	MACHINETYPE=unknown
    elif [ -f "${SYSTEMS}/${MACHINETYPE}_${OSTYPE}.mak" ]; then
	OSREV=
    elif [ -f "${SYSTEMS}/unknown_${OSTYPE}.mak" ]; then
        MACHINETYPE=unknown
	OSREV=
    elif [ "$OSTYPE" = "RedHatLinux" -o "$OSTYPE" = "DebianGNULinux" ]; then
	MACHINETYPE=unknown
	OSTYPE=Linux
	OSREV=
    elif [ "$OSTYPE" = "Darwin" ]; then
        OSREV=
    else
	OSTYPE=unknown
	OSREV=
    fi
fi

echo ' ###########################################################################'
echo ' ## This file is created automatically from your config file.'
echo ' ## Do not hand edit.'
echo ' ## Created:'`date`
echo ' ###########################################################################'

echo ''

echo "OSTYPE:=$OSTYPE"
echo "MACHINETYPE:=$MACHINETYPE"
echo "OSREV:=$OSREV"
echo "SYSTEM_LOADED:=1"

exit 0
