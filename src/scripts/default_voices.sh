#!/bin/sh
#####################################################-*-mode:shell-script-*-
##                                                                       ##
##                                                                       ##
##                  Language Technologies Institute                      ##
##                     Carnegie Mellon University                        ##
##                         Copyright (c) 2017                            ##
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
##  CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK         ##
##  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ##
##  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ##
##  SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE      ##
##  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ##
##  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ##
##  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ##
##  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ##
##  THIS SOFTWARE.                                                       ##
##                                                                       ##
###########################################################################
##                                                                       ##
##  Download and install the defauly voice and lexicons                  ##
##                                                                       ##
###########################################################################

if [ ! -f ../festival/src/include/festival.h ]
then
    echo Not in the right directory: cannot install Festival default voice
    echo You should be in the festival source top level directory
    echo Where ls -l src/include/festival.h is found

    exit -1
fi

if [ ! -d packed ]
then
   mkdir packed
fi

( cd packed;
  wget http://www.festvox.org/packed/festival/2.4/voices/festvox_kallpc16k.tar.gz;
  wget http://www.festvox.org/packed/festival/2.4/festlex_CMU.tar.gz;
  wget http://www.festvox.org/packed/festival/2.4/festlex_POSLEX.tar.gz
)

THISDIR=`pwd`

( cd ..;
  tar zxvf $THISDIR/packed/festvox_kallpc16k.tar.gz;
  tar zxvf $THISDIR/packed/festlex_CMU.tar.gz;
  tar zxvf $THISDIR/packed/festlex_POSLEX.tar.gz
)

exit 0



