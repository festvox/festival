###########################################################################
##                                                                       ##
##  ---------------------------------------------------------------      ##
##      The HMM-Based Speech Synthesis System (HTS): version 1.1b        ##
##                        HTS Working Group                              ##
##                                                                       ##
##                   Department of Computer Science                      ##
##                   Nagoya Institute of Technology                      ##
##                                and                                    ##
##    Interdisciplinary Graduate School of Science and Engineering       ##
##                   Tokyo Institute of Technology                       ##
##                      Copyright (c) 2001-2003                          ##
##                        All Rights Reserved.                           ##
##                                                                       ##
##  Permission is hereby granted, free of charge, to use and             ##
##  distribute this software and its documentation without               ##
##  restriction, including without limitation the rights to use,         ##
##  copy, modify, merge, publish, distribute, sublicense, and/or         ##
##  sell copies of this work, and to permit persons to whom this         ##
##  work is furnished to do so, subject to the following conditions:     ##
##                                                                       ##
##    1. The code must retain the above copyright notice, this list      ##
##       of conditions and the following disclaimer.                     ##
##                                                                       ##
##    2. Any modifications must be clearly marked as such.               ##
##                                                                       ##
##  NAGOYA INSTITUTE OF TECHNOLOGY, TOKYO INSITITUTE OF TECHNOLOGY,      ##
##  HTS WORKING GROUP, AND THE CONTRIBUTORS TO THIS WORK DISCLAIM        ##
##  ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL           ##
##  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT       ##
##  SHALL NAGOYA INSTITUTE OF TECHNOLOGY, TOKYO INSITITUTE OF            ##
##  TECHNOLOGY, HTS WORKING GROUP, NOR THE CONTRIBUTORS BE LIABLE        ##
##  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY            ##
##  DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,      ##
##  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTUOUS       ##
##  ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR              ##
##  PERFORMANCE OF THIS SOFTWARE.                                        ##
##                                                                       ##
###########################################################################
##  This directory contains some low level support for statitsical       ##
##  parametric speech synthesis, mostly take from Nagoya Institutes of   ##
##  Technologies HTS Engine (though often with significant modification  ##
##                                                                       ##
##  These are specifically designed to support the Clustergen synthesis  ##
##  technique.                                                           ##
##                                                                       ##
##  Alan W Black (awb@cs.cmu.edu)                                        ##
###########################################################################
TOP=../../..
DIRNAME=src/modules/clustergen

H = mlsa_resynthesis.h HTS_vocoder_me.h vc.h simple_mlpg.h
CPPSRCS = clustergen.cc mlsa_resynthesis.cc vc.cc simple_mlpg.cc HTS_vocoder_me.cc
SRCS = $(CPPSRCS)
OBJS = $(CPPSRCS:.cc=.o) 

FILES=Makefile $(SRCS) $(H)

LOCAL_INCLUDES = -I../include -I ../hts_engine -DFESTIVAL

INLIB = $(TOP)/src/lib/libFestival.a

ALL = .buildlib

include $(TOP)/config/common_make_rules


