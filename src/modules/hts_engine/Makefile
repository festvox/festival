# ----------------------------------------------------------------- #
#           The HMM-Based Speech Synthesis System (HTS)             #
#           festopt_hts_engine developed by HTS Working Group       #
#           http://hts-engine.sourceforge.net/                      #
# ----------------------------------------------------------------- #
#                                                                   #
#  Copyright (c) 2001-2011  Nagoya Institute of Technology          #
#                           Department of Computer Science          #
#                                                                   #
#                2001-2008  Tokyo Institute of Technology           #
#                           Interdisciplinary Graduate School of    #
#                           Science and Engineering                 #
#                                                                   #
# All rights reserved.                                              #
#                                                                   #
# Redistribution and use in source and binary forms, with or        #
# without modification, are permitted provided that the following   #
# conditions are met:                                               #
#                                                                   #
# - Redistributions of source code must retain the above copyright  #
#   notice, this list of conditions and the following disclaimer.   #
# - Redistributions in binary form must reproduce the above         #
#   copyright notice, this list of conditions and the following     #
#   disclaimer in the documentation and/or other materials provided #
#   with the distribution.                                          #
# - Neither the name of the HTS working group nor the names of its  #
#   contributors may be used to endorse or promote products derived #
#   from this software without specific prior written permission.   #
#                                                                   #
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND            #
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,       #
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF          #
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE          #
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS #
# BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,          #
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   #
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,     #
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON #
# ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,   #
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    #
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE           #
# POSSIBILITY OF SUCH DAMAGE.                                       #
# ----------------------------------------------------------------- #

TOP=../../..
DIRNAME=src/modules/hts_engine

H = HTS_engine.h HTS_hidden.h

CPPSRCS = fest2hts_engine.cc
CSRCS = HTS_audio.c HTS_engine.c HTS_gstream.c HTS_label.c HTS_misc.c HTS_model.c HTS_pstream.c HTS_sstream.c HTS_vocoder.c

SRCS = $(CPPSRCS) $(CSRCS)

OBJS = $(CPPSRCS:.cc=.o) $(CSRCS:.c=.o)
INFOS = README COPYING INSTALL AUTHORS

FILES=Makefile $(SRCS) $(H) $(INFOS)

LOCAL_INCLUDES = -I ../include -D FESTIVAL

INLIB = $(TOP)/src/lib/libFestival.a

ALL = .buildlib

include $(TOP)/config/common_make_rules
