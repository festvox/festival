
              The Festival Speech Synthesis System
                   version 2.5 February 2017
 
This directory contains the Festival Speech Synthesis System,
developed at CSTR, University of Edinburgh. The project was originally
started by Alan W Black and Paul Taylor but many others have been
involved (see ACKNOWLEDGEMENTS file for full list).

Festival offers a general framework for building speech synthesis
systems as well as including examples of various modules.  As a whole
it offers full text to speech through a number APIs: from shell level,
though a Scheme command interpreter, as a C++ library, and an Emacs
interface.  Festival is multi-lingual (currently English (US and UK)
and Spanish are distributed but a host of other voices have been
developed by others) though English is the most advanced.

The system is written in C++ and uses the Edinburgh Speech Tools
for low level architecture and has a Scheme (SIOD) based command
interpreter for control.  Documentation is given in the FSF texinfo
format which can generate, a printed manual, info files and HTML.

COPYING

Festival is free.  Earlier versions were restricted to non-commercial
use but we have now relaxed those conditions.  The licence is an X11
style licence thus it can be incorporated in commercial products
and free source products without restriction.  See COPYING for the
actual details.

INSTALL

Festival should run on any standard Unix platform.  It has already run
on Solaris, SunOS, Linux and FreeBSD.  It requires a C++ compiler (GCC
2.7.2, 2.8.1, 2.95.[123], 3.2.3 3.3.2 RedHat "gcc-2.96", gcc 3.3, gcc
4.4.x and gcc-4.5.x, gcc-6.2.0 are our standard compilers) to
install. A port to Windows XP/NT/95/98 and 2000 using either Cygnus
GNUWIN32, this is still new but many people are successfully using it,
it works fine with Windows 10 bash

A detailed description of installation and requirements for the whole
system is given in the file INSTALL read that for details.

NEWS

Keep abreast of Festival News by regularly checking the Festival homepage
   http://www.cstr.ed.ac.uk/projects/festival/
or the US site
   http://festvox.org/festival/

New in Festival 2.5
   Support for gcc6 (which is a somewhat different dialect of C++)

New in Festival 2.2
   updates to hts (hts_engine 1.07) and clustergen

New in Festival 2.1
   Support for various new GCC compilers
   Improved support for hts, clustergen, clunits and multisyn voices
   lots of wee bugs fixed

