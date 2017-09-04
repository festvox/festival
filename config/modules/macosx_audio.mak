 ###########################################################################
 ##                                                                       ##
 ##                 Author: Brian Foley (bfoley@compsoc.nuigalway.ie)     ##
 ##                   Date: Wed Feb 17 2004                               ##
 ## --------------------------------------------------------------------  ##
 ## Definitions for MacOS X audio support.                                ##
 ##                                                                       ##
 ###########################################################################

INCLUDE_MACOSX_AUDIO=1

MOD_DESC_MACOSX_AUDIO=(from EST) CoreAudio audio module for MacOS X systems

AUDIO_DEFINES += -DSUPPORT_MACOSX_AUDIO

MODULE_LIBS += -framework CoreAudio -framework AudioUnit -framework AudioToolbox -framework Carbon
