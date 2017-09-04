
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
 //                                                                        \\
 //                 Centre for Speech Technology Research                  \\
 //                      University of Edinburgh, UK                       \\
 //                        Copyright (c) 1996,1997                         \\
 //                         All Rights Reserved.                           \\
 //   Permission to use, copy, modify, distribute this software and its    \\
 //   documentation for research, educational and individual use only, is  \\
 //   hereby granted without fee, subject to the following conditions:     \\
 //    1. The code must retain the above copyright notice, this list of    \\
 //       conditions and the following disclaimer.                         \\
 //    2. Any modifications must be clearly marked as such.                \\
 //    3. Original authors' names are not deleted.                         \\
 //   This software may not be used for commercial purposes without        \\
 //   specific prior written permission from the authors.                  \\
 //   THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        \\
 //   DISCLAIM ALL WARRANTIES With REGARD TO THIS SOFTWARE, INCLUDING      \\
 //   ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   \\
 //   SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     \\
 //   FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    \\
 //   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   \\
 //   AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          \\
 //   ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       \\
 //   THIS SOFTWARE.                                                       \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
 //                                                                        \\
 //                  Author: Richard Caley (rjc@cstr.ed.ac.uk)                         \\
 //  --------------------------------------------------------------------  \\
 //  Class which describes what festival voices are available.                                               \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\

package cstr.festival.jsapi ;

import java.lang.*;
import java.util.*;
import java.awt.*;

import javax.speech.*;
import javax.speech.synthesis.*;

public class EngineCentral
	implements javax.speech.EngineCentral
{
  private static  String server;
  private static int port;

  static 
    {
      server = System.getProperty("festival.server",
				  "localhost");
      
      port = Integer.getInteger("festival.port",
				1314).intValue();
    }
  
  Voice rab = new Voice("Roger Burroughs",
			Voice.GENDER_MALE,
			Voice.AGE_MIDDLE_ADULT,
			"LPC Diphone"
			);
  Voice ked = new Voice("Kurt Dusterhoff",
			Voice.GENDER_MALE,
			Voice.AGE_YOUNGER_ADULT,
			"LPC Diphone"
			);

  SynthesizerModeDesc festival_rab 
	=  new FestivalModeDesc(server,
				port,
				server+":"+Integer.toString(port),
				Locale.UK,
				Boolean.TRUE,
				new Voice [] {rab}
				);

  SynthesizerModeDesc festival_ked
	=  new FestivalModeDesc(server,
				port,
				server+":"+Integer.toString(port),
				Locale.US,
				Boolean.TRUE,
				new Voice [] {ked}
				);
  
  public EngineList createEngineList(EngineModeDesc require)
    throws SecurityException
    {
      EngineList engines = new EngineList();

      engines.addElement(festival_rab);
      engines.addElement(festival_ked);

      return engines;
    }
}
