
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
 //                  Author: Richard Caley (rjc@cstr.ed.ac.uk)             \\
 //  --------------------------------------------------------------------  \\
 //  Simple excercise of the java speech API synthesis methods.            \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\


package cstr.testPrograms ;

import java.lang.*;
import java.util.*;

import javax.speech.*;
import javax.speech.synthesis.*;

import cstr.festival.jsapi.*;

class MySpeakableListener
		extends SpeakableAdapter
{
  SpeakableEvent lastEvent=null;

  public MySpeakableListener()
    {
      lastEvent=null;
    }

  public synchronized void topOfQueue(SpeakableEvent e)
    {
      lastEvent=e;
      notifyAll();
    }

  public synchronized void markerReached(SpeakableEvent e)
    {
      lastEvent=e;
    }

  public synchronized void speakableStarted(SpeakableEvent e)
    {
      lastEvent=e;
    }

  public synchronized void speakableCancelled(SpeakableEvent e)
    {
      lastEvent=e;
    }

  public synchronized void speakablePaused(SpeakableEvent e)
    {
      lastEvent=e;
    }

  public synchronized void speakableResumed(SpeakableEvent e)
    {
      lastEvent=e;
    }

  public synchronized void speakableEnded(SpeakableEvent e)
    {
      lastEvent=e;
      notifyAll();
    }

  public synchronized void wordStarted(SpeakableEvent e)
    {
      lastEvent=e;
      System.out.println("SpeakableEvent wordStarted");
    }

  public synchronized boolean waitForEnd()
    {
      while (lastEvent == null 
	     || ( lastEvent.getId() != SpeakableEvent.SPEAKABLE_ENDED
		  && lastEvent.getId() != SpeakableEvent.SPEAKABLE_CANCELLED
		  )
	      )
	{
	  try { wait(); } 
	  catch (InterruptedException e) 
	    {
	      return false;
	    }
	}
      return true;
    }
}

class VerboseSpeakableListener
	extends MySpeakableListener
	implements SpeakableListener
{
  public VerboseSpeakableListener()
    {
      super();
    }

  public synchronized void topOfQueue(SpeakableEvent e)
    {
      super.topOfQueue(e);
      System.out.println("SpeakableEvent '"+ e.getSource() +"' topOfQueue");
    }

  public synchronized void markerReached(SpeakableEvent e)
    {
      super. markerReached(e);
      System.out.println("SpeakableEvent '"+ e.getSource() +"' markerReached");
    }

  public synchronized void speakableStarted(SpeakableEvent e)
    {
      super.speakableStarted(e);
      System.out.println("SpeakableEvent '"+ e.getSource() +"' speakableStarted");
    }

  public synchronized void speakableCancelled(SpeakableEvent e)
    {
      super.speakableCancelled(e);
      System.out.println("SpeakableEvent '"+ e.getSource() +"' speakableCancelled");
    }

  public synchronized void speakablePaused(SpeakableEvent e)
    {
      super.speakablePaused(e);
      System.out.println("SpeakableEvent '"+ e.getSource() +"' speakablePaused");
    }

  public synchronized void speakableResumed(SpeakableEvent e)
    {
      super.speakableResumed(e);
      System.out.println("SpeakableEvent '"+ e.getSource() +"' speakableResumed");
    }

  public synchronized void speakableEnded(SpeakableEvent e)
    {
      super.speakableEnded(e);
      System.out.println("SpeakableEvent '"+ e.getSource() +"' speakableEnded");
    }

  public synchronized void wordStarted(SpeakableEvent e)
    {
      super.wordStarted(e);
      System.out.println("SpeakableEvent '"+ e.getSource() +"' wordStarted");
    }

}

public class SayHelloWorld
{
  public static void main(String [] args)
    {
      SynthesizerModeDesc desc=null;
      boolean verbose=false;
      boolean sync=false;
      String [] text;

      int i;
      for(i=0; i<args.length; i++)
	if (args[i].equals("-l"))
	  {
	    listSynthesisers();
	    System.exit(1);
	  }
	else if (args[i].equals("-r"))
	  {
	    registerSynthesizer(args[++i]);
	  }
	else if (args[i].equals("-v"))
	  {
	    verbose=true;
	  }
	else if (args[i].equals("-s"))
	  {
	    sync=true;
	  }
	else if (args[i].substring(0,1).equals("-"))
	  {
	    System.err.println("Unknown option " + args[i]);
	    System.exit(1);
	  }
	else
	  break;

      if (i < args.length)
	{
	  text = new String[args.length-i];
	  for(int j=0; i<args.length; i++, j++)
	    {
	      text[j]=args[i];
	    }
	}
      else
	text = new String [] {"Hello world."};

      Synthesizer synth=null;

      try {
	synth = Central.createSynthesizer(desc);
      } catch (EngineException ex) {
	System.err.println("Engine Exception: "+ex.getMessage());
	System.exit(1);
      }

      if (synth==null)
	{
	  System.err.println("Error creating synthesizer");
	  System.exit(1);
	}

      if (verbose)
	describeSynthesizer((SynthesizerModeDesc)synth.getEngineModeDesc());

      try {
	synth.allocate();
      } catch (EngineException ex) {
	System.err.println("EngineException: "+ex.getMessage());
	System.exit(1);
      }
      
      MySpeakableListener listener=null;
	  
      for(int k=0; k<text.length; k++)
	{
	  if (verbose)
	    listener = new VerboseSpeakableListener();
	  else
	    listener = new MySpeakableListener();

	  if (verbose)
	    System.out.println("Text is '"+ text[k] +"'");
	  synth.speakPlainText(text[k], listener);

	  if (sync)
	    listener.waitForEnd();
	}
      if (!sync && listener != null)
	listener.waitForEnd();

      if (verbose)
	System.out.println("finishing");

       try {
	 synth.deallocate();
      } catch (EngineException ex) {
	System.err.println("EngineException: "+ex.getMessage());
	System.exit(1);
      }
       // If we get here play has finished, but stupid JMF
      // has left threads running so the *%!%!%!*!* interpreter won't
      // exit.
       System.exit(0);
    }

  static void registerSynthesizer(String cl)
    {
      try {
	Central.registerEngineCentral(cl);
      } catch (EngineException ex) {
	System.err.println("Engine exception: "+ex.getMessage());
	System.exit(1);
      }
    }


  static void listSynthesisers()
    {
      try {
	EngineList synths = Central.availableSynthesizers(null);

	Enumeration them = synths.elements();

	while (them.hasMoreElements())
	  {
	    SynthesizerModeDesc desc= (SynthesizerModeDesc)them.nextElement();
	    describeSynthesizer(desc);
	    System.out.println("");
	  }

      } catch (SecurityException ex) {
	System.err.println("Security exception listing synthesisers: "+ex.getMessage());
	System.exit(1);
      }
    }

  static void describeSynthesizer(SynthesizerModeDesc desc)
    {
      Voice [] voices = desc.getVoices();

      System.out.print("engine=" +desc.getEngineName());

      if (desc instanceof FestivalModeDesc)
	{
	  System.out.print(" [" + ((FestivalModeDesc)desc).getServer() +
			   ":" + Integer.toString(((FestivalModeDesc)desc).getPort()) + 
			   "]");
	}
      System.out.println("");
      System.out.println("\tlocale  = " +desc.getLocale());
      System.out.println("\tmode    = " +desc.getModeName());
      System.out.println("\trunning = " +desc.getRunning());
      System.out.print("\tvoices  =");
	    
      for(int i=0; i<voices.length; i++)
	System.out.print((i==0?" ":", ")+voices[i].getName());

      System.out.println("");
    }

}
