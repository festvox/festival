
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
 //  A special case of SynthesizerQueueItem for festival.                  \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\


package cstr.festival.jsapi ;

import java.lang.*;
import java.util.*;
import java.awt.*;

import javax.speech.*;
import javax.speech.synthesis.*;

import cstr.festival.client.*;
import cstr.est.*;

class SynthesisRequestListener
	implements RequestListener
{
  FestivalQueueItem item;
  FestivalSynthesizer synth;

  public SynthesisRequestListener(FestivalQueueItem i,
				  FestivalSynthesizer s)
    {
      item=i;
      synth=s;
    }

  public void requestRunning(Request r)
    {
    }

  public void requestResult(Request r, Object res)
    {
      // System.out.println("Result="+res);
      if (res == null)
	return;
      else if (res instanceof Wave)
	item.queue.add((Wave)res);
    }

  public void requestError(Request r, String mes)
    {
      System.err.println("Festival Error: "+mes);
      item.queue.finished();
    }

  public void requestFinished(Request r)
    {
      item.queue.finished();
    }
}

public class FestivalQueueItem
		extends SynthesizerQueueItem
		implements Runnable
{
  private FestivalSynthesizer synth;
  private Request fRequest=null;
  private RequestListener fListener=null;
  private int toPlay = -1;
  private Thread thread=null;
  private String mode;
  MessageQueue queue;

  public FestivalQueueItem(FestivalSynthesizer s, String text, String m, SpeakableListener listener)
    {
      super(text, text, m.equals("text"), listener);
      synth=s;
      mode=m;
    }

  public FestivalQueueItem(FestivalSynthesizer s, Speakable source, SpeakableListener listener)
    {
      super(source, source.getJSMLText(), false, listener);
      synth=s;
      mode="jsml";
    }

  public void startSynthesis()
    {
      SynthesisRequestListener l = new SynthesisRequestListener(this, synth);
      queue = new MessageQueue();
      fRequest = synth.getSession().request(
					     "(tts_text \""+text+"\" '"+mode+")", 
					     l);
    }

  public void startPlaying()
    {
      thread = new Thread(this);
      thread.setPriority(thread.getPriority()+2);
      thread.start();
    }

  public boolean running()
    {
      return thread!=null;
    }

  public void cancel()
    {
      if (fRequest != null && fListener != null)
	fRequest.removeRequestListener(fListener);
      fRequest=null;
      fListener=null;
    }

  public void run()
    {
      synth.speakableStarted(this, listener);
      // System.out.println("Start playing "+text);
      while(queue.isActive())
	{
	  Wave wv = (Wave)queue.get();
	  // System.out.println("Play "+wv);
	  if (wv != null)
	    {
	      wv.play();
	    }
	}
      // System.out.println("Stop playing "+text);
      synth.speakableEnded(this, listener);
    }

  
}
