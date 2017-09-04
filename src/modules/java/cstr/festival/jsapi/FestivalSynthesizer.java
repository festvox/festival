
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
 //  A JSAPI synthesizer which talks to festivel.                          \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\

package cstr.festival.jsapi ;

import java.lang.*;
import java.util.*;
import java.io.*;
import java.net.*;

import javax.speech.*;
import javax.speech.synthesis.*;

import cstr.festival.client.*;

public class FestivalSynthesizer
		extends com.sun.speech.engine.BaseEngine
		implements javax.speech.synthesis.Synthesizer
{
  FestivalModeDesc desc;
  private String server;
  private int port;
  private Session session;
  private Vector speakableListeners = new Vector(0);
  private JobQueue queue;
  private String voice;

  public FestivalSynthesizer(FestivalModeDesc d)
    {
      desc=d;
      server=d.getServer();
      port=d.getPort();

      session = null;
      queue = new JobQueue();
    }

  public void allocate()
		throws EngineException, EngineStateError
    {
      super.allocate();
      try {
	session = new Session(server, port);
      } catch (IOException ex) {
	super.deallocate();
	throw new EngineException("IO Exception: "+ex.getMessage());
      }

      session.initialise();

      Object res = session.synchronousRequest("(tts_return_to_client)");

      // System.out.println("(tts_return_to_client)="+res);

      Object res2 = session.synchronousRequest("(Parameter.set 'Wavefiletype 'snd)");

      // System.out.println("(Parameter.set 'Wavefiletype 'snd)="+res2);

      setVoice("rab");
    }

  public void setVoice(String v)
    {
      voice=v;
      Object res = session.synchronousRequest("(voice_"+ voice +"_diphone)");
    }

  public void deallocate()
		throws EngineException, EngineStateError
    {
      super.deallocate();
      session.terminate(false);
    }

  public Session getSession()
    {
      return session;
    }

  public EngineModeDesc getEngineModeDesc()
    {
      return desc;
    }

  public void addSpeakableListener(SpeakableListener listener)
    {
      speakableListeners.addElement(listener);
    }

  public void removeSpeakableListener(SpeakableListener listener)
    {
      speakableListeners.removeElement(listener);
    }

  public void cancel()
    {
      cancelJob((FestivalQueueItem)queue.top());
    }

  public void cancel(Object it)
    {
      FestivalQueueItem item = findJobFor(it);
      if (item != null)
	cancelJob(item);
    }

  public void cancelAll()
    {
      Enumeration jobs = queue.elements();

      while (jobs.hasMoreElements())
	{
	  FestivalQueueItem item = (FestivalQueueItem)jobs.nextElement();
	  cancelJob(item);
	}
    }

  public Enumeration enumerateQueue()
    {
      return queue.elements();
    }

  public SynthesizerProperties getSynthesizerProperties()
    {
      return null;
    }

  public String phoneme(String p)
    {
      return "";
    }

  public void speak(String text, SpeakableListener listener)
    {
      FestivalQueueItem item = new FestivalQueueItem(this, text, "jsml", listener);
      
      addJob(item);
    }

  public void speak(URL url, SpeakableListener listener)
    {
      String text = "URL not yet supported";
      FestivalQueueItem item = new FestivalQueueItem(this, text, "text", listener);
      
      addJob(item);
    }

  
  public void speak(Speakable text, SpeakableListener listener)
    {
      FestivalQueueItem item = new FestivalQueueItem(this, text, listener);
      
      addJob(item);
    }

  public void speakPlainText(String text, SpeakableListener listener)
    {
      FestivalQueueItem item = new FestivalQueueItem(this, text, "text", listener);
      
      addJob(item);
    }

  protected void checkQueue()
    {
      if (queue.isEmpty())
	{
	  engineState &= ~Synthesizer.QUEUE_NOT_EMPTY;
	  engineState |= Synthesizer.QUEUE_EMPTY;
	}
      else
	{
	  engineState &= ~Synthesizer.QUEUE_NOT_EMPTY;
	  engineState |= Synthesizer.QUEUE_EMPTY;
	  toTop((FestivalQueueItem)queue.top());
	}
    }

  protected void addJob(FestivalQueueItem item)
    {
      queue.add(item);
      item.startSynthesis();
      checkQueue();
    }

  protected void cancelJob(FestivalQueueItem item)
    {
      queue.remove(item);
      item.cancel();
      SpeakableEvent e = new SpeakableEvent(item.getSource(), SpeakableEvent.SPEAKABLE_CANCELLED);
      if (item.getSpeakableListener() != null)
	item.getSpeakableListener().speakableCancelled(e);
      for(int i=0; i<speakableListeners.size(); i++)
	((SpeakableListener)speakableListeners.elementAt(i)).speakableCancelled(e);
      checkQueue();
    }

  protected void toTop(FestivalQueueItem item)
    {
      if (item.running())
	return;

      item.startPlaying();

      SpeakableEvent e = new SpeakableEvent(item.getSource(), SpeakableEvent.TOP_OF_QUEUE);
      if (item.getSpeakableListener() != null)
	item.getSpeakableListener().topOfQueue(e);
      for(int i=0; i<speakableListeners.size(); i++)
	((SpeakableListener)speakableListeners.elementAt(i)).topOfQueue(e);
    }

  protected FestivalQueueItem findJobFor(Object source)
    {
      Enumeration jobs = queue.elements();

      while (jobs.hasMoreElements())
	{
	  FestivalQueueItem item = (FestivalQueueItem)jobs.nextElement();
	  if (item.getSource() == source)
	    return item;
	}
      return null;
    }

  void speakableStarted(FestivalQueueItem item,
				   SpeakableListener l)
    {
      SpeakableEvent e = new SpeakableEvent(item.getSource(), 
					    SpeakableEvent.SPEAKABLE_STARTED);
      if (l != null)
	l.speakableStarted(e);
      for(int i=0; i<speakableListeners.size(); i++)
	((SpeakableListener)speakableListeners.elementAt(i)).speakableStarted(e);
    }

  void speakableEnded(FestivalQueueItem item,
				   SpeakableListener l)
    {
      queue.remove(item);
      SpeakableEvent e = new SpeakableEvent(item.getSource(), 
					    SpeakableEvent.SPEAKABLE_ENDED);
      if (l != null)
	l.speakableEnded(e);
      for(int i=0; i<speakableListeners.size(); i++)
	((SpeakableListener)speakableListeners.elementAt(i)).speakableEnded(e);
      checkQueue();
    }
}
