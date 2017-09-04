
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
 //                                                                        \\
 //                 Centre for Speech Technology Research                  \\
 //                      University of Edinburgh, UK                       \\
 //                        Copyright (c) 1996,1997                         \\
 //                         All Rights Reserved.                           \\
 //   Permission is hereby granted, free of charge, to use and distribute  \\
 //   this software and its documentation without restriction, including   \\
 //   without limitation the rights to use, copy, modify, merge, publish,  \\
 //   distribute, sublicense, and/or sell copies of this work, and to      \\
 //   permit persons to whom this work is furnished to do so, subject to   \\
 //   the following conditions:                                            \\
 //    1. The code must retain the above copyright notice, this list of    \\
 //       conditions and the following disclaimer.                         \\
 //    2. Any modifications must be clearly marked as such.                \\
 //    3. Original authors' names are not deleted.                         \\
 //    4. The authors' names are not used to endorse or promote products   \\
 //       derived from this software without specific prior written        \\
 //       permission.                                                      \\
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
 //  Record of a request made to a session.                                \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\


package cstr.festival.client ;

import java.lang.*;
import java.util.*;
import java.awt.*;

public class Request
{
  public String command;
  public Integer id;
  public Session session;

  private Vector results;
  protected boolean running=false;
  protected String error=null;
  protected boolean finished=false;

  private RequestListener listener;

  public Request(String c, Integer i, Session s)
    {
      command=c;
      id=i;
      session=s;

      results= new Vector(1);
    }

  public synchronized boolean isFinished()
    {
      return finished;
    }

  public synchronized void waitForUpdate()
    {
      try { wait(); } catch (InterruptedException e) { }
      // System.out.println("wait done");
    }
  

  public synchronized void notifyResult(Object r)
    {
      results.addElement(r);
      if (listener != null)
	listener.requestResult(this, r);
      notifyAll();
    }

  public synchronized  void notifyRunning()
    {
      running = true;
      if (listener != null)
	listener.requestRunning(this);
      notifyAll();
    }

  public synchronized  void notifyError(String message)
    {
      error = message;
      if (listener != null)
	listener.requestError(this, message);
      notifyAll();
    }

  public synchronized void notifyFinished()
    {
      finished = true;
      if (listener != null)
	listener.requestFinished(this);
      notifyAll();
    }

  public synchronized void addRequestListener(RequestListener l)
    {
      listener = l;
      
      if (running)
	l.requestRunning(this);
      for(int i=0; i < results.size(); i++)
	l.requestResult(this, results.elementAt(i));
      if (error != null)
	l.requestError(this, error);
      if (finished)
	l.requestFinished(this);
    }

  public synchronized void removeRequestListener(RequestListener l)
    {
      if (listener==l)
	listener = null;
    }
}

