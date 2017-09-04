
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
 //  A queue which causes requests to wait until something arrives.        \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\


package cstr.festival.jsapi ;

import java.lang.*;
import java.util.*;
import java.awt.*;

import cstr.festival.client.*;

public class MessageQueue
	extends JobQueue
{
  private boolean active;

  public MessageQueue()
    {
      super(5);
      active=true;
    }

  public synchronized void add(Object thing)
    {
      // System.out.println("MQ add");
      super.add(thing);
      notifyAll();
    }

  public synchronized void finished()
    {
      active=false;
      notifyAll();
    }

  
  public synchronized boolean isActive()
    {
      return active|| !isEmpty();
    }

  public synchronized Object get()
    {
      Object o;
      
      while (isEmpty() && active)
	{
	  try {
	    // System.out.println("MQ wait");
	    wait();
	    // System.out.println("MQ wait done");
	  } catch (InterruptedException e) {
	    break;
	  }
	}

      if (isEmpty())
	return null;

      return super.get();
    }

  
  
}
