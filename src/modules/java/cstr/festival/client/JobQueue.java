
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
 //  A thread safe queue based on Vector. Could be faster.                 \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\


package cstr.festival.client ;

import java.lang.*;
import java.util.*;
import java.awt.*;

public class JobQueue 
{
  Vector q;

  public JobQueue(int space)
    {
      q = new Vector(space);
    }

  public JobQueue()
    {
      q = new Vector();
    }

  public synchronized void add(Object thing)
    {
      q.addElement(thing);
    }

  public synchronized void remove(Object thing)
    {
      q.removeElement(thing);
    }

  public synchronized boolean isEmpty()
    {
      return q.isEmpty();
    }

  public synchronized Object top()
    {
     Object o;

      if (q.isEmpty())
	o = null;
      else
	o = q.firstElement(); 
      return o;
    }

  public synchronized Object get()
    {
      Object o;

      if (q.isEmpty())
	o = null;
      else
	{
	  o = q.firstElement();
	  q.removeElementAt(0);
	}
      return o;
    }

  public synchronized Enumeration elements()
    {
      return q.elements();
    }
}
