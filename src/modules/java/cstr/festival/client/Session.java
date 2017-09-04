
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
 //  Objects representing sessions on festival servers.                    \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\

package cstr.festival.client ;

import java.lang.*;
import java.util.*;
import java.awt.*;
import java.io.*;
import java.net.*;

import cstr.est.*;

class RequestRecorder implements RequestListener
{
  Object result;

  public RequestRecorder()
    {
    }

  public void requestRunning(Request r)
    {
    }

  public void requestResult(Request r, Object res)
    {
      result=res;
    }

  public void requestError(Request r, String mes)
    {
    }

  public void requestFinished(Request r)
    {
    }
}

public class Session {
  protected Festival festival;
  private int lastID=0;
  protected Hashtable requests;

  public Session(Socket sock)
    {
      festival = new Festival(sock);
      requests = new Hashtable(10);
    }

  public Session(InetAddress addr, int p)
		throws IOException
    {
      festival = new Festival(addr, p);
      requests = new Hashtable(10);
    }

  public Session(String host, int p)
		throws IOException, UnknownHostException
    {
      festival = new Festival(InetAddress.getByName(host), p);
      requests = new Hashtable(10);
    }

  protected void finalize()
    {
      terminate(false);
    }

  public void initialise()
    {
      festival.connect();
    }

  public void terminate(boolean carefully)
    {
      festival.disconnect(carefully);
    }

  public Request request(String c, RequestListener l)
    {
      Integer id = new Integer(newID());
      Request r =  new Request(c,id, this);
      if (l!= null)
	r.addRequestListener(l);
      requests.put(id, r);

      festival.newJob(id, c, this);

      return r;
    }

  public Object synchronousRequest(String c)
    {
      RequestRecorder l = new RequestRecorder();

      Request r = request(c, l);

      while (!r.isFinished())
	r.waitForUpdate();

      return l.result;
    }

  public void notifyRunning(Integer id)
    {
       Request r =  (Request)requests.get(id);

       r.notifyRunning();
    }

  public void notifyError(Integer id, String message)
    {
       Request r =  (Request)requests.get(id);

       r.notifyError(message);
    }

  public void notifyResult(Integer id, int type, byte [] result )
    {
       Request r =  (Request)requests.get(id);
       Object oRes = result;

       if (type == Festival.FT_WAVE)
	 try {
	   Wave wv = new Wave(result);
	   oRes = wv;
	 } catch (UnsupportedEncodingException ex) {
	   oRes = ex;
	 }
       else if (type == Festival.FT_SCHEME)
	 {
	   oRes = new String(result);
	 }
	 
       r.notifyResult(oRes);
    }

  public void notifyFinished(Integer id)
    {
       Request r =  (Request)requests.get(id);
       // it's finished, so we can forget about it.
       requests.remove(id);
       r.notifyFinished();
    }

  private int newID()
    {
      return ++lastID;
    }
}
