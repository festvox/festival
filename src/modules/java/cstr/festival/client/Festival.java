
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
 //  The thread which actually communicates with festival.                 \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\


package cstr.festival.client ;

import java.lang.*;
import java.util.*;
import java.awt.*;
import java.io.*;
import java.net.*;

import cstr.est.*;

class Job
{
  Integer id;
  String command;
  Session session;

  public Job(Integer i, String c, Session s)
    {
      id=i;
      command=c;
      session=s;
    }
}

class Festival extends Thread
{
  public static final int FT_SCHEME = 1;
  public static final int FT_WAVE = 2;

  protected static byte [] endm;
  protected Socket s;
  protected String hostname=null;
  protected InetAddress address = null;
  protected int port=-1;
  protected boolean closeOnExit;

  protected PrintWriter out;
  protected InputStream in;

  protected boolean ok;

  protected JobQueue jobs;

  private byte buffer[];
  private int buffered_p;
  private int buffered_e;

  static {
    String end="ft_StUfF_key";
    endm = new byte[end.length()];

    for(int i=0;i<endm.length; i++)
      {
	endm[i] = (byte)end.charAt(i);
      }

  }

  protected Festival(Socket sock, boolean close)
    {
      s=sock;
      closeOnExit=close;
      jobs = new JobQueue();
      buffer = new byte[8096];
      buffered_p=0;
      buffered_e=0;
      ok=true;
    }

  public Festival(Socket sock)
    {
      this(sock, false);
    }

  public Festival(InetAddress addr, int p)
		throws IOException
    {
      this(new Socket(addr, p), true);
      address=addr;
      port=p;
    }

  public Festival(String host, int p)
		throws IOException, UnknownHostException
    {
      this(InetAddress.getByName(host), p);
      hostname=host;
    }

  public void run()
    {
      try {
	setPriority(getPriority()+1);
	out = new PrintWriter(s.getOutputStream());
	in = s.getInputStream();
	while (true)
	  {
	    if (jobs.isEmpty())
	      {
		if (ok)
		  suspend();
		else
		  break;
	      }
	    else
	      {
		Job job = (Job)jobs.get();
		// System.out.println("sending "+job.command);
		out.println(job.command);
		out.flush();

		String status;

		job.session.notifyRunning(job.id);

		while (true)
		  {
		    status = getStatus();
		    if (status.startsWith("ER"))
		      {
			job.session.notifyError(job.id, "");
			break;
		      }
		    else if (status.startsWith("LP"))
		      {
			byte [] res=getResult();
			job.session.notifyResult(job.id, FT_SCHEME, res);
		      }
		    else if (status.startsWith("WV"))
		      {
			byte[] res=getResult();
			job.session.notifyResult(job.id, FT_WAVE, res);
		      }
		    else if (status.startsWith("OK"))
		      {
			break;
		      }
		    else
		      {
			byte [] res=getResult();
			job.session.notifyError(job.id, "unknown type");
		      }
		  }
		
		job.session.notifyFinished(job.id);
	      }
	  }
      } catch (IOException ex) {
	System.err.println("IO Error '"+ex.getMessage()+"'");
      } finally {
	try {
	  closeSocket();
	} catch (IOException ex) {
	  System.err.println("Error closing festival socket '"+ex.getMessage()+"'");
	}
	while (!jobs.isEmpty())
	  {
	    System.out.println("Left Job '"+((Job)jobs.get()).command+"'");
	  }
      }
    }

  public void connect()
    {
      // System.err.println("Connecting");
      start();
    }

  public void disconnect(boolean carefully)
    {
      // System.err.println("Disconnecting");
      ok=false;
      resume();
      if (carefully)
	{
	  try {
	    join();
	  } catch (InterruptedException ex) {
	    stop();
	  };
	}
      else
	stop();
    }

  private void closeSocket() throws IOException
    {
      if (closeOnExit)
	{
	  // System.err.println("Closing festival socket");
	  s.close();
	}
    }

  public void newJob(Integer id, String c, Session s)
    {
      jobs.add(new Job(id,c,s));
      resume();
    }

  // finally! To the things which talk to festival.

  private byte[] readTo(InputStream s, char end) throws IOException
    {
      if (buffered_e == buffered_p)
	{
	  buffered_p=0;
	  buffered_e=s.read(buffer, 0, 8096);
	}

      byte [] res=null;
      for(int i=buffered_p; i<buffered_e; i++)
	if (buffer[i] == (byte)end)
	  {
	    res = new byte[i-buffered_p+1];
	    break;
	  }

      if (res==null)
	res = new byte[buffered_e-buffered_p];

      for(int i=0; i< res.length; i++)
	res[i] = buffer[buffered_p+i];

      buffered_p = buffered_p+res.length;

      return res;
    }

  protected String getLine() throws IOException
    {
      StringBuffer line= new StringBuffer(20);

      while (true)
	{
	  byte [] bit = readTo(in, '\n');
	  line.append(new String(bit));

	  //System.out.println("got '"+line.toString()+"'");

	  if (line.charAt(line.length()-1) == '\n')
	    break;
	}

      //System.out.println("result '"+line.toString()+"'");
      return line.toString();
    }

	  
  protected String getStatus() throws IOException
    {
      String line = getLine();

      // System.out.println("        Status = '"+line+"'");
      return line==null?"EOF":line;
    }

  protected byte[] getResult() throws IOException
    {
      byte [] res = new byte[0];

      while (true)
	{
	  byte [] bit = readTo(in, 'y');

	  int end = bit.length-endm.length;

	  if (end < 0)
	    end = bit.length;
	  else
	    {
	      for(int i=0; i< endm.length; i++)
		if (bit[i+end] != endm[i])
		  {
		    end = bit.length;
		    break;
		  }
	    }

	  byte  [] new_res = new byte[res.length+end];

	  for (int i=0; i< res.length; i++)
	    new_res[i] = res[i];
	  for (int i=0; i< end; i++)
	    new_res[res.length+i] = bit[i];

	  res= new_res;

	  if (end < bit.length)
	    break;
	}

      return res;
      

      /*
      String line;
      StringBuffer val = new StringBuffer();

      if (pending != null)
	{
	  line=pending;
	  pending=null;
	}
      else
	line = in.readLine();

      while (line != null)
	{
	  int endm = line.indexOf("ft_StUfF_key");

	  if (endm >=0)
	    {
	      val.append(line.substring(0, endm));
	      pending = line.substring(endm+12);
	      break;
	    }
	  else
	    {
	      val.append(line);
	      val.append("\n");
	    }

	  line = in.readLine();
	}
      return val.toString();
      */
    }
}
