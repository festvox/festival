
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
 //  Mainline for Java festival client.                                    \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\

package cstr.festival ;

import java.lang.*;
import java.util.*;
import java.awt.*;
import java.io.*;
import java.net.*;

import cstr.festival.client.*;
import cstr.festival.scheme.*;

class RequestHandler implements RequestListener
{
  Thread mainline;

  public RequestHandler(Thread main)
    {
      mainline=main;
    }

  public void requestRunning(Request r)
    {
      System.out.println("Request "+r.command+" running");
    }

  public void requestResult(Request r, Object res)
    {
      System.out.println("Request "+r.command+" result="+res);
    }

  public void requestError(Request r, String mes)
    {
      System.out.println("Request "+r.command+" error "+mes);
    }

  public void requestFinished(Request r)
    {
      System.out.println("Request "+r.command+" finished");
    }
}

public class Client {

  public static void useage(String error)
    {
      if (error != null)
	{
	  System.out.println("");
	  System.out.println("	"+error);
	}
      System.out.println("");
      System.out.println("Useage: festival_client_java [options] file...");
      System.out.println("");
      System.out.println("	--help			Show this message.");
      System.out.println("	--server <host>		Connect to named host.");
      System.out.println("	--port <portnum>	Use the given port.");
      System.out.println("	--sync			Wait for each expression to finish");
      System.out.println("				    executing before reading next.");
      System.out.println("	--wait			Wait for all expressions to finish");
      System.out.println("				    executing before exiting.");
      
      System.exit(error!=null?1:0);
    }

  public static void main (String[] args)
    {
      String server="localhost";
      int port=1314;
      int i;
      boolean waitAtEnd=false;
      boolean sync=false;

      for(i=0; i<args.length; i++)
	{
	  if (args[i].equals("--help"))
	      useage(null);
	  else if (args[i].equals("--server"))
	    {
	      i++;
	      if (i>=args.length)
		useage("Need name after --server");

	      server = args[i];
	    }
	  else if (args[i].equals("--port"))
	    {
	      i++;
	      if (i>=args.length)
		useage("Need name after --server");

	      try {
		port = Integer.parseInt(args[i]);
	      } catch (NumberFormatException ex) {
		useage("Not a valid port number '" + args[i] + "'");
	      }

	    }
	  else if (args[i].equals("--wait"))
	    waitAtEnd=true;
	  else if (args[i].equals("--sync"))
	    sync=true;
	  else if (args[i].startsWith("--"))
	    useage("Unknown argument "+args[i]);
	  else
	    break;
	}

      System.out.println("Server='"+server+":"+port+"'");

      Session s=null;

      try {
	s = new Session(server, port);
      } catch (UnknownHostException ex) {
	useage("Unknown host '"+ex.getMessage()+"'");
      } catch (IOException ex) {
	useage("Can't connect '"+ex.getMessage()+"'");
      }

      s.initialise();

      System.out.println("Connected");

      if (i == args.length)
	{
	  args = new String[] { "-" };
	  i=0;
	}

    RequestListener handler = new RequestHandler(Thread.currentThread());

    file:
      for(; i<args.length; i++)
	{
	  System.out.println("	'"+args[i]+"'");

	  String filename = args[i];

	  Reader reader = null;

	  try {
	    if (filename.equals("-"))
	      reader = new InputStreamReader(System.in);
	    else
	      reader =  new InputStreamReader(new FileInputStream(filename));
	  } catch (FileNotFoundException ex) {
	    useage("Can't open '"+ ex.getMessage() + "'");
	  }

	  ReflectingSchemeReader sreader = new ReflectingSchemeReader(reader);

	  try {
	    String exp;
	    while (true)
	      {
		if (filename.equals("-"))
		  {
		    System.out.print("> ");
		    System.out.flush();
		  }

		exp=sreader.nextExprString();

		if (exp==null)
		  break;

		System.out.println(": "+exp);
		Request r = s.request(exp, handler);
		if (sync)
		  while (!r.isFinished())
		    r.waitForUpdate();
	      }
	    } catch (IOException ioex) {
	      System.out.println("	Error "+ioex.getMessage());
	      break file;
	    }
	  System.out.println("--EOF--");
	}

      System.out.println("call terminate");
      s.terminate(waitAtEnd);
    }
};
