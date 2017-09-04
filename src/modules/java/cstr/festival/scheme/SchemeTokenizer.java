
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
 //  A tokeniser for scheme expressions.                                   \\
 //                                                                        \\
 //  This should obviously be a subclass of  java.io.StreamTokenizer,      \\
 //  but that is as much use as a chocolate teapot so we have to go from   \\
 //  first principles.                                                     \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\

package cstr.festival.scheme ;

import java.lang.*;
import java.util.*;
import java.awt.*;
import java.io.*;

public class SchemeTokenizer 
{
  public static final int TT_EOF = StreamTokenizer.TT_EOF;  
  public static final int TT_WORD = StreamTokenizer.TT_WORD;  
  static final int TT_NOTHING = -4;  

  protected Reader r;

  public String sval;
  public int ttype;

  protected int pending =-1;

  public SchemeTokenizer(Reader rd)
    {
      r=rd;

      ttype=TT_NOTHING;
    }

  public int nextToken() throws IOException
    {
      int c;

      if (pending >=0)
	{
	  c=pending;
	  pending = -1;
	}
      else
	c = r.read();

      // Skip whitespace and comments
      while (true)
	{
	  while (Character.isWhitespace((char)c))
	      c = r.read();
	  if (c == ';')
	    {
	      while (c != '\n' && c != '\r')
		c = r.read();
	      while (c == '\n' || c == '\r')
		c = r.read();
	    }
	  else
	    break;
	}

      if (c <0)
	ttype = TT_EOF;
      else if (c == '"')
	{
	  ttype = c;
	  StringBuffer b = new StringBuffer(100);
	  boolean escape=false;

	  while ((c=r.read()) != '"' || escape)
	    {
	      if (escape)
		{
		  if (c == 'n')
		    c='\n';
		  else if (c == 'r')
		    c='\r';
		  else if (c == 't')
		    c='\t';

		  b.append((char)c);
		  escape=false;
		}
	      else if (c == '\\')
		  escape=true;
	      else
		{
		  b.append((char)c);
		  escape=false;
		}
	    }

	  sval = b.toString();
	}
      else if (Character.isLetterOrDigit((char)c) || c=='.' || c=='_' || c=='-' || c=='*' || c==':')
	{
	  ttype = TT_WORD;
	  StringBuffer b = new StringBuffer(100);
	  
	  b.append((char)c);

	  while (Character.isLetterOrDigit((char)(c=r.read())) || c=='.' || c=='_' || c=='-' || c=='*' || c==':')
	    b.append((char)c);

	  pending=c;

	  sval = b.toString();
	}
      else
	ttype = c;

      return ttype;
    }

}
