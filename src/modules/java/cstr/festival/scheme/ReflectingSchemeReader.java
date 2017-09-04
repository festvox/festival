

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
 //  A Scheme reader which returns the s expression as s atring suitable   \\
 //  for passing on to a scheme interpreter, eg for sending to festival.   \\
 //                                                                        \\
 //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\


package cstr.festival.scheme ;

import java.lang.*;
import java.util.*;
import java.awt.*;
import java.io.*;


public class ReflectingSchemeReader extends SchemeReader

{

  public ReflectingSchemeReader(Reader r)
    {
      super(r);
    }

  private int parseSexp(StringBuffer b)
		throws IOException
    {

      tk.nextToken();

      while (tk.ttype == StreamTokenizer.TT_EOL)
	{
	  b.append(" ");
	  tk.nextToken();
	}

      if (tk.ttype == StreamTokenizer.TT_EOF)
	return SE_EOF;

      if (tk.ttype == '\'')
	{
	  b.append("'");
	  int t = parseSexp(b);
	  return t;
	}

      if (tk.ttype == ')')
	{
	  b.append(") ");
	  return SE_CB;
	}

      if (tk.ttype == '(')
	{
	  b.append("(");
	  while (true)
	    {
	      int se_type = parseSexp(b);
	      
	      if (se_type == SE_EOF
		  || se_type == SE_CB)
		return SE_LIST;
	    }
	}

      if (tk.ttype == StreamTokenizer.TT_WORD)
	{
	  b.append(tk.sval);
	  b.append(" ");
	  return SE_ATOM;
	}
	
      if (tk.ttype == '"')
	{
	  b.append('"');
	  int s=b.length();
	  b.append(tk.sval);
	  for(int i=s; i<b.length(); i++)
	    if (b.charAt(i) == '"')
	      {
		b.insert(i++, '\\');
		b.setCharAt(i,'"');
	      }
	    else if (b.charAt(i) == '\n')
	      {
		b.insert(i++, '\\');
		b.setCharAt(i,'n');
	      }
	    else if (b.charAt(i) == '\r')
	      {
		b.insert(i++, '\\');
		b.setCharAt(i,'r');
	      }
	    else if (b.charAt(i) == '\t')
	      {
		b.insert(i++, '\\');
		b.setCharAt(i,'r');
	      }
	    else if (b.charAt(i) == '\\')
	      {
		b.insert(i++, '\\');
		b.setCharAt(i,'\\');
	      }
	  b.append("\" ");
	  return SE_ATOM;
	}

      if (tk.ttype >= ' ' && tk.ttype <= '\u00ff')
	{
	  b.append((char)tk.ttype);
	  b.append(' ');
	  return SE_ATOM;
	}

      System.out.println("UNEXPECTED "+tk.ttype+" "+tk.sval);
      return SE_EOF;
    }

  public Object nextExpr()
		throws IOException
    {
      return (Object)nextExprString();
    }

  public String nextExprString()
		throws IOException
    {
      StringBuffer exp = new StringBuffer(80);

      int type = parseSexp(exp);

      return type == SE_EOF ? (String)null : exp.toString();
    }
}
