/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                       Copyright (c) 2004                              */
/*                        All Rights Reserved.                           */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
/*   1. The code must retain the above copyright notice, this list of    */
/*      conditions and the following disclaimer.                         */
/*   2. Any modifications must be clearly marked as such.                */
/*   3. Original authors' names are not deleted.                         */
/*   4. The authors' names are not used to endorse or promote products   */
/*      derived from this software without specific prior written        */
/*      permission.                                                      */
/*                                                                       */
/*  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        */
/*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   */
/*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*                          Author: Rob Clark                            */
/*                            Date: Jan  2004                            */
/* --------------------------------------------------------------------- */
/*                   Diphone backing off procedure                       */
/*                                                                       */
/*  Substitute a target phone for another (or multiple others).          */
/*                                                                       */
/*  The class holds a list of rules. Each rule is represented by a list  */
/*  The rule is interpreted as "The head can be replaced by the tail"    */
/*                                                                       */
/*                                                                       */
/*************************************************************************/


#include "DiphoneBackoff.h"


const EST_String DiphoneBackoff::default_match = "_";


DiphoneBackoff::DiphoneBackoff(LISP l_backofflist)
{

  EST_StrList list;
  LISP l;


  for ( l = l_backofflist ; l != NIL ; l = cdr(l))
    {
      siod_list_to_strlist(car(l),list);

      if(list.length() < 2) 
	  EST_warning("BackoffList: ignoring invalid entry %s\n" , (const char*)list.first());
      else
	  backofflist.append(list);
    }
}

// The function EST_TList::append copies data, so this is not necessary.
//
// DiphoneBackoff::~DiphoneBackoff()
// {
//   EST_Litem *p;
//
//   for (p = backofflist.head(); p != 0; p = p->next())
//    delete backofflist(p);
// }


/*
 * This version of backoff just takes two phone names as input and
 * returns a diphone name as output. It is entirely up to the caller
 * what to do with this information.
 * Note that it assumes substitutions are 1 phone for 1 phone.
 */

EST_String DiphoneBackoff::backoff(EST_String left, EST_String right)
{

  EST_Litem *p;
  EST_String head,sub,result,rl,rr;

  rl = left;
  rr = right;
  p = backofflist.head();
  while( p!= 0 )
    {
      int i = 0;
      head = backofflist(p).nth(i++);
      sub = backofflist(p).nth(i++);

      if ( head == left || ( head == default_match && left != sub) )
	{
	  rl = sub;
	  p = 0;
	}
      else if ( head == right || ( head == default_match && right != sub) )
	{
	  rr = sub;
	  p = 0;
	}
      else
	p = p->next();
    }
  
  if ( left != rl || right != rr )
    result = EST_String::cat(rl,"_",rr); 
  else
    result = EST_String::Empty;

  return result;
}


/* 
 * This version of backoff takes a pointer to a target diphone in the
 * Segment as input. It changes the target phone sequence by
 * substituting a phone for one OR MORE phone as the rules
 * dictate. If the left phone of the diphone is altered a diphone
 * mismatch will occur in the between sucessive candidate lists. If
 * the right phone is substituted this will not happen, as the
 * candidate list for the next diphone will be constrcted from the
 * corrected list.
 */

int DiphoneBackoff::backoff(EST_Item *p1)
{
    EST_Item *p2, *pp, *pps;
    EST_String n1,n2,head,sub,full_sub,bo;

    bool done = false;
    EST_Litem *p;
 

    if(! p1)
        EST_error("Backoff received null item.");
    if ( ! (p2 = inext(p1)) )
        EST_error("Backoff didn't get passed a diphone.");

    n1=p1->S("name");
    n2=p2->S("name");

    p = backofflist.head();
    // for each rule.
    while( p!= 0 &&  !done )
    {
      
        int i = 0 ;
        head = backofflist(p).nth(i++);

        pp = 0;

        // Match head of rule to left phone, or if head of the rule is the defualt substitution
        // do it if it hasn't already been done.
        if( (head == n1) || ( (head == default_match) && !is_defaultbackoff(p1) ) )
            pp = p1;
        // if this fails, try the right phone.
        else if( (head == n2) || ( (head == default_match) && !is_defaultbackoff(p2) ) )
            pp = p2;

        if(pp)
	{
            bo = pp->S("name");
            sub = backofflist(p).nth(i++);
            full_sub = sub;

            pp->set("name",sub);
            set_backoff(pp);
            if(head.matches(default_match))
                set_defaultbackoff(pp);
            while (i < backofflist(p).length())
	    {
                sub = backofflist(p).nth(i++);
                full_sub = EST_String::cat(full_sub," ",sub);
                pp->insert_after();
                pps = pp->as_relation("SylStructure");
                pp = inext(pp);
                // insert in SylStructure as well.
                pps->insert_after(pp);

                pp->set("name",sub);
                set_backoff(pp);
                if(head.matches(default_match))
                    set_defaultbackoff(pp);
	    }
            EST_warning("Missing diphone: %s_%s. Changing %s to %s.\n",
                        (const char *)n1,
                        (const char *)n2,
                        (const char *)bo,
                        (const char *)full_sub);
            done = true;
	}
        p = p->next();
    }

    if (done)
        return 0;
    else
        return 1;
}


ostream& DiphoneBackoff::print(ostream &st) const
{
  EST_Litem *p;

  for (p = backofflist.head(); p != 0; p = p->next())
    st << backofflist(p);
  return st;
}

ostream& operator << (ostream &st, const DiphoneBackoff dbo)
{
  dbo.print(st);
  return st;
}

void DiphoneBackoff::set_defaultbackoff(EST_Item *it) const
{
  it->set("defaultbackoff",1);
}

void DiphoneBackoff::set_backoff(EST_Item *it) const
{
  if(it->f_present("backoff"))
    it->set("backoff",it->I("backoff")+1);
  else
    it->set("backoff",1);
}

int DiphoneBackoff::is_defaultbackoff(const EST_Item *it) const
{
  if(it->f_present("defaultbackoff"))
    return 1;
  else return 0;
}

int DiphoneBackoff::is_backoff(const EST_Item *it) const
{
  if(it->f_present("backoff"))
    return 1;
  else return 0;
}
