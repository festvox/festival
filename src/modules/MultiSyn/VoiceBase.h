/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                 (University of Edinburgh, UK) and                     */
/*                           Korin Richmond                              */
/*                         Copyright (c) 2002                            */
/*                         All Rights Reserved.                          */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
/*                                                                       */
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
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT   */
/*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*                          Author: Korin Richmond                       */
/*                            Date:  Aug  2002                           */
/* --------------------------------------------------------------------- */
/* Abstract base class for "voices" - a top level interface to any code  */
/* which knows how to take a preprocessed utterance object and fill in   */
/* the information for subsequent synthesis by later festival modules    */
/*************************************************************************/


#ifndef __VOICEBASE_H__
#define __VOICEBASE_H__

#include "EST_Val.h"
#include "EST_Val_defs.h"

// EST_TKVL.h is necessary because of header dependencies
// which should probably be fixed at root, then this include
// could be removed...
#include "EST_TKVL.h"
#include "siod_defs.h"

class EST_Utterance;

class VoiceBase {
public:
  VoiceBase() : _verbosity(0), _name( EST_String::Empty ) {};
  virtual ~VoiceBase() {}; 
  virtual void initialise( bool ignore_bad_tag=false ) = 0;

  virtual EST_String name() { return _name; }
  virtual void set_name(EST_String n) { _name = n;}

  virtual void setVerbosity( unsigned int level ) { _verbosity=level; }
  virtual unsigned int verbosity() const { return _verbosity; }
  
  //virtual bool synthesiseWave( EST_Utterance *utt ) = 0;
  // this function should at best be moved to concatenative voice
  // subclass and the above one implemented instead in order to
  // generalise to non concatenative synthesis methods
  virtual void getUnitSequence( EST_Utterance *utt )=0;

  virtual unsigned int numDatabaseUnits() const = 0;
  virtual unsigned int numUnitTypes() const = 0;
  virtual bool unitAvailable( const EST_String &unit ) const = 0;
  virtual unsigned int numAvailableCandidates( const EST_String &unit ) const =0;


private:
  //EST_Features params;
  unsigned int _verbosity;
  EST_String _name;
};

SIOD_REGISTER_CLASS_DCLS(voice,VoiceBase)
VAL_REGISTER_CLASS_DCLS(voice,VoiceBase)



/**@name Generalis[ed/able] notion of a "voice" within festival
*/ 

//@{

/** Object oriented approach for better or for worse... 
*/

#endif // __VOICEBASE_H__

