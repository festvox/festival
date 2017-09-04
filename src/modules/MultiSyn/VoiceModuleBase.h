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
//*************************************************************************/
/*                                                                       */
/*                          Author: Korin Richmond                       */
/*                            Date:  Aug  2002                           */
/* --------------------------------------------------------------------- */
/* Abstract base class for "voice modules" - a top level interface to    */
/* any collection of speech material in the voice database               */
/*************************************************************************/

#ifndef __VOICEMODULEBASE_H__
#define __VOICEMODULEBASE_H__

#include "EST_Val.h"
#include "EST_Val_defs.h"

// EST_TKVL.h is necessary because of header dependencies
// which should probably be fixed at root, then this include
// could be removed...
#include "EST_TKVL.h"
#include "siod_defs.h"

#include "EST_FlatTargetCost.h"


class EST_Utterance;
class VoiceBase;

class VoiceModuleBase {
public:
  VoiceModuleBase( VoiceBase *parent = 0 ): _parentVoice(parent) {};
  virtual ~VoiceModuleBase() {};
  virtual void initialise(const EST_TargetCost *tc, bool ignore_bad_tag=false) = 0;
  virtual unsigned int numModuleUnits() const = 0;
  virtual unsigned int numUnitTypes() const = 0;
  virtual unsigned int numAvailableCandidates( const EST_String &unit ) const =0;

private:
  //EST_Features params;
  const VoiceBase *_parentVoice;
};

SIOD_REGISTER_CLASS_DCLS(voicemodule,VoiceModuleBase)
VAL_REGISTER_CLASS_DCLS(voicemodule,VoiceModuleBase)



/**@name Generalis[ed/able] notion of a unit selection "voice" module 
within festival. 
*/ 

//@{

/** Object oriented approach for better or for worse... 
This is primarily intended to be a "module" from the point of view of a
collection of *related* speech database material (such as a "weather" module)
or a "news broadcast" module.  At the very least, though, it is just a 
collection of speech database material. 
*/

#endif // __VOICEMODULEBASE_H__

