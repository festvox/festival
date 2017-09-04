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
/* A diphone unit selection "voice module"                               */
/* (implemented using a list of utterance objects)                       */
/*************************************************************************/

#ifndef __DIPHONEVOICEMODULE_H__
#define __DIPHONEVOICEMODULE_H__

#include "VoiceModuleBase.h"
#include "EST_DiphoneCoverage.h"
#include "siod_defs.h"
#include "EST_Val_defs.h"
#include "EST_String.h"

#include "EST_viterbi.h"

#include "EST_types.h" // for EST_StrList

#include "EST_FlatTargetCost.h"

class EST_Utterance;
class EST_Relation;
class EST_VTCandidate;
class EST_VTPath;
class EST_Features;
class EST_Track;
class EST_Wave;
class EST_Item;

// return standard join point time for this segment
// (one half of a diphone)
float getJoinTime( const EST_Item *seg );

//template<class T> class EST_TStringHash;
#include "EST_THash.h"
template<class T> class EST_TList;
typedef EST_TList<EST_Item*> ItemList;


SIOD_REGISTER_CLASS_DCLS(du_voicemodule,DiphoneVoiceModule)
VAL_REGISTER_CLASS_DCLS(du_voicemodule,DiphoneVoiceModule)

// following is necessary to make some of a candidate's information
// available in a faster way that EST_Item feature lookups (critically
// the join cost coefficients EST_FVectors for example)
// i.e. yet another temporary hack... (would be better if EST_viterbi
// code allowed other things apart from just EST_Item* in order to 
// perform the search)
class DiphoneCandidate {
public:
  DiphoneCandidate( const EST_Item *phone1,
		    const DiphoneVoiceModule *p,
		    const EST_FVector *left,
		    const EST_FVector *right ) 
    : ph1(phone1), dvm( p ), l_coef(left), r_coef(right),
    ph1_jccid(-1), ph1_jccindex(-1), ph2_jccid(-1), ph2_jccindex(-1){};

  const EST_Item *ph1;
  const DiphoneVoiceModule *dvm;
  const EST_FVector *l_coef;
  const EST_FVector *r_coef;
  int ph1_jccid, ph1_jccindex;
  int ph2_jccid, ph2_jccindex;
};

VAL_REGISTER_CLASS_DCLS(diphonecandidate,DiphoneCandidate)

class DiphoneVoiceModule : public VoiceModuleBase {
public:
  DiphoneVoiceModule( const EST_StrList& basenames,
		      const EST_String& uttDir,
		      const EST_String& wavDir,
		      const EST_String& pmDir,
		      const EST_String& coefDir,
		      unsigned int srate = 16000, 
		      const EST_String& uttExt  = ".utt",
		      const EST_String& wavExt  = ".wav",
		      const EST_String& pmExt   = ".pm",
		      const EST_String& JCCoefExt = ".coef",
          const EST_String& TCCoefExt = ".tcoef" );

  virtual ~DiphoneVoiceModule();

  virtual void initialise(const EST_TargetCost *tc, bool ignore_bad_tag=false );
  virtual unsigned int numModuleUnits() const;
  virtual unsigned int numUnitTypes() const;
  virtual unsigned int numAvailableCandidates( const EST_String &unit ) const;
  

  ///// Some "debugging" functions - deliberately don't mind doing
  // slow things like returning copies of things.  Such functions are
  // not intended to do important things, but just to make it easier
  // to work out whats "in" the voice database object.

  // return copy of utterance number
  bool getUtterance( EST_Utterance **utt, int n ) const;


  // return pointer to utterance which has feature "feat_name"
  // set to value "value"
  bool getUtterance( EST_Utterance **utt, 
		     const EST_String &feat_name,
		     const EST_Val &value ) const;
  void getDiphoneCoverageStats(EST_DiphoneCoverage *dc) const;

//   int DiphoneVoiceModule::getCandidateList( const EST_Item& target, 
// 					    const EST_TargetCost& tc,
// 					    EST_VTCandidate *head,
// 					    EST_VTCandidate *tail ) const;



  int getCandidateList( const EST_Item& target, 
			const EST_TargetCost *tc,
			const TCDataHash *tcdh,
			const float tc_weight,
			EST_VTCandidate **head,
			EST_VTCandidate **tail ) const;

  // append all instances of a certain phone present in the utterances
  // in this voice.  Returns the number added 
  int getPhoneList( const EST_String &phone, ItemList &list ); 

private:
  // don't allow copying of Voices (for now?)
  DiphoneVoiceModule( const DiphoneVoiceModule& );
  DiphoneVoiceModule& operator=( const DiphoneVoiceModule& );

  // Flatpack
  void flatPack( EST_Relation *segs, const EST_TargetCost *tc) const;

  void addTCoefficients( EST_Relation *segs, const EST_Track& coefs );
  void addJCoefficients( EST_Relation *segs, const EST_Track& coefs );
  void addToCatalogue( const EST_Utterance *utt, int *num_ignored, bool ignore_bad=false ); 
  void getDiphone( const EST_Item *phone1, 
		   EST_Track* coef, EST_Wave* sig, int* midframe,
		   bool extendLeft=0, bool extendRight=0 ) const;
  
  friend class DiphoneUnitVoice;
  
private:
  EST_StrList fileList;
  EST_String utt_dir;  // utterance files
  EST_String utt_ext;
  EST_String pm_dir;   // pitch marks  
  EST_String pm_ext;
  EST_String coef_dir; // for coefficients that aren't pitch syncronous
  EST_String JCCoef_ext; 
  EST_String TCCoef_ext; 
  EST_String wave_dir; // waveform (or residual)
  EST_String wave_ext;

  unsigned int wav_srate; //sample rate of voice waveform data

  TCDataHash *tcdatahash;

  EST_TList<EST_Utterance *> *utt_dbase;
  EST_TStringHash<ItemList*> *catalogue;
};

#endif // __DIPHONEVOICEMODULE_H__

