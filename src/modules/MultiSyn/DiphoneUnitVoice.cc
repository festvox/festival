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
/* first stab at a diphone unit selection "voice" - using a list of      */
/* utterance objects                                                     */
/*************************************************************************/

#include "festival.h"
#include "DiphoneUnitVoice.h"
#include "DiphoneVoiceModule.h"
#include "EST_DiphoneCoverage.h"
#include "EST_rw_status.h"
#include "EST_viterbi.h"
#include "EST_Track.h"
#include "EST_track_aux.h"
#include "EST_Wave.h"
#include "EST_THash.h"
#include "EST_TList.h"
#include "EST_types.h"
#include "ling_class/EST_Utterance.h"
#include "siod.h"
#include "siod_est.h"
#include "safety.h"
#include <cstdlib>

#include "EST_TargetCost.h"
#include "TargetCostRescoring.h"
#include "EST_JoinCost.h"
#include "EST_JoinCostCache.h"

#include "EST_Val.h"

SIOD_REGISTER_TYPE(itemlist,ItemList)
VAL_REGISTER_TYPE(itemlist,ItemList)

// from src/modules/UniSyn_diphone/us_diphone.h
// this won't be staying here long...
void parse_diphone_times(EST_Relation &diphone_stream, 
			 EST_Relation &source_lab);

SIOD_REGISTER_CLASS(du_voice,DiphoneUnitVoice)
VAL_REGISTER_CLASS(du_voice,DiphoneUnitVoice)

static void my_parse_diphone_times(EST_Relation &diphone_stream, 
				   EST_Relation &source_lab)
{
  EST_Item *s, *u;
  float dur1, dur_u, p_time=0.0;
  
  // NOTE: because of the extendLeft/extendRight phone join hack for missing diphones, 
  // the unit linked list *may be* shorter that the segment list. 
  //(admittedly could cause confusion)

  for( s=source_lab.head(), u=diphone_stream.head(); (u!=0)&&(s!=0); u=inext(u), s=inext(s)){
    EST_Track *pm = track(u->f("coefs"));
    
    int end_frame = pm->num_frames() - 1;
    int mid_frame = u->I("middle_frame");
    
    dur1 = pm->t(mid_frame);
    dur_u = pm->t(end_frame);
    
    s->set("end", (p_time+dur1) );
    
    p_time += dur_u;
    u->set("end", p_time);
    
    if( u->f_present("extendRight") ){//because diphone squeezed out (see above)
        s = inext(s);
      s->set("end", p_time );
    }
  }

  if(s)
    s->set("end", (p_time));
}

// temporary hack necessary because decoder can only take a 
// function pointer (would be better to relax this restriction in 
// the EST_Viterbi_Decoder class, or in a replacement class, rather
// than using this hack)
static DiphoneUnitVoice *globalTempVoicePtr = 0;

DiphoneUnitVoice::DiphoneUnitVoice( const EST_StrList& basenames,
				    const EST_String& uttDir,
				    const EST_String& wavDir,
				    const EST_String& pmDir,
				    const EST_String& coefDir,
				    unsigned int sr,
				    const EST_String& uttExt,
				    const EST_String& wavExt,
				    const EST_String& pmExt,
				    const EST_String& JCCoefExt,
            const EST_String& TCCoefExt )

  : pruning_beam( -1 ),
    ob_pruning_beam( -1 ),
    tc_rescoring_beam( -1 ),
    tc_rescoring_weight( 0.0 ),
    tc_weight( 1.0 ),
    jc_weight( 1.0 ),
    jc_f0_weight( 1.0 ),
    jc_power_weight( 1.0 ),
    jc_spectral_weight( 1.0 ),
    prosodic_modification( 0 ),
    wav_srate( sr ),
    jc( 0 ),
    jc_delete( false ),
    tc( 0 ),
    tc_delete( false ),
    tcdh( 0 )

{
  // make the default voice module with the supplied parameters
  addVoiceModule( basenames, uttDir, wavDir, pmDir, coefDir,
		  wav_srate,
		  uttExt, wavExt, pmExt, JCCoefExt, TCCoefExt );

  diphone_backoff_rules = 0;
}

void DiphoneUnitVoice::initialise( bool ignore_bad_tag )
{
  if( jc == 0 )
    EST_error( "Need to set join cost calculator for voice" );

  if( tc == 0 )
    EST_error( "Need to set target cost calculator for voice" );

  EST_TList<DiphoneVoiceModule*>::Entries it;
  
  for( it.begin(voiceModules); it; it++ )
    (*it)->initialise( tc, ignore_bad_tag );
}

bool DiphoneUnitVoice::addVoiceModule( const EST_StrList& basenames,
				       const EST_String& uttDir,
				       const EST_String& wavDir,
				       const EST_String& pmDir,
				       const EST_String& coefDir,
				       unsigned int srate,
				       const EST_String& uttExt,
				       const EST_String& wavExt,
				       const EST_String& pmExt,
				       const EST_String& JCCoefExt,
               const EST_String& TCCoefExt )

{
  DiphoneVoiceModule *vm;

  if( srate != wav_srate )
    EST_error( "Voice samplerate: %d\nmodule samplerate: %d", 
	       wav_srate, srate );

  vm = new DiphoneVoiceModule( basenames, uttDir, wavDir, pmDir, coefDir,
			       srate,
			       uttExt, wavExt, pmExt, JCCoefExt, TCCoefExt );
  CHECK_PTR(vm);
  
  registerVoiceModule( vm );

  return true;
}
  

void DiphoneUnitVoice::registerVoiceModule( DiphoneVoiceModule *vm )
{
  voiceModules.append( vm );
}


void DiphoneUnitVoice::setJoinCost( EST_JoinCost *jcost, bool del )
{
  if( jc_delete == true )
    if( jc != 0 )
      delete jc;

  jc = jcost;
  jc_delete = del;
}

void DiphoneUnitVoice::setTargetCost( EST_TargetCost *tcost, bool del )
{
  if( tc_delete == true )
    if( tc != 0 )
      delete tc;

  tc = tcost;
  tc_delete = del;
}


DiphoneUnitVoice::~DiphoneUnitVoice()
{
  EST_TList<DiphoneVoiceModule*>::Entries it;
  
  for( it.begin(voiceModules); it; it++ )
    delete( *it );

  if(diphone_backoff_rules)
    delete diphone_backoff_rules;

  if( jc_delete == true )
    if( jc != 0 )
      delete jc;

  if( tc_delete == true )
    if( tc != 0 )
      delete tc;

  if(tcdh)
    delete tcdh;

}


void DiphoneUnitVoice::addToCatalogue( const EST_Utterance *utt )
{
  // needed?
}


void DiphoneUnitVoice::getDiphone( const EST_VTCandidate *cand, 
				   EST_Track* coef, EST_Wave* sig, int *midframe,
				   bool extendLeft, bool extendRight )
{
  // The need for this function in this class is a bit messy, it would be far
  // nicer just to be able to ask the Candidate itself to hand over the relevant
  // synthesis parameters.  In future, it will work that way ;)

  // put there by DiphoneVoiceModule::getCandidateList
  const DiphoneCandidate *diphcand = diphonecandidate( cand->name );

  const DiphoneVoiceModule* parentModule = diphcand->dvm;  
  EST_Item *firstPhoneInDiphone = cand->s;

  // need to call right getDiphone to do the actual work
  parentModule->getDiphone( firstPhoneInDiphone, coef, sig, midframe, extendLeft, extendRight );
} 

// REQUIREMENT: the unit relation must have previously been used to initialise the 
// Viterbi decoder from which the path was produced.
void DiphoneUnitVoice::fillUnitRelation( EST_Relation *units, const EST_VTPath *path )
{
  EST_Item *it=units->tail();

  for ( ; path != 0 && it != 0; path=path->from, it=iprev(it) ){
    EST_Track *coefs = new EST_Track;
    CHECK_PTR(coefs);
    EST_Wave *sig = new EST_Wave;
    CHECK_PTR(sig);
    int midf;

    getDiphone( path->c, coefs, sig, &midf,
		it->f_present("extendLeft"), it->f_present("extendRight"));
    
    EST_Item *firstPhoneInDiphone = path->c->s;
    it->set_val( "sig", est_val( sig ) );
    it->set_val( "coefs", est_val( coefs ) );
    it->set( "middle_frame", midf );
    it->set( "source_utt", firstPhoneInDiphone->relation()->utt()->f.S("fileid"));
    it->set_val( "source_ph1", est_val( firstPhoneInDiphone ));
    it->set( "source_end", firstPhoneInDiphone->F("end"));
    it->set( "target_cost", path->c->score );
    
    //have to recalculate join cost as it's not currently saved anywhere
    if( path->from == 0 )
      it->set( "join_cost", 0.0);
    else{
      // join cost between right edge of left diphone and vice versa
      const DiphoneCandidate *l_diph = diphonecandidate(path->from->c->name);
      const DiphoneCandidate *r_diph = diphonecandidate(path->c->name);

      it->set( "join_cost", (*jc)( l_diph, r_diph ) );
    }
  } 
}

// The use of the globalFunctionPtr in this function is a really just a temporary hack 
// necessary because the decoder as it stands at present can only take a function pointer 
// (would be better to relax this restriction in the EST_Viterbi_Decoder class, or in a 
// replacement class, rather than using this hack)
// static EST_VTPath* extendPath( EST_VTPath *p, EST_VTCandidate *c,
// 	 		       EST_Features&)
// { 
//   EST_VTPath *np = new EST_VTPath;
//   CHECK_PTR(np);

//   if( globalTempVoicePtr ==0 )
//     EST_error( "globalTempVoicePtr is not set, can't continue" );
  
//   const EST_JoinCost &jcost = globalTempVoicePtr->getJoinCostCalculator();
  
//   np->c = c;
//   np->from = p;
//   np->state = c->pos;
  
//   if ((p == 0) || (p->c == 0))
//     np->score = c->score;
//   else{
//     // join cost between right edge of left diphone and vice versa
//     np->score = p->score + c->score + jcost( inext(p->c->s), c->s );
//   }
//   return np;
// }
static EST_VTPath* extendPath( EST_VTPath *p, EST_VTCandidate *c,
	 		       EST_Features&)
{ 
  EST_VTPath *np = new EST_VTPath;
  CHECK_PTR(np);

  if( globalTempVoicePtr ==0 )
    EST_error( "globalTempVoicePtr is not set, can't continue" );
  
  const EST_JoinCost &jcost = globalTempVoicePtr->getJoinCostCalculator();
  
  np->c = c;
  np->from = p;
  np->state = c->pos;
  
  if ((p == 0) || (p->c == 0))
    np->score = c->score;
  else{
    const DiphoneCandidate *l_diph = diphonecandidate(p->c->name);
    const DiphoneCandidate *r_diph = diphonecandidate(c->name);

    // join cost between right edge of left diphone and vice versa
    np->score = p->score + c->score + jcost( l_diph, r_diph );
  }
  return np;
}

// This function is a really just a temporary hack necessary because the decoder
// as it stands at present can only take a function pointer (would be better to relax
// this restriction in the EST_Viterbi_Decoder class, or in a replacement class, rather
// than using this hack)
static EST_VTCandidate* getCandidatesFunction( EST_Item *s,
					       EST_Features &f)
{
  DiphoneUnitVoice *duv = globalTempVoicePtr;  
  if( duv==0 )
    EST_error( "Candidate source voice is unset" );

  return duv->getCandidates( s, f );
}

// Function which, given an item from the timeline relation that
// was originally used to initialise the EST_Viterbi_Decoder
// returns a pointer to a linked list of EST_VTCandidates
// (this is provided to the viterbi decoder upon its construction
// and (in)directly called by it as part of the decoding process...)
EST_VTCandidate* DiphoneUnitVoice::getCandidates( EST_Item *s, 
						  EST_Features &f) const 
{
  EST_VTCandidate *c = 0;
  EST_VTCandidate *moduleListHead = 0;
  EST_VTCandidate *moduleListTail = 0;

  // these objects [c/sh]ould be a parameter visible in the user's script
  // land, and will be in future...

  // tc now a member
  // EST_DefaultTargetCost default_target_cost;
  // EST_TargetCost *tc = &default_target_cost;
  // or
  //  EST_SchemeTargetCost scheme_target_cost(rintern( "targetcost"));
  //  EST_TargetCost *tc = &scheme_target_cost;

  EST_TList<DiphoneVoiceModule*>::Entries module_iter;
  int nfound, total=0;

  ////////////////////////////////////////////////////////////////
  // join linked list of candidates from each module into one list
  for( module_iter.begin(voiceModules); module_iter; module_iter++ ){
    nfound = (*module_iter)->getCandidateList( *s, 
					       tc, 
					       tcdh,
					       tc_weight,
					       &moduleListHead, 
					       &moduleListTail );
    if( nfound>0 ){
      moduleListTail->next = c;
      c = moduleListHead;
      total += nfound;
    }
  }

  if( total==0 )
    EST_error( "Couldn't find diphone %s", (const char*)s->S("name") );
  
  if( verbosity() > 0 )
    printf( "Number of candidates found for target \"%s\": %d\n", 
	    (const char*)s->S("name"), total );
  
  if( ! ((tc_rescoring_beam == -1.0) || (tc_rescoring_weight <= 0.0)) )
    rescoreCandidates( c, tc_rescoring_beam, tc_rescoring_weight );

  return c;
}

void DiphoneUnitVoice::diphoneCoverage(const EST_String filename) const
{

  EST_DiphoneCoverage dc;
  EST_TList<DiphoneVoiceModule*>::Entries module_iter;

  // for each module
  for( module_iter.begin(voiceModules); module_iter; module_iter++ )
    (*module_iter)->getDiphoneCoverageStats(&dc);
 
  dc.print_stats(filename);
   
}



bool DiphoneUnitVoice::synthesiseWave( EST_Utterance *utt )
{
  getUnitSequence( utt );

  return true; 
}



void DiphoneUnitVoice::getUnitSequence( EST_Utterance  *utt )
{
  EST_Relation *segs = utt->relation( "Segment" );
  EST_Relation *units = utt->create_relation( "Unit" );
  
  if(!tcdh)
    tcdh = new TCDataHash(20);
  else
    tcdh->clear();

  // Initialise the Unit relation time index for decoder
  EST_String diphone_name;
  EST_StrList missing_diphones;

  EST_Item *it=segs->head();
  if( it == 0 )
    EST_error( "Segment relation is empty" );

  bool extendLeftFlag = false;
  for( ; inext(it); it=inext(it) )
    {
      EST_String l = it->S("name");
      EST_String r = inext(it)->S("name");

      EST_String diphone_name = EST_String::cat(l,"_",r);
      EST_String orig = diphone_name;

      if(tc->is_flatpack())
	tcdh->add_item( it , ((EST_FlatTargetCost *)tc)->flatpack(it) );


      // First attempt back off:
      // If missing diphone is an interword diphone, insert a silence!
      // Perceptual results say this is prefered.

      if ( diphone_name != EST_String::Empty &&
	   !this->unitAvailable(diphone_name) )
	{
	  EST_Item *s1,*s2;
	  EST_Item *w1=0,*w2=0;

	  cerr << "Missing diphone: "<< diphone_name << endl;

	  if((s1 = parent(it,"SylStructure")))
	    w1= parent(s1,"SylStructure");
	  if( (s2 = parent(inext(it),"SylStructure")))
	    w2= parent(s2,"SylStructure");

	  if( w1 && w2 && (w1 != w2) )
	    {
	      EST_Item *sil;

	      cerr << " Interword so inserting silence.\n";

	      sil = it->insert_after();
	      sil->set("name",ph_silence());

	      r = inext(it)->S("name");
	      diphone_name = EST_String::cat(l,"_",r);

	    }
	}


      // Simple back off.
      // Change diphone name for one we actually have.
           
      while(diphone_name != EST_String::Empty &&
	    !this->unitAvailable(diphone_name) && 
	    diphone_backoff_rules)
	{

	  cerr << " diphone still missing, backing off: " << diphone_name << endl;
	 
	  diphone_name = diphone_backoff_rules->backoff(l,r);
	  l = diphone_name.before("_");
	  r = diphone_name.after("_");
	  
	  cerr << " backed off: " << orig << " -> " << diphone_name << endl;

	  if( verbosity() > 0 ){
	    EST_warning("Backing off requested diphone %s to %s", 
			orig.str(), 
			diphone_name.str() );
	  }
	}
      

      //// Complex backoff.  Changes the segment stream to the right,
      //// may still leave a discontinuity to the left. This could be
      //// fixed, but it would requires a better search. Rob's thoughts
      //// are that the simple method works better, unless it resorts to
      //// a bad default rule.


      //    while(!this->unitAvailable(diphone_name) && 
      //          diphone_backoff_rules && 
      //          !diphone_backoff_rules->backoff(it))
      //      diphone_name = EST_String::cat(it->S("name"),"_",inext(it)->S("name"));
      
      if( !this->unitAvailable( diphone_name ) ){
	missing_diphones.append( diphone_name );
	if(units->tail())
	  units->tail()->set( "extendRight", 1 );
	extendLeftFlag = true; // trigger for next unit to make up second half of missing diphone
      }
      else{
	EST_Item *t = units->append();
	t->set( "name", diphone_name );
	if(orig != diphone_name)
	  t->set( "missing_diphone",orig);
	t->set_val( "ph1", est_val(it) );
	if( extendLeftFlag == true ){
	  t->set( "extendLeft", 1 );
	  extendLeftFlag = false;
	}
      }
    }

  // stop if necessary units are still missing.
  if( missing_diphones.length() > 0 ){
      for( EST_Litem *it=missing_diphones.head(); it!=0 ; it=it->next() )
      printf( "requested diphone missing: %s\n", missing_diphones(it).str() );
 
    EST_warning("Making phone joins to compensate...");
    //    EST_error("Unable to synthesise utterance due to missing diphones");
  }

  // Make the decoder do its thing
  // -1 means number of states at each time point not fixed
  EST_Viterbi_Decoder v( getCandidatesFunction, extendPath, -1 );  

  // turn on pruning if necessary
  if( (pruning_beam>0) || (ob_pruning_beam>0) )
    v.set_pruning_parameters( pruning_beam, ob_pruning_beam );
 
  // temporary hack necessary because decoder can only take a 
  // function pointer (would be better to relax this restriction in 
  // the EST_Viterbi_Decoder class, or in a replacement class, rather
  // than using this hack)
  globalTempVoicePtr = this;

  v.set_big_is_good(false);

  if( verbosity() > 0 )
    v.turn_on_trace();

  v.initialise( units );
  v.search();

  // take hold of the best path (end thereof)
  EST_VTPath *bestp=0;
  if( !v.result( &bestp ) )
    EST_error( "No best candidate sequence found" );

  // fill in the best path features in the Unit Relation
  fillUnitRelation( units, bestp );

  my_parse_diphone_times( *units, *segs );
}


/////////////////////////////////////////////////////////////////////////////////////
// Canned example experimental code (proof of concept rather than intelligently done)

static inline bool itemListContainsItem( const ItemList* il, const EST_Item *item )
{
  ItemList::Entries it;
  
  for( it.begin( *il ); it; it++ )
    if( (*it) == item )
      return true;

  return false;
}


static EST_VTCandidate* getCandidatesWithOmissionsFunction( EST_Item *s, EST_Features &f )
{
  DiphoneUnitVoice *duv = globalTempVoicePtr;  
  if( duv==0 )
    EST_error( "Candidate source voice is unset" );

  //get candidate list as usual
  EST_VTCandidate *candlist = duv->getCandidates( s, f );
  
  //filter out candidates on basis of omission list (yes, this is quite dumb)
  if( s->f_present( "omitlist" ) ){

    EST_warning( "omitlist found in unit %s", s->S("name").str() );

    ItemList *omitlist = itemlist( s->f("omitlist") );
    
    //until one candidate remains as head (to keep hold of list head)
    while( candlist != 0 && itemListContainsItem( omitlist, candlist->s ) ){
      EST_VTCandidate *del_cand = candlist;
      candlist = candlist->next;
      del_cand->next = 0; //so deletion doesn't trigger total list deletion
      delete del_cand;
    }
    
    //then continue down list
    EST_VTCandidate *prev = candlist;
    EST_VTCandidate *cand = candlist->next;
    while( cand!=0 ){
      if( itemListContainsItem( omitlist, cand->s ) ){ //delete cand on true
	prev->next = cand->next;
	cand->next = 0; //so deletion doesn't trigger total list deletion
	delete cand;
	cand = prev;
      }
      cand = cand->next;
    }
    
    if( candlist == 0 )
      EST_error( "zero candidates remain after filtering" );
    
  }
  
  return candlist;
}

// For when the utterance already has the unit sequence, with certain candidates 
// flagged as to be avoided, or mandatory and so on...
void DiphoneUnitVoice::regetUnitSequence( EST_Utterance *utt )
{
  // Unit relation should already be in existence for decoder
  EST_Relation *units = utt->relation( "Unit" );
  EST_Item *it=units->head();
  if( it == 0 )
    EST_error( "Unit relation is empty" );
  
  // Make the decoder do its thing (again)
  // -1 means number of states at each time point not fixed
  EST_Viterbi_Decoder v( getCandidatesWithOmissionsFunction, extendPath, -1 );  

  // turn on pruning if necessary
  if( (pruning_beam>0) || (ob_pruning_beam>0) )
    v.set_pruning_parameters( pruning_beam, ob_pruning_beam );
 
  // temporary hack necessary because decoder can only take a 
  // function pointer (would be better to relax this restriction in 
  // the EST_Viterbi_Decoder class, or in a replacement class, rather
  // than using this hack)
  globalTempVoicePtr = this;

  v.set_big_is_good(false);

  if( verbosity() > 0 )
    v.turn_on_trace();

  v.initialise( units );
  v.search();

  // take hold of the best path (end thereof)
  EST_VTPath *bestp=0;
  if( !v.result( &bestp ) )
    EST_error( "No best candidate sequence found" );

  // fill in the best path features in the Unit Relation
  fillUnitRelation( units, bestp );

  EST_Relation *segs = utt->relation("Segment");
  my_parse_diphone_times( *units, *segs );
}

// End canned example experimental code ///////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////


bool DiphoneUnitVoice::unitAvailable( const EST_String &diphone ) const
{ 
  EST_TList<DiphoneVoiceModule*>::Entries it;
  
  for( it.begin(voiceModules); it; it++ )
    if( (*it)->numAvailableCandidates(diphone) > 0 )
      return true;

  return false;
}

unsigned int DiphoneUnitVoice::numAvailableCandidates( const EST_String &diphone ) const
{
  unsigned int number = 0;
  EST_TList<DiphoneVoiceModule*>::Entries it;
  
  for( it.begin(voiceModules); it; it++ )
    number += (*it)->numAvailableCandidates(diphone);
  
  return number;
}


////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
// special case of the above for utterances structures that are
// actually in the voice database, which doesn't do any search
// This is useful for doing copy synthesis of utterances (eg.
// to test out resynthesis, prosodic modification and so on)
void DiphoneUnitVoice::getCopyUnitUtterance( const EST_String &utt_fname, 
					     EST_Utterance **utt_out ) const
{
  // need to find which, if any, voice module has this utterance
  // in its list
  EST_TList<DiphoneVoiceModule*>::Entries module_iter;
  EST_Utterance *db_utt=0;
  for( module_iter.begin(voiceModules); module_iter; module_iter++ )
    if( (*module_iter)->getUtterance(&db_utt, "fileid", utt_fname) == true )
      break;

  if( db_utt == 0 )
    EST_error( "Could not find Utterance %s in any voice module",
	       utt_fname.str() );
  else{
    // deep copy database utterance and fill in Unit relation
    *utt_out = new EST_Utterance( *db_utt );
    CHECK_PTR(utt_out);

    EST_Utterance myUtt( *db_utt );

    cerr << myUtt.relation_present( "Segment" ) << " "
	 << myUtt.num_relations() <<endl;
    

    cerr << db_utt->relation_present( "Segment" ) << " "
	 << (*utt_out)->relation_present( "Segment" ) << " "
	 << (*utt_out)->num_relations() <<endl;


    EST_Relation *segs = (*utt_out)->relation( "Segment" );
    EST_Relation *units = (*utt_out)->create_relation( "Unit" );
  
    // Initialise the Unit relation + fill in necessary/suitable
    // synthesis parameters
    EST_String ph1, ph2;
    EST_Item *it = segs->tail();
    EST_Item *db_utt_seg_it = db_utt->relation( "Segment" )->tail();
    if( it == 0 )
      EST_error( "Segment relation is empty" );
    else{
      ph2 = it->S("name");    
      while( ((it=iprev(it))!=0) && 
	     ((db_utt_seg_it=iprev(db_utt_seg_it))!=0) ){
	EST_Track *coefs = new EST_Track;
	CHECK_PTR(coefs);
	EST_Wave *sig = new EST_Wave;
	CHECK_PTR(sig);
	int midf;
	
	(*module_iter)->getDiphone( db_utt_seg_it, coefs, sig, &midf );
	
	ph1 = it->S("name");
	EST_Item *t = units->prepend();
	t->set( "name", EST_String::cat(ph1,"_",ph2) );
	t->set_val( "ph1", est_val(it) );
	t->set_val( "sig", est_val( sig ) );
	t->set_val( "coefs", est_val( coefs ) );
	t->set( "middle_frame", midf );
	t->set( "source_utt", db_utt->f.S("fileid"));
	t->set_val( "source_ph1", est_val( db_utt_seg_it ));
	t->set( "source_end", db_utt_seg_it->F("end"));
	t->set( "target_cost", 0.0 );
	t->set( "join_cost", 0.0);
	
	ph2 = ph1;
      }
    }
    my_parse_diphone_times( *units, *segs );
    
    // this is for copy synthesis, so copy actual timings
    //for( EST_Item *seg = segs->head(); it!=0; it=inext(it) )
      //seg->set( "end", seg->F("source_end") );
  }  
}

////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////



unsigned int DiphoneUnitVoice::numUnitTypes() const
{
  //necessary?
  return 0;
}

unsigned int DiphoneUnitVoice::numDatabaseUnits() const
{
  unsigned int sum=0;
  
  EST_TList<DiphoneVoiceModule*>::Entries it;
  
  for( it.begin( voiceModules ); it; it++ )
    sum += (*it)->numModuleUnits();
  
  return sum;
}


//////////////////////////////////////////////////////////////////////////

void DiphoneUnitVoice::set_diphone_backoff(DiphoneBackoff *dbo)
{
  if (diphone_backoff_rules)
    delete diphone_backoff_rules;
  diphone_backoff_rules = dbo;
}


int DiphoneUnitVoice::getPhoneList( const EST_String &phone, ItemList &list )
{
  unsigned int n=0;

  EST_TList<DiphoneVoiceModule*>::Entries it;
  for( it.begin( voiceModules ); it; it++ )
    n += (*it)->getPhoneList( phone, list );

  return n;
}



void DiphoneUnitVoice::precomputeJoinCosts( const EST_StrList &phones, bool verbose  )
{
  EST_StrList::Entries it;
  for( it.begin( phones ); it; it++ ){
    ItemList *l = new ItemList;
    CHECK_PTR(l);

    unsigned int n = getPhoneList( (*it), *l );
    
    if( verbose==true )
      cerr << "phone " << (*it) << "  "  << n << " instances\n";
      
    if( n>0 ){
      jc->computeAndCache( *l, true ); //verbose=true
    }
    else
      EST_warning( "Phone %s not listed in voice", (*it).str() );

    delete l;
  }
}

void DiphoneUnitVoice::fill_target_coefficients(EST_Utterance *utt, EST_Track *tcCoefs)
{

  EST_Relation *segs = utt->relation( "Segment" );

  // Assume that the first voice module can provide this method.
  EST_TList<DiphoneVoiceModule*>::Entries it;
  it.begin( voiceModules );

  (*it)->addTCoefficients(segs, *tcCoefs);
}

