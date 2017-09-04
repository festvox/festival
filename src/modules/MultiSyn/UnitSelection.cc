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
/*                 Author: Korin Richmond                                */
/*                   Date: August 2002                                   */
/* --------------------------------------------------------------------- */
/*             Generalis[ed/able] Unit Selection synthesis               */
/*                                                                       */
/*************************************************************************/

#include "siod.h"
#include "EST.h"
#include "UnitSelection.h"
#include "DiphoneUnitVoice.h"
#include "DiphoneVoiceModule.h"
#include "EST_JoinCost.h"
#include "EST_TargetCost.h"
#include "EST_FlatTargetCost.h"
#include "EST_HybridTargetCost.h"
#include "safety.h"


static LISP FT_voice_get_units(LISP l_voice, LISP l_utt)
{
  EST_Utterance *u = get_c_utt(l_utt);
  VoiceBase *v = voice( l_voice );

  // Find units and put in utterance  
  v->getUnitSequence( u );

  return l_utt;
}


///////////////////////////////////////////////////////////////////////////////
// experimental candidate omission stuff //////////////////////////////////////
static LISP FT_voice_reget_units(LISP l_duv, LISP l_utt)
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_duv)) ){
    EST_Utterance *u = get_c_utt(l_utt);
    duv->regetUnitSequence( u );
  }
  else
    EST_error( "du_voice_reget_units: expects DiphoneUnitVoice" );

  return l_utt;
}

static LISP FT_utt_tag_unit( LISP l_utt, LISP l_unitnum )
{
  EST_Utterance *u = get_c_utt(l_utt);
  const int n = get_c_int( l_unitnum );
  
  if( n<1 )
    EST_error( "unit number must be greater than 1" );

  EST_Item *it = u->relation("Unit")->first();  
  int i;
  for( i=1; i<=n && it!= 0; i++ )
    it=inext(it);
  
  if( i<=n )
    EST_error( "unit number greater than number of items in unit relation") ;
 
  ItemList* omitlist=0;
  
  if( !it->f_present("omitlist") ){
    omitlist = new ItemList;
    CHECK_PTR(omitlist);
    it->set_val( "omitlist", est_val(omitlist) );
  }
  else
    omitlist = itemlist( it->f("omitlist"));

  EST_Item *currentCandidateUsed = item(it->f("source_ph1"));
  
  fprintf(stderr,  "setting omit flag on unit %d (item %p)\n", i-1, currentCandidateUsed ); 

  omitlist->append( currentCandidateUsed );
  
  return l_utt;
}


///////////////////////////////////////////////////////////////////////////////


static LISP FT_voice_get_name(LISP l_voice)
{
  VoiceBase *v = voice( l_voice );
  EST_String n = v->name();

  return strintern(n);
}

static LISP FT_voice_set_name(LISP l_voice, LISP l_name)
{
  EST_String n = get_c_string(l_name);
  VoiceBase *v = voice( l_voice );
  
  v->set_name( n );
  
  return NIL;
}

static LISP FT_voice_init( LISP l_voice, LISP l_ignore_bad_tag )
{
  VoiceBase *v = voice( l_voice );

  bool ignore_bad_tag = false;
  if( l_ignore_bad_tag != NIL )
    ignore_bad_tag = true;
  
  v->initialise( ignore_bad_tag  );
  return NIL;
}


static LISP FT_voice_debugLevel( LISP l_voice, LISP l_level )
{
  VoiceBase *v = voice( l_voice );

  if( l_level != NIL )
    v->setVerbosity( get_c_int(l_level) );

  return flocons( v->verbosity() );
}

static void parseVoiceDataParams( LISP l_dataparams, 
				  EST_String *uttDir, 
				  EST_String *wavDir,
				  EST_String *pmDir,
				  EST_String *coefDir,
				  EST_String *uttExt,
				  EST_String *wavExt,
				  EST_String *pmExt,
				  EST_String *JCCoefExt,
          EST_String *TCCoefExt)
{
  int listlen = siod_llength( l_dataparams );

  if( listlen == 8 || listlen == 9){
    *uttExt  = get_c_string( CAR1(CDR4(l_dataparams)) );
    *wavExt  = get_c_string( CAR2(CDR4(l_dataparams)) );
    *pmExt   = get_c_string( CAR3(CDR4(l_dataparams)) );
    *JCCoefExt = get_c_string( CAR4(CDR4(l_dataparams)) );
    if( listlen == 8 )
      *TCCoefExt = EST_String::Empty;
    else 
      *TCCoefExt = get_c_string( CAR5(CDR4(l_dataparams)) );
  }
  else if( listlen == 4 ){ //set some defaults
    *uttExt  = ".utt";
    *wavExt  = ".wav";
    *pmExt   = ".pm";
    *JCCoefExt = ".coef"; 
    *TCCoefExt = EST_String::Empty;
  }
  else
    EST_error( "Incorrect number of voice data parameters" );    

  *uttDir  = get_c_string( CAR1(l_dataparams) );
  *wavDir  = get_c_string( CAR2(l_dataparams) );
  *pmDir   = get_c_string( CAR3(l_dataparams) );
  *coefDir = get_c_string( CAR4(l_dataparams) );    
}


static LISP FT_make_du_voice( LISP l_bnames, LISP l_datadirs, LISP l_srate ) 
{ 
  EST_String uttDir, wavDir, pmDir, coefDir;
  EST_String uttExt, wavExt, pmExt, JCCoefExt, TCCoefExt;
  
  int wav_srate = get_c_int( l_srate );
  if( wav_srate <= 0 )
    EST_error( "Waveform sample rate set to %d", wav_srate );
  
  parseVoiceDataParams( l_datadirs,
			&uttDir, &wavDir, &pmDir, &coefDir,
			&uttExt, &wavExt, &pmExt, &JCCoefExt, &TCCoefExt );

  EST_StrList bnames;
  siod_list_to_strlist( l_bnames, bnames );
  
  DiphoneUnitVoice *v;
  v = new DiphoneUnitVoice( bnames, 
			    uttDir, wavDir, pmDir, coefDir,
			    static_cast<unsigned int>(wav_srate),
			    uttExt, wavExt, pmExt, JCCoefExt, TCCoefExt );

  CHECK_PTR(v);
  
  return siod( static_cast<VoiceBase*>(v) );
}


static LISP FT_make_du_voice_module( LISP l_bnames, LISP l_datadirs, LISP l_srate )
{
  EST_String uttDir, wavDir, pmDir, coefDir;
  EST_String uttExt, wavExt, pmExt, JCCoefExt, TCCoefExt;

  int wav_srate = get_c_int( l_srate );
  if( wav_srate <= 0 )
    EST_error( "Waveform sample rate set to %d", wav_srate );

  parseVoiceDataParams( l_datadirs,
			&uttDir, &wavDir, &pmDir, &coefDir,
			&uttExt, &wavExt, &pmExt, &JCCoefExt, &TCCoefExt );
  
  EST_StrList bnames;
  siod_list_to_strlist( l_bnames, bnames );
 
  DiphoneVoiceModule *vm;
  vm = new DiphoneVoiceModule( bnames, 
			       uttDir, wavDir, pmDir, coefDir,
			       static_cast<unsigned int>(wav_srate),
			       uttExt, wavExt, pmExt, JCCoefExt, TCCoefExt );
  CHECK_PTR(vm);

  return siod( vm );
}

static LISP FT_voice_add_module( LISP l_duv, LISP l_bnames, LISP l_datadirs, LISP l_srate )
{
  EST_String uttDir, wavDir, pmDir, coefDir;
  EST_String uttExt, wavExt, pmExt, JCCoefExt, TCCoefExt;


  int wav_srate = get_c_int( l_srate );
  if( wav_srate <= 0 )
    EST_error( "Waveform sample rate set to %d", wav_srate );

  parseVoiceDataParams( l_datadirs,
			&uttDir, &wavDir, &pmDir, &coefDir,
			&uttExt, &wavExt, &pmExt, &JCCoefExt, &TCCoefExt );
  
  EST_StrList bnames;
  siod_list_to_strlist( l_bnames, bnames );

  if( DiphoneUnitVoice* duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_duv)) ){
    if( ! duv->addVoiceModule(bnames, uttDir, wavDir, pmDir, coefDir,
			      static_cast<unsigned int>(wav_srate),
			      uttExt, wavExt, pmExt, JCCoefExt, TCCoefExt ) )
      EST_error( "voice.addModule failed" );
  }
  else
    EST_error( "voice_add_module: expects DiphoneUnitVoice for now" );
  
  return NIL;
}


static LISP FT_du_voice_function( LISP x )
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(x)) ){
    // do things you can only do to a DiphoneUnitVoice
    (void) duv;
  }
  else
    EST_error( "du_voice_function: expects DiphoneUnitVoice" );

  return NIL;
}


static LISP FT_du_voice_precomputeJoinCosts( LISP l_voice, LISP l_phones )
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    EST_StrList phones;
    siod_list_to_strlist( l_phones, phones );
    duv->precomputeJoinCosts( phones  );
  }
  else
    EST_error( "du_voice_function: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_du_voice_set_pruning_beam( LISP l_voice, LISP l_width )
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    duv->set_pruning_beam( get_c_float( l_width ) );
  }
  else
    EST_error( "du_voice_set_pruning: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_du_voice_set_ob_pruning_beam( LISP l_voice, LISP l_width )
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    duv->set_ob_pruning_beam( get_c_float( l_width ) );
  }
  else
    EST_error( "du_voice_set_pruning: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_du_voice_set_tc_rescoring_beam( LISP l_voice, LISP l_width )
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    duv->set_tc_rescoring_beam( get_c_float( l_width ) );
  }
  else
    EST_error( "du_voice_set_tc_scoring_beam: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_du_voice_set_tc_rescoring_weight( LISP l_voice, LISP l_weight )
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    duv->set_tc_rescoring_weight( get_c_float( l_weight ) );
  }
  else
    EST_error( "du_voice_set_tc_rescoring_weight: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_du_voice_set_target_cost_weight( LISP l_voice, LISP l_weight )
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    duv->set_target_cost_weight( get_c_float( l_weight ) );
  }
  else
    EST_error( "du_voice_set_target_cost_weight: expects DiphoneUnitVoice" );

  return NIL;
}

// static LISP FT_du_voice_set_join_cost_weight( LISP l_voice, LISP l_weight )
// {
//   if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
//     duv->set_join_cost_weight( get_c_float( l_weight ) );
//   }
//   else
//     EST_error( "du_voice_set_target_cost_weight: expects DiphoneUnitVoice" );

//   return NIL;
// }

static LISP FT_du_voice_set_prosodic_modification( LISP l_voice, LISP l_mod )
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    duv->set_prosodic_modification( get_c_int( l_mod ) );
  }
  else
    EST_error( "du_voice_set_prosodic_modification: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_du_voice_prosodic_modification(LISP l_voice)
{
  int pm;
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) )
    {
      pm = duv->get_prosodic_modification();
      if (pm == 0)
	return NIL;
      else
	return truth;
    }
  else
    {
    EST_error( "du_voice_prosodic_modification: expects DiphoneUnitVoice" );
    return NIL;
    }
}



static LISP FT_du_voice_set_diphonebackoff(LISP l_voice, LISP l_list)
{

  DiphoneBackoff *dbo;


 if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) )
   {

     dbo = new DiphoneBackoff(l_list);
     CHECK_PTR(dbo);
     duv->set_diphone_backoff(dbo);
   }
 else
   EST_error( "du_voice_set_diphone_backoff: expects DiphoneUnitVoice" );

 return NIL;
}

static LISP FT_du_voice_setTargetCost(LISP l_voice, LISP l_tc)
{
  EST_TargetCost *tc=0;

  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) )
    {
      if( l_tc == NIL ){
	tc = new EST_TargetCost();
	CHECK_PTR(tc);
      }
      else if(l_tc == truth){
	tc = new EST_DefaultTargetCost();
	CHECK_PTR(tc);
      }
      else if(TYPE(l_tc) == tc_closure) {
	tc = new EST_SchemeTargetCost(l_tc);
	CHECK_PTR(tc);
      }
      else if(streq(get_c_string(l_tc),"flat")){
	tc = new EST_FlatTargetCost();
	CHECK_PTR(tc);
      }
      else if(streq(get_c_string(l_tc),"apml")){
	tc = new EST_APMLTargetCost();
	CHECK_PTR(tc);
      }
      else if(streq(get_c_string(l_tc),"hybrid")){
  tc = new EST_HybridTargetCost();
  CHECK_PTR(tc);
      }
      else if(streq(get_c_string(l_tc),"singing")){
	tc = new EST_SingingTargetCost();
	CHECK_PTR(tc);
      }
      else
	EST_error( "du_voice_setTargetcost: Unknown targetcost type." );

      duv->setTargetCost(tc,true);
    }
  else
    EST_error( "du_voice_setTargetcost: expects DiphoneUnitVoice" );
  
  return NIL;
}

static LISP FT_du_voice_setJoinCost(LISP l_voice, LISP l_tc)
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) )
    {
      EST_JoinCost *jc=0;
      if( l_tc == truth ){
	jc = new EST_JoinCost();
	CHECK_PTR(jc);
      }
      else
	EST_error( "du_voice_setJoinCost: currently t is the only supported second arguement" );
      
      duv->setJoinCost(jc,true);
    }
  else
    EST_error( "du_voice_setJoinCost: expects DiphoneUnitVoice" );
  
  return NIL;
}

static LISP FT_voicemodule_getUtterance( LISP l_vm, LISP l_n )
{
  EST_Utterance *utt = 0;

  if( DiphoneVoiceModule *dvm = dynamic_cast<DiphoneVoiceModule*>(voice(l_vm)) ){
    dvm->getUtterance( &utt, get_c_int(l_n) );
  }
  else
    EST_error( "du_voicemodule_function: expects DiphoneVoiceModule" );

  EST_warning( "EST_Utterance = %x\n", utt );
  
  return siod( utt );
}


static LISP FT_voice_getUtteranceByFileID( LISP l_vm, LISP l_fileid )
{
  EST_Utterance *utt = 0;

  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_vm)) ){
    duv->getCopyUnitUtterance( get_c_string(l_fileid), &utt );
  }
  else
    EST_error( "du_voicemodule_function: expects DiphoneVoiceModule" );
  
  EST_warning( "EST_Utterance = %x\n", utt );
  
  return siod( utt );
}


static LISP FT_voice_unit_count( LISP l_voice )
{
  VoiceBase *v = voice( l_voice );
  return flocons( static_cast<double>(v->numDatabaseUnits()) );
}

static LISP FT_voice_unit_type_count( LISP l_voice )
{
  VoiceBase *v = voice( l_voice );
  return flocons( static_cast<double>(v->numUnitTypes()) );
}

static LISP FT_voice_unit_available( LISP l_voice, LISP l_unit )
{
  VoiceBase *v = voice( l_voice );
  
  if( v->unitAvailable( get_c_string( l_unit ) ) )
    return truth;
  
  return NIL;
}

static LISP FT_voice_num_available_candidates( LISP l_voice, LISP l_unit )
{
  VoiceBase *v = voice( l_voice );

  unsigned int number = v->numAvailableCandidates( get_c_string(l_unit) ); 

  return flocons( number );
}

static LISP FT_du_voice_diphone_coverage( LISP l_voice, LISP l_filename)
{
  DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice));
  EST_String filename = get_c_string(l_filename);
 
  duv->diphoneCoverage(filename);

  return NIL;
}

static LISP FT_du_voice_set_jc_f0_weight( LISP l_voice, LISP l_val )
{

  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    duv->set_jc_f0_weight( get_c_float( l_val ) );
    if (duv->get_jc())
      duv->get_jc()->set_f0_weight( get_c_float( l_val ) );
  }
  else
    EST_error( "du_voice_set_jc_f0_weight: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_du_voice_get_jc_f0_weight( LISP l_voice )
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    return flocons(duv->get_jc_f0_weight());
  }
  else
    EST_error( "du_voice_get_jc_f0_weight: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_du_voice_set_jc_power_weight( LISP l_voice, LISP l_val )
{

  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    duv->set_jc_power_weight( get_c_float( l_val ) );
    if (duv->get_jc())
      duv->get_jc()->set_power_weight( get_c_float( l_val ) );
  }
  else
    EST_error( "du_voice_set_jc_power_weight: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_du_voice_get_jc_power_weight( LISP l_voice )
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    return flocons(duv->get_jc_power_weight());
  }
  else
    EST_error( "du_voice_get_jc_power_weight: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_du_voice_set_jc_spectral_weight( LISP l_voice, LISP l_val )
{

  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    duv->set_jc_spectral_weight( get_c_float( l_val ) );
    if (duv->get_jc())
      duv->get_jc()->set_spectral_weight( get_c_float( l_val ) );
  }
  else
    EST_error( "du_voice_set_jc_spectral_weight: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_du_voice_get_jc_spectral_weight( LISP l_voice )
{
  if( DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice)) ){
    return flocons(duv->get_jc_spectral_weight());
  }
  else
    EST_error( "du_voice_get_jc_spectral_weight: expects DiphoneUnitVoice" );

  return NIL;
}

static LISP FT_fill_target_coefficients(LISP l_voice, LISP l_utt, LISP l_trackfile)
{
  EST_Utterance *utt = get_c_utt(l_utt);
  DiphoneUnitVoice *duv = dynamic_cast<DiphoneUnitVoice*>(voice(l_voice));
  EST_String filename = get_c_string(l_trackfile);

  EST_Track tcCoefs;
  if( (tcCoefs.load(filename) != read_ok ))
    EST_error( "Couldn't load data file %s", (const char*)filename );
  duv->fill_target_coefficients(utt, &tcCoefs);

  return l_utt;
}


void festival_MultiSyn_init(void)
{
  proclaim_module("MultiSyn");

  init_subr_2("voice.getUnits", FT_voice_get_units,
  "(voice.getUnits VOICE UTT)\n\
    Voice object VOICE looks at the segment relation in utterance UTT\n\
    and adds a suitable unit sequence in the Unit relation.");

  init_subr_2("utt.tag_unit", FT_utt_tag_unit,
  "(utt.tag_unit UTT INT)\n\
    Tags the candidate used in Unit INT in the Unit relation for omission in\n\
    subsequent reruns of viterbi search for the unit sequence.");

  init_subr_2("du_voice.regetUnits", FT_voice_reget_units,
  "(du_voice.regetUnits DU_VOICE UTT)\n\
    Voice object DU_VOICE looks at the unit relation in utterance UTT\n\
    redoes the viterbi, respecting candidates flagged for omission");

  init_subr_1("voice.getName", FT_voice_get_name,
  "(voice.getName VOICE)\n\
    Gets the name of a voice.");

  init_subr_2("voice.setName", FT_voice_set_name,
  "(voice.setName VOICE NAME)\n\
    Sets the name of a voice.");

  init_subr_2("voice.debugLevel", FT_voice_debugLevel,
  "(voice.debugLevel VOICE LEVEL)\n\
    Query and/or set the level of debugging for VOICE to LEVEL (positive int).\n\
    A level of 0 switches off all debugging messages in the voice.  Leaving\n\
    level unspecified simply returns the current level.");

  init_subr_3( "make_du_voice", FT_make_du_voice,
  "(make_du_voice BASENAMES DATADIRS SAMPLERATE)\n\
    Creates a Diphone UnitSelection Voice, using the list of file basenames\n\
    in LISP list BASENAMES, and the four directory strings in the DATADIRS list.\n\
    The voice waveform data files are sampled at SAMPLERATE." );

  init_subr_3( "make_du_voice_module", FT_make_du_voice_module,
  "(make_du_voice_module BASENAMES DATADIRS SAMPLERATE)\n\
    Creates a Diphone UnitSelection Voice Module, using the list of file basenames\n\
    in LISP list BASENAMES, and the four directory strings in the DATADIRS list.\n\
    The voice waveform data files are sampled at SAMPLERATE." );

  init_subr_4( "voice.addModule", FT_voice_add_module,
  "(voice.addModule VOICE BASENAMES DATADIRS SAMPLERATE)\n\
    Creates a Diphone UnitSelection Voice Module, using the list of file basenames\n\
    in LISP list BASENAMES, and the three directory strings in the remaining\n\
    argument DATADIRS and adds it to the current voice. The voice waveform data\n\
    files are sampled at SAMPLERATE." );
    
  init_subr_2("voice.init", FT_voice_init,
  "(voice.init VOICE IGNORE_BAD)\n\
    Perform any necessary initialisation for the UnitSelection Voice object VOICE.\n\
    If optional IGNORE_BAD is not nil, then phones marked with a \"bad\" feature\n\
    in the segment relation will not be added to the diphone inventory" );

  init_subr_2("voice.getUtteranceByFileID", FT_voice_getUtteranceByFileID,
  "(voice.getUtteranceByFileID VOICE FILEIDSTRING)\n\
    Returns copy of the Utterance in the voice module database, with\n\
    all the Unit relation filled in, ready for synthesis.");

  init_subr_2("voicemodule.getUtterance", FT_voicemodule_getUtterance,
  "(voicemodule.getUtterance VOICEMODULE UTTNUMBER)\n\
  Returns copy of UTTNUMBER Utterance in the voice module database." );

  init_subr_1("voice.numUnitTypes", FT_voice_unit_type_count,
  "(voice.numUnitTypes VOICE)\n\
    Number of different unit types available in Voice object VOICE.");

  init_subr_1("voice.numUnits", FT_voice_unit_count,
  "(voice.numUnits VOICE)\n\
    Total units available in Voice object VOICE.");

  init_subr_2("voice.unitAvailable", FT_voice_unit_available,
  "(voice.unitAvailable VOICE UNIT)\n\
    Returns true or false whether speech fragment UNIT (string) is\n\
    present in the VOICE");

  init_subr_2("voice.numAvailableCandidates", FT_voice_num_available_candidates,
  "(voice.numAvailableCandidates VOICE UNIT)\n\
    Returns the number of instances of speech fragment UNIT (string)\n\
    present in the VOICE");

  init_subr_1("du_voice_function", FT_du_voice_function,
  "(du_voice_function DU_VOICE)\n\
    Does something to a DU_VOICE only");

  init_subr_2("du_voice.precomputeJoinCosts", FT_du_voice_precomputeJoinCosts,
  "(du_voice.precomputeJoinCosts DU_VOICE PHONELIST)\n\
    Calculate and store the join costs for all instances of phones present\n\
    in the phone list.");

  init_subr_2("du_voice.set_pruning_beam", FT_du_voice_set_pruning_beam,
  "(du_voice.set_pruning_beam DU_VOICE BEAMFLOAT)\n\
    Sets the beam pruning parameter for Viterbi search");

  init_subr_2("du_voice.set_ob_pruning_beam", FT_du_voice_set_ob_pruning_beam,
  "(du_voice.set_ob_pruning_beam DU_VOICE BEAMFLOAT)\n\
    Sets the observation beam pruning parameter for Viterbi search");

  init_subr_2("du_voice.set_tc_rescoring_beam", FT_du_voice_set_tc_rescoring_beam,
  "(du_voice.set_tc_rescoring_beam DU_VOICE BEAMFLOAT)\n\
    Sets the target cost rescoring beam width for Viterbi search (set to -1.0 to disable)");

  init_subr_2("du_voice.set_tc_rescoring_weight", FT_du_voice_set_tc_rescoring_weight,
  "(du_voice.set_tc_rescoring_weight DU_VOICE WEIGHTFLOAT)\n\
    Sets the target cost rescoring weight for Viterbi search (set to 0.0 to disable)");

  init_subr_2("du_voice.set_target_cost_weight", FT_du_voice_set_target_cost_weight,
  "(du_voice.set_target_cost_weight DU_VOICE FLOAT)\n\
    Sets the target cost weight (default is 1)");

  /* 
   * This is currently not implemented, due to problems of passing 
   *  such a parameter to the viterbi extend path function.
   * 
   *  init_subr_2("du_voice.set_join_cost_weight", FT_du_voice_set_join_cost_weight,
   *  "(du_voice.set_join_cost_weight DU_VOICE FLOAT)\n	\
   *  Sets the join cost weight (default is 1)");
   */

  init_subr_2("du_voice.set_jc_f0_weight", FT_du_voice_set_jc_f0_weight,
  "(du_voice.set_jc_f0_weight DU_VOICE FLOAT)\n\
   Sets the joincost f0 weight (default 1)");
 
  init_subr_1("du_voice.get_jc_f0_weight", FT_du_voice_get_jc_f0_weight,
  "(du_voice.get_jc_f0_weight DU_VOICE)\n\
    Gets the joincost f0 weight");

  init_subr_2("du_voice.set_jc_power_weight", FT_du_voice_set_jc_power_weight,
  "(du_voice.set_jc_power_weight DU_VOICE FLOAT)\n\
   Sets the joincost power weight (default 1)");
 
  init_subr_1("du_voice.get_jc_power_weight", FT_du_voice_get_jc_power_weight,
  "(du_voice.get_jc_f0_weight DU_VOICE)\n\
    Gets the joincost f0 weight");

  init_subr_2("du_voice.set_jc_spectral_weight", FT_du_voice_set_jc_spectral_weight,
  "(du_voice.set_jc_spectral_weight DU_VOICE FLOAT)\n\
   Sets the joincost spectral weight (default 1)");
 
  init_subr_1("du_voice.get_jc_spectral_weight", FT_du_voice_get_jc_spectral_weight,
  "(du_voice.get_jc_f0_weight DU_VOICE)\n\
    Gets the joincost f0 weight");

  init_subr_2("du_voice.set_prosodic_modification", FT_du_voice_set_prosodic_modification,
  "(du_voice.set_prosodic_modification DU_VOICE INT)\n\
    Turns prosodic modification on or off (default is 0 [off])\n\
    This will only work if durations and f0 targets are provided");

  init_subr_1("du_voice.prosodic_modification", FT_du_voice_prosodic_modification,
  "(du_voice.prosodic_modification DU_VOICE)\n\
    Status of prosodic modification on or off.");

  init_subr_2("du_voice.setDiphoneBackoff", FT_du_voice_set_diphonebackoff,
  "(du_voice.setDiphoneBackoff DU_VOICE LIST)\n\
    Adds diphone backoff rules to the voice.");

  init_subr_2("du_voice.setJoinCost", FT_du_voice_setJoinCost,
  "(du_voice.setJoinCost DU_VOICE JOINCOST)\n\
   Sets the voice joincost function.\n\
   If t is specified then the default joincost is used.");

  init_subr_2("du_voice.setTargetCost", FT_du_voice_setTargetCost,
  "(du_voice.setTargetCost DU_VOICE TARGETCOST)\n\
   Sets the voice targetcost  function.\n\
   If t is specified then the default targetcost is used.\n\
   If nil is specified then a null targetcost is used.\n\
   If a closure is specified, this is called as the target cost.\n\
   If 'apml is specified and apml targetcost is uses.");

  init_subr_2("du_voice.getDiphoneCoverage", FT_du_voice_diphone_coverage,
  "(du_voice.getDiphoneCoverage DU_VOICE FILENAME)\n\
   prints diphone coverage information for this voice\n\
   use filename '-' for stdout.");

  init_subr_3("multisyn_hybrid_fill_target_coefficients", FT_fill_target_coefficients,
  "(multisyn_hybrid_fill_target_coefficients VOICE UTT TRACKFILE)\n\
    Use the voice to add the given target cost coefficients to the utterance.");
  

}



