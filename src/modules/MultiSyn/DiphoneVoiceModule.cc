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

#include "DiphoneVoiceModule.h"
#include "EST_TargetCost.h"
#include "EST_viterbi.h"
#include "EST_rw_status.h"
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

#include "EST_Val.h"

// from src/modules/UniSyn_diphone/us_diphone.h
// this won't be staying here long...
void parse_diphone_times(EST_Relation &diphone_stream, 
			 EST_Relation &source_lab);

SIOD_REGISTER_CLASS(du_voicemodule,DiphoneVoiceModule)
VAL_REGISTER_CLASS(du_voicemodule,DiphoneVoiceModule)

VAL_REGISTER_CLASS(diphonecandidate,DiphoneCandidate)

// defined in a single place to avoid inconsistency.
// Given a phone segment item, return the standard cut point
// time, calculated in the standard way.
float getJoinTime( const EST_Item *seg )
{
  float midt=0.0;

  // hack to avoid overhead of string creation and deletion
  // (EST feature access should really be changed to take 
  // const char* instead of const EST_String& )
  static const EST_String cl_end_str( "cl_end" );
  static const EST_String dipth_str( "dipth" );
  static const EST_String start_str( "start" );

  // work out boundary for diphone join
  if( seg->f_present(cl_end_str) ) // join at cl_end point for stops
    midt = seg->features().val("cl_end").Float();
  else if( seg->f_present(dipth_str) ) // join at 25% through a diphthong
    midt = 0.75*seg->F(start_str) 
      + 0.25*seg->features().val("end").Float();
  else
    midt = ( seg->F(start_str)
	     + seg->features().val("end").Float() ) / 2.0;

  return midt;
}

DiphoneVoiceModule::DiphoneVoiceModule( const EST_StrList& basenames,
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
					
  : fileList( basenames ),
    utt_dir ( uttDir  ),
    utt_ext ( uttExt  ),
    pm_dir( pmDir  ),
    pm_ext( pmExt  ),
    coef_dir( coefDir ),
    JCCoef_ext( JCCoefExt ),
    TCCoef_ext( TCCoefExt ),
    wave_dir( wavDir  ),
    wave_ext( wavExt  ),
    wav_srate( sr ),
    tcdatahash ( 0 ),
    utt_dbase( 0 ),
    catalogue( 0 )
{
  
}


void DiphoneVoiceModule::addTCoefficients( EST_Relation *segs, const EST_Track& coefs )
{
    // This currently copies 4 frames into the segment.
    // The first at the start time, second 0.25 through the segment, etc...

    float t0, t5, tstep;
    EST_FVector *f;
    const int num_coefs = coefs.num_channels();

    // Feature names. There is also a copy of these in EST_HybridTargetCost.cc
    static const EST_String start_str("start");
    static const EST_String ll_str("target_ll");
    static const EST_String l_str("target_l");
    static const EST_String r_str("target_r");
    static const EST_String rr_str("target_rr");

    EST_Item *seg=segs->head();
 
    for( ; seg!=0; seg=inext(seg) )
    {
        if(iprev(seg))
            t0 = iprev(seg)->features().val("end").Float();
        else 
            t0 = 0.0;
        t5 = seg->features().val("end").Float();
        tstep = (t5 - t0)/4;
    

        //cout << "Processing phone: " << seg->name() << endl;
    
        f = new EST_FVector(num_coefs);
        CHECK_PTR(f);
        coefs.copy_frame_out(coefs.index(t0), *f);
        seg->features().set_val( ll_str, est_val(f) );
        //cout << " ll: " << t0 << endl;
        //f->est_save("-","est_ascii");


        f = new EST_FVector(num_coefs);
        CHECK_PTR(f);
        coefs.copy_frame_out(coefs.index(t0 + tstep), *f);
        seg->features().set_val( l_str, est_val(f) );    
        //cout << " l: " << t0 + tstep << endl;
        //f->est_save("-","est_ascii");


        f = new EST_FVector(num_coefs);
        CHECK_PTR(f);
        coefs.copy_frame_out(coefs.index(t0 + 2.0*tstep), *f);
        seg->features().set_val( r_str, est_val(f) );    
        //cout << " r: " << t0 + 2.0*tstep << endl;
        //f->est_save("-","est_ascii");

        f = new EST_FVector(num_coefs);
        CHECK_PTR(f);
        coefs.copy_frame_out(coefs.index(t0 + 3.0*tstep), *f);
        seg->features().set_val( rr_str, est_val(f) );    
        //cout << " rr: " << t0 + 3.0*tstep << endl;
        //f->est_save("-","est_ascii");

    }
}


void DiphoneVoiceModule::addJCoefficients( EST_Relation *segs, const EST_Track& coefs )
{
    float startt, midt, endt;
    EST_FVector *startf, *midf, *endf;
    const int num_coefs = coefs.num_channels();
  
    // hack to avoid overhead of string creation and deletion
    // (EST feature access should really be changed to take 
    // const char* instead of const EST_String& )
    static const EST_String startcoef_str("startcoef");
    static const EST_String midcoef_str("midcoef");
    static const EST_String endcoef_str("endcoef");
    static const EST_String start_str("start");

    EST_Item *seg=segs->head();
    startt = seg->F(start_str);
  
    startf = new EST_FVector(num_coefs);
    CHECK_PTR(startf);
    coefs.copy_frame_out(coefs.index(startt), *startf); //this one not shared
 
    for( ; seg!=0; seg=inext(seg) )
    {
        // work out boundary for diphone join
        midt = getJoinTime( seg );
    
        // copy frames out and set as features
        seg->features().set_val( startcoef_str, est_val(startf) );

        midf = new EST_FVector(num_coefs);
        CHECK_PTR(midf);
        coefs.copy_frame_out(coefs.index(midt), *midf);    
        seg->features().set_val( midcoef_str, est_val(midf) );
    
        endt = seg->features().val("end").Float();
        endf = new EST_FVector(num_coefs);
        CHECK_PTR(endf);
        coefs.copy_frame_out(coefs.index(endt), *endf);    
        seg->features().set_val( endcoef_str, est_val(endf) );
    
        startf = endf; // phones share frame at phone boundary (reference counted in EST_Val)
    }
}

void DiphoneVoiceModule::flatPack( EST_Relation *segs,
				   const EST_TargetCost *tc ) const
{

    const EST_FlatTargetCost *ftc = (EST_FlatTargetCost *)tc;

    for( EST_Item *seg=segs->head(); inext(seg) !=0; seg=inext(seg) )
        tcdatahash->add_item(seg, ftc->flatpack(seg));

}

void DiphoneVoiceModule::initialise( const EST_TargetCost *tc, bool ignore_bad_tag )
{
  EST_Utterance *u=0;
  EST_Relation *segs=0;

  tcdatahash = new TCDataHash(500);

  utt_dbase = new EST_TList<EST_Utterance *>;
  CHECK_PTR(utt_dbase);
  
  catalogue = new EST_TStringHash<ItemList*>( 2500 );
  CHECK_PTR(catalogue);

  int numIgnoredPhones=0;

  if(ignore_bad_tag)
    EST_warning( "Looking for bad flags");
  else
    EST_warning( "Ignoring bad flags");


  for( EST_Litem *it=fileList.head(); it!=0 ; it=it->next() ){
    u = new EST_Utterance;
    CHECK_PTR(u);
    
    if( (u->load(utt_dir+fileList(it)+utt_ext)) != read_ok )
      EST_error( "Couldn't load utterance %s\n", 
		 (const char*)fileList(it) );
    
    segs = u->relation( "Segment" );
    
    // add join cost coefficients (at middle of phones)
    EST_Track JCCoefs;
    if( (JCCoefs.load((coef_dir+fileList(it)+JCCoef_ext))) != read_ok )
      EST_error( "Couldn't load data file %s", 
		 (const char*) (coef_dir+fileList(it)+JCCoef_ext) );      
    
    addJCoefficients( segs, JCCoefs );

    // Load target cost Coefficients if specified
    if ( TCCoef_ext != EST_String::Empty ) {
      
      EST_Track TCCoefs;
      
      if( (TCCoefs.load((coef_dir+fileList(it)+TCCoef_ext))) != read_ok )
        EST_error( "Couldn't load data file %s", 
     (const char*) (coef_dir+fileList(it)+TCCoef_ext) );      
    
    addTCoefficients( segs, TCCoefs );

    }    


    if (tc->is_flatpack())
      {
	flatPack(segs,tc);
	u->remove_relation("Token");
	u->remove_relation("Word");
	u->remove_relation("Phrase");
	u->remove_relation("Syllable");
	u->remove_relation("SylStructure");
	u->remove_relation("IntEvent");
	u->remove_relation("Intonation");
      }    

    addToCatalogue( u, &numIgnoredPhones, ignore_bad_tag );
    utt_dbase->append( u );
  }
  
  if(ignore_bad_tag)
    EST_warning( "Ignored %d phones with bad flag set\n", numIgnoredPhones ); 
}

DiphoneVoiceModule::~DiphoneVoiceModule()
{
  if( utt_dbase != 0 ){
    EST_Litem *it = utt_dbase->head();
    for( ; it!=0 ; it=it->next() )
      delete (*utt_dbase)(it);
    delete utt_dbase;
  }
  
  delete catalogue;

  if(tcdatahash)
    delete tcdatahash;

}

void DiphoneVoiceModule::addToCatalogue( const EST_Utterance *utt, int *num_ignored, bool ignore_bad )
{
    EST_Item *item, *next_item;
    ItemList *diphoneList;
    const EST_String *ph1, *ph2;
    int found=0;
  
    static const EST_String bad_str( "bad" );

    item = (utt->relation( "Segment" ))->tail();
    if( item!=0 ){
        ph2 = &(item->features().val("name").String());
    
        while( (item=iprev(item)) != 0 )
        {
            next_item = inext(item);

            // You'd think we need to check both item->f_present(bad_str) and 
            // next_item->f_present(bad_str) like this:
            //if((item->f_present(bad_str) || next_item->f_present(bad_str)) && ignore_bad == true){
            // But experiment showed that then each time one diphone too many would be
            // ignored.  This was partly compensated by a bug pesent up to r1.14     
            // (a iteration within "if(item=item->prev()!=0)" just before the "continue")
            // which caused the leftmost bad phone in a row of bad phones NOT to be ignored
            // when the length of the row was even (or when it was odd and ended in the 
            // utterance-final phone, which is never checked for badness).
            if(item->f_present(bad_str) && ignore_bad == true){

                (*num_ignored)++;

                EST_warning( "Ignoring diphone \"%s_%s\" (LEFT %s in %s at %fs, bad flag \"%s\")", 
                             item->S("name").str(), 
                             next_item->S("name").str(), 
                             item->S("name").str(), 
                             utt->f.S("fileid").str(), 
                             item->F("end"),
                             item->S("bad").str() );
	
                if(iprev(item) != 0){
                    continue;
                }
                else 
                    break; //already at start of list, so finish up
            }
      
            ph1 = &(item->features().val("name").String());

            //    EST_warning( "Adding phone \"%s\" (%s, %f) to diphoneList %s_%s",
            //                item->S("name").str(),
            //                utt->f.S("fileid").str(),
            //                item->F("end"),
            //                item->S("name").str(),
            //                next_item->S("name").str());
      
            diphoneList = catalogue->val(EST_String::cat(*ph1,"_",*ph2), found);
      
            if( !found ){
                diphoneList = new ItemList;
                CHECK_PTR(diphoneList);
                catalogue->add_item(EST_String::cat(*ph1,"_",*ph2), diphoneList, 1); // no_search=1
            }
     
            diphoneList->append( item );
      
            ph2 = ph1;
        }
    }
}

void DiphoneVoiceModule::getDiphone( const EST_Item *phone1, 
				     EST_Track* coef, EST_Wave* sig, int *midframe,
				     bool extendLeft, bool extendRight ) const
{
    EST_Item *phone2 = inext(phone1);
	
  // load the relevant parts
  const EST_String &fname = phone1->relation()->utt()->f.val("fileid").String();

  static const EST_String start_str( "start" );

  float startt,midt,endt;
  
  if( extendLeft==true )
    startt = phone1->F(start_str);
  else
    startt = getJoinTime( phone1 );
  
  midt = phone1->features().val("end").Float();

  if( extendRight==true )
    endt = phone2->features().val("end").Float();
  else
    endt = getJoinTime( phone2 );
  
  // get pitchmarks for pitch synchronous synthesis
  EST_Track *tempcoef = new EST_Track;
  CHECK_PTR(tempcoef);
  if( (tempcoef->load((pm_dir+fname+pm_ext))) != read_ok )
    EST_error( "Couldn't load data file %s", 
	       (const char*) (pm_dir+fname+pm_ext) );
  
  // following few lines effectively moves segment boundaries to
  // line up with pitch periods. 
  int copy_start = tempcoef->index( startt );
  int copy_end   = tempcoef->index( endt );
  //copy_end -= 1; //so that adjacent units don't start and end with same frame

  int copy_len   = copy_end - copy_start;
  //int copy_len   = copy_end - copy_start + 1;

  startt = tempcoef->t( copy_start );
  endt = tempcoef->t( copy_end );
  
  if( copy_len == 0 ){
    EST_warning( "%s(%f->%f): %s_%s diphone length means 1 pitchmark will be duplicated",
		 fname.str(), startt, endt, phone1->S("name").str(), phone2->S("name").str() );
    copy_len=1;
  }
  else if( copy_len < 0 ){
    EST_error( "%s(%f->%f): %s_%s diphone length renders %d pitchmark",
	       fname.str(), startt, endt, phone1->S("name").str(), phone2->S("name").str(), copy_len );
  }
  
  tempcoef->copy_sub_track( *coef, copy_start, copy_len );
  
  *midframe = coef->index( midt );
  
  // adjust timing, which Festival synthesis code makes assumptions about
  // SPECIFICALLY, the unisyn module wants all units to start from
  // the first value above 0.0 (as the first pitch mark)
  float t_off = (copy_start!=0) ? tempcoef->t(copy_start-1) : 0.0;
  int nframes = coef->num_frames();
  for( int i=0; i<nframes; ++i )
    coef->t(i) -= t_off;

  //start waveform at previous pitchmark (this is period approximation used) 
  int st_sample = (int)rint( t_off * (float) wav_srate ); 

  //preferably end waveform at following pitchmark (follows convention in UniSyn module)
  int end_sample;
  if( copy_end < tempcoef->num_frames() )
    end_sample = (int) rint( tempcoef->t(copy_end) * (float) wav_srate );
  //if( copy_end+1 < tempcoef->num_frames() )
  //  end_sample = (int) rint( tempcoef->t(copy_end+1) * (float) wav_srate );
  else{
    // estimate from previous pitch mark shift
    int pp_centre_sample = (int) rint( endt * (float) wav_srate );
    int pp_first_sample  = (int) rint( tempcoef->t(copy_end) * (float) wav_srate );
    //int pp_first_sample  = (int) rint( tempcoef->t(copy_end-1) * (float) wav_srate );
    end_sample           = (2*pp_centre_sample)-pp_first_sample;    
  }

  // (obviously, we would want to load and cache any files         //
  // which haven't been loaded yet, rather than just load          //
  // the parts each and every time)                                //
  if( sig->load( wave_dir+fname+wave_ext,                          //
		 st_sample, end_sample-st_sample+1) != read_ok )   //
    EST_error( "Couldn't load data file %s",                       //
	       (const char*) (wave_dir+fname+wave_ext) );          //

  delete tempcoef;
}


inline EST_VTCandidate* makeCandidate( const EST_Item *target_ph1,
				       const EST_Item *cand_ph1,
				       const EST_TargetCost *tc,
				       const TCData *tcd,
				       const TCDataHash *tcdatahash,
				       float tc_weight,
				       const DiphoneVoiceModule *dvm_p )
{
  // hack to avoid overhead of string creation and deletion
  // (EST feature access should really be changed to take 
  // const char* instead of const EST_String& )
  static const EST_String extendLeft_str("extendLeft");
  static const EST_String extendRight_str("extendRight");
  static const EST_String jccid_str("jccid");

  EST_VTCandidate *c = new EST_VTCandidate;
  CHECK_PTR(c);
  
  EST_Item *cand_ph2 = inext(cand_ph1);

  // set up all the members we can here
  c->s = const_cast<EST_Item*>(cand_ph1);
  
  EST_FVector *left, *right;
  if( target_ph1->f_present( extendLeft_str ) )
    left = fvector( cand_ph1->features().val( "startcoef" ) );
  else
    left = fvector( cand_ph1->features().val( "midcoef" ) );

  if( inext(target_ph1)->f_present( extendRight_str ) )
    right = fvector( cand_ph2->features().val( "endcoef" ) );
  else
    right = fvector( cand_ph2->features().val( "midcoef" ) );
  
  // an abuse of the "name" EST_Val member to store data we want instead
  // of what is intended to go there
  // (will become unnecessary with a more general candidate class)
  DiphoneCandidate *cand = new DiphoneCandidate( cand_ph1, dvm_p, left, right );
  CHECK_PTR(cand);
  c->name = est_val( cand ); //to get synthesis parameters (deleted by EST_Val c->name) 

  if( cand_ph1->f_present( jccid_str ) ){
    cand->ph1_jccid = cand_ph1->features().val( "jccid" ).Int();
    cand->ph1_jccindex = cand_ph1->features().val( "jccindex" ).Int();
  }

  if( cand_ph2->f_present( jccid_str ) ){
    cand->ph1_jccid = cand_ph2->features().val( "jccid" ).Int();
    cand->ph1_jccindex = cand_ph2->features().val( "jccindex" ).Int();
  }
   
  if(tc->is_flatpack())
    c->score = tc_weight*
      ((const EST_FlatTargetCost *)tc)
      ->operator()( tcd, 
		    tcdatahash->val( const_cast<EST_Item*>(cand_ph1) ) );
  else
    c->score = tc_weight*tc->operator()( target_ph1, cand_ph1 );
 

  return c;
}

inline void itemListToCandidateList( ItemList::Entries &it,
				     EST_VTCandidate **head,
				     EST_VTCandidate **tail,
				     const EST_Item *target_ph1,
				     const EST_TargetCost *tc,
				     const TCDataHash *tcdh,
				     const TCDataHash *tcdatahash,
				     float tc_weight,
				     int count,
				     const DiphoneVoiceModule *dvm_p )

{
  int i=0;
  
  if( count > 0 ){
    TCData *tcd = tcdh->val( const_cast<EST_Item*>(target_ph1) );
    EST_VTCandidate *nextc = 0;
    
    // make last one first
    EST_VTCandidate *c = makeCandidate( target_ph1, (*it), tc, tcd, tcdatahash, tc_weight, dvm_p );
    c->next = nextc;
    *tail = c;
    
    // then iterate back prepending to linked list
    // (order reversed because using c->next)
    nextc = c;
    it++; i++;
    for( ; (it && i<count); it++, i++ ){
      c = makeCandidate( target_ph1, (*it), tc, tcd, tcdatahash, tc_weight, dvm_p );
      c->next = nextc;
      nextc = c;
    }

    *head = c; // keep hold of last one set up
  }

  return;
}

int DiphoneVoiceModule::getCandidateList( const EST_Item& target, 
					  const EST_TargetCost* tc,
					  const TCDataHash *tcdh,
					  float tc_weight,
					  EST_VTCandidate **head,
					  EST_VTCandidate **tail ) const
{ 
  int nfound = 0;
  const EST_Item *target_ph1 = item(target.f("ph1"));

  int found = 0;
  const ItemList *candidateItemList = catalogue->val( target.S("name"), found );
  if( found != 0 ){
    nfound = candidateItemList->length();
    
    ItemList::Entries it = ItemList::Entries(*candidateItemList);

    itemListToCandidateList( it,
			     head, tail, 
			     target_ph1, 
			     tc, tcdh, tcdatahash, tc_weight,
			     nfound, this );
  }

  return nfound;
}


int DiphoneVoiceModule::getPhoneList( const EST_String &phone, ItemList &list )
{
  unsigned int n=0;

  if( utt_dbase != 0 ){
    for( EST_Litem *it=utt_dbase->head(); it!=0 ; it=it->next() ){
      EST_Item *ph=(*utt_dbase)(it)->relation("Segment")->head();
      for( ; ph!=0; ph=inext(ph) ){
	if( ph->S("name") == phone ){
	  list.append( ph );
	  n++;
	}
      }
    }
  }

  return n;
}

bool DiphoneVoiceModule::getUtterance( EST_Utterance** utt, int n ) const
{
  if( n<0 || n>(utt_dbase->length()-1) )
    EST_error( "Utterance index out of bounds" );

  if( utt == 0 )
    EST_error( "Invalid utterance" );

  // deep copy the utterance in question
  *utt = new EST_Utterance( *(utt_dbase->nth(n)) );
  CHECK_PTR(utt);

  return true;
}


bool DiphoneVoiceModule::getUtterance( EST_Utterance **utt, 
				       const EST_String &feat_name,
				       const EST_Val &value ) const
{
  //search down list of utterance structures, comparing
  // fileid feature.  If find a match, return pointer to that
  // utterance.
  for( EST_Litem *it=utt_dbase->head(); it!=0 ; it=it->next() )
    if( (*utt_dbase)(it)->f.val(feat_name) == value ){
      *utt = (*utt_dbase)(it);
      return true;
    }

  return false;
} 

void DiphoneVoiceModule::getDiphoneCoverageStats(EST_DiphoneCoverage *dc) const
{
  for( EST_Litem *it=utt_dbase->head(); it!=0 ; it=it->next() )
    dc->add_stats((*utt_dbase)(it));
}



unsigned int DiphoneVoiceModule::numUnitTypes() const
{
  return catalogue ? catalogue->num_entries() : 0;
}

unsigned int DiphoneVoiceModule::numModuleUnits() const
{
  unsigned int sum=0;
  
  if( catalogue != 0 ){
    EST_TStringHash<ItemList*>::Entries it;
    
    for( it.begin( *catalogue ); it; it++ )
      sum += it->v->length(); //EST_UList.length() counts the entries :(
  }
  
  return sum;
}


unsigned int DiphoneVoiceModule::numAvailableCandidates( const EST_String &unit ) const
{
  int number=0;

  int found=0;
  const ItemList *candidateItemList = catalogue->val( unit, found );

  if( found > 0 )
    number = candidateItemList->length();
  
  return number;
}    
