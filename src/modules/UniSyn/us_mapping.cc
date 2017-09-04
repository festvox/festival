/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                       Copyright (c) 1996,1997                         */
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
/*                 Author: Paul Taylor                                   */
/*                   Date: 6 Jan 1998                                    */
/* --------------------------------------------------------------------- */
/*            LPC residual synthesis alternative version                 */
/*                                                                       */
/*************************************************************************/

#include "EST_error.h"
#include "EST_inline_utils.h"
#include "us_synthesis.h"

#include "Phone.h"

#include <fstream>

// void make_segment_single_mapping(EST_Relation &source_lab, 
// 				 EST_Track &source_pm, 
// 				 EST_Track &target_pm, EST_IVector &map) 
// {
//     int i = 0;
//     int s_i_start, s_i_end, t_i_start, t_i_end;
//     EST_Item *s;
//     float s_end, s_start, t_end, t_start, f, m;
//     map.resize(target_pm.num_frames());
    
//     s_start = t_start = 0.0;

//     if (target_pm.t(target_pm.num_frames() - 1) < 
// 	source_lab.tail()->F("end",0))
//     {
// 	EST_warning("Target pitchmarks end before end of target segment "
// 		    "timings (%f vs %f). Expect a truncated utterance\n",
// 		    target_pm.t(target_pm.num_frames() - 1),
// 	            source_lab.tail()->F("end",0.0));
//     }

//     //cout << "Source_pm" << source_pm.equal_space() << endl << endl;
//     //cout << "Target_pm" << target_pm.equal_space() << endl << endl;

//     for (s = source_lab.head(); s; s = inext(s))
//     {
// 	s_end = s->F("source_end");
// 	t_end = s->F("end");
	
// 	s_i_start = source_pm.index_below(s_start);
// 	s_i_end = source_pm.index_below(s_end);
// 	t_i_start = target_pm.index_below(t_start);
// 	t_i_end = target_pm.index_below(t_end);

// 	// fudge to make sure that at least one frame is available
// 	if (s_i_end <= s_i_start)
// 	    s_i_end += 1;
	
// 	//printf("%d %d %d %d\n", s_i_start, s_i_end, t_i_start, t_i_end);
// 	//printf("%f %f %f %f\n\n", s_start, s_end, t_start, t_end);
	
// 	m = float (s_i_end - s_i_start)/ float(t_i_end - t_i_start);
// 	for (i = t_i_start, f = 0.0; i < t_i_end; ++i, ++f)
//             map[i] = EST_NINT(f * m) + s_i_start;
// 	s_start = s->F("source_end");
// 	t_start = s->F("end");
//     }
//     if (i == 0)
// 	map.resize(0);  // nothing to synthesize
//     else
// 	map.resize(i - 1);
// }


void make_segment_single_mapping(EST_Relation &source_lab, 
				 EST_Track &source_pm, 
				 EST_Track &target_pm, EST_IVector &map) 
{
    int i = 0;
    int s_i_start, s_i_end, t_i_start, t_i_end;
    EST_Item *s;
    float s_end, s_start, t_end, t_start, m;
    map.resize(target_pm.num_frames());
  
    s_start = t_start = 0.0;
    s_i_start = t_i_start = 0;

    if (target_pm.t(target_pm.num_frames() - 1) < 
        source_lab.tail()->F("end",0))
    {
        EST_warning("Target pitchmarks end before end of target segment "
                    "timings (%f vs %f). Expect a truncated utterance\n",
                    target_pm.t(target_pm.num_frames() - 1),
                    source_lab.tail()->F("end",0.0));
    }
  
  
  
    for (s = source_lab.head(); s; s = inext(s))
    {

        //      printf( "*********************************************\nphone %s\n", s->S("name").str());

        s_end = s->F("source_end");
        t_end = s->F("end");

        s_i_end = source_pm.index_below(s_end);
        t_i_end = target_pm.index_below(t_end);

        // make sure that at least one frame is available
        if (s_i_end <= s_i_start)
            s_i_end += 1;

        //       printf("(s_i_start s_i_end t_i_start t_i_end) %d %d %d %d\n",
        //       	     s_i_start, s_i_end, t_i_start, t_i_end ); 
        //       printf("(s_i_start-s_i_end t_i_start-t_i_end) %d %d\n",
        // 	     s_i_end - s_i_start, t_i_end - t_i_start); 
      


        // OK for time alignment mapping function here to be single 
        // linear across subcomponents?...
        //      m =  float(t_i_end-t_i_start+1)/float (s_i_end-s_i_start+1);
        m = (t_end-t_start)/(s_end-s_start);
        //m =1.0;
        //      printf( "m=%f\n", m );

        // time offsets for relative times 
        float apm_t_off = (s_i_start==0) ? 0.0 : source_pm.t(s_i_start-1);
        float tpm_t_off = (t_i_start==0) ? 0.0 : target_pm.t(t_i_start-1);

        //       printf("apm_t_off = %f\ntpm_t_off = %f\n", apm_t_off, tpm_t_off); 

        int apm_i = s_i_start;                         // analysis pitch mark index
        float apm_t = source_pm.t(apm_i)-apm_t_off;// analysis pitch mark time
        float next_apm_t = source_pm.t(apm_i+1)-apm_t_off;
       
        for( i=t_i_start; i<=t_i_end; ++i ){
            float tpm_t = target_pm.t(i)-tpm_t_off;      // target pitch mark time
	
            // find closest pitchmark (assume only need forward search from current
            // point, since pitchmarks should always be increasing)
            while( (apm_i<=s_i_end) && (fabs((next_apm_t*m)-tpm_t) <= fabs((apm_t*m)-tpm_t)) ){
                // 	  printf("(next_apm_t apm_t) %f %f\n", 
                // 		 fabs((next_apm_t*m)-tpm_t), fabs((apm_t*m)-tpm_t) );
                apm_t = next_apm_t;
                ++apm_i;
                next_apm_t = source_pm.t(apm_i+1)-apm_t_off;
            }
	
            // // 	printf( "tpm %d = apm %d\n", i, apm_i );

            // 	int slow_index = source_pm.index( target_pm(i) );

            // 	printf( "(my slow) %d %d\n", apm_i, slow_index ); 
	
            map[i] = apm_i;
        }
      
        // for next loop
        if (inext(s))
        {
            s_i_start = s_i_end+1;
            t_i_start = t_i_end+1;
            s_start = source_pm.t(s_i_start);
            t_start = target_pm.t(t_i_start);
        }
    }
    if (i == 0)
        map.resize(0);  // nothing to synthesize
    else
        map.resize(i);
}


void make_linear_mapping(EST_Track &pm, EST_IVector &map) 
{
    int pm_num_frames = pm.num_frames();
  
    map.resize(pm_num_frames);
  
    for (int i = 0; i < pm_num_frames; ++i)
        map[i] = i;
}


static bool contiguous( const EST_Item*left, const EST_Item* right )
{
    if( (inext(item(left->f("source_ph1")))) == item(right->f("source_ph1")) )
        return true;
  
    return false;
} 


// note this function overlooks the spacing of the 1st pitchmark (could
// assume previous pitchmark at 0.0 time...)
static void pitchmarksToSpaces( const EST_Track &pm, EST_IVector *spaces, 
				int start_pm, int end_pm, int wav_srate )
{
  int left_pm, right_pm;
  int num_frames = end_pm-start_pm;
  spaces->resize( num_frames, 0 );

  left_pm = (int) rint( pm.t(start_pm)*wav_srate ); //should always be > 0
  for( int i=0; i<num_frames; ++i ){
    right_pm = (int) rint( pm.t(start_pm+i+1)*wav_srate );
    (*spaces)[i] = right_pm - left_pm;	
    left_pm = right_pm;    
  } 
}


void make_join_interpolate_mapping( const EST_Track &source_pm,
				    EST_Track &target_pm, 
				    const EST_Relation &units, 
				    EST_IVector &map )
{
  // would be good if this parameter was available from the current
  // voice...
  float wav_srate = wave(units.head()->f("sig"))->sample_rate();

  // currently, the pitchmarks are just moved, there's no deletion or
  // insertion
  int target_pm_length = source_pm.length();
  target_pm.resize(target_pm_length, source_pm.num_channels());

  // places to keep temporary debugging information
  EST_IVector source_spacing(target_pm_length);
  EST_IVector target_spacing(target_pm_length);
  EST_IVector voicing(target_pm_length);

  // handle special case of first half of first diphone unit
  EST_Item *diphone_left = units.head();

  int left_start_index = diphone_left->I("middle_frame");
  int left_end_index = source_pm.index(diphone_left->F("end"));

  for( int i=0; i<left_start_index; ++i ){
    target_pm.t(i) = source_pm.t(i);
    voicing[i] = 0;
  }
  // middle loop 
  for( EST_Item *diphone_right=inext(diphone_left); 
       diphone_right; 
       diphone_right=inext(diphone_left) ){

    printf( "%s\t%f\n", diphone_left->S("name").str(), diphone_left->F("end"));
    
    int right_start_index = left_end_index + 1; 
    int right_end_index = right_start_index + diphone_right->I("middle_frame");
    
    printf( "%d %d %d %d (l_start, l_end, r_start, r_end\n",
	    left_start_index, 
	    left_end_index,
	    right_start_index,
	    right_end_index );

    EST_String join_phone_name = inext(item(diphone_left->f("ph1")))->S("name");

    cerr << "phone contigous " << contiguous(diphone_left,diphone_right) << endl;

    ///////////DEBUG//////////////////////////////////////////////
    int voicing_val;
    if( ph_is_sonorant( join_phone_name ) &&
	! ph_is_silence( join_phone_name ) ){
      voicing_val = 1;
    }
    else
      voicing_val = 0;

    for( int i=left_start_index; i<right_end_index; ++i )
      voicing[i] = voicing_val;
    ///////////DEBUG//////////////////////////////////////////////
    

    //if not contigous and current phone is a sonorant, then do smoothing
    if( (!contiguous(diphone_left,diphone_right)) &&
	ph_is_sonorant( join_phone_name ) && 
	(!ph_is_silence( join_phone_name) )){

      cerr << "smoothing phone " << join_phone_name << "\n";
      
      printf( "** Calculating spaces **\n" );
      
      ///////////////////////////////////////////////////////////////////////
      // calculate pitchmark spacings expressed as number of waveform samples 
      EST_IVector spaces;

      pitchmarksToSpaces( source_pm, &spaces, 
			  left_start_index, right_end_index, 
			  (int)wav_srate );

      int num_frames = right_end_index-left_start_index;

      printf( "** Adjusting spaces**\n" );      
//       ///////////////////////////////////////////////////////////////////////
//       // now smooth spacings (kind of recursive ad hoc averaging...)

//       for( int i=left_end_index-left_start_index; i>=0; --i ){
// 	int ns = (spaces[i]+spaces[i+1])/2;
// 	spaces[i+1] += spaces[i] - ns;
// 	spaces[i] = ns;
//       }
      
//       for( int i=right_start_index-left_start_index; i<num_frames-1; ++i){
// 	int ns =  (spaces[i]+spaces[i+1])/2;
// 	spaces[i+1] += spaces[i] - ns;
// 	spaces[i] = ns;
//       }
      
//       printf("** new spaces ** \n" );
//       for( int i=0; i<num_frames; ++i )
// 	printf( "space %d (%d) = %d samples\n", i, num_frames, spaces[i] );


      int join_i = left_end_index-left_start_index;
      int joindiff = spaces[join_i+1] - spaces[join_i];


      const unsigned int DEFAULT_SMOOTHN = 5;
      

      //modify left
      int smoothn = min( DEFAULT_SMOOTHN, join_i );

      for( int i=0; i<smoothn; ++i )
	spaces[join_i-i] += (int) rint(joindiff*((float)(smoothn-i)/(2*smoothn)));
 
      //modify right
      joindiff = -joindiff;
      smoothn = min( DEFAULT_SMOOTHN, num_frames-join_i );

      for( int i=0; i<smoothn; ++i )
	spaces[join_i+1+i] += (int) rint( joindiff*((float)(smoothn-i)/(2*smoothn)));


      ////////////////////////////////////////////////////////////////////////
      // copy modified pitchmark positions back into target_pm track
      printf( "** using modified spaces ** \n" );
      
      for( int i=left_start_index; i<right_end_index; ++i ){
	printf( "Using space %d for target pitchmark %d\n", i-left_start_index, i );
	target_pm.t(i) = target_pm.t(i-1) + ((float)spaces[i-left_start_index]/wav_srate);
      }
    }
    else{
      cerr << "no smoothing for " << join_phone_name << "\n";
      for( int i=left_start_index; i<right_end_index; ++i ){
	printf( "Using source pm %d for target pitchmark %d\n", i, i );
	target_pm.t(i) = source_pm.t(i);
      }      
    }

    cerr <<endl;

    // for the next iteration
    left_start_index = right_end_index; // +1 ???
    left_end_index = source_pm.index( diphone_right->F("end") );
    diphone_left = diphone_right;
  }

  // copy the remaining pitchmarks
  for( int i=left_start_index; i<target_pm_length; ++i )
    target_pm.t(i) = source_pm.t(i);

  make_linear_mapping( target_pm, map );

  //////////////////////////////////////////////////////////////////////
  // save for comparison before and after this smoothing function

  // source spacing
  pitchmarksToSpaces( source_pm,
		      &source_spacing, 
		      0, target_pm_length-1, 
		      (int)wav_srate );
  
  ofstream outfile( "/home/korin/projects/smoothing_temp/f0/source_spacing.est" );
  if( !outfile )
    EST_error( "Couldn't open source pitchmark spacing output file" );
  
  outfile << source_spacing << endl;
  outfile.close();
  
  // target spacing
  pitchmarksToSpaces( target_pm,
		      &target_spacing, 
		      0, target_pm_length-1, 
		      (int)wav_srate );

  ofstream afterfile( "/home/korin/projects/smoothing_temp/f0/target_spacing.est" );
  if( !afterfile)
    EST_error( "Couldn't open target pitchmark spacing output file" );

  afterfile << target_spacing << endl;
  afterfile.close();

  ofstream voicingfile( "/home/korin/projects/smoothing_temp/f0/voicing.est" );
  if( !voicingfile)
    EST_error( "Couldn't open target pitchmark spacing output file" );

  voicingfile << voicing << endl;
  voicingfile.close();
}  

void make_join_interpolate_mapping2( const EST_Track &source_pm,
				     EST_Track &target_pm, 
				     const EST_Relation &units, 
				     EST_IVector &map )
{
  // would be good if this parameter was available from the current
  // voice...
  float wav_srate = wave(units.head()->f("sig"))->sample_rate();

  // currently, the pitchmarks are just moved, there's no deletion or
  // insertion
  int target_pm_length = source_pm.length();
  target_pm.resize(target_pm_length, source_pm.num_channels());

  // places to keep temporary debugging information
  EST_IVector source_spacing(target_pm_length);
  EST_IVector target_spacing(target_pm_length);
  EST_IVector voicing(target_pm_length);

  // handle special case of first half of first diphone unit
  EST_Item *diphone_left = units.head();

  int left_start_index = diphone_left->I("middle_frame");
  int left_end_index = source_pm.index(diphone_left->F("end"));

  for( int i=0; i<left_start_index; ++i ){
    target_pm.t(i) = source_pm.t(i);
    voicing[i] = 0;
  }
  // middle loop 
  for( EST_Item *diphone_right=inext(diphone_left); 
       diphone_right; 
       diphone_right=inext(diphone_left) ){

    printf( "%s\t%f\n", diphone_left->S("name").str(), diphone_left->F("end"));
    
    int right_start_index = left_end_index + 1; 
    int right_end_index = right_start_index + diphone_right->I("middle_frame");
    
    printf( "%d %d %d %d (l_start, l_end, r_start, r_end\n",
	    left_start_index, 
	    left_end_index,
	    right_start_index,
	    right_end_index );

    EST_String join_phone_name = inext(item(diphone_left->f("ph1")))->S("name");

    cerr << "phone contigous " << contiguous(diphone_left,diphone_right) << endl;

    ///////////DEBUG//////////////////////////////////////////////
    int voicing_val;
    if( ph_is_sonorant( join_phone_name ) &&
	! ph_is_silence( join_phone_name ) ){
      voicing_val = 1;
    }
    else
      voicing_val = 0;

    for( int i=left_start_index; i<right_end_index; ++i )
      voicing[i] = voicing_val;
    
    ///////////DEBUG//////////////////////////////////////////////
    

//     //if current phone is a sonorant, then add to concat_spaces wave to do
//     //smoothing at end of sonorant stretch
//     EST_Wave concat_spaces(10000, 1, 1); //sample rate is irrelevant
//     int cs_index=0;

//     EST_IVector spaces;
//     if(	ph_is_sonorant( join_phone_name ) && 
// 	(!ph_is_silence( join_phone_name) )){
      
//       cerr << "smoothing phone " << join_phone_name << "\n";
      
//       printf( "** Calculating spaces **\n" );
      
//       ///////////////////////////////////////////////////////////////////////
//       // calculate pitchmark spacings expressed as number of waveform samples 
//       pitchmarksToSpaces( source_pm, &spaces, 
// 			  left_start_index, right_end_index, 
// 			  (int)wav_srate );

//       // copy into right part of concat_spaces
//       for( int i=0; i<spaces.length(); i++ )
// 	concat_spaces.a_no_check(cs_index+i) = spaces[i];

//       cs_index += spaces.length();
//     }
//     else{
//       // if we have some saved up pitchmarks, process them, and continue as 
//       // usual
//       if( cs_index > 0 ){
// 	////////////////////////////////////////////////////////////////////////
// 	// filter them to modify


// 	////////////////////////////////////////////////////////////////////////
// 	// copy modified pitchmark positions back into correct region of 
// 	// target_pm track
// 	printf( "** using modified spaces ** \n" );
	
// 	for( int i=left_start_index; i<right_end_index; ++i ){
// 	  printf( "Using space %d for target pitchmark %d\n", i-left_start_index, i );
// 	  target_pm.t(i) = target_pm.t(i-1) + ((float)spaces[i-left_start_index]/wav_srate);
// 	}

// 	// prepare for next voiced section
// 	concat_spaces.fill(0);
// 	cs_index = 0;
//       }
    
    cerr << "no smoothing for " << join_phone_name << "\n";
    for( int i=left_start_index; i<right_end_index; ++i ){
      printf( "Using source pm %d for target pitchmark %d\n", i, i );
      target_pm.t(i) = source_pm.t(i);
    }      
    
    
    cerr <<endl;
    
    // for the next iteration
    left_start_index = right_end_index; // +1 ???
    left_end_index = source_pm.index( diphone_right->F("end") );
    diphone_left = diphone_right;
  }
  
  // copy the remaining pitchmarks
  for( int i=left_start_index; i<target_pm_length; ++i )
    target_pm.t(i) = source_pm.t(i);

  make_linear_mapping( target_pm, map );

  //////////////////////////////////////////////////////////////////////
  // save for comparison before and after this smoothing function

  // source spacing
  pitchmarksToSpaces( source_pm,
		      &source_spacing, 
		      0, target_pm_length-1, 
		      (int)wav_srate );
  
  ofstream outfile( "/home/korin/projects/smoothing_temp/f0/source_spacing.est" );
  if( !outfile )
    EST_error( "Couldn't open source pitchmark spacing output file" );
  
  outfile << source_spacing << endl;
  outfile.close();
  
  // target spacing
  pitchmarksToSpaces( target_pm,
		      &target_spacing, 
		      0, target_pm_length-1, 
		      (int)wav_srate );

  ofstream afterfile( "/home/korin/projects/smoothing_temp/f0/target_spacing.est" );
  if( !afterfile)
    EST_error( "Couldn't open target pitchmark spacing output file" );

  afterfile << target_spacing << endl;
  afterfile.close();

  ofstream voicingfile( "/home/korin/projects/smoothing_temp/f0/voicing.est" );
  if( !voicingfile)
    EST_error( "Couldn't open target pitchmark spacing output file" );

  voicingfile << voicing << endl;
  voicingfile.close();

  if( const_cast<EST_Track&>(source_pm).save( "/home/korin/projects/smoothing_temp/f0/sourceCoef.est" )
      != write_ok )
    EST_warning( "couldn't write sourceCoef.est file" );
}  


void us_mapping(EST_Utterance &utt, const EST_String &method)
{
    EST_Relation *source_lab, *target_lab;
    EST_IVector *map;
    EST_Track *source_coef=0, *target_coef=0;

    source_coef = track(utt.relation("SourceCoef")->head()->f("coefs"));
    target_coef = track(utt.relation("TargetCoef")->head()->f("coefs"));

    map = new EST_IVector;
    
//    cout << "mapping method: " << method << endl;
    if (method != "segment_single")
	source_lab = utt.relation("SourceSegments");
    target_lab = utt.relation("Segment", 1);

/*    if (method == "segment")
	make_segment_double_mapping(*source_lab, *source_coef, *target_lab, 
			     *target_coef, *map);
    else if (method == "dp_segment")
	make_dp_mapping(*source_lab, *source_coef, *target_lab, 
			     *target_coef, "Match", *map);
			     */
    if (method == "linear")
	make_linear_mapping(*source_coef, *map);
    else if (method == "segment_single")
	make_segment_single_mapping(*target_lab, *source_coef,
			     *target_coef, *map);
    else if (method == "interpolate_joins"){
      cerr << "Doing interpolate_joins\n";
      EST_Relation *units = utt.relation("Unit");
      make_join_interpolate_mapping(*source_coef, *target_coef, *units,*map);
    }
    else if (method == "interpolate_joins2"){
      cerr << "Doing interpolate_joins2\n";
      EST_Relation *units = utt.relation("Unit");
      make_join_interpolate_mapping2(*source_coef, *target_coef, *units,*map);
    }
    else
      EST_error("Mapping method \"%s\" not found\n", (const char *)method);

    utt.create_relation("US_map");
    EST_Item *item = utt.relation("US_map")->append();
    item->set_val("map", est_val(map));
}


void add_wave_to_utterance(EST_Utterance &u, EST_Wave &sig, 
			const EST_String &name)
{
    u.create_relation(name);
    EST_Item *item = u.relation(name)->append();
    item->set_val("wave", est_val(&sig));
}

void map_to_relation(EST_IVector &map, EST_Relation &r, 
		     const EST_Track &source_pm, 
		     const EST_Track &target_pm)
{
    EST_Item *s, *t, *a=NULL;
    EST_Utterance *u = r.utt();
    int i;

//    cout << "source: " << source_pm;
//    cout << "target: " << target_pm;

    u->create_relation("smap");
    u->create_relation("tmap");

    for (i = 0; i < source_pm.num_frames(); ++i)
    {
	s = u->relation("smap")->append();
	s->set("index", i);
	s->set("end", source_pm.t(i));
    }

    for (i = 0; i < target_pm.num_frames(); ++i)
    {
	s = u->relation("tmap")->append();
	s->set("index", i);
	s->set("end", target_pm.t(i));
    }

    EST_Item *last_s = 0;

    for (s = u->relation("smap")->head(); s; s = inext(s))
    {
	int n = s->I("index");
	for (t = u->relation("tmap")->head(); t; t = inext(t))
	{
	    if (map(t->I("index")) == n)
	    {
		if (last_s != s)
		    a = u->relation("lmap")->append(s);
		last_s = s;
		a->append_daughter(t);
		t->set("map", n);
	    }
	}
    }
}

/*
void make_segment_double_mapping(EST_Relation &source_lab, 
				 EST_Track &source_pm, 
				 EST_Relation &target_lab,
				 EST_Track &target_pm, EST_IVector &map) 
{
    int i = 0;
    int s_i_start, s_i_end, t_i_start, t_i_end;
    EST_Item *s, *t;
    float s_end, s_start, t_end, t_start, f, m;
    map.resize(target_pm.num_frames());
    
    s_start = t_start = 0.0;

    if (target_pm.t(target_pm.num_frames() - 1) < 
	target_lab.tail()->F("end"))
	EST_warning("Target pitchmarks end before end of target segment "
		    "timings. Expect a truncated utterance.\n");

    for (s = source_lab.head(), t = target_lab.head(); s && t; 
	 s = inext(s), t = inext(t))
    {
        if (s->S("name") != t->S("name"))
	  cerr << "Warning: Source and Target segment names do not match: "
	       << s->S("name") << " " << t->S("name") << endl;

	s_end = s->F("end");
	t_end = t->F("end");
	
	s_i_start = source_pm.index_below(s_start);
	s_i_end = source_pm.index_below(s->F("end"));
	t_i_start = target_pm.index_below(t_start);
	t_i_end = target_pm.index_below(t->F("end"));

	// fudge to make sure that at least one frame is available
	if (s_i_end <= s_i_start)
	    s_i_end += 1;
	
	// printf("%d %d %d %d\n", s_i_start, s_i_end, t_i_start, t_i_end);
	// printf("%f %f %f %f\n", s_start, s_end, t_start, t_end);
	
	m = float (s_i_end - s_i_start)/ float(t_i_end - t_i_start);
	for (i = t_i_start, f = 0.0; i < t_i_end; ++i, ++f)
            map[i] = EST_NINT(f * m) + s_i_start;

	s_start = s->F("end");
	t_start = t->F("end");
    }
    if (i == 0)
	map.resize(0);  // nothing to synthesize
    else
	map.resize(i - 1);
}


void make_dp_mapping(EST_Relation &source_lab, EST_Track &source_pm, 
		     EST_Relation &target_lab, EST_Track &target_pm, 
		     const EST_String &match_name, EST_IVector &map) 
{
    int i = 0, j;
    int s_i_start, s_i_end, t_i_start, t_i_end;
    EST_Item *s, *t;
    float s_end, s_start, t_end, t_start, f, m, prev_end;
    map.resize(target_pm.num_frames());

    map.fill(-1);
    
    s_start = t_start = 0.0;

    // should really be replaced by feature functions.
    for (prev_end = 0.0, s = source_lab.head(); s; s = inext(s))
    {
	s->set("start", prev_end);
	prev_end = s->F("end");
    }

    // should really be replaced by feature functions.
    for (prev_end = 0.0, s = target_lab.head(); s; s = inext(s))
    {
	s->set("start", prev_end);
	prev_end = s->F("end");
    }

    if (target_pm.t(target_pm.num_frames() - 1) < 
	target_lab.tail()->F("end", 1))
	EST_warning("Target pitchmarks end before end of target segment "
		    "timings. Expect a truncated utterance.\n");

    for (s = source_lab.head(); s; s = inext(s))
    {
	s_start = s->F("start");

	cout << "source: " << *s << endl;

	while (s && (!s->in_relation(match_name)))
	    s = inext(s);

	cout << "active source: " << *s << endl;

	s_end = s->F("end");

	cout << "daughter: " << daughter1(s->as_relation(match_name)) << endl;
	cout << "parent: " << parent(s->as_relation(match_name)) << endl;

	t = parent(s->as_relation(match_name));

	cout << "active target: " << *t << endl;

	t_end = t->F("end");
	t_start = t->F("start");

	s_i_start = source_pm.index_below(s_start);
	s_i_end = source_pm.index_below(s->F("end"));
	t_i_start = target_pm.index_below(t_start);
	t_i_end = target_pm.index_below(t->F("end"));

	// fudge to make sure that at least one frame is available
	if (s_i_end <= s_i_start)
	    s_i_end += 1;
	
	//printf("%d %d %d %d\n", s_i_start, s_i_end, t_i_start, t_i_end);
	//printf("%f %f %f %f\n", s_start, s_end, t_start, t_end);
	
	m = float (s_i_end - s_i_start)/ float(t_i_end - t_i_start);
	for (i = t_i_start, f = 0.0; i < t_i_end; ++i, ++f)
            map[i] = EST_NINT(f * m) + s_i_start;

	cout << endl;

    }
    
    for (i = 0, j = 0; i < target_pm.num_frames(); ++i)
    {
	cout << map(i) << " ";
	if (map(i) != -1)
	{
	    map[j] = map(i);
	    cout << map(j) << " ";
	    target_pm.t(j++) = target_pm.t(i);
	}
    }

    if (j == 0)
	map.resize(0);  // nothing to synthesize
    else
	map.resize(j);
}
*/
