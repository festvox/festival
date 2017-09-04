/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                 (University of Edinburgh, UK) and                     */
/*                           Rob Clark                                   */
/*                         Copyright (c) 2015                            */
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
/*                          Author: Rob Clark                            */
/*                            Date: June 2015                            */
/* --------------------------------------------------------------------- */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*************************************************************************/


#include "EST_HybridTargetCost.h"
#include "EST_FMatrix.h"
#include "ling_class/EST_Item.h"


static float kl_divergence(EST_FVector *a, EST_FVector *b);

float EST_HybridTargetCost::operator()(const EST_Item* targ, const EST_Item* cand) const 
{ 
  set_targ_and_cand(targ,cand);
  score = 0.0;
  weight_sum = 0.0;

  score += add_weight(50.0)*kl_features_cost();

  return score / weight_sum;
}


float EST_HybridTargetCost::kl_features_cost() const
{
    /* In cases where there is a missing diphone and the current target is extended, there should be a feature:
       targ or cand->features().val("extendLeft").String() [or extendRight] set, in which case 6
       comparisons should be made rather than 4.
    */

    // Feature names copied from DiphoneVoiceModule.cc
    static const EST_String start_str("start");
    static const EST_String ll_str("target_ll");
    static const EST_String l_str("target_l");
    static const EST_String r_str("target_r");
    static const EST_String rr_str("target_rr");

    EST_FVector *targv_l_r = NULL;
    EST_FVector *targv_l_rr = NULL;
    EST_FVector *targv_r_ll = NULL;
    EST_FVector *targv_r_l = NULL;

    // First get the Fvectors.
    /* This is complicated by the fact that added interword silences won't have features */
    if( targ->features().present(r_str)) {
        targv_l_r  = fvector(targ->features().val( r_str ));
        targv_l_rr = fvector(targ->features().val( rr_str ));
    }

    if( inext(targ)->features().present(ll_str)) {
        targv_r_ll = fvector(inext(targ)->features().val( ll_str ));
        targv_r_l  = fvector(inext(targ)->features().val( l_str ));
    }

    EST_FVector *candv_l_r  = fvector(cand->features().val( r_str ));
    EST_FVector *candv_l_rr = fvector(cand->features().val( rr_str ));
    EST_FVector *candv_r_ll = fvector(inext(cand)->features().val( ll_str ));
    EST_FVector *candv_r_l  = fvector(inext(cand)->features().val( l_str ));

    return 0.25 * (kl_divergence(targv_l_r,candv_l_r) + kl_divergence(targv_l_rr,candv_l_rr)
                   + kl_divergence(targv_r_ll,candv_r_ll) + kl_divergence(targv_r_l,candv_r_l));
}


  // Symmetric KL divergence for vectors where first n/2 elements are the means
  //   and the second n/2 are the diagonal covariances for Gaussians 
 float kl_divergence(EST_FVector *a, EST_FVector *b)
 {

  // If the target has no features (inserted interword silence) then just return 0.0
  if (!a)
    return 0.0;

  int l1 = a->length();
  int l2 = b->length();

  if (l1 != l2 ) {
    cout << "kl_divergence vector length error: (" << l1 << " , " << l2 << " )" << endl;
    return 1.0; 
  }

  // First extract means and stddevs.
  EST_FVector mean_a;
  EST_FVector mean_b;
  EST_FVector std_a;
  EST_FVector std_b;

  a->sub_vector(mean_a, 0, l1/2);
  b->sub_vector(mean_b, 0, l2/2);
  a->sub_vector(std_a, l1/2, l1/2);
  a->sub_vector(std_b, l2/2, l2/2);

  // Calculate the individual terms of the KL divergence
  // each way around for symmetry
  float trace_term1 = 0.0;
  float trace_term2 = 0.0;
  float sq_tmp;
  float mahalanobis_term1 = 0.0;
  float mahalanobis_term2 = 0.0;
  float det_term1 = 1.0;
  float det_term2 = 1.0;
  

  for (int i = 0 ; i < std_a.length() ; i++) {
    // trace term reduces to an elementwise sum of ratios of stddevs.
    trace_term1 += std_a.a(i) / std_b.a(i);
    trace_term2 += std_b.a(i) / std_a.a(i);
    // The Mahalanobis term reduces to the elementwise sum of the squared mean 
    //  differences divided by the stddevs. 
    sq_tmp = (mean_a.a(i) - mean_b.a(i)) * (mean_a.a(i) - mean_b.a(i));
    mahalanobis_term1 += sq_tmp / std_b.a(i);
    mahalanobis_term2 += sq_tmp / std_a.a(i);
    // The ratio of determinants reduced to the product of elementwise stddev ratios. 
    det_term1 *= std_b.a(i) / std_a.a(i);
    det_term2 *= std_a.a(i) / std_b.a(i);
  }

  // Return the average of non-symmetric kl divergences
  return 0.25 * ( trace_term1 + mahalanobis_term1 - std_a.length() + log(det_term1)
             + trace_term2 + mahalanobis_term2 - std_b.length() + log(det_term2) ) ;

 }






