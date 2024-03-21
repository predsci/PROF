


#' Generate a trajectory-matching matrix for the 'semi_sorted_randA' forecast 
#' aggregation method.
#'
#' @param n integer - the number of forecast profiles
#' @param correlation_val numeric [-1, 1] - a continuous parameter that defines
#' a correlation between the aggregate uncertainties. NOTE: only the sign of 
#' correlation_val is used here.  correlation_val and sample_var are not
#' independent (see sample_var below).
#' @param sample_var numeric [0, n] - Sample var controls the strength of 
#' correlation in the mixing matrix.  We usually define it as 
#' (n - n*abs(correlation_val)),
#' but it is an independent input in this function so the user may define their 
#' own relationship.
#'
#' @return
#' @export
#'
#' @examples
semi_sorted_randA <- function(n, correlation_val=1, 
                              sample_var=n - n*abs(correlation_val)) {
  Aindex = 1:n
  Aorder = sample(x=1:n, size=n, replace=FALSE)
  Bindex = rep(0, n)
  alg_fail_count = 0
  for (ii in Aorder) {
    Aval = ii
    available = setdiff(Aindex, Bindex)
    if (sample_var<1) {
      if (correlation_val >= 0) {
        Bindex[ii] = Aval
      } else {
        Bindex[ii] = n-Aval+1
      }
    } else {
      if (correlation_val >= 0) {
        sample_set = available[(available <= (Aval + sample_var)) &
                                 (available >= (Aval - sample_var))]
      } else {
        sample_set = available[(available <= (n-Aval + sample_var)) &
                                 (available >= (n-Aval - sample_var))]
      }
      # sometimes all matches in the window are already taken.
      # for now, the simple solution is to choose from all available
      if (length(sample_set)==0) {
        # sample_set = available
        # alg_fail_count = alg_fail_count + 1
        # OR double the sample_var until it includes some available indices
        # browser()
        temp_sample_var = sample_var
        while (length(sample_set)==0) {
          temp_sample_var = temp_sample_var*2
          if (correlation_val >= 0) {
            sample_set = available[(available <= (Aval + temp_sample_var)) &
                                     (available >= (Aval - temp_sample_var))]
          } else {
            sample_set = available[(available <= (n-Aval + temp_sample_var)) &
                                     (available >= (n-Aval - temp_sample_var))]
          }
        }
      } 
      if (length(sample_set)==1) {
        Bindex[ii] = sample_set
      } else {
        Bindex[ii] = sample(sample_set, size=1)
      }
    }
  }
  return(list(Aindex=Aindex, Bindex=Bindex, alg_fail_count=alg_fail_count))
}


#' Perform a 'Linear Scaling' aggregation of forecasts.
#' 
#' Unlike the 'semi_sorted_randA' method, this method preserves the 
#' aggregated median sum (i.e. median(A) + median(B) = median(output)). The 
#' uncertainty intervals are linearly interpolated using the results of 
#' semi_sorted_randA at an error correlation of 0 and 1 (or 0 and -1 for
#' negative values) as anchor points.
#' @param A numeric vector of sorted forecast profiles for pathogen A. 
#' @param B numeric vector of sorted forecast profiles for pathogen B.
#' @param cor_val numeric [-1, 1] designating the expected error/uncertainty 
#' correlation between the forecasts for A and B.
#'
#' @return numeric vector of the combined A and B profiles/samples.
#' @export
#'
#' @examples
lin_scale_uncert_profs <- function(A, B, cor_val=1.) {
  # combine forecast vectors in a way that scales from 
  # e_cor = 0 to e_cor = 1 continuously and preserves median
  
  # first generate the e_cor = 0 vector/quantiles
  semi_list = semi_sorted_randA(n=length(A), correlation_val=0, 
                                sample_var=length(A))
  zero_comb = sort(A + B[semi_list$Bindex]) 
  zero_median = median(zero_comb)
  
  if (cor_val >= 0.) {
    # calculate the e_cor = 1 combination
    semi_list = semi_sorted_randA(n=length(A), correlation_val=1., 
                                  sample_var=0)
    one_comb = sort(A + B[semi_list$Bindex])
    one_median = median(one_comb)
    
    # Calculate linearly interpolated widths
    zero_dist = zero_comb - zero_median
    one_dist = one_comb - one_median
    comb_dist = cor_val*(one_dist - zero_dist) + zero_dist
    out_profs = one_median + comb_dist
  } else if (cor_val < 0.) {
    # calculate the e_cor = -1 combination
    semi_list = semi_sorted_randA(n=length(A), correlation_val=-1., 
                                  sample_var=0)
    one_comb = sort(A + B[semi_list$Bindex])
    one_median = median(one_comb)
    
    # Calculate linearly interpolated widths
    zero_dist = zero_comb - zero_median
    one_dist = one_comb - one_median
    comb_dist = cor_val*(one_dist - zero_dist) + one_dist
    out_profs = one_median + comb_dist
  } 
  return(out_profs)
} 


#' Evaluate the combined uncertainty of two forecasts aggregated by addition.
#' 
#' This function is a wrapper for multiple methods ('semi_sorted_randA', 'lin_scale'). 'semi_sorted_randA' is a more proper combination of profiles.  'lin_scale' uses the uncertainty range of 'semi_sorted_randA', but ensures that the combined forecast has a median equal to the sum of the aggregate forecast medians.  The methods assume that vectors A and B are sorted samples of a single forecast target for pathogen A and pathogen B.  
#' @param method_name character - 'semi_sorted_randA' or 'lin_scale'. 
#' @param A numeric vector of sorted forecast profiles for pathogen A. 
#' @param B numeric vector of sorted forecast profiles for pathogen B.
#' @param cor_val numeric [-1, 1] designating the expected error/uncertainty correlation between the forecasts for A and B.
#' @param sample_var numeric non-negative error correlation variance. Used only for 'semi_sorted_randA', this controls the variance in the profile mixing matrix.
#'
#' @return numeric vector of the combined A and B profiles/samples.
#' @export
#'
#' @examples
eval_comb <- function(method_name, A, B, cor_val, sample_var=NULL) {
  # wrapper for the different profiles combination methods
  
  if (method_name == "lin_scale") {
    out_profs = lin_scale_uncert_profs(A, B, cor_val)
  } else if (method_name == "semi_sorted_randA") {
    n_profs = length(A)
    if (is.null(sample_var)) {
      sample_var = n_profs - n_profs*abs(cor_val)
    }
    semi_list = semi_sorted_randA(n=n_profs, correlation_val=cor_val, 
                                  sample_var=sample_var)
    out_profs = A[semi_list$Aindex] + B[semi_list$Bindex]
  } else {
    out_profs = NA
  }
  return(out_profs)
}


