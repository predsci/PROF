
# The functions in this file ensure that we use a consistent
# structure for organizing model and code parameters.



#' Produce an empty parameter list for the SEIRH model.
#' 
#' Each pathogen to be fit/forecast requires a set of model and fit parameters.
#' This function produces an empty list of lists with the structure and entries 
#' required by the fitting function(s) for an SEIRH model.
#'
#' @return A list of lists for modeling and fitting parameters
#' @export
#'
# @examples
init_model_seirh <- function() {

  out = list()

  # Designate which model
  out[['model']] = 'seirh'
  # set initial values for the compartments of the model
  out[['constant_dis_pars']] = list(S0=NA,
                                    E0=NA,
                                    I0=NA,
                                    R0=NA,
                                    H0=NA)
  # Set model parameters for disease transmissibility, incubation time,
  # recovery time, Infection-Hospitalization-Rate, etc
  # set parameter ranges and starting values for fitting
  out[['dis_par_ranges']] = list(
    par_names=c('Beta', 'gamma', 'pH', 'mu_H1H2', 'mu_EI', 'rho', 'baseline', 
                'I0', 'time0','mu_HR', 'immn_wn'),
    parmin=list('Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'mu_EI'= NA, 
                'rho'=NA, 'baseline'=NA, 'I0' = NA, 'time0' = NA, 'mu_HR' = NA, 
                'immn_wn' = NA),
    parmax=list('Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'mu_EI'= NA, 
                'rho'=NA, 'baseline'=NA, 'I0' = NA, 'time0' = NA, 'mu_HR' = NA, 
                'immn_wn' = NA),
    par=list('Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'mu_EI'= NA, 
             'rho'=NA, 'baseline'=NA, 'I0' = NA, 'time0' = NA, 'mu_HR' = NA, 
             'immn_wn' = NA)
  )
  
  # Set which parameters will be optimized.  The functional form of Beta(t)
  # is multi-parameter.  'Beta' appearing in this vector simply implies
  # that the Beta(t) parameters should be optimized (rather than the user
  # setting Beta to a single fixed value in out$dis_par_ranges$par above.)
  out[['par_opt']] = c('mu_H1H2', 'mu_EI', 'pH', 'baseline', 'I0', 'time0',
                       'Beta')

  # Set parameters for fitting routine
  out[['mcmc_pars']] = list(nMCMC = 1e6,
                         nlines = 1e4)


  return(out)
}


#' Produce an empty parameter list for the SIRH model.
#' 
#' Each pathogen to be fit/forecast requires a set of model and fit parameters.
#' This function produces an empty list of lists with the structure and entries 
#' required by the fitting function(s) for an SIRH model.
#'
#' @return A list of lists for modeling and fitting parameters
#' @export
#'
# @examples
init_model_sirh <- function() {

  out = list()

  # Designate which model
  out[['model']] = 'sirh'
  # set initial values for the compartments of the model
  out[['constant_dis_pars']] = list(S0=NA,
                                    I0=NA,
                                    R0=NA,
                                    H0=NA)
  # Set model parameters for disease transmissibility, incubation time,
  # recovery time, Infection-Hospitalization-Rate, etc
  # set parameter ranges and starting values for fitting
  out[['dis_par_ranges']] = list(
    par_names=c('Beta', 'gamma', 'pH', 'mu_H1H2', 'rho', 'baseline', 'I0', 
                'time0','mu_HR', 'immn_wn'),
    parmin=list('Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'rho'=NA, 
                'baseline'=NA, 'I0' = NA, 'time0' = NA, 'mu_HR' = NA, 
                'immn_wn' = NA),
    parmax=list('Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'rho'=NA, 
                'baseline'=NA, 'I0' = NA, 'time0' = NA, 'mu_HR' = NA, 
                'immn_wn' = NA),
    par=list('Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'rho'=NA, 
             'baseline'=NA, 'I0' = NA, 'time0' = NA, 'mu_HR' = NA, 
             'immn_wn' = NA)
  )

  # Set which parameters will be optimized.  The functional form of Beta(t)
  # is multi-parameter.  'Beta' appearing in this vector simply implies
  # that the Beta(t) parameters should be optimized (rather than the user
  # setting Beta to a single fixed value in out$dis_par_ranges$par above.)
  out[['par_opt']] = c('mu_H1H2', 'pH', 'baseline', 'I0', 'time0', 'Beta')
  
  # Set parameters for fitting routine
  out[['mcmc_pars']] = list(nMCMC  = 1e6,
                            nlines = 1e4)


  return(out)
}


#' Full parameter list structure for multiple diseases.
#' 
#' Each disease to be fit must have an entry in the parameter list. This
#' function simply loops through pathogens and populates its list entry
#' with the appropriate model structure.
#'
#' @param diseases Character vector listing all diseases to be fit.
#' @param models Character vector stating the model to be used for each
#' disease.
#'
#' @return A list of disease parameter lists.
#' @export
#'
# @examples
init_par_list <- function(diseases=c("covid19", "influenza"),
                          models=c("seirh", "sirh")) {

  par_list = list()

  for (ii in 1:length(diseases)) {
    model = models[ii]
    if (model == "seirh") {
      par_list[[diseases[ii]]] = init_model_seirh()
    } else if (model == "sirh") {
      par_list[[diseases[ii]]] = init_model_sirh()
    } else {
      stop("Models currently supported: 'seirh' and 'sirh'.")
    }

  }

  return(par_list)
}


#' Write parameter list to YAML file.
#'
#' @param par_list Parameter list—usually the output of PROF::init_par_list
#' @param file_path Full path where file to be written.
#'
#' @return NULL
#' @export
#' @import yaml
#'
# @examples
write_par_list_yaml <- function(par_list=NULL, file_path=NULL) {

  if (is.null(par_list) | is.null(file_path)) {
    stop("par_list_yaml() requires an input list and complete file_path.")
  } else {
    write_yaml(par_list, file=file_path)
    print(paste0("Parameter list written to ", file_path))
  }
  return(NULL)
}


#' Read parameter list from YAML file.
#' 
#' The fitting function requires a specific list structure for the parameters.
#' See PROF/parameters/param_exmpl.yml for an example YAML file with 
#' documentation. Or use PROF::init_pars_list() to generate an appropriate 
#' structure in R.
#' @param file_path Full path for file to be read.
#'
#' @return List of parameter lists
#' @export
#' @import yaml
#'
# @examples
read_par_list_yaml <- function(file_path=NULL) {
  par_list = read_yaml(file=file_path)

  par_list = read_yaml(file=file_path)
  return(par_list)
}


check_par_list <- function(par_list) {
  # This function will run some basic checks on a user-generated par_list
  # to catch formatting issues.

}


