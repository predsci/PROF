
# The functions in this file ensure that we use a consistent
# structure for organizing model and code parameters.

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
    par_names=c('Beta', 'gamma', 'pH', 'mu_H1H2', 'mu_EI', 'rho', 'baseline', 'I0', 'time0'),
    parmin=list('Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'mu_EI'= NA, 'rho'=NA, 'baseline'=NA, 'I0' = NA, 'time0' = NA),
    parmax=list('Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'mu_EI'= NA, 'rho'=NA, 'baseline'=NA, 'I0' = NA, 'time0' = NA),
    par=list(   'Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'mu_EI'= NA, 'rho'=NA, 'baseline'=NA, 'I0' = NA, 'time0' = NA)
  )

  # Set parameters for fitting routine
  out[['mcmc_pars']] = list(nMCMC  = 1e6,
                         nlines = 1e4)


  return(out)
}


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
    par_names=c('Beta', 'gamma', 'pH', 'mu_H1H2', 'rho', 'baseline', 'I0', 'time0'),
    parmin=list('Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'rho'=NA, 'baseline'=NA, 'I0' = NA, 'time0' = NA),
    parmax=list('Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'rho'=NA, 'baseline'=NA, 'I0' = NA, 'time0' = NA),
    par=list(   'Beta'=NA, 'gamma'=NA, 'pH'=NA, 'mu_H1H2'=NA, 'rho'=NA, 'baseline'=NA, 'I0' = NA, 'time0' = NA)
  )

  # Set parameters for fitting routine
  out[['mcmc_pars']] = list(nMCMC  = 1e6,
                            nlines = 1e4)


  return(out)
}


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


write_par_list_yaml <- function(par_list=NULL, file_path=NULL) {

  if (is.null(par_list) | is.null(file_path)) {
    stop("par_list_yaml() requires an input list and complete file_path.")
  } else {
    write_yaml(par_list, file=file_path)
    print(paste0("Parameter list written to ", file_path))
  }
}


read_par_list_yaml <- function(file_path=NULL) {
  par_list = read_yaml(file=file_path)

  par_list = read_yaml(file=file_path)
  return(par_list)
}


check_par_list <- function(par_list) {
  # This function will run some basic checks on a user-generated par_list
  # to catch formatting issues.

}


