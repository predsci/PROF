# PROF

Welcome to the documentation site for the PROF package. PROF is an R package (with Fortran code) for fitting and forecasting infectious disease 
incidence. The package ingests publicly-available
confirmed hospital admission data fits mechanistic models to the data, and provides short-term 
probabilistic forecasts. Currently, the package supports fitting and forecasting the individual
and combined burden of influenza and COVID-19 at the state level. An S[I]2HR and SE[I]2HR models
are used to fit the two pathogens and both use a flexible time-dependent transmission term. Once the User
selects a state, and either one or both viruses, the PROF sequential fitting procedure determines the joint posterior distribution for the model
parameters.  The joint posterior distribution is then used with the model to generate location-specific
probabilistic forecasts of the near-term number of hospital admissions. If both viruses are chosen, this procedure is done twice and the total hospital burden forecast is estimated by combining the trajectory profiles of each disease in multiple ways, including random, ordered, and in-between.

# Compartmental Models

Will be explained here including sketches

![Compartmental Modles](img/models_sketch.png){ align = left } 
# Time-Dependent Force of Infection

Will be explained here

# Methodology

Explain the fitting procedure, forecasting and combining forecasts


