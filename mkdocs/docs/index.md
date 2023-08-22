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

# Data



# Compartmental Models

PROF uses compartmental models with a flexible time-dependent transmission term to fit daily hospitalization data. For both COVID-19 and influenza, the models include a hospitalization compartment which is split into two subcompartments. This split ensures that the model preserves the correct generation time (T<sub>g</sub>) and that the ratio between cumulative recovered and hospitalized individuals is determined by the infection hospitalization ratio (p<sub>H</sub>). For influenza, individuals begin in the susceptible compartment. If exposed they transition (&beta;(t)) to the infectious compartment where there is a probability of hospitalization or recovery (p<sub>H</sub>/1-p<sub>H</sub>) Individuals progress (1/&mu;) from the first hospitalization compartment (H<sub>1</sub>) to the second (H<sub>2</sub>), and the number entering this compartment is recorded (and optimized, see below). 


![Influenza Model](img/model_influenza.png)


A similar model is used for COVID-19 with the only difference being the addition of the Exposed (but not yet infectious) compartment.

![covid19 Model](img/model_covid19.png)

# Time-Dependent Force of Infection

We use a smoothly varying two-value functional form to describe the time-dependent reproduction number: R(t) = &beta;(t)&gamma;, where &beta;(t) is the time-dependent transmission rate is &gamma; is the total recovery rate. 
![transmission-term-equation](img/transmission_term_eq.png)
The above equation produces a smooth curve where at roughly time t<sub>0</sub> the value of R(t) transitions from R<sub>0</sub> to R<sub>1</sub> 
with an approximate transition time of ~2L days.

# Methodology - Fitting, Forecasting and Combined Burden

Explain the fitting procedure, forecasting and combining forecasts


