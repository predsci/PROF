# PROF
A Package for Respiratory Disease Open-source Forecasting (PROF). PROF is an R package 
for fitting and forecasting infectious disease incidence. The package ingests publicly-available
confirmed hospital admission data fits mechanistic models to the data, and provides short-term 
probabilistic forecasts. Currently, the package supports fitting and forecasting the individual
and combined burden of influenza and COVID-19 at the state level. An S[I]2HR and SE[I]2HR models
are used to fit the two pathogens and both use a flexible time-dependent transmission term. Once the User
selects a state, and either one or both viruses, the PROF sequential fitting procedure determines the joint posterior distribution for the model
parameters.  The joint posterior distribution is then used with the model to generate location-specific
probabilistic forecasts of the near-term number of hospital admissions. If both viruses are chosen, this procedure is done twice and the total hospital burden forecast is estimated by combining the trajectory profiles of each disease in multiple ways: random, ordered, and in-between.


## Requirements
This R package requires an R or RStudio installation and Fortran/GCC compilers.

## PROF Installation Instructions

### Using 'devtools' in an R console
>\> library(devtools)  
>\> install_git(url="https://github.com/predsci/PROF")  

<!--- 
NOTE: This method is convenient, but it may still be worthwhile to download the 
repository (see next subsection) to a user directory. The scripts in examples/ 
directory and the manual (dice/vignettes/dice.pdf) are quite useful.

### Manually from command line
Navigate to your preferred directory  

> $ cd mydir  

Download the repository from GitHub (requires git command line tools)  

> $ git clone https://github.com/predsci/DICE4.git

Navigate into the local repo directory  

> $ cd DICE4

Use python script to compile from source  

> $ ./compile.py

NOTE: If you do not wish to or cannot install DICE globally, it can also be installed 
to a local R-library using 'R CMD build dice' and 'R CMD INSTALL -l /my_lib_loc dice'
-->

## Getting Started
The PROF/prof_dev directory provides an example script that is a good starting point for using the package.  This script relies on two other scripts in the same directory and hence we suggest to start as follows.

Create a 'test' directory 

>\> mkdir test

 navigate to the 'test' directory

 >\> cd test

Copy all three scripts from PROF/prof_dev to the 'test' directory

>\> cp /path/to/PROF/prof_dev/*R .

Open R or R studio and the 'example.R' script

Read the script and execute it, preferably first line by line. Please note that you will likely need to edit directory paths in the script as appropriate.

## Documentation and Examples

Documentation and Examples can be found here: <put link to predsci.github.io/PROF>
## Acknowledgements
The development of PROF is supported by the Council of State and Territorial Epidemiologists (CSTE) and the Centers for Disease Control and Prevention (CDC)
through cooperative agreement number NU38OT000297, as part of the "Development of forecast, analytic, and visualization tools to improve outbreak response and support public health decision-making" project.

## Contact and Contributors

Michal Ben-Nun (mbennun@predsci.com)

James Turtle     (jturtle@predsci.com)

Pete Riley       (pete@predsci.com)



