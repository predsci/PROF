# PROF
A Package for Respiratory Disease Open-source Forecasting (PROF) is an R package 
for fitting and forecasting infectious disease incidence.

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
In general, the scripts in examples/ are a good way to get started and the manual 
dice/vignettes/dice.pdf contains much more detailed information along with some 
walk-throughs. Help pages for PROF and YYY are also good starting points:  

> \> library(PROF)  
> \> ?PROF  
> \> ?YYY  

