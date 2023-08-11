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
The PROF/prof_dev directory provides an example script that is a good starting point for using the package.  This script relies on the two other scripts in the same directory and hence we suggest to start as follows.

Create a 'test' directory 

>\> mkdir test

 navigate to the 'test' directory

 >\> cd test

Copy all three scripts from PROF/prof_dev to the 'test' directory

>\> cp /path/to/PROF/prof_dev/*R .

Open R or R studio and the 'example.R' script

Read the script and execute it, preferably first line by line


