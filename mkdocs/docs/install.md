# PROF Installation Instructions

This R package requires an R or Rstudio installation and Fortran/GCC compilers. We strongly recommend using the package with its user-friendly Shiny-app GUI (see below).

## Using 'devtools' in an R console

``` bash
> library(devtools)

> install_git(url="<https://github.com/predsci/PROF>")
```

## Cloning through GitHub

Users are also welcome to download the package directly either as a zip file or by cloning the repository.

To clone:

``` bash
> git clone https://github.com/predsci/PROF.git
```

Then, open an R or Rstudio session. Navigate to the location of the cloned project, open the PROF.Rproj file and install the package.

## Shiny-app PROF installation

After successfully installing the PROF package please proceed to clone the GUI for it:

``` bash
> git clone https://github.com/predsci/PROF-shiny-app.git
```

To launch the GUI open the `ui.R` file within PRO-shiny-app
