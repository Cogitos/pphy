# PPHY

- **Author:** Guillaume T. Vallet, gtvallet@gmail.com, Université de Montréal, CRIUGM
- **Version:** 0.2
- **Date:** 2014/05/08
- **Update:** 2014/11/04 -- Fix minor bug to display the horizontal line of the PSS

*Pphy* is R package developed for a personal use to process psychophysic data.
*Pphy* uses the [``modelfree``](http://personalpages.manchester.ac.uk/staff/d.h.foster/software-modelfree/latest/home) package to fit the data locally per subject.
The slope and the point of subjective equivalence are extracted per subjects and conditions.
The package also provide a simple plot function based on the ``ggplot2`` package to draw a graphic per subject and one graphic of the averaged data with standard error (optionally).
The means per subjects and conditions are also returned.

The detailed description of the functions as well as examples are provided in the R documentation.
Type ``?*function_name*`` in your R console to access it.


## Table of Contents

- [Licence](#licence)
- [Dependencies](#dependencies)
- [Installation](#install)
- [Functions](#functions)


## <a name='licence'></a>Licence

This package is released under the [Creative Common Attribution-NonCommercial-ShareAlike 4.0 International](http://creativecommons.org/licenses/by-nc-sa/4.0/) license.


## <a name='dependencies'></a>Dependencies

*Pphy* depends on the following pacakges :

- ``plyr``,
- ``ggplot2``,
- ``grid``,
- ``gridExtra``,
- ``predata``.

All these packages, except *predata*, can be install by typing ``install.packages('package_name')`` in your R console.
The *predata* package should install from Github as the the *pphy* package (see below).

## <a name='install'></a>Installation

To install a R package from Github, you first need to install the devtools package.
In R, type ``install.packages('devtools')``.
Then install *pphy* with the following command : ``install_github('cogitos/pphy')``.
You can also install *predata* by typing ``install_github('cogitos/predata')``.
And now enjoy the package!


## <a name='functions'></a>Functions

### fitPPCurve

This function is simple wrapper to the ``modelfree`` package to fit locally a set of psychophysic data and then extract the slope and the point subjective equivalence.

### psychophy

This function will run the *fitPPCurve* function for each subject and condition of the experiment.
The function will return the raw data, the means per subjects and conditions, the extracted indexes from the fitted values (e.g. slopes) and a list of two plots.
The first plot is a matrix of plots, one per subject, with the raw data and the fitted data.
The second plot is the averaged means across subjects with standard errors.

### plotPPCurve

This function use the ``ggplot2`` package to draw a psychometric curve into a nice and clean visual theme.
The function automatically draw the point of objective equivalence and can add the standard errors as well.
