% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/psychophy.R
\name{psychophy}
\alias{psychophy}
\title{Psychophy processes psychophysic raw data to fitted value}
\usage{
psychophy(data, wid = "subject_nr", stim = NULL, resp = "correct",
  vars = NULL, axnames = NULL)
}
\arguments{
\item{data}{A data frame in long format (one row per record).}

\item{wid}{A string indicating the column name of the subjects
or observations. Defaults to "subject_nr".}

\item{stim}{A string indicating the column name of the physical
stimulation (e.g. luminance, sound intensity...).}

\item{resp}{A string indicating the column name of the response
as 0 and 1. The function will remove all values below 0 and above 1.
Defaults to "correct".}

\item{vars}{A vector of string indicating the column name of the
dependent variables.}

\item{axnames}{A vector of length 2 with the strings to use as
y label and x label in the graphs. The function automatically add
"Proportion of " for the y label. Defaults to NULL.}
}
\value{
Return a list with :
 Means_per_subjects: a data frame of means by condition and by
  subjects, a data frame
 Descript_data: a data frame of descriptive data by condition
 Fit: a data frame of fitted values (slopes and PSS)
 Graphs: a list of two graphics, by subjects (BySubj) and global.
}
\description{
This function processes psychophysic raw data to compute
 descriptive data by condition and by subjects; fit the data
 by subject; and extract the slopes and PSS index.
}
\details{
The fitting is done using the fitPPCurve function of the
 present package which use the 'modelfree' package to fit
 the data locally
(see \url{http://personalpages.manchester.ac.uk/staff/d.h.foster/software-modelfree/latest/index.html)}
}
\examples{
### Generate fake date to use the function ---------------------------------
# Levels of the physical stimulation
data = rbind(
   data.frame(subject=1, stim = 1:10, cond='c1',
       corr = c(0, 0, 3, 5, 10, 14, 16, 18, 19, 20),
       trials = c(20, 20, 19, 20, 18, 19, 20, 20, 19, 20)),
   data.frame(subject=2, stim = 1:10, cond='c1',
       corr = c(1, 0, 2, 6, 11, 15, 15, 19, 20, 20),
       trials = c(19, 20, 20, 20, 19, 20, 18, 20, 19, 20)),
   data.frame(subject=3, stim = 1:10, cond='c1',
       corr = c(0, 2, 3, 6, 10, 16, 15, 18, 19, 20),
       trials = c(20, 20, 19, 20, 20, 20, 18, 20, 18, 20)),
   data.frame(subject=1, stim = 1:10, cond='c2',
       corr = c(0, 1, 4, 6, 11, 15, 17, 18, 19, 20),
       trials = c(20, 20, 19, 20, 18, 19, 20, 20, 19, 20)),
   data.frame(subject=2, stim = 1:10, cond='c2',
       corr = c(0, 0, 3, 8, 13, 16, 17, 19, 20, 20),
       trials = c(19, 20, 20, 20, 19, 20, 18, 20, 19, 20)),
   data.frame(subject=3, stim = 1:10, cond='c2',
       corr = c(0, 1, 3, 7, 12, 14, 16, 17, 19, 20),
       trials = c(20, 20, 19, 20, 20, 20, 18, 20, 18, 20))
   )

### Fitting the curve with the modelfree adapation -------------------------
fitted = psychophy(data, wid='subject', stim='stim', resp='corr',
               vars='cond')
fitted
}
\author{
Guillaume T. Vallet \email{gtvallet@gmail.com}

Maintainer: Guillaume T. Vallet \email{gtvallet@gmail.com}
}
\seealso{
See \code{vignette("locglmfit", package = "modelfree")} for more details on
the fit function used.
}
\keyword{psychophysic}

