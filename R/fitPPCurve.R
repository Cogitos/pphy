#' Modelfree derivate to fit a psychometric curve
#' 
#' This function use the 'modelfree'package to fit locally 
#' the data of a psychophysic experiment 
#' (see \url{http://personalpages.manchester.ac.uk/staff/d.h.foster/software-modelfree/latest/index.html)}
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, Universit\'{e} de Montr\'{e}al
#' 
#' Maintainer: Guillaume T. Vallet \email{gtvallet@@gmail.com}
#' @param stim_level A vector of the different level of the physical 
#'   stimulation (e.g. luminance, sound intensity...). If strings are
#'   used, the function will convert the stings as simple digits.
#' @param nb_corr A vector of the number of 'correct' responses of a 
#'  given subject in a given condition.
#' @param nb_trials A vector of number of trials of a given subject
#'  in a given condition (should be of the same length than the nb_corr).
#' @return Return a list with a vector of fitted value for each point
#'  of the initial nb_corr variable and the extracted slope, PSS and JND values.
#' @keywords psychophysic 
#' @export
#' @examples
#' ### Generate fake date to use the function ---------------------------------
#' # Levels of the physical stimulation
#' stim = 1:10
#' # Number of correct responses for each level of stim
#' corr = c(0, 0, 3, 5, 10, 14, 16, 18, 19, 20)
#' # Number of trial per condition actually included
#' trials = c(20, 20, 19, 20, 18, 19, 20, 20, 19, 20)
#' 
#' ### Fitting the curve with the modelfree adapation -------------------------
#' fitted = fitPPCurve(stim, corr, trials)
#' fitted
#' 
#' ### Visuallizing the fit ---------------------------------------------------
#' plot(stim, corr/trials)
#' lines(stim, fitted$pfit, col='red')
#' 
#' @seealso See \code{vignette("locglmfit", package = "modelfree")} for more details on
#' the fit function used.

fitPPCurve = function(stim_level, nb_corr, nb_trials){
  # GT Vallet  -- CRIUGM
  # 2014/04/09 -- v01
  # 2014/05/28 -- v01.5 -- Fix level stim not used as PSS value
  #                        Add JND and increase x resolution
  
  toreturn = list(NULL) # prepare the list to return the results
  # prepare the arguments needed for the modelfree package
  if( is.character(stim_level) ){
    stim_level = 1:length( unique(stim_level) )
  }else if( is.factor(stim_level) ){
    stim_level = as.numeric(levels(stim_level))[stim_level]
  }else{
    stim_level = unique(stim_level)  
  }
  bwd_min = min( diff( stim_level) )
  bwd_max = max( stim_level ) - min( stim_level )
  bwd     = bandwidth_cross_validation( nb_corr, nb_trials, stim_level, c( bwd_min, bwd_max ) )
  bwd     = bwd$deviance
  
  # fit the data
  numxfit = 499 # Number of points to be generated minus 1
  xfit = (max(stim_level)-min(stim_level)) * (0:numxfit) / numxfit + min(stim_level) # Values of x at which to estimate the psychometric function
  xfit = sort(unique(c(xfit, stim_level)))
  pfit = locglmfit( xfit, nb_corr, nb_trials, stim_level, bwd )$pfit
  
  # extract the slopes and the PSS   
  fitted  = threshold_slope( pfit , xfit ) 
  pss     = round( fitted$x_th, 4 )
  slope   = round( fitted$slope*100, 4 )
  x25     = threshold_slope( pfit , xfit, 0.25 )
  x75     = threshold_slope( pfit , xfit, 0.75 )
  jnd     = round( (x75$x_th - x25$x_th) / 2, 4 )
 
  # add data to return to the list
  toreturn[[1]] = pfit[xfit %in% stim_level]
  toreturn[[2]] = cbind(slope, pss, jnd)
  names(toreturn)[[1]]  = 'pfit'
  names(toreturn)[[2]]  = 'fitted'
  
  return( toreturn )
}