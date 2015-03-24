#' Psychophy processes psychophysic raw data to fitted value
#' 
#' This function processes psychophysic raw data to compute 
#'  descriptive data by condition and by subjects; fit the data
#'  by subject; and extract the slopes and PSS index.
#'  
#' The fitting is done using the fitPPCurve function of the
#'  present package which use the 'modelfree' package to fit
#'  the data locally
#' (see \url{http://personalpages.manchester.ac.uk/staff/d.h.foster/software-modelfree/latest/index.html)}
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}
#' 
#' Maintainer: Guillaume T. Vallet \email{gtvallet@@gmail.com}
#' @param data A data frame in long format (one row per record).
#' @param wid A string indicating the column name of the subjects
#'  or observations. Defaults to "subject_nr".
#' @param stim A string indicating the column name of the physical
#'  stimulation (e.g. luminance, sound intensity...). 
#' @param resp A string indicating the column name of the response
#'  as 0 and 1. The function will remove all values below 0 and above 1.
#'  Defaults to "correct".
#' @param vars A vector of string indicating the column name of the
#'  dependent variables.
#' @param axnames A vector of length 2 with the stings to use as
#'  y label and x label in the graphs. The function automatically add
#'  "Proportion of " for the y label. Defaults to NULL. 
#' @return Return a list with : 
#'  Means_per_subjects: a data frame of means by condition and by
#'   subjects, a data frame 
#'  Descript_data: a data frame of descriptive data by condition
#'  Fit: a data frame of fitted values (slopes and PSS)
#'  Graphs: a list of two graphics, by subjects (BySubj) and global.
#' @keywords psychophysic 
#' @export
#' @examples
#' ### Generate fake date to use the function ---------------------------------
#' # Levels of the physical stimulation
#' data = rbind(
#'    data.frame(subject=1, stim = 1:10, cond='c1',
#'        corr = c(0, 0, 3, 5, 10, 14, 16, 18, 19, 20),
#'        trials = c(20, 20, 19, 20, 18, 19, 20, 20, 19, 20)),
#'    data.frame(subject=2, stim = 1:10, cond='c1',
#'        corr = c(1, 0, 2, 6, 11, 15, 15, 19, 20, 20),
#'        trials = c(19, 20, 20, 20, 19, 20, 18, 20, 19, 20)),
#'    data.frame(subject=3, stim = 1:10, cond='c1',
#'        corr = c(0, 2, 3, 6, 10, 16, 15, 18, 19, 20),
#'        trials = c(20, 20, 19, 20, 20, 20, 18, 20, 18, 20)),
#'    data.frame(subject=1, stim = 1:10, cond='c2',
#'        corr = c(0, 1, 4, 6, 11, 15, 17, 18, 19, 20),
#'        trials = c(20, 20, 19, 20, 18, 19, 20, 20, 19, 20)),
#'    data.frame(subject=2, stim = 1:10, cond='c2',
#'        corr = c(0, 0, 3, 8, 13, 16, 17, 19, 20, 20),
#'        trials = c(19, 20, 20, 20, 19, 20, 18, 20, 19, 20)),
#'    data.frame(subject=3, stim = 1:10, cond='c2',
#'        corr = c(0, 1, 3, 7, 12, 14, 16, 17, 19, 20),
#'        trials = c(20, 20, 19, 20, 20, 20, 18, 20, 18, 20))
#'    )    
#' 
#' ### Fitting the curve with the modelfree adapation -------------------------
#' fitted = psychophy(data, wid='subject', stim='stim', resp='corr', 
#'                vars='cond')
#' fitted
#' 
#' @seealso See \code{vignette("locglmfit", package = "modelfree")} for more details on
#' the fit function used.

psychophy <- function(data, wid='subject_nr', stim=NULL, resp='correct', vars=NULL, axnames=NULL){
  # GT Vallet  -- CRIUGM
  # 2014/04/14 -- v1
  # 2014/05/28 -- v1.5 -- Fix a minor bug in the generation of graphs

  # Prepare when to display a warning
  op = options("warn")
  on.exit(options(op))
  
  ################################################################
  ###
  ###        PREPARE THE DATA
  ###
  #Sort the unique levels of the physical stimulus
  stim_level = sort( unique( data[, match(stim, names(data))] ) )
  #Replace the orignal name of the response column by 'resp' to simply function reading
  names(data)[match(resp, names(data))] = 'resp'
  #Filter responses to keep only a binary response mode 0/1
  datat = subset(data, data$resp >= 0 && data$resp <=1)
  
  # compute the number of correct response, the number of trials, the mean by
  #   stimulus level by condition and by subject after removing incorrect response
  dt.subj = ddply(data, 
                  c(wid, vars, stim), 
                  summarise,
                      nb_corr   = sum(resp),
                      nb_trials = length(resp),
                      ratio = round(mean(resp), 2)
                  )
  #Copy the stimulus column to a new column names 'stim'
  dt.subj$stim = dt.subj[ , match(stim, names(dt.subj)) ]
  
  
  ################################################################
  ###
  ###        FIT RAW DATA WITH THE MODELFREE PACKAGE
  ###
  options(warn=1)
  warning( "Fitting data could take time, please be patient" )
  options(warn=-1)
  #Fit data with modelfree
  fit = dlply(dt.subj, c(wid, vars), function(df) fitPPCurve( df$stim, df$nb_corr, df$nb_trials )) 
  
  # compute the mean by subject and by condition
  dt.avg = ddply(dt.subj, 
                  c(wid, vars), 
                  summarise,
                  mean = mean(nb_corr)
  )
  
  # format the pfit and the slopes/pss/jnd indexes extracted from the fitted data
  pfitted  = ldply(fit, function(x) rbind(x[[1]]))
  dtfitted = ldply(fit, function(x) rbind(x[[2]]))
  names(pfitted)[-c(1:(1+length(vars)))] = as.character(stim_level)
  pfitted$subj  = dt.avg[,match(wid,names(dt.avg))]
#  dtfitted$subj = dt.avg[,match(wid,names(dt.avg))]
#  dtfitted$cond = dt.avg[,match(vars,names(dt.avg))]
#  dtfitted      = dtfitted[,c(1:5)]
  
  # Compute means, standard errors and confidence intervalls per conditions across subjects
  descp_data = reportWithin(data=dt.subj, dv='ratio', within=c(vars, stim), wid=wid)
  descp_data = subset(descp_data, select=-c(ratio_norm))
  
  if( length(vars)>1  ){
    # Format as wide table the average data
    dt.avg = reshape(descp_data, idvar=vars, timevar=stim, direction="wide", drop=c("N","sd","se","ci"))
    names(dt.avg)[-c(1:length(vars))] = stim_level
  }

  temp = reshape(pfitted, direction="long", varying=list(as.character(stim_level)), v.names="pfit", 
                 idvar=c(wid, vars), timevar="Stim")
  pfit = temp$pfit
  dt.subj = cbind(dt.subj, pfit)

  
  ################################################################
  ###
  ###        PRODUCE PLOT PER SUBJECT AND AVERAGED
  ###
  if( length(stim_level)%%2 == 0){
    if( is.character(stim_level) | is.factor(stim_level) ){
      xax =  ( length(stim_level)/2 ) + 0.5
      x1 = stim_level[1]
    }else{
      xax =  mean(stim_level)
      x1 = min(stim_level)
    }
  }else{
     xax =  mean(stim_level)
     x1 = min(stim_level)
  }
  xstim = as.character( stim_level )

  # Add a new column to plot the unique condition or the interaction of conditions
  dt.subj$ggcond = interaction(dt.subj[,match(vars,names(dt.subj))])

  # Draw the graphs by subject
  plot.bysubj = ggplot(dt.subj, aes_string(x=stim, y='ratio', group='ggcond')) + 
            geom_point(aes(group=ggcond, color=ggcond, shape=ggcond), size=2) + 
            geom_line(aes_string(x=stim, y='pfit', group='ggcond', color="ggcond"), size=1) +
            facet_wrap(as.formula(paste("~", wid)))
  if( !is.null(axnames) ){
    plot.bysubj = plot.bysubj + xlab(axnames[2]) + ylab(paste("Proportion of '", axnames[1], "'",  sep=''))
  }
  # Add the lines to mark the object point of equality
  plot.bysubj = plot.bysubj +
            geom_segment(mapping=aes_string(x=xax, y=0, xend=xax, yend=0.5), 
                  color='gray50', linetype="dashed", size=0.2) +
            geom_segment(mapping=aes_string(x=x1, y=0.5, xend=xax, yend=0.5), 
                  color='gray50', linetype="dashed", size=0.2)
  # Customize the theme
  plot.bysubj = plot.bysubj + theme_bw() + 
            theme(# Grid ---------------------------
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  # Legend -------------------------
                  legend.title=element_blank(), 
                  legend.position="top",
                  legend.key.width=unit(4, "lines"), legend.key.height=unit(2, "lines"),
                  legend.text=element_text(size = 16),
                  # Axis ---------------------------
                  axis.title.x = element_text(face = 'bold', size = 20, vjust =-1), 
                  axis.title.y = element_text(face = 'bold', size = 20, vjust = 0.1),
                  axis.text.x  = element_text(size = 20), 
                  axis.text.y  = element_text(size = 20),
                  # Facet -------------------------
                  strip.text.x = element_text(size = 16),
                  plot.margin  = unit(c(.5,0.5,1.5,.9), "cm")
            )  
  
  if( length(vars)>1  ){
    plot.avg = plotPPCurve(reportWithin(data=dt.subj, dv='ratio', within=c('ggcond', stim), wid=wid),
                           vars=c(wid, 'ggcond'), xvar=stim, resp='ratio', se=T, axnames=axnames)
  }else{
    plot.avg = plotPPCurve(descp_data, vars=c(wid, vars), xvar=stim, resp='ratio', se=T, axnames=axnames)
  }

  return(list(Means_per_subjects=dt.subj, Descript_data=descp_data, Fit=dtfitted,
              Graphs=list(BySubj=plot.bysubj, Global=plot.avg)))
}