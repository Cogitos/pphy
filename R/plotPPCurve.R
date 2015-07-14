#' A custom ggplot2 wrapper
#' 
#' This function allows you to create a ggplot psychometric curve
#'  with automatic theming and addition of lines of the objective 
#'  point of equivalance. Optionnaly, the standard errors can be added.
#'  
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}
#' 
#' @param dt A data frame with mean already computed
#' @param vars A vector of variables to use including the subjects
#' @param xvar A variable to designate the x axis
#' @param resp A string of the column name the dependent variable (VD) (y axis)
#' @param se A logical value to display standard error (should be called se) Defaults to False
#' @param axnames A vector of length 2 with the stings to use as y label and x label
#' @keywords ggplot2
#' @export
#' @examples
#' plotme()

plotPPCurve = function(dt, vars=NULL, xvar=NULL, resp="resp", se=F, axnames=NULL){
# GT Vallet  -- Lyon2 University
# 2013/07/01 -- v1
# 2014/03/18 -- v2 Fix bug to add the PSE line on psychophysic plot
  
  # Remove subjects from the list of variables
  if( length(vars)>1 ){
    vars = vars[-1]
  }else{
    vars=1
  }
  
  names(dt)[match(resp,names(dt))] = "resp"
  resp = "resp"
  
  # Create the graph
  pd    = position_dodge(.1) # move them .05 to the left and right
  dodge = position_dodge(width=0.9) # errorbar position in histogram
  # Main graph
  plot  = ggplot(dt, aes_string(x=xvar, y=resp, group=vars))

  # Add error bars in psychometric curves
  if( se==T ){
      plot = plot + geom_errorbar(aes(ymin=resp-se, ymax=resp+se), width=.3, colour="gray30", position=pd)  
  }
  
  # Add line(s) and points
  if( vars == 1 ){
    plot = plot + geom_line(aes_string(group=vars), size=1, position=pd) 
  }else{
    plot = plot + geom_line(aes_string(linetype=vars, color=vars), size=1, position=pd)
  }
  plot = plot + geom_point(shape=21, fill="white", size=4, position=pd) 
    
  # Add axe labels
  if( !is.null(axnames) ){
    plot = plot + ylab(paste("Proportion of", axnames[1], sep=" ")) + xlab(axnames[2])   
  }
    
  # For psychophysic experiment, add the POS
  # Line 50%-----------------------------
  if( is.character(dt[,xvar])){
    xax = ceiling( length(unique(dt[,xvar]))/2 )
  }else if( is.factor(dt[,xvar])==T ){
    xax = mean(as.numeric(unique(dt[,xvar])))
  }else {
    xax = mean(unique(dt[,xvar]))
  }
  plot  = plot +
            geom_segment(mapping=aes_string(x=xax, y=0, xend=xax, yend=.5), color='gray50', linetype="dashed", size=.2) +
            geom_segment(mapping=aes_string(x=0, y=0.5, xend=xax, yend=.5), color='gray50', linetype="dashed", size=.2)
  
  # Customize the theme and layout
  plot = plot + theme_bw() + 
    theme(# Grid ---------------------------
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          # Legend -------------------------
          legend.title = element_blank(), 
          legend.text  = element_text(size = 18),
          legend.key   = element_rect(colour = 'black'),
          legend.justification = c(0, 1), legend.position = c(0, 1),
          legend.key.width = unit(4, "lines"), legend.key.height=unit(2, "lines"),
          # Axis ---------------------------
          axis.title.x = element_text(face = 'bold', size = 20, vjust =-1), 
          axis.title.y = element_text(face = 'bold', size = 20, vjust = 0.1),
          axis.text.x  = element_text(size = 18), 
          axis.text.y  = element_text(size = 18),
          plot.margin  = unit(c(.5,0.5,1.5,.9), "cm")
    ) +  
    scale_x_discrete(breaks=unique(dt[,xvar]), labels=unique(dt[,xvar])) + 
    scale_y_continuous(expand = c(0, 0))
  
  return(plot)
}
