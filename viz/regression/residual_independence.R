
plt_residual_independence <- function(model,diagnostic_checks = TRUE,fit_line=TRUE,date_order = NA){
  
  
  # only allow lm objects
  if (!inherits(model, "lm"))
    stop("use only with \"lm\" objects")
  
  
  if (is.na(date_order)){
    x <- as.integer(rownames(model$model))
  } else{
    # put stuff for time series here
  }
  
  # change title based on if user wants to see interpretations
  if (diagnostic_checks==TRUE){
    subtitle <-  'Diagnostic Checklist: \n - This plot is only applicable if data is in the order of collection \n - Residuals do not increase or decrease with time \n - Positive serial correlation does not exist (shaped like sin plot) \n - Negative serial correlation does not exist (positive points always followed by negative)'
  } else {
    subtitle <- NULL
  }
  
  plt <- ggplot() + 
    geom_point(aes(x=x, y=model$residuals)) + 
    geom_hline(yintercept=0, col="red2", linetype="dashed", size=.65) +
    xlab('Observation Order') +
    ylab('Residual') + 
    ggtitle('Residual Independence plot', subtitle = subtitle) 
  
  fit_line = TRUE
  if (fit_line == TRUE) {
    plt <- plt + geom_line(aes(x=x, y=model$residuals))
  }
  
  
  
  return(plt)
}
