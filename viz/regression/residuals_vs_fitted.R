

plt_residual_vs_fitted <- function(model, diagnostic_checks=TRUE){
  
  require(ggplot2)
  
  # only allow lm objects
  if (!inherits(model, "lm"))
    stop("use only with \"lm\" objects")
  
  # change title based on if user wants to see interpretations
  if (diagnostic_checks==TRUE){
    subtitle <-  'Diagnostic Checklist: \n - Non-linearity: Residuals follow no clear trend. Fit line (blue) is close to zero line (red) \n - Heteroscedasticity: Residuals do not grow or shrink with change in x. \n - Outliers: No one or few residual(s) stands out from basic pattern.'
  } else {
    subtitle <- NULL
  }
  
  plt <- ggplot(model, aes(.fitted, .resid)) + 
    geom_point() + 
    stat_smooth(method="loess", se = FALSE, size=.70, color='dodgerblue')+
    geom_hline(yintercept=0, col="red2", linetype="dashed", size=.65) +
    xlab('Fitted Value') +
    ylab('Residual') + 
    ggtitle('Residuals vs. Fitted Values', subtitle = subtitle) 
  
  
  return(plt)
  
}