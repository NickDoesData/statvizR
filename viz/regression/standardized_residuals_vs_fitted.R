


plt_standardized_residuals <- function(model, diagnostic_checks=TRUE){
  
  require(ggplot2)
  require(dplyr)
  
  # only allow lm objects
  if (!inherits(model, "lm"))
    stop("use only with \"lm\" objects")
  
  
  df_standard <- data.frame(fitted_value = model$fitted.values, residual_type = 'Standardized',residual=rstandard(model))
  df_student <- data.frame(fitted_value = model$fitted.values, residual_type = 'Studentized', residual=rstudent(model))
  df <- rbind(df_standard, df_student)
  
  
  df <- df %>% 
    mutate(outlier = factor(
      case_when(
        residual <2 & residual >=-2 ~ 1,
        residual >=2 & residual <3 ~ 2,
        residual >=3 ~ 3, 
        residual <= -2 & residual > -3 ~ 2,
        residual <=-3 ~ 3))
    )
  
  # convert 1,2,3 to labels for chart. residuals may or may not contain outliers/potential
  # outliers, so we'll use a named vector to get a proper mapping
  outlier_levels <- c('Not an Outlier', 'Potential Outlier', 'Outlier')
  names(outlier_levels) <- c(1,2,3)
  levels(df$outlier) <- outlier_levels[levels(df$outlier)]
  
  # change title based on if user wants to see interpretations
  if (diagnostic_checks==TRUE){
    subtitle <-  'Diagnostic Checklist: \n - Values beyond 3 or -3 are considered outliers \n - Standardized residuals are calcualted as the residual/standard error of model \n - Studentized residuals are calcualted as the residual/standard error of model \nwith that given point removed'
  } else {
    subtitle <- NULL
  }
  
  
  plt <- ggplot(data=df) +
    geom_point(aes(x=fitted_value, y=residual, color=outlier)) +
    scale_colour_manual(values = c("Black", "dodgerblue", "red2")) +
    facet_wrap( ~ residual_type) +
    ylab('Residual') +
    xlab('Fitted Value') +
    geom_hline(yintercept=3, col="red2", linetype="dashed", size=.65) +
    geom_hline(yintercept=-3, col="red2", linetype="dashed", size=.65) +
    geom_hline(yintercept=2, col="dodgerblue", linetype="dashed", size=.65) +
    geom_hline(yintercept=-2, col="dodgerblue", linetype="dashed", size=.65) +
    ggtitle('Fitted vs. Stanardized Residual Plot', subtitle=subtitle) +
    theme(legend.title = element_blank()) 
  
  return(plt)
}