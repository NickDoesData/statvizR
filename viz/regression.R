
library(ggplot2)

df <- read.csv('C:/Users/nheitzman/OneDrive - Allegion/blog/fitess.csv')



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


model <- lm(OXY ~ MAXPULSE, data=df)

fitted_value_plot <- plt_residual_vs_fitted(model)
fitted_value_plot

std_plt <- plt_standardized_residuals(model)
std_plt


independence_plt <- plt_residual_independence(model)
independence_plt

plot(model, which=2)

x <- df$MAXPULSE
pnorm(x, mean(x), sd(x))



df <- read.csv('desired_height.csv')
colnames(df) <- c('height', 'desired_height')

ggplot(data=df) +
  geom_point(aes(y=desired_height, x=height)) +
  ggtitle("Plot of Heights vs. Desired Height")


model <- lm(df$desired_height ~ df$height)
fitted_value_plot <- plt_residual_vs_fitted(model)
fitted_value_plot


ggplot() +
  geom_histogram(aes(x=model$residuals),binwidth = 3) +
  ggtitle("Histogram of Model Residuals")

 
plt <- ggplot(model) +
  geom_point(aes(x=qqnorm(rstandard(model))[[1]], y=rstandard(model))) + 
  geom_smooth(aes(x=qqnorm(rstandard(model))[[1]], y=rstandard(model)), method='lm') +
  xlab('Theoretical Quantiles') + 
  ylab("Sample Quantiles") +
  ggtitle('Q-Q Plot')
plt

plot(model ,which = 2, xlab='Theoretical Quantiles', ylab='sdfsd')
xlab('Theoretical Quantiles') 
ylab("Sample Quantiles")

mean(model$residuals)

library(tidyr)
anscombe %>% 
  gather(x1, x2, x3, x4, key='x', value='xval') %>% 
  gat

library(readr)
anscombe_tidy <- read_csv('anscombe_tidy.csv')

ggplot(data=anscombe_tidy) +
  geom_point(aes(x=x, y=y)) + 
  facet_wrap( ~ group) +
  geom_smooth(aes(x=x, y=y), method='lm', se = FALSE) + 
  ggtitle('Anscombes Quartet')


summary(lm(y ~ x, anscombe_tidy[anscombe_tidy$group=='group 1',]))
summary(lm(y ~ x, anscombe_tidy[anscombe_tidy$group=='group 2',]))
summary(lm(y ~ x, anscombe_tidy[anscombe_tidy$group=='group 3',]))

ggplot(data = iris) +
  geom_point(aes(x=Petal.Length, y=Petal.Width)) +
  geom_smooth(aes(x=Petal.Length, y=Petal.Width), method='lm', se=FALSE) +
  ggtitle('Plot off Iris Petal Measurements')

model <- lm(iris$Petal.Width ~ iris$Petal.Length)
ggplot() +
  geom_histogram(aes(x=model$residuals), binwidth = .1) +
  ggtitle("Histogram of Model Residuals")

