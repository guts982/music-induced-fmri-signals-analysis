---
title: "assignmentStats"
author: "Amit Karn"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.



## Assignment Source Code

### Install packages if not already installed
```{r}
# install.packages("ggplot2")
# install.packages("skimr")
# install.packages("plotly")
# install.packages("rsample")
# install.packages("patchwork")
```

### Importing Dependencies Libraries
```{r}
library(ggplot2)
library(skimr)
library(plotly)
library(rsample)
library(patchwork)  # For combining plots

```


### Import and Prepare data for analysis
```{r}
signals_df <- read.csv("./data.csv") 
time_df <- read.csv("./time.csv", header = FALSE, skip = 1)
colnames(time_df)<-c("time")
signals_df <- cbind(time_df, signals_df)
```

### View Sample Data
```{r}
head(signals_df, 5)
tail(signals_df, 5)
```

### View Data Summary
```{r}
skim(signals_df)
```


### TASK 1: PRELIMINARY DATA ANALYSIS

## TASK 1.1 Time series plots (of input and output signal)

### Generates a series of line plots for all input variables (x1,x3,x4,x5) against time
```{r}
plot_ly(signals_df, x = ~time, y = ~x1, type = 'scatter', mode = 'lines', 
        name = 'x1') %>%
  add_trace(y = ~x3, name = 'x3') %>%
  add_trace(y = ~x4, name = 'x4') %>%
  add_trace(y = ~x5, name = 'x5') %>%
  layout(title = "Time Series of Input Signals(X)",
         xaxis = list(title = "Time (Seconds)"),
         yaxis = list(title = "Signal Value"))
```

### Generate individual line plots for all inputs
```{r}

x1_time <- plot_ly(signals_df, x = ~time, y = ~x1, type = 'scatter', mode = 'lines', name = 'X1') %>%
  layout(title = "Time Series of x1",
         xaxis = list(title = "Time (Seconds)",gridcolor = '#ffff'),
         yaxis = list(title = "X1", gridcolor = '#ffff',  range = list(-4, 4)) )

x3_time <- plot_ly(signals_df, x = ~time, y = ~x3, type = 'scatter', mode = 'lines', name = 'X3') %>%
  layout(title = "Time Series of x3",
         xaxis = list(title = "Time (Seconds)",gridcolor = '#ffff'),
         yaxis = list(title = "X3", gridcolor = '#ffff',  range = list(-5, 5)) )

x4_time <- plot_ly(signals_df, x = ~time, y = ~x4, type = 'scatter', mode = 'lines', name = 'X4') %>%
  layout(title = "Time Series of x4",
         xaxis = list(title = "Time (Seconds)",gridcolor = '#ffff'),
         yaxis = list(title = "X4", gridcolor = '#ffff',  range = list(-5, 5)) )

x5_time <- plot_ly(signals_df, x = ~time, y = ~x5, type = 'scatter', mode = 'lines', name = 'X5') %>%
  layout(title = "Time Series of x5",
         xaxis = list(title = "Time (Seconds)",gridcolor = '#ffff'),
         yaxis = list(title = "X5", gridcolor = '#ffff',  range = list(-5, 5)) )

x1_time
x3_time
x4_time
x5_time

# Plot all using subplots function of plotly
plotly::subplot(x1_time, x3_time,  x4_time, x5_time, nrows = 4, shareX = TRUE,  titleX =TRUE, titleY = TRUE )  %>%
  layout(plot_bgcolor='#F8F4EC', title="Time series analysis of input signals (X1,X3,X4,X5) with time")
```

### Generate and plot the output(x2) against time
```{r}
x2_time <- plot_ly(signals_df, x = ~time, y = ~x2, type = 'scatter', mode = 'lines', name = 'x2') %>%
  layout(title = "Time Series of output signal (X2) with time",
         xaxis = list(title = "Time (Seconds)",zerolinewidth = 2,gridcolor = '#ffff'),
         yaxis = list(title = "X2 (Output Signal)",zerolinewidth = 2,gridcolor = '#ffff'),
         plot_bgcolor="#BCF2F6", paper_bgcolor="#fff", colorway="#334756"
  )
x2_time
```


## TASK 1.2 Distribution for each signal (time-series) 

### Input Signals Distribution
```{r}
x1_distribution = ggplot(signals_df, aes(x = x1)) +    
  geom_histogram(aes(y = after_stat(density) ), bins=10,fill = "#CC2B52") + 
  stat_density(geom="line", color="#424242", linewidth=.8) +
  geom_rug() +
  labs(x="X1", y="Density")

x3_distribution = ggplot(signals_df, aes(x = x3)) +    
  geom_histogram(aes(y = after_stat(density) ), bins=10,fill = "#88C273") + 
  stat_density(geom="line", color="#424242", linewidth=.8) +
  geom_rug() +
  labs(x="X3", y="Density")

x4_distribution = ggplot(signals_df, aes(x = x4)) +    
  geom_histogram(aes(y = after_stat(density) ), bins=10,fill = "#37AFE1") + 
  stat_density(geom="line", color="#424242", linewidth=.8) +
  geom_rug() +
  labs(x="X4", y="Density")

x5_distribution = ggplot(signals_df, aes(x = x5)) +    
  geom_histogram(aes(y = after_stat(density) ), bins=10,fill = "#FF77B7") + 
  stat_density(geom="line", color="#424242", linewidth=.8) +
  geom_rug() +
  labs(x="X5", y="Density")

plotly::subplot(x1_distribution,  x3_distribution,x4_distribution, x5_distribution, nrows=2, shareX = FALSE,
                titleY = TRUE, titleX = TRUE) %>% layout(plot_bgcolor='#F8F4EC', title="Distribution of X1, X3, X4, and X5")
```

### Output Signal Distribution
```{r}
x2_distribution = ggplot(signals_df, aes(x = x2)) +    
  geom_histogram(aes(y = after_stat(density) ), bins=10,fill = "#5A72A0") + 
  stat_density(geom="line", color="#1A2130", linewidth=.8) +
  geom_rug()

ggplotly(x2_distribution) %>% layout(title="Distribution of Output(X2) Signal",
                                     xaxis = list(title="X2 Signal (Output)"),
                                     yaxis = list(title="Density"))
```

### Distributions of every signal (inputs and output) stacked together
```{r}
distribution_df = data.frame(rbind(data.frame(values = signals_df$x1, Inputs = "X1"),
                        data.frame(values= signals_df$x3, Inputs = "X3"),
                        data.frame(values= signals_df$x4, Inputs = "X4"),
                        data.frame(values =signals_df$x5, Inputs = "X5")))

distribution_plot <- ggplot(distribution_df, aes(x = values)) +
  geom_histogram(aes(x=values, y = after_stat(density), fill=Inputs), bins = 10, alpha=0.5)+
  stat_density(aes(x=values, y = after_stat(density), color=Inputs),geom="line", size=.8) +
  geom_rug() 
distribution_plotly <- ggplotly(distribution_plot) %>% layout(plot_bgcolor='#FFF0DC', 
                                              title="Distribution of Input (X) Signals ", 
                                              xaxis= list(title="Input (X) Signal"), 
                                              yaxis = list(title="Density"))
distribution_plotly
```


## TASK 1.3 Correlation and scatter plots (between different combination of input and output signals) to examine their dependencies

### Correlation of Input Signals (X1,X3,X4,X5) with output signal (X2) shown separately
```{r}
x1_correlation = plot_ly(signals_df, x=~x1, y=~x2, type="scatter", mode="markers") %>% 
  layout(plot_bgcolor='#F8F4EC', title="Corelation of X1 input with X2 output",
         xaxis = list(title="X1 Signal (Input)"), yaxis = list(title="X2 Signal (Output)"))


x3_correlation = plot_ly(signals_df, x=~x3, y=~x2, type="scatter", mode="markers") %>% 
  layout(plot_bgcolor='#F8F4EC', title="Corelation of X3 input to X2 output", 
         xaxis = list(title="X3 Signal (Input)"), yaxis = list(title="X2 Signal (Output)"))


x4_correlation = plot_ly(signals_df, x=~x4, y=~x2, type="scatter", mode="markers") %>% 
  layout(plot_bgcolor='#F8F4EC', title="Corelation of X3 input to y output", 
         xaxis = list(title="X4 Signal (Input)"), yaxis = list(title="X2 Signal (Output)"))


x5_correlation = plot_ly(signals_df, x=~x5, y=~x2, type="scatter", mode="markers") %>% 
  layout(plot_bgcolor='#F8F4EC', title="Corelation of X5 input to X2 output", 
         xaxis = list(title="X5 Signal (Input)"), yaxis = list(title="X2 Signal (Output)"))

plotly::subplot(x1_correlation,  x3_correlation,x4_correlation, x5_correlation, nrows=2, 
                shareX = FALSE,
                titleY = TRUE, titleX = TRUE) %>% layout(plot_bgcolor='#F8F4EC',
                                                         title="Correlation of Input Signals (X1,X3,X4,X5) with output signal (X2)")

```

### Correlation of input signals with output signal combined
```{r}
plot_ly(signals_df) %>%
  add_trace( x=~x1, y=~x2,type = "scatter",mode = "markers", name = "X1") %>%  
  add_trace( x=~x3, y=~x2,type = "scatter",mode = "markers", name = "X3") %>%  
  add_trace( x=~x4, y=~x2, type = "scatter",mode = "markers", name = "X4") %>%  
  add_trace( x=~x5, y=~x2,type = "scatter",mode = "markers", name = "X5")  %>% 
  layout(plot_bgcolor='#F8F4EC', title="Corelation of X Signals(Input) with X2 Signal (Output).", 
         xaxis = list(title="X1,X3,X4,X5 Signals (Input)"), yaxis = list(title="X2 Signal (Output)"))
```



### TASK 2: REGRESSION: MODELLING THE RELATIONSHIPS BETWEEN SIGNALS

## TASK 2.1: Estimate model parameters θ = {θ1,θ2,...,θbias}^T for every candidate model using Least Squares (θhat = (X^T*X)^(-1) * X^T * y  ), using the provided input and output signals datasets. (Use all the data for training.)

# Generate models (Design matrices)
```{r}
# Generate models
# Model 1: x2 = θ1x4 +θ2x3^2 + θbias 
generateModel1 <- function(df){
  onesMatrix <- matrix(1, length(df$x2), 1)
  return (cbind(df$x4, df$x3^2, onesMatrix))  # Design matrix
}

# Model 2: x2 = θ1x4 +θ2x3^2 + θ3x5 + θbias 
generateModel2 <- function(df){
  onesMatrix <- matrix(1, length(df$x2), 1)
  return (cbind(df$x4, df$x3^2, df$x5, onesMatrix))   # Design matrix
}

# Model 3: x2 = θ1x3 +θ2x4 + θ3x5^3  
generateModel3 <- function(df){
  return (cbind(df$x3, df$x4, df$x5^3))  # Design matrix
}

# Model 4: x2 = θ1x4 +θ2x3^2 + θ3x5^3 + θbias 
generateModel4 <- function(df){
  onesMatrix <- matrix(1, length(df$x2), 1)
  return (cbind(df$x4, df$x3^2, df$x5^3, onesMatrix))  # Design matrix
}

# Model 5: x2 = θ1x4 +θ2x1^2 + θ3x3^2 + θbias 
generateModel5 <- function(df){
  onesMatrix <- matrix(1, length(df$x2), 1)
  return (cbind(df$x4, df$x1^2, df$x3^2, onesMatrix))  # Design matrix
}

```


### Theta estimator θ = (X^T * X)^−1 * X^T * y
```{r}
thetaHat <- function(model, y){
  return (solve(t(model) %*% model) %*% t(model) %*% y)
}
```

### Estimate theta hat and y hat for each model

### Model 1
```{r}
model1 = generateModel1(signals_df)
model1_thetaHat = thetaHat(model1, signals_df$x2)
model1_yHat = model1 %*% model1_thetaHat
print("Model 1 θhat")
print(model1_thetaHat[,1])
print("Model 1 Yhat")
print(model1_yHat[1:5,])
```

### Model 2
```{r}
model2 = generateModel2(signals_df)
model2_thetaHat = thetaHat(model2, signals_df$x2)
model2_yHat = model2 %*% model2_thetaHat
print("Model 2 θhat")
print(model2_thetaHat[,1])
print("Model 2 Yhat")
print(model2_yHat[1:5,])
```

### Model 3
```{r}
model3 = generateModel3(signals_df)
model3_thetaHat = thetaHat(model3, signals_df$x2)
model3_yHat = model3 %*% model3_thetaHat
print("Model 3 θhat")
print(model3_thetaHat[,1])
print("Model 3 Yhat")
print(model3_yHat[1:5,])
```

### Model 4
```{r}
model4 = generateModel4(signals_df)
model4_thetaHat = thetaHat(model4, signals_df$x2)
model4_yHat = model4 %*% model4_thetaHat
print("Model 4 θhat")
print(model4_thetaHat[,1])
print("Model 4 Yhat")
print(model4_yHat[1:5,])
```

### Model 5
```{r}
model5 = generateModel5(signals_df)
model5_thetaHat = thetaHat(model5, signals_df$x2)
model5_yHat = model5 %*% model5_thetaHat
print("Model 5 θhat")
print(model5_thetaHat[,1])
print("Model 5 Yhat")
print(model5_yHat[1:5,])
```


## TASK 2.2: Compute the model residual error (error) sum of squared errors (RSS), for every candidate model.

### Function to calculate Residual Sum of Squares  (SSE-Sum of Squares due to error)
 RSS = Σ(y_actual - y_predicted)^2
```{r}
calculateRSS <- function(y, y_hat_model){
  return (sum((y-y_hat_model)^2))
}
```

```{r}
# Compute RSS for each candidate models
model1_RSS = calculateRSS(signals_df$x2, model1_yHat)
model2_RSS = calculateRSS(signals_df$x2, model2_yHat)
model3_RSS = calculateRSS(signals_df$x2, model3_yHat)
model4_RSS = calculateRSS(signals_df$x2, model4_yHat)
model5_RSS = calculateRSS(signals_df$x2, model5_yHat)
# Print the models and their RSS in a table
rsss = c(model1_RSS, model2_RSS, model3_RSS, model4_RSS, model5_RSS)
data.frame(Model=c("Model 1","Model 2", "Model 3", "Model 4","Model 5"), 
           RSS=rsss)
```

## TASK 2.3: Log-likelihood function for every candidate model

### Functions for computing the variance(σ2 = RSS/(N-1)) and log-likelihoods for all candidate models
```{r}
calculateVariance <- function(N, RSS){
  return  (RSS/(N-1))
}

calculateLogLikelihood <- function(N, variance, RSS) {
  return (-(N/2)*(log(2*pi))-(N/2)*(log(variance))-(1/(2*variance))*RSS)
}
# Total Number of rows/sample data
N = length(signals_df$x2) 

# Calculate variances for each model
model1_variance = calculateVariance(N, model1_RSS)
model2_variance = calculateVariance(N, model2_RSS)
model3_variance = calculateVariance(N, model3_RSS)
model4_variance = calculateVariance(N, model4_RSS)
model5_variance = calculateVariance(N, model5_RSS)

# Calculate log-likelihood for each model
model1_likelihood  = calculateLogLikelihood(N, model1_variance, model1_RSS)
model2_likelihood  = calculateLogLikelihood(N, model2_variance, model2_RSS)
model3_likelihood  = calculateLogLikelihood(N, model3_variance, model3_RSS)
model4_likelihood  = calculateLogLikelihood(N, model4_variance, model4_RSS)
model5_likelihood  = calculateLogLikelihood(N, model5_variance, model5_RSS)

# Display the obtained values in a table
variances <- c(model1_variance, model2_variance, model3_variance, model4_variance, model5_variance)
likelihoods <- c(model1_likelihood, model2_likelihood, model3_likelihood, model4_likelihood, model5_likelihood)

data.frame(Model=c("Model 1","Model 2", "Model 3", "Model 4","Model 5"), 
           Variance=variances,
           Likelihood=likelihoods)

```


## TASK 2.4: Compute Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC)

### Calculate the AIC for all candidate models
```{r}
# Calculate Akaike Information Criterion (AIC)
calculateAIC <- function(N, thetahat, likelihood){
  k = length(thetahat)
  return (2 * k-2 * likelihood)
}

model1_AIC = calculateAIC(N, model1_thetaHat, model1_likelihood)
model2_AIC = calculateAIC(N, model2_thetaHat, model2_likelihood)
model3_AIC = calculateAIC(N, model3_thetaHat, model3_likelihood)
model4_AIC = calculateAIC(N, model4_thetaHat, model4_likelihood)
model5_AIC = calculateAIC(N, model5_thetaHat, model5_likelihood)

modelLabels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")
aics = c(model1_AIC, model2_AIC, model3_AIC, model4_AIC, model5_AIC)
data.frame(Models = modelLabels, AIC = aics)
```

###	Calculate BIC for all candidate models
```{r}
calculateBIC <- function(N, thetahat, likelihood){
  k = length(thetahat)
  return (k* log(N) - 2*likelihood)
}

model1_BIC = calculateBIC(N, model1_thetaHat, model1_likelihood)
model2_BIC = calculateBIC(N, model2_thetaHat, model2_likelihood)
model3_BIC = calculateBIC(N, model3_thetaHat, model3_likelihood)
model4_BIC = calculateBIC(N, model4_thetaHat, model4_likelihood)
model5_BIC = calculateBIC(N, model5_thetaHat, model5_likelihood)

bics = c(model1_BIC, model2_BIC, model3_BIC, model4_BIC, model5_BIC)

data.frame(Models = modelLabels, BIC = bics)
```


## TASK 2.5: Distribution of model prediction errors (residuals)

### Calculate model prediction error and plot QQ-plot
```{r}

calculateError <- function(y, yHat){
  return (y - yHat)
}

plotQQ <-function(modelError){
  error_fig = ggplot(data.frame(modelError), aes(sample=modelError)) +
    geom_qq(color = "#0A5EB0" ) +
    geom_qq_line(color = "#FF4545" )
  
  return (
          ggplotly(error_fig)  %>% 
            layout( plot_bgcolor='#F8F4EC', 
                    xaxis = list(title=list(text="Theoritical Quantities",font=list(size=10) )), 
                    yaxis = list(title=list(text="Sample Quantiles",font=list(size=10)))
                                          ) 
          )
}

model1_error = calculateError(signals_df$x2, model1_yHat)
qq1 <- plotQQ(model1_error)

model2_error = calculateError(signals_df$x2, model2_yHat)
qq2 <- plotQQ(model2_error)

model3_error = calculateError(signals_df$x2, model3_yHat)
qq3 <- plotQQ(model3_error)

model4_error = calculateError(signals_df$x2, model4_yHat)
qq4 <- plotQQ(model4_error)

model5_error = calculateError(signals_df$x2, model5_yHat)
qq5 <- plotQQ(model5_error)

plotly::subplot(qq1, qq2,  qq3, qq4,qq5, nrows = 3, shareX = FALSE,  titleX =TRUE, titleY = TRUE  )  %>%
  layout(  plot_bgcolor = '#F8F4EC',
           annotations = list(
             list(x = 0.0, y = 1.0, text = "QQ plot of model 1", showarrow = FALSE, xref = "paper", yref = "paper"),
             list(x = 0.57, y = 1.0, text = "QQ plot of model 2", showarrow = FALSE, xref = "paper", yref = "paper"),
             list(x = 0.0, y = 0.62, text = "QQ plot of model 3", showarrow = FALSE, xref = "paper", yref = "paper"),
             list(x = 0.57, y = 0.62, text = "QQ plot of model 4", showarrow = FALSE, xref = "paper", yref = "paper"),
             list(x = 0.0, y = 0.28, text = "QQ plot of model 5", showarrow = FALSE, xref = "paper", yref = "paper")
           ) 
      )
```


## TASK 2.6: Best Regression Model Selection

#### Table of all RSS, AIC, BIC and Likelihoods for all candidate models
```{r}
data.frame(Model = modelLabels, RSS = rsss, AIC = aics, BIC = bics,  Likelihood = likelihoods)
```

## TASK 2.7: Best Model Evaluation with t-test

### Generate training and testing data and their parameters
```{r}
# Generate training and testing data
set.seed(123)
split_data = initial_split(signals_df, prop = .7)
training_data = training(split_data)
testing_data = testing(split_data)

training_model = generateModel2(training_data)
testing_model = generateModel2(testing_data)

training_thetaHat <- thetaHat(training_model, training_data$x2)

training_yHat = training_model %*% training_thetaHat
testing_yHat = testing_model %*% training_thetaHat

```

### t-test
```{r}
ttest = t.test(training_yHat, testing_yHat, mu = 100, alternative = "two.sided", conf.level=0.95)
ttest
```

```{r}
confidence_interval1 = ttest$conf.int[1] # Lower bound of 95% CI
confidence_interval12 = ttest$conf.int[2] # Upper bound of 95% CI
std_error = ttest$stderr # Standard error of the mean difference
c_i = c(confidence_interval1,confidence_interval12)
print("Confidence Interval")
c_i
print("Standard Error")
std_error


```

### Plotting the 
```{r}



# Plot the density distribution of training and testing data results
training_plot = ggplot(training_data, aes(x = x2)) +   
  stat_density(geom="line", color = "#0A5EB0") + 
  geom_vline(xintercept = confidence_interval1, linetype="dashed", color="#FF4545")+ 
  geom_vline(xintercept = confidence_interval12, linetype="dashed", color="#FF4545")+
  geom_vline(xintercept = std_error, linetype="dashed", color="#424242") +
  labs(x="X2", y="Density")


testing_plot = ggplot(testing_data, aes(x = x2)) +    
  stat_density(geom="line", color = "#0A5EB0") + 
  geom_vline(xintercept = confidence_interval1, linetype="dashed", color="#FF4545")+ 
  geom_vline(xintercept = confidence_interval12, linetype="dashed", color="#FF4545")+
  geom_vline(xintercept = std_error, linetype="dashed", color="#424242") +
  labs(x="X2", y="Density")


subplot(training_plot,  testing_plot, nrows=1, shareX = FALSE,
        titleY = TRUE, titleX = TRUE) %>% 
        layout(plot_bgcolor='#F8F4EC', title="Distributions of Training and Testing Data",
               annotations = list(
                 list(x = 0.25, y = -0.05, text = "Training", showarrow = FALSE, xref = "paper", yref = "paper"),
                 list(x = 0.75, y = -0.05, text = "Testing", showarrow = FALSE, xref = "paper", yref = "paper")
               )
               )
                                                 
  

```



## TASK 3: Approximate Bayesian Computation (ABC)

## TASK 3.1:  Select Parameters for computing Posterior Estimation
```{r}
# Select the top two largest absolute parameters from Model 2
abs_theta <- abs(model2_thetaHat)  
selected_ids <- order(abs_theta, decreasing = TRUE)[1:2] 
theta1_id <- selected_ids[1] 
theta2_id <- selected_ids[2]  

# Sample size to perform the ABC rejection
sample_size <- 1500 
```


## TASK 3.2: Determine uniform range of prior distribution
```{r}
# Generate a series of uniform parameters for the selected coefficients centered around 
# the estimated parameters with a range of +/- 0.5.
theta1_prior <- model2_thetaHat[theta1_id] + runif(sample_size, -0.5, 0.5)  
theta2_prior <- model2_thetaHat[theta2_id] + runif(sample_size, -0.5, 0.5)

prior_df <- data.frame(theta1_prior=theta1_prior, theta2_prior=theta2_prior)
skim(prior_df)
head(prior_df,10)
```


## TASK 3.3: Perform rejection ABC by drawing samples from prior distribution
```{r}
# Empty vectors to store the accepted parameters
theta1_accepted <- c()
theta2_accepted <- c()

# Threshold epsilon which is the minimum value for the RSS to be accepted, with
# k as 1.5, meaning the abc RSS can only be 1.5 times greater than the actual RSS of model2
epsilon_threshold <- 1.5 * sum((signals_df$x2 - model2_yHat)^2)  

# The count of the accepted parameters or posterior
counter <- 0

for (i in 1:sample_size) {
  
  # Construct a new parameter matrix containing the sampled parameters for the prior parameters, 
  # and fixed initial parameters from parameter estimates of model 2
  parameter_matrix <- model2_thetaHat
  parameter_matrix[theta1_id] <- theta1_prior[i]
  parameter_matrix[theta2_id] <- theta2_prior[i]
  
  # Compute outputs using the parameters from prior distribution
  abc_yHat <- model2 %*% parameter_matrix
  
  # Compute RSS 
  abc_RSS <- sum((signals_df$x2 - abc_yHat)^2)
  
  # Accept/reject the computed yHat based on whether it is less than the threshold value or epsilon
  if (abc_RSS < epsilon_threshold) {
    # store the accepted prior parameters to the posterior parameter 
    theta1_accepted <- c(theta1_accepted, theta1_prior[i])
    theta2_accepted <- c(theta2_accepted, theta2_prior[i])
    counter <- counter + 1 # increase the posterior counter
  }
}


# Store the accepted parameters by combining them into a dataframe
posterior_df <- data.frame(theta1 = theta1_accepted, theta2 = theta2_accepted)

skim(posterior_df)
head(posterior_df, 10)



```



## TASK 3.4: Plot the joint and marginal posterior distribution
```{r}

# Joint Posterior Distribution (Scatter Plot)
joint_plot <- ggplot(posterior_df, aes(x = theta1, y = theta2)) +
  geom_point(alpha = 0.5, color = "#7E5CAD") +
  labs(title = "Joint Posterior Distribution", x = "Theta1", y = "Theta2") +
  theme_minimal()
joint_plot


# Marginal Posterior for Theta1
marginal_theta1 <- ggplot(posterior_df, aes(x = theta1)) +
  geom_histogram(bins = 30, fill = "#5A72A0", alpha = 0.7) +
  labs(title = "Marginal Posterior for Theta1", x = "Theta1", y = "Density") +
  theme_minimal()

# Marginal Posterior for Theta2
marginal_theta2 <- ggplot(posterior_df, aes(x = theta2)) +
  geom_histogram(bins = 30, fill = "#FF4545", alpha = 0.7) +
  labs(title = "Marginal Posterior for Theta2", x = "Theta2", y = "Density") +
  theme_minimal()



# Combine Plots
combined_plot <- (marginal_theta1 + marginal_theta2)
combined_plot <- combined_plot + plot_annotation(title = "ABC Posterior and Data Distributions")
combined_plot


# Distribution of Data (X2)
data_distribution <- ggplot(signals_df, aes(x = x2)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "#5A72A0", alpha = 0.8) +
  geom_density(color = "#FF4545", linetype = "dashed", lwd=1.5) +
  labs(title = "Distribution of X2 (Output Data)", x = "X2", y = "Density") +
  theme_minimal()

data_distribution

```

