###############################################################################
#------------------------------------------------------------------------------
# TASK-2   (Regression)
# ---------> MODELLING THE RELATIONSHIPS BETWEEN SIGNALS
#------------------------------------------------------------------------------
###############################################################################

# Import library
library(rsample)  # Spliting data into training and testing data

# Import Data Files

# Brain Signals Data (X1, X2(output), X3,X4, X5
signals_df <- read.csv("./data.csv") 

# Time Progression Data (By removing the default header, and removing first row)
time_df <- read.csv("./time.csv", header = FALSE, skip = 1)

# Rename the first column to `time`
colnames(time_df)<-c("time")

# Merge the `time` column to `signals_df`
signals_df <- cbind(time_df, signals_df)

# Total rows
total_rows = nrow(signals_df)


###############################################################################
#------------------------------------------------------------------------------
# 2.1 Estimate model parameters θ = {θ1,θ2,...,θbias}^T for every candidate 
# model using Least Squares (θhat = (X^T*X)^(-1) * X^T * y  ), using the 
# provided input and output signals datasets. (Use all the data for training.)
#------------------------------------------------------------------------------
###############################################################################



# Theta estimator θ = (X^T * X)^−1 * X^T * y
thetaHat <- function(model, y){
  return (solve(t(model) %*% model) %*% t(model) %*% y)
}

# Generate a column vector of ones with a size equal to the number of rows 
# in signals_df to add bias term (θbias) to the regression models, representing
# constant part of the equation
# onesMatrix <- matrix(1, total_rows, 1)

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


# Estimate theta hat and y hat for each model
model1 = generateModel1(signals_df)
model1_thetaHat = thetaHat(model1, signals_df$x2)
model1_yHat = model1 %*% model1_thetaHat

model2 = generateModel2(signals_df)
model2_thetaHat = thetaHat(model2, signals_df$x2)
model2_yHat = model2 %*% model2_thetaHat

model3 = generateModel3(signals_df)
model3_thetaHat = thetaHat(model3, signals_df$x2)
model3_yHat = model3 %*% model3_thetaHat

model4 = generateModel4(signals_df)
model4_thetaHat = thetaHat(model4, signals_df$x2)
model4_yHat = model4 %*% model4_thetaHat

model5 = generateModel5(signals_df)
model5_thetaHat = thetaHat(model5, signals_df$x2)
model5_yHat = model5 %*% model5_thetaHat


###############################################################################
#------------------------------------------------------------------------------
# 2.2 Compute the Model Residual Sum of Squares (RSS)
#------------------------------------------------------------------------------
###############################################################################

# Residual Sum of Squares  (SSE-Sum of Squares due to error)
calculateRSS <- function(y, y_hat_model){
  return (sum((y-y_hat_model)^2))
}

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

###############################################################################
#------------------------------------------------------------------------------
# 2.3 Compute the log-likelihood function for each candidate
#------------------------------------------------------------------------------
###############################################################################

# Compute the variance σ2 = RSS/(N-1)
calculateVariance <- function(N, RSS){
  return  (RSS/(N-1))
}

# Compute the log-likelihood for a model
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

variances <- c(model1_variance, model2_variance, model3_variance, model4_variance, model5_variance)
likelihoods <- c(model1_likelihood, model2_likelihood, model3_likelihood, model4_likelihood, model5_likelihood)

data.frame(Model=c("Model 1","Model 2", "Model 3", "Model 4","Model 5"), 
           Variance=variances,
           Likelihood=likelihoods)

###############################################################################
#------------------------------------------------------------------------------
# 2.4 Compute Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC)
#------------------------------------------------------------------------------
###############################################################################

# Calculate Akaike Information Criterion (AIC)
calculateAIC <- function(N, thetahat, likelihood){
  k = length(thetahat)
  return (2 * k-2 * likelihood)
}


# Print AICS
data.frame(Models = modelLabels, AIC = aics)


model1_AIC = calculateAIC(N, model1_thetaHat, model1_likelihood)
model2_AIC = calculateAIC(N, model2_thetaHat, model2_likelihood)
model3_AIC = calculateAIC(N, model3_thetaHat, model3_likelihood)
model4_AIC = calculateAIC(N, model4_thetaHat, model4_likelihood)
model5_AIC = calculateAIC(N, model5_thetaHat, model5_likelihood)

modelLabels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")
aics = c(model1_AIC, model2_AIC, model3_AIC, model4_AIC, model5_AIC)


#	Calculate Bayesian Information Criterion (BIC)
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

# Print BICs
data.frame(Models = modelLabels, BIC = bics)


# data.frame(Models = modelLabels, Variance = variances, Likelihood = likelihoods)


###############################################################################
#------------------------------------------------------------------------------
# 2.5  Distribution of model prediction errors (residuals)
#------------------------------------------------------------------------------
###############################################################################

# Calculate model prediction error
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



###############################################################################
#------------------------------------------------------------------------------
# 2.6 Best Regression Model Selection
#------------------------------------------------------------------------------
###############################################################################

# Table of all RSS, AIC, BIC and Likelihoods for all candidate models
data.frame(Model = modelLabels, RSS = rsss, AIC = aics, BIC = bics,  Likelihood = likelihoods)
# Model 2 is the best model as AIC, BIC and RSS is the lowest for this model, and also 
# the qq-plot satisfies normally distribution


###############################################################################
#------------------------------------------------------------------------------
# 2.7 Best Model Evaluation with t-test
#------------------------------------------------------------------------------
###############################################################################


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

# testing_yHat
# mean(training_yHat)-mean(testing_yHat) # diff mean = -0.4354867


ttest = t.test(training_yHat, testing_yHat, mu = 100, alternative = "two.sided", conf.level=0.95)
ttest


confidence_interval1 = ttest$conf.int[1] # Lower bound of 95% CI
confidence_interval12 = ttest$conf.int[2] # Upper bound of 95% CI
std_error = ttest$stderr # Standard error of the mean difference


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
                                                 
  


thetaHatTraining = thetaHat(training_model, training_data$x2)

dis_test=density(training_data$x2)

testing_data_distribution = ggplot(training_data, aes(x = x2)) +    
  geom_histogram(aes(y = after_stat(density) ), bins=10,fill = "#5A72A0") +
  stat_density(geom="line", color="#1A2130", linewidth=.8) +
  geom_rug()
ggplotly(testing_data_distribution) %>% layout(title="Distribution of Testing Data",
                                     xaxis = list(title="X2 Signal (Output)"),
                                     yaxis = list(title="Density"))


# z_95 <- qnorm(1 - 0.05 / 2)  # 95% confidence level
# z_95






