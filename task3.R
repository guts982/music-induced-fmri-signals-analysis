###############################################################################
#------------------------------------------------------------------------------
# TASK-3  Approximate Bayesian Computation (ABC)
#------------------------------------------------------------------------------
###############################################################################

library(patchwork)  #combines plots

# Import task2 for necessary scripts
source("task2.R")

###############################################################################
#------------------------------------------------------------------------------
# 3.1 Select Parameters for computing Posterior Estimation
#------------------------------------------------------------------------------
###############################################################################

# Select the top two largest absolute parameters from Model 2
abs_theta <- abs(model2_thetaHat)  
selected_ids <- order(abs_theta, decreasing = TRUE)[1:2] 
theta1_id <- selected_ids[1] 
theta2_id <- selected_ids[2]  

# Sample size to perform the ABC rejection
sample_size <- 1500 


###############################################################################
#------------------------------------------------------------------------------
# 3.2 Determine uniform range of prior distribution
#------------------------------------------------------------------------------
###############################################################################


# Generate a series of uniform parameters for the selected coefficients centered around 
# the estimated parameters with a range of +/- 0.5.
theta1_prior <- model2_thetaHat[theta1_id] + runif(sample_size, -0.5, 0.5)  
theta2_prior <- model2_thetaHat[theta2_id] + runif(sample_size, -0.5, 0.5)



###############################################################################
#------------------------------------------------------------------------------
# 3.3 Perform rejection ABC by drawing samples from prior distribution
#------------------------------------------------------------------------------
###############################################################################


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
head(posterior_df)




###############################################################################
#------------------------------------------------------------------------------
# 3.3 Plot the joint and marginal posterior distribution
#------------------------------------------------------------------------------
###############################################################################


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



# combined_plot <- (joint_plot / (marginal_theta1 + marginal_theta2)) / data_distribution



