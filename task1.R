###############################################################################
#------------------------------------------------------------------------------
# TASK-1    (Preliminary Data Analysis)
# ---------> INITIAL EXPLORATORY DATA ANALYSIS (EDA)
#------------------------------------------------------------------------------
###############################################################################

# Import libraries
library(ggplot2)
library(skimr)   # Enhanced summary of dataset using skim(df)
library(plotly)  # Creates interactive plots and visualizations.

# Import Data Files

# Brain Signals Data (X1, X2(output), X3,X4, X5
signals_df <- read.csv("./data.csv") 

# Time Progression Data (By removing the default header, and removing first row)
time_df <- read.csv("./time.csv", header = FALSE, skip = 1)

# Rename the first column to `time`
colnames(time_df)<-c("time")

# Merge the `time` column to `signals_df`
signals_df <- cbind(time_df, signals_df)

#################################################
# Display Summary Statistics of the signals data
#------------------------------------------------
# head and tail of the data
head(signals_df, 5)
tail(signals_df, 5)
View(signals_df)  # All data

# summary (simple and enhanced)
summary(signals_df)
skim(signals_df)

# check for missing data
sum(is.na(signals_df))



###############################################################################
#------------------------------------------------------------------------------
# 1.1 Time series plots (of input and output signal)
#------------------------------------------------------------------------------
###############################################################################

# View input signals and time
View(signals_df[, c("time","x1","x3","x4","x5")]) 

# Generates a series of line plots for all input variables (x1,x3,x4,x5) against 
# time
plot_ly(signals_df, x = ~time, y = ~x1, type = 'scatter', mode = 'lines', 
        name = 'x1') %>%
  add_trace(y = ~x3, name = 'x3') %>%
  add_trace(y = ~x4, name = 'x4') %>%
  add_trace(y = ~x5, name = 'x5') %>%
  layout(title = "Time Series of Input Signals(X)",
         xaxis = list(title = "Time (Seconds)"),
         yaxis = list(title = "Signal Value"))



# Generate individual line plots for all inputs
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


# View out signal and time
View(signals_df[, c("time","x2")]) 

# Generate and plot the output(x2) against time
x2_time <- plot_ly(signals_df, x = ~time, y = ~x2, type = 'scatter', mode = 'lines', name = 'x2') %>%
  layout(title = "Time Series of output signal (X2) with time",
         xaxis = list(title = "Time (Seconds)",zerolinewidth = 2,gridcolor = '#ffff'),
         yaxis = list(title = "X2 (Output Signal)",zerolinewidth = 2,gridcolor = '#ffff'),
         plot_bgcolor="#BCF2F6", paper_bgcolor="#fff", colorway="#334756"
  )
x2_time

###############################################################################
#------------------------------------------------------------------------------
# 1.2 Distribution for each signal (time-series) 
#------------------------------------------------------------------------------
###############################################################################
x1_distribution = ggplot(signals_df, aes(x = x1)) +    
  geom_histogram(aes(y = after_stat(density) ), bins=25,fill = "#CC2B52") +
  stat_density(geom="line", color="#424242", linewidth=.8) +
  geom_rug() +
  labs(x="X1", y="Density")

x2_distribution = ggplot(signals_df, aes(x = x2)) +    
  geom_histogram(aes(y = after_stat(density) ), bins=10,fill = "#5A72A0") + 
  stat_density(geom="line", color="#1A2130", linewidth=.8) +
  geom_rug()

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

# Input Signals Distribution
x1_distribution
x3_distribution
x4_distribution
x5_distribution



# Output Signal Distribution
ggplotly(x2_distribution) %>% layout(title="Distribution of Output(X2) Signal",
                                     xaxis = list(title="X2 Signal (Output)"),
                                     yaxis = list(title="Density"))

# Plot all distributions at once
plotly::subplot(x1_distribution,  x3_distribution,x4_distribution, x5_distribution, nrows=2, shareX = FALSE,
                titleY = TRUE, titleX = TRUE) %>% layout(plot_bgcolor='#F8F4EC', title="Distribution of X1, X3, X4, and X5")




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




###############################################################################
#------------------------------------------------------------------------------
# 1.3 Correlation and scatter plots (between different combination of input and 
# output signals) to examine their dependencies
#------------------------------------------------------------------------------
###############################################################################


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

x1_correlation
x3_correlation
x4_correlation
x5_correlation


plotly::subplot(x1_correlation,  x3_correlation,x4_correlation, x5_correlation, nrows=2, 
                shareX = FALSE,
                titleY = TRUE, titleX = TRUE) %>% layout(plot_bgcolor='#F8F4EC',
                                                         title="Correlation of Input Signals (X1,X3,X4,X5) with output signal (X2)")




plot_ly(signals_df) %>%
  add_trace( x=~x1, y=~x2,type = "scatter",mode = "markers", name = "X1") %>%  
  add_trace( x=~x3, y=~x2,type = "scatter",mode = "markers", name = "X3") %>%  
  add_trace( x=~x4, y=~x2, type = "scatter",mode = "markers", name = "X4") %>%  
  add_trace( x=~x5, y=~x2,type = "scatter",mode = "markers", name = "X5")  %>% 
  layout(plot_bgcolor='#F8F4EC', title="Corelation of X Signals(Input) with X2 Signal (Output).", 
         xaxis = list(title="X1,X3,X4,X5 Signals (Input)"), yaxis = list(title="X2 Signal (Output)"))



ggpairs(signals_df)

