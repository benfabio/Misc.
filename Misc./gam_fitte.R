
### gam_fitter.R
### 03/04/2025 - ©Fabio Benedetti (Plant Ecology group, IPS, Uni Bern)
###              with input from ChatGPT3 and advice from Philipp Brun (WSL)

### ----------------------------------------------------------------------------------------------------------------

# Load necessary libraries
install.packages("mgcv")
library("mgcv") # For fitting GAMs in R
library("tidyverse")
library("ggpubr") # To make panels of ggplots
library("gratia") # Alternative package for plotting GAM outputs - not mandatory

### ----------------------------------------------------------------------------------------------------------------

### Showing how to fit a bivariate Generalized Additive Modle (GAM) with interaction
### terms between temperature and precipitation in order to model the niche of a
### theoretical plant species (species distribution modelling) 

### First, read this: https://noamross.github.io/gams-in-r-course/ 

### Main steps: 
# - Generate environmental gradients (temperature & precipitation).
# - Define a species response function (in the present example:
#   higher presence probability in warm & dry conditions).
# - Sample presence/absence using a probability function.
# - Fit a GAM to model the niche
# - Show various options to visualize the modelled niche
# - Show how to evaluate the GAM and assess if it's overfitting

## Step 1: Generate environmental data
set.seed(123) # for reproducibility #RClub2025
n <- 500  # Number of observations
temperature <- rnorm(n, mean = 20, sd = 5)  # Warmer mean temperature
precipitation <- rnorm(n, mean = 80, sd = 30)  # Lower mean precipitation

## Step 2: Define species' niche preference (higher in warm & dry areas)
probability <- plogis(2 + 0.1 * temperature - 0.05 * precipitation) # Sigmoid function, more presences in warm/dry areas

# NOTE: Change the code here to simulate other types of niches
# (i.e., higher in cold and wet conditions, or higher in warm and wet conditions,
#  or no preferences at all)

## Step 3: Simulate presence/absence data
species_presence <- rbinom(n, 1, probability)  # Bernoulli trial with probability

# Combine into a dataframe
data <- data.frame(temperature, precipitation, species_presence)
# Examine: 
# summary(data)
# head(data); tail(data)

# Quick ggplot to compare the position of presences (1) and absences (0) in env space
p1 <- ggplot(data = data, aes(x = factor(species_presence), y = temperature, fill = factor(species_presence))) +
    geom_violin(colour = "black") + geom_boxplot(fill = "white", colour = "black", width = .2) +
    scale_fill_manual(values = c("#80cdc1","#dfc27d")) + ylab("Temperature") + xlab("") + 
    theme_bw() + guides(fill = "none")

p2 <- ggplot(data = data, aes(x = factor(species_presence), y = precipitation, fill = factor(species_presence))) +
    geom_violin(colour = "black") + geom_boxplot(fill = "white", colour = "black", width = .2) +
    scale_fill_manual(values = c("#80cdc1","#dfc27d")) + ylab("Precipitation") + xlab("") + 
    theme_bw() + guides(fill = "none")
# Arrange the two plots in a panel
ggarrange(p1,p2, align = "hv", nrow = 1, ncol = 2)
### As you can see, virtual species prefers warmer tempertaures and drier conditions

## Step 4: Fit a GAM with tensor product smooth for niche modeling
# ?gam to chekc what the tensor (te()) does
# te(temperature, precipitation): This creates a smooth interaction between temperature and precipitation.
# family = binomial: Assumes a logistic response (appropriate for SDMs based on occurrences)
gam_model <- gam(species_presence ~ te(temperature, precipitation), family = binomial, data = data)

# Check model output
summary(gam_model)
# Skill of the model is given by the deviance explained (0-100%)
# (Here deviance explained is 28.3% - which is reasonable for SDMs)
# (< 25% would be bad, > 50% is good, > 75% is very good, > 85% is excellent, > 95% is suspicious = overfitting likely)

## Visualize the niche modelled by the basic model
# Create a grid for prediction
temp_seq <- seq(min(data$temperature), max(data$temperature), length.out = 100)
precip_seq <- seq(min(data$precipitation), max(data$precipitation), length.out = 100)
new_data <- expand.grid(temperature = temp_seq, precipitation = precip_seq)
# Predict species probability across niche space
new_data$prediction <- predict(gam_model, new_data, type = "response")
# Contour plot of species niche
ggplot(new_data, aes(x = temperature, y = precipitation, fill = prediction)) +
  geom_raster() +  # Heatmap-style visualization
  geom_contour(aes(z = prediction), color = "black") +  # Contour lines
  scale_fill_viridis_c(name = "Probability") +  # Color scale
  labs(title = "Virtual Species Niche", x = "Temperature (°C)", y = "Precipitation (mm)") +
  theme_minimal()
# C'est beau non?
### This is the lot Philipp was referring to when we discussed.

## If you want a nice 3D version of it: vis.gam()
?vis.gam
vis.gam(gam_model_select,
        view = c("temperature", "precipitation"), 
        type = "response", theta = 35,
        ticktype = "detailed", color = 'heat'
) # vis.gam

## If you want a specialized GAM plotting tool: 'gratia' package
# Plot smooths separately
# draw(gam_model_select, residuals = TRUE)
# je trouve ca bof perso


## Step 5: Use gam.check() for diagnostics
?gam.check
gam.check(gam_model) # Check the smoothness and model fit


## Step 6: Use 'select = TRUE' in gam() to automatically optimize smoothness selection
# This option automatically penalizes excessive smoothness (if the model overfits)
gam_model_select <- gam(species_presence ~ te(temperature, precipitation), 
                        family = binomial, 
                        select = TRUE,
                        data = data)

## Use gam.check() again on "gam_model_select"
gam.check(gam_model_select)  # Check smoothness and residuals for the selected model

# Visualize the niche modeled by the new GAM
temp_seq <- seq(min(data$temperature), max(data$temperature), length.out = 100)
precip_seq <- seq(min(data$precipitation), max(data$precipitation), length.out = 100)
new_data <- expand.grid(temperature = temp_seq, precipitation = precip_seq)
# Predict species probability across niche space
new_data$prediction <- predict(gam_model_select, new_data, type = "response")
# Contour plot of species niche
ggplot(new_data, aes(x = temperature, y = precipitation, fill = prediction)) +
  geom_raster() +  # Heatmap-style visualization
  geom_contour(aes(z = prediction), color = "black") +  # Contour lines
  scale_fill_viridis_c(name = "Probability") +  # Color scale
  labs(title = "Virtual Species Niche", x = "Temperature (°C)", y = "Precipitation (mm)") +
  theme_minimal()

## In this example, the difference between the two GAM is minimal to be honest. 
## But, when you run gam.check(gam_model) & gam.check(gam_model_select), check for:
# p-values for the smooth terms: Ensure they are significant
# Residual plots: Check for any non-random patterns.
# Smoothness selection: If the plot looks overly wiggly, it may indicate overfitting, which can be adjusted by increasing the smoothness penalty.

## You can also use AIC/BIC to compare models:
AIC(gam_model, gam_model_select) # very little differences
# (truly better models should have really lower AIC, here they are the same)
BIC(gam_model, gam_model_select) # very little differences too.



### Ensuring that your GAM is not overfitting the data is crucial for making valid inferences and predictions. Overfitting happens when the model learns the noise in the data instead of the true underlying pattern, leading to poor generalization to new data.
### How to make sure the GAM is not overfitting?

## One common source of overfitting in GAMs is choosing too many basis functions (value of 'k' too large), which can lead to overly flexible models. The select = TRUE argument (already done above) in mgcv automatically penalizes excessive smoothness and selects the optimal complexity for each term.

## Use gam.check() for diagnostics (see above again). What to look for with gam.check():
# Deviance residuals: Ensure residuals do not exhibit any clear patterns.
# The smooth terms' wiggliness: If smooth terms appear overly wiggly, it might indicate overfitting. You can adjust the smoothness penalty by decreasing k or increasing the sp argument (smoothing parameter).
# Effective degrees of freedom: If the degrees of freedom of a smooth term are very high, it could indicate overfitting!

## Use AIC/BIC as indicators: Lower AIC/BIC values suggest better models = less risk of overfitting.
## (I don't fully agree with this though)

## Plot the smooth terms! After fitting the model, you can plot the smooth terms to inspect if they are overly complex and potentially overfitting the data.
plot(gam_model_select, shade = TRUE)
# If the smooth terms are very wiggly (not the case at all here!), it could indicate that your model is overfitting.
# The smoother the plots, the better. They show more general trends rather than noise...

## Check for Overfitting with the Residuals
# Plot the residuals to see if the model is overfitting the data. Ideally, the residuals should show random noise with no apparent patterns.
plot(resid(gam_model), main = "Residuals")
# If your residuals are showing strong groupings/patterns, this indicates either overfitting or that the model has not captured the real pattern in the data (it learnt spurious relationships instead).


### ----------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------