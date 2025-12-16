# Your task is to determine what factors are the key drivers of body mass in your data, 
# whilst accounting for any factors which may systematically bias your results.

# reset environment
rm(list=ls())

# load libraries
library(tidyverse)
library(performance)
library(Hmisc)
library(ggpubr)
library(rstatix)
library(fitdistrplus)
library(MASS)
library(see)
library(TMB)
library(glmmTMB)
library(DHARMa)
library(data.table)
library(caret)
library(modelbased)

#####
# 1. IMPORT DATA
##### 

# read in the full dataset (as a tibble)
df <- read_tsv("data/mass_sj19031.tsv")

# check class (= data.frame/tibble)
class(df)

# describe the data for an overview
describe(df)
### can see some missing data/NAs, as well as typos.
### 3 typos in lion_presence - asbent, preesent, preseet



#####
# 2. TIDY DATA
#####

# 2.1 OUTLIERS
# histogram of entire data mass to view distribution/check potential outliers
mass_hist <- df %>%
  ggplot(aes(x = body_mass)) +
  # add histogram with bin width at 250kg
  geom_histogram(fill = "#9EB9F3", color = "black", binwidth = 250) +
  theme_minimal() +
  labs(title = "Histogram of giraffe body mass across original dataset") +
  xlab("Body mass (kg)") +
  ylab("Count")
mass_hist

# boxplot of entire dataset mass to check potential outliers
mass_box <- df %>%
  ggplot(aes(x = "", y = body_mass)) +
  geom_boxplot(width = 0.25, fill = "coral1") +
  theme_lucid() +
  labs(title = "Box plot of giraffe mass across original dataset") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("Body mass (kg)")
mass_box

### box plot dots are potential outliers identified by R using IQR criterion (see ref link)
### identified 5 potential outliers in giraffe mass

# using identify_outliers from rstatix to check outliers and extreme values in dataset using IQR criteria
outlier_df <- identify_outliers(data = df,
                  variable = "body_mass")

# need to sort each env_var into its own column name with values such that each row is an individual observation

# 2.2 NA REMOVE AND FIX TYPOS
# to ensure consistent data structure across all, remove N/A variables
df_clean <- df %>%
  
  # remove outliers identified earlier using anti_join() - justification is largest giraffes rarely exceed 2000kg, and outliers statistically:
  anti_join(outlier_df)  %>%
  
  # go from long format to wide format for env_var so each row is an individual observation - will use tidyverse pivot_wider() function:
  pivot_wider(names_from = env_var, values_from = env_value) %>%

  # remove NA values identified previously in env_value, but from the new 'wide columns'
  drop_na(landscape, vegetation, lion_presence, distance_to_water_km, average_annual_rainfall_mm, predator_density_per_sq_km) 


# check cleaned data 
describe(df_clean)
  

# fix typos 
df_clean$lion_presence[df_clean$lion_presence == "Preesnt"] <- "Present" 
df_clean$lion_presence[df_clean$lion_presence == "Preseet"] <- "Present"
df_clean$lion_presence[df_clean$lion_presence == "Asbent"] <- "Absent"

# describe to check changes
describe(df_clean)





### 3. VISUALISE

# 3.1 SUMMARIES
# mean mass across different parks
mass_parks <- df_clean %>%
  group_by(park) %>%
  summarise(mean_mass = mean(body_mass))
mass_parks

# mean mass across different landscape, vegetation, and lion presence
mass_lvl <- df_clean %>%
  group_by(landscape, lion_presence, vegetation, observer) %>%
  summarise(mean_mass = mean(body_mass)) %>%
  arrange(desc(mean_mass))
mass_lvl

# violin for mass across parks
plot_mass_parks <- df_clean %>%
  
  ggplot(mapping = aes(x = park, y = body_mass)) +
  
  geom_violin(aes(fill = park)) + 
  
  # aesthetics
  geom_boxplot(width = 0.1, col = "black")+
  ggtitle("Violin plot of giraffe mass across the parks using cleaned data") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Body mass (kg)") + 
  xlab("Park") 

plot_mass_parks

#  3.2 SCATTER PLOTS
# distance to water vs body mas scatter plot
plot_water <- df_clean %>%
  ggplot(aes(x=log(distance_to_water_km+ 1), y=body_mass)) +
  geom_point(col="coral1") +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y~x) +
  xlab("log Distance to water") +
  ylab("Body mass (kg)") +
  facet_wrap(. ~ park) +
  theme_lucid() 
plot_water

# average rainfall vs body mass scatter plot
plot_rain <- df_clean %>%
  ggplot(aes(x=average_annual_rainfall_mm, y=body_mass)) +
  geom_point(col="#9EB9F3") +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y~x) +
  xlab("Average annual rainfall (mm)") +
  ylab("Body mass (kg)") +
  facet_wrap(. ~ park) +
  theme_lucid() 
plot_rain

# predator density vs body mass scatter plot
plot_predator <- df_clean %>%
  ggplot(aes(x=log(predator_density_per_sq_km), y=body_mass)) +
  geom_point(col="#F89C74") +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y~x) +
  xlab("log Predator density per square kilometre") +
  ylab("Body mass (kg)") +
  facet_wrap(. ~ park) +
  theme_lucid() 
plot_predator


# qq plot for all mass
qqplot_mass <- df_clean %>%
  ggqqplot(x = "body_mass")
qqplot_mass


# want to try cox-bos to estimate best trsnaformation off linear model to approximte normal:
lm_body_mass  <- lm(body_mass ~ 1, df_clean)

# calculate log likelihood from -2 to 2
boxcox(lm_body_mass, lambda = seq(-2, 2, 0.1))
### best appears to be ~0.25 which is between sqrt(0.5) and cube(0.33)



# 3.3 FAMILY CHECKING
# testing some families against the continuous response variable for error structure
plot(fitdist(df_clean$body_mass, "norm"))
plot(fitdist(df_clean$body_mass, "gamma"))
plot(fitdist(df_clean$log_body_mass, "norm"))
plot(fitdist(df_clean$cube_body_mass, "norm"))
### definitely appears to fit gamma slightly better


# 3.4 GLMM models
# response variable = body mass
# predictors:
# fixed effect (categorical) = landscape, vegetation, lion presence
# fixed effects (continuous) = distance to water, rainfall, and predator density
# groups/grouping factors:
# = random effect = park, observer
# therefore will use a GLMM

# random slopes vs intercepts - which predictor gets which?
# random slope = only if slope of predictor is expected to differ across groups
# some random slopes to consider:
### 1. distance to water (predictor) may vary across parks (groups)
### 2. predator density may vary across parks
### 3. average annual rainfall across parks



# GAUSSIAN GLMM
# glmm using glmmTMB
glmm_gauss <- glmmTMB(
  
  # fixed effects
  body_mass ~ landscape +
    vegetation +
    lion_presence +
    scaled_water_distance +
    scaled_rainfall +
    scaled_predators + 
    
    # park as random effect - each gets own intercept
    (1 | park) +
    # observer as random effect - each gets own intercept
    (1 | observer),
  
  data = df_clean,
  
  # using gaussian family
  family = gaussian()
)

glmm_gamma <- glmmTMB(
  
  body_mass ~ landscape +
    vegetation +
    lion_presence +
    distance_to_water_km +
    average_annual_rainfall_mm +
    predator_density_per_sq_km + 
    
    # park as random effect - each gets own intercept
    (1 | park) +
    # observer as random effect - each gets own intercept
    (1 | observer),
  
  data = df_clean,
  family = Gamma(link = "log")
)


# 3.4.1 check model summaries
summary(glmm_gauss)
summary(glmm_gamma)



# 3.4.2 Dharma model check
glmm_gauss_check <- simulateResiduals(fittedModel = glmm_gauss)
plot(glmm_gauss_check)

glmm_gamma_check <- simulateResiduals(fittedModel = glmm_gamma)
plot(glmm_gamma_check)





# 3.4.3 test for over/under dispersion in glmm
testDispersion(glmm_gauss)
testDispersion(glmm_gamma)
### dispersion ~1 for all = residuals variance matches expected variance
### therefore all models pass this dispersion test



### 3.4.5 both models appear to fit well 
### will compare each head to head
AIC(glmm_gamma, glmm_gauss)
BIC(glmm_gamma, glmm_gauss)
### glmm_gauss is rank deficient model, and gets NA. some predictors may be perfectly colinear in this model



# 3.4.6 test which predictors may be perfectly colinear in gauss model
matrix_gauss <- model.matrix(glmm_gauss)
findLinearCombos(matrix_gauss)
### no perfectly colinear predictors found. rank deficiency warning due to something else



# 3.4.7 performance package checks
# multicolinearity testing
#mcol_gauss <- check_collinearity(glmm_gauss)
#plot(mcol_gauss)
check_collinearity(glmm_gamma)
# check_collinearity(glmm_gauss)

mcol_gamma <- check_collinearity(glmm_gamma)
plot(mcol_gamma)

check_overdispersion(glmm_gamma)
check_overdispersion(glmm_gauss)

# check model
check_model(glmm_gamma)
# check_model(glmm_gauss)



# 3.4.8 GAMMA fitted vs observed
fit_model_gamma <- data.frame(
  "Predicted" = predict(glmm_gamma, type = "response"),
  "Observed" = df_clean$body_mass
)

# plot predicted vs observed with 1:1 line
ggplot(fit_model_gamma) +
  # each point (x) = observed, (y) = model prediction
  geom_point(aes(x = Observed, y = Predicted)) +
  # add linear regression of observed:fitted
  geom_smooth(
    aes(x = Observed, y = Predicted, colour = "gamma"),
    method = lm,
    se = FALSE) +
  # add 1:1 linear regression to plot to check main regression
  geom_smooth(aes(x = Observed, y = Observed, colour = "observed"), method = "lm", se = FALSE, linetype = "dashed") +
  
  scale_colour_manual(name = NULL,
                      breaks = c("observed", "gamma"),
                      labels = c("Observed", "Gamma Model"),
                      values = c("black", "#1ABC9C")) +
  
  xlab("Body mass (observed)") +
  ylab("Body mass (predicted)") +
  
  ggtitle("Predicted vs. Observed for Gamma Model against df_clean") +
  theme_lucid()

# 3.4.8 GAUSS fitted vs observed
fit_model_gauss <- data.frame(
  "Predicted" = predict(glmm_gauss, type = "response"),
  "Observed" = df_clean$body_mass
)

# plot predicted vs observed with 1:1 line
ggplot(fit_model_gauss) +
  # each point (x) = observed, (y) = model prediction
  geom_point(aes(x = Observed, y = Predicted)) +
  # add linear regression of observed:fitted
  geom_smooth(
    aes(x = Observed, y = Predicted, colour = "gauss"),
    method = lm,
    se = FALSE) +
  # add 1:1 linear regression to plot to check main regression
  geom_smooth(aes(x = Observed, y = Observed, colour = "observed"), method = "lm", se = FALSE, linetype = "dashed") +
  
  scale_colour_manual(name = NULL,
                      breaks = c("observed", "gauss"),
                      labels = c("Observed", "Gauss Model"),
                      values = c("black", "#154360")) +
  
  xlab("Body mass (observed)") +
  ylab("Body mass (predicted)") +
  
  ggtitle("Predicted vs. Observed for Gauss Model against df_clean") +
  theme_lucid()
  
### clearly not a 1:1 regression, but not too poor at the moment



# 3.4.9 model predictions with modelbased - compare models visually
pred_gamma <- estimate_expectation(glmm_gamma)
pred_gauss <- estimate_expectation(glmm_gauss)

# create df of predicted values based off models' body mass
models_predictions <- tibble(
  observed = df_clean$body_mass,
  predicted_gamma = pred_gamma$Predicted,
  predicted_gauss = pred_gauss$Predicted
)







# GAUSSIAN GLMM 2
# using log of body mass
glmm_gauss2 <- glmmTMB(
  
    log(body_mass) ~ landscape +
    vegetation +
    lion_presence +
    distance_to_water_km +
    average_annual_rainfall_mm +
    predator_density_per_sq_km + 
    
    # park as random effect - each gets own intercept
    (1 | park) +
    # observer as random effect - each gets own intercept
    (1 | observer),
  
  data = df_clean,
  family = gaussian()
)


#  check model summaries
summary(glmm_gauss2)



#  Dharma model check
glmm_gauss2_check <- simulateResiduals(fittedModel = glmm_gauss2)
plot(glmm_gauss2_check)




#  test for over/under dispersion in glmm
testDispersion(glmm_gauss2)
### dispersion ~1 for all = residuals variance matches expected variance
### therefore all models pass this dispersion test



###  both models appear to fit well 
### will compare each head to head
AIC(glmm_gamma, glmm_gauss2)
BIC(glmm_gamma, glmm_gauss2)
### glmm_gauss2 scores significantly better on AIC and BIC -> model going forward


#  performance package checks
# multicolinearity testing
#mcol_gauss <- check_collinearity(glmm_gauss)
#plot(mcol_gauss)
check_collinearity(glmm_gauss2)

check_overdispersion(glmm_gauss2)

check_model(glmm_gauss2)



# 3.4.8 fitted vs observed
fit_model_gauss2 <- data.frame(
  "Predicted" = exp(predict(glmm_gauss2, type = "response")),
  "Observed" = df_clean$body_mass
)

# plot predicted vs observed with 1:1 line
ggplot(fit_model_gauss2) +
  # each point (x) = observed, (y) = model prediction
  geom_point(aes(x = Observed, y = Predicted)) +
  # add linear regression of observed:fitted
  geom_smooth(
    aes(x = Observed, y = Predicted, colour = "gauss2"),
    method = lm,
    se = FALSE) +
  # add 1:1 linear regression to plot to check main regression
  geom_smooth(aes(x = Observed, y = Observed, colour = "observed"), method = "lm", se = FALSE, linetype = "dashed") +
  
  scale_colour_manual(name = NULL,
                      breaks = c("observed", "gauss2"),
                      labels = c("Observed", "Gauss2 Model"),
                      values = c("black", "#FF5733")) +
  
  xlab("Body mass (observed)") +
  ylab("Body mass (predicted)") +
  
  ggtitle("Predicted vs. Observed for Gauss2 Model against df_clean") +
  theme_lucid()



# 3.4.9 model predictions with modelbased - compare models visually

# also create one for gaussian2 model
pred_gauss2_log <- estimate_expectation(glmm_gauss2)
pred_gauss2 <- exp(pred_gauss2_log$Predicted)








# GAUSSIAN GLMM 3
# going to try removing some predictors from the model, starting with the least significant
glmm_gauss3 <- glmmTMB(
  
  log(body_mass) ~ landscape +
    vegetation +
    distance_to_water_km +
    average_annual_rainfall_mm +
    
    ### removed lion presence and predator density per sq km
  
    # park as random effect - each gets own intercept
    (1 | park) +
    # observer as random effect - each gets own intercept
    (1 | observer),
  
  data = df_clean,
  family = gaussian()
)

check_model(glmm_gauss3)
summary(glmm_gauss3)
AIC(glmm_gamma, glmm_gauss2, glmm_gauss3)
BIC(glmm_gamma, glmm_gauss2, glmm_gauss3)
# better scores and lower df <- lion presence and predator density wasn't adding anything meaningful to the model



fit_model_gauss3 <- data.frame(
  "Predicted" = exp(predict(glmm_gauss3, type = "response")),
  "Observed" = df_clean$body_mass
)

# plot predicted vs observed with 1:1 line
ggplot(fit_model_gauss3) +
  # each point (x) = observed, (y) = model prediction
  geom_point(aes(x = Observed, y = Predicted)) +
  # add linear regression of observed:fitted
  geom_smooth(
    aes(x = Observed, y = Predicted, colour = "gauss3"),
    method = lm,
    se = FALSE) +
  # add 1:1 linear regression to plot to check main regression
  geom_smooth(aes(x = Observed, y = Observed, colour = "observed"), method = "lm", se = FALSE, linetype = "dashed") +
  
  scale_colour_manual(name = NULL,
                      breaks = c("observed", "gauss3"),
                      labels = c("Observed", "Gauss3 Model"),
                      values = c("black", "#FFC300")) +
  
  xlab("Body mass (observed)") +
  ylab("Body mass (predicted)") +
  
  ggtitle("Predicted vs. Observed for Gauss3 Model against df_clean") +
  theme_lucid()



#  model predictions with modelbased - compare models visually

# add gauss2 model to df
pred_gauss2_log <- estimate_expectation(glmm_gauss2)
pred_gauss2 <- exp(pred_gauss2_log$Predicted)
# and for predicted from gauss2
models_predictions$predicted_gauss2 <- pred_gauss2


# also create one for gauss3 model
pred_gauss3_log <- estimate_expectation(glmm_gauss3)
pred_gauss3 <- exp(pred_gauss3_log$Predicted)
# and for predicted from gauss3
models_predictions$predicted_gauss3 <- pred_gauss3

models_predictions %>%
  # plot observed vs. fitted (predicted)
  ggplot() +
  # add points from each model including gauss2 model
  geom_smooth(aes(x = observed, y = predicted_gamma, colour = "gamma"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = observed, y = predicted_gauss, colour = "gauss"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = observed, y = predicted_gauss2, colour = "gauss2"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = observed, y = predicted_gauss3, colour = "gauss3") , method = "lm", se = FALSE) +
  
  # 1:1 observed body mass from df_clean
  geom_line(aes(x = observed, y = observed), linetype = "dashed") +
  
  # adding a legend 
  scale_colour_manual(name = NULL,
                     breaks = c("gamma", "gauss", "gauss2", "gauss3"),
                     labels = c("Gamma Model", "Gauss Model", "Gauss2 Model", "Gauss3 Model"),
                     values = c("#1ABC9C", "#154360", "#FF5733", "#FFC300")) +
  
  ggtitle("Observed vs Predicted body mass for each model") +
  ylab("Body mass (predicted)") +
  xlab("Body mass (observed)") +
  theme_lucid()




# next step: look at marginal effects


### COMMUNICATE





### REFERENCE LINKS
# boxplot outlier detection: https://statsandr.com/blog/outliers-detection-in-r/
# rstatix identify_outlier(): https://www.rdocumentation.org/packages/rstatix/versions/0.7.3/topics/identify_outliers
# for geom_smooth: https://stackoverflow.com/questions/19699858/ggplot-adding-regression-line-equation-and-r2-with-facet


