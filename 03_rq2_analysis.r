#Question 2: Predicting MSPB from HCAHPS domain scores (RQ2)
# Goal: Model hospital-level MSPB as a function of key HCAHPS domains
# using (1) multiple linear regression and (2) a non-linear XGBoost model.


###Part 1:Multiple Linear Regression

#Step 1: Setting the data

library(dplyr)
library(tidyr)
library(caret)
library(xgboost)
library(car)
library(lmtest)
library(sandwich)
library(lm.beta)
library(tibble)


#A. Select the key variables &  Group by Facility and Question to get hospital-level mean for each domain
#domain_star: average star rating per facility and HCAHPS question
#mspb: average MSPB per facility

domain_data <- merged_data %>%
  select(Facility.ID, HCAHPS.Question, Patient.Survey.Star.Rating, Score)

domain_summary <- domain_data %>%
  group_by(Facility.ID, HCAHPS.Question) %>%
  summarise(
    domain_star = mean(Patient.Survey.Star.Rating, na.rm = TRUE),
    mspb = mean(Score, na.rm = TRUE)
  ) %>%
  ungroup()


#B. Reshaping (pivot wider) to make one column per domain (wide format)
domain_wide <- domain_summary %>%
  pivot_wider(
    names_from = HCAHPS.Question,
    values_from = domain_star
  )


names(domain_wide) <- c("Facility.ID", "mspb", "Care", "Clean", "Comm.Med", 
                        "Discharge", "Doctor", "Nurse", "Overall", "Quiet",
                        "Recommend", "Staff", "Summary")     #renaming for my convenience

summary(domain_wide)
colnames(domain_wide)



# D. Keep only variables used in the model and drop rows with missing values
rq2_data <- domain_wide %>%
  select(mspb, Care, Discharge, Doctor, Nurse, Clean, Quiet, Recommend) %>%
  na.omit()

summary(rq2_data)
colnames(rq2_data)


# Step 2: Multiple Linear Regression (RQ2) (Dependent variable: MSPB;Independent variables: selected HCAHPS domains)
model_rq2 <- lm(
  mspb ~ Care + Discharge + Doctor + Nurse + Clean + Quiet + Recommend,
  data = rq2_data
)
summary(model_rq2)



#step 3: Check Multicolinearity
#5–10 would indicate problematic multicollinearity

vif(model_rq2)



#step 4: Robust SE (HC3)

robust_se_rq2 <- vcovHC(model_rq2, type = "HC3") #HC3 robust covariance matrix for the OLS model
coeftest(model_rq2, vcov = robust_se_rq2) #Coefficient table with robust SE and p-values


#step 5: Standardized Betas (for comparing relative importance)
model_rq2_beta <- lm.beta(model_rq2)
summary(model_rq2_beta)




###Part 2 - Non-Linear XGBoost model.

set.seed(123)  #This is for reproducibility

#A. Cross-validation scheme: k-fold cross-validation & repeated 10-fold CV (5 times)
ctrl_xgb <- trainControl(
  method      = "repeatedcv",   
  number      = 10,
  repeats     = 5,
  verboseIter = FALSE
)

#B. Simple tuning grid for XGBoost hyperparameters
xgb_grid <- expand.grid(
  nrounds          = c(100, 200),  # = number of boosting iterations
  max_depth        = c(2, 3),   # = tree depth (controls model complexity)
  eta              = c(0.05), # = learning rate (shrinkage)
  gamma            = 0,            # minimum loss reduction
  colsample_bytree = c(0.8),    # column subsampling rate
  min_child_weight = 1,            # minimum sum of instance weight in a child
  subsample        = c(0.8)    # row sub-sampling rate
)

#C. Fit XGBoost regression using the same predictors as the OLS model
xgb.set.config(verbosity = 0)

set.seed(123)

xgb_fit <- suppressMessages(suppressWarnings(
  train(
    mspb ~ Care + Discharge + Doctor + Nurse + Clean + Quiet + Recommend,
    data      = rq2_data,
    method    = "xgbTree",
    trControl = ctrl_xgb,
    tuneGrid  = xgb_grid,
    metric    = "RMSE"
  )
))

#D. Overview of the fitted XGBoost model and selected hyperparameters
xgb_fit
#Result: eXtreme Gradient Boosting 
#2657 samples
#7 predictor
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 5 times) 
#Summary of sample sizes: 2391, 2392, 2391, 2392, 2391, 2391, ... 
#Resampling results across tuning parameters:
#max_depth  nrounds  RMSE        Rsquared   MAE       
#2          100      0.07222377  0.1127038  0.05385110
#2          200      0.07205151  0.1159475  0.05374197
#3          100      0.07208307  0.1162090  0.05372249
#3          200      0.07215946  0.1148696  0.05382182

xgb_fit$bestTune
#Result:
#nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
#2     200         2 0.05     0              0.8                1       0.8

varImp(xgb_fit)
#Result: xgbTree variable importance
#Overall
#Discharge 100.000
#Nurse      80.938
#Doctor     49.034
#Care       20.495
#Quiet       3.686
#Clean       1.706
#Recommend   0.000


#E. Extract performance of the best XGBoost model

best_xgb <- xgb_fit$results %>%
  dplyr::filter(
    nrounds          == xgb_fit$bestTune$nrounds,
    max_depth        == xgb_fit$bestTune$max_depth,
    eta              == xgb_fit$bestTune$eta,
    colsample_bytree == xgb_fit$bestTune$colsample_bytree,
    subsample        == xgb_fit$bestTune$subsample
  ) %>%
  dplyr::select(RMSE, MAE, Rsquared)

best_xgb   #use these numbers in the write-up to compare with OLS

#Result:
#RMSE        MAE        Rsquared
#1 0.07205151 0.05374197 0.1159475


#F. Variable importance from XGBoost
# Variable importance helps check whether the non-linear model
# prioritizes similar domains as the linear model.
xgb_imp <- varImp(xgb_fit, scale = TRUE)
print(xgb_imp)

#Result: xgbTree variable importance
#Overall
#Discharge 100.000
#Nurse      80.938
#Doctor     49.034
#Care       20.495
#Quiet       3.686
#Clean       1.706
#Recommend   0.000



#G. quick plot for my own reference

#Extract and clean importance table
imp_df <- xgb_imp$importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable") %>%
  dplyr::filter(!grepl("^NA", variable)) %>%
  dplyr::arrange(Overall)


#Horizontal barplot

library(ggplot2)
imp_df$variable <- factor(imp_df$variable,
                          levels = imp_df$variable)

ggplot(imp_df, aes(x = variable, y = Overall)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "XGBoost Variable Importance for MSPB",
    x = NULL,
    y = "Relative Importance"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )