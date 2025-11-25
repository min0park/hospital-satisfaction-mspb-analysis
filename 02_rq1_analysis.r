#Question 1:

#1 Summarizing data & collapse multiple survey questions per hospital (Facility.ID)
#into one average star rating and one average MSPB score per hospital (for the OLS)

library(dplyr)

hcahps_summary <- merged_data %>%
  group_by(Facility.ID) %>%  #Grouping rows by unique hospital ID
  summarise(
    mean_star = mean(Patient.Survey.Star.Rating, na.rm = TRUE),
    mspb = mean(Score, na.rm = TRUE)
    )

#2 EDA between the two variable (mean_star vs mspb of each hospitals)
plot(hcahps_summary$mean_star, hcahps_summary$mspb,
     pch = 16,
     col = rgb(0, 0, 0, 0.25),
     xlab="Average HCAHPS Star Rating",
     ylab="MSPB Score",
     main = ""
     )

cor.test(hcahps_summary$mean_star, 
         hcahps_summary$mspb, 
         method = "spearman", 
         exact = FALSE) # p-value: < 2.2e-16 , ρ (rho) = -0.3154
#RQ1 Preliminary finding:
#There is a statistically significant inverse relationship between hospital patient experience (HCAHPS)
#and Medicare spending (MSPB), aligning with prior findings that higher satisfaction is associated with
#lower cost per beneficiary.
#(statistically significant negative association, modest but consistent cost–quality relationship.)


# 3 Fit the baseline OLS model (MSPB ~ mean_star) =
# Interprets the average change in MSPB for a one-star increase in HCAHPS
model_ols <- lm(mspb ~ mean_star, data = hcahps_summary)
summary(model_ols) # View standard OLS summary (coefficients, R^2, etc.)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.38981 -0.04378 -0.00260  0.03902  0.57841 

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.085233   0.005701  190.36   <2e-16 ***
#  mean_star   -0.030729   0.001795  -17.12   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#4 Checking for the assumptions / report robust SEs
#Loading packages for robust standard errors and tests:
library(lmtest)
library(sandwich)

#Computing heteroskedasticity-robust (HC3) standard errors since real-world hospital data often violate constant variance assumption (heteroskedasticity).
robust_se <- vcovHC(model_ols, type = "HC3")

#Getting robust coefficient test results: coeftest() replaces default SEs with robust ones, giving more reliable p-values.
robust_results <- coeftest(model_ols, vcov = robust_se)
robust_results



#5: Create a tidy table with robust CIs for reporting (effect size per 1-star increase with 95% robust CI)
library(broom)

coef_table <- tidy(model_ols, conf.int = TRUE) %>%
  mutate(
    robust_se  = sqrt(diag(robust_se))[match(term, names(coef(model_ols)))],
    robust_t   = estimate / robust_se,
    robust_p   = 2 * pt(abs(robust_t), df = df.residual(model_ols), lower.tail = FALSE),
    robust_low = estimate + qt(0.025, df = df.residual(model_ols)) * robust_se,
    robust_up  = estimate + qt(0.975, df = df.residual(model_ols)) * robust_se
  )
coef_table


#6: Basic diagnostics to check OLS assumptions

#6a. Residuals vs Fitted = look for funnel shapes (heteroskedasticity)
plot(
  model_ols,
  which = 1,
  col = rgb(0, 0, 0, 0.4),
  pch = 16,
  main = ""
)
abline(h = 0, col = "red", lwd = 1)

#6b. Normal Q-Q plot = large-N makes normality less critical, but still check gross deviations
qqnorm(
  rstandard(model_ols),
  pch = 16,
  col = rgb(0, 0, 0, 0.4),
  main = ""
)
qqline(rstandard(model_ols), col = "blue", lwd = 2)

#6c. Formal heteroskedasticity test (Breusch-Pagan). If p<0.05, prefer robust SEs.
bptest(model_ols)



#7: 10-fold cross-validation (repeated) to assess out-of-sample error (to reduce overfitting risk and provides RMSE/MAE to report)
library(caret)

set.seed(820)  #for reproducibility
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

cv_fit <- train(
  mspb ~ mean_star,
  data = hcahps_summary,
  method = "lm",
  trControl = ctrl,
  metric = "RMSE"
)
cv_fit$results #shows CV RMSE and MAE
#intercept  RMSE      Rsquared  MAE         RMSESD      RsquaredSD    MAESD
#TRUE       0.072523  0.1048022 0.05430599  0.004701702 0.04337271    0.002328733