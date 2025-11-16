#Question 2:

#Step 1: Setting the data

library(dplyr)
library(tidyr)

#A. Select the key variables &  Group by Facility and Question to get hospital-level mean for each domain
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




# Step 2: Multiple Linear Regression (RQ2) (Dependent variable: MSPB;Independent variables: selected HCAHPS domains)
model_rq2 <- lm(
  mspb ~ Care + Discharge + Doctor + Nurse + Clean + Quiet + Recommend,
  data = domain_wide
)
summary(model_rq2)


#step 3: Check Multicolinearity
library(car)
vif(model_rq2)


#step 4: Robust SE (HC3)

library(lmtest)
library(sandwich)
robust_se_rq2 <- vcovHC(model_rq2, type = "HC3")
coeftest(model_rq2, vcov = robust_se_rq2)


#step 5: Standardized Betas
library(lm.beta)
model_rq2_beta <- lm.beta(model_rq2)
summary(model_rq2_beta)
