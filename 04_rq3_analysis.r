#Question 3:


library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)  
library(factoextra) 
library(Matrix)
library(scales)
library(arules)

#1. Build a 2D feature set for clustering: mean_star vs mspb (hospital-level)
hcahps_summary <- merged_data %>%
  group_by(Facility.ID) %>%
  summarise(
    mean_star = mean(Patient.Survey.Star.Rating, na.rm = TRUE),
    mspb      = mean(Score, na.rm = TRUE)
  ) %>% ungroup() %>%
  drop_na()

#2. Scale features (k-means is distance-based; scaling avoids unit bias)
X <- scale(hcahps_summary[, c("mean_star", "mspb")])

#3. Pick K: Elbow (WSS) + Silhouette
set.seed(820)
wss <- sapply(2:8, function(k) kmeans(X, centers = k, nstart = 50)$tot.withinss)
sil <- sapply(2:8, function(k) {
  km <- kmeans(X, centers = k, nstart = 50)
  mean(silhouette(km$cluster, dist(X))[, 3])
})

###(optional) Quick elbow/silhouette plots
qplot(2:8, wss, geom="line") + labs(x="K", y="Total WSS", title="Elbow plot")
qplot(2:8, sil, geom="line") + labs(x="K", y="Mean Silhouette", title="Silhouette scores")



#4. Fit k-means with chosen K (inspect elbow/silhouette; start with K=4 as a reasonable default)
set.seed(820)
K <- 4
km <- kmeans(X, centers = K, nstart = 100)

#5. Robustness: k-medoids (PAM) is less sensitive to outliers than k-means
pam_fit <- pam(X, k = K)  # k-medoids clustering

#6. Attach cluster labels and profile clusters
res_kmeans <- hcahps_summary %>%
  mutate(cluster_km = factor(km$cluster),
         cluster_pam = factor(pam_fit$clustering))

## Profile by cluster: means and counts
profile_km <- res_kmeans %>%
  group_by(cluster_km) %>%
  summarise(
    n = n(),
    mean_star = mean(mean_star),
    mspb = mean(mspb),
    .groups = "drop"
  )

profile_pam <- res_kmeans %>%
  group_by(cluster_pam) %>%
  summarise(
    n = n(),
    mean_star = mean(mean_star),
    mspb = mean(mspb),
    .groups = "drop"
  )

print(profile_km)
print(profile_pam)



#7. Visualize clusters (k-means)
ggplot(res_kmeans, aes(mean_star, mspb, color = cluster_km)) +
  geom_point(alpha = 0.6, size = 1.6) +
  labs(x = "Average HCAHPS Star Rating", y = "MSPB Score",
       title = paste0("K-means Clusters (K=", K, ")")) +
  theme_minimal()




# PART B. Cluster profiling with domains

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

domain_plus <- domain_wide %>%
  select(Facility.ID, mspb, Care, Discharge, Doctor, Nurse, Clean, Quiet, Recommend) %>%
  inner_join(res_kmeans %>% select(Facility.ID, cluster_km, cluster_pam), by = "Facility.ID")


# Profile domains by k-means cluster
domain_profile_km <- domain_plus %>%
  group_by(cluster_km) %>%
  summarise(across(c(mspb, Care, Discharge, Doctor, Nurse, Clean, Quiet, Recommend), mean, na.rm = TRUE),
            n = n(), .groups = "drop")
print(domain_profile_km)







# PART C. Apriori rule mining (actionable patterns)
# Goal: discretize MSPB and domain scores into Low/Medium/High and mine rules like {High Nurse, High Discharge} => {Low MSPB}


#1. Discretize MSPB (continuous) into Low / Mid / High using tertiles and HCAHPS domains (1–5) into Low / Mid / High using fixed cut-points.

disc_df <- domain_plus %>%
  mutate(
    # MSPB: use empirical tertiles (quantiles are fine because MSPB is continuous)
    MSPB_bin = cut(
      mspb,
      breaks = quantile(mspb, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("MSPB_Low", "MSPB_Mid", "MSPB_High")
    ),
    
    # HCAHPS domains: scores are 1–5, so use equal-width bins:
    # 1–2.5 = Low, 2.5–3.5 = Mid, 3.5–5 = High
    Nurse_bin = cut(
      Nurse,
      breaks = c(0.5, 2.5, 3.5, 5.5),
      include.lowest = TRUE,
      labels = c("Nurse_Low", "Nurse_Mid", "Nurse_High")
    ),
    Doctor_bin = cut(
      Doctor,
      breaks = c(0.5, 2.5, 3.5, 5.5),
      include.lowest = TRUE,
      labels = c("Doctor_Low", "Doctor_Mid", "Doctor_High")
    ),
    Discharge_bin = cut(
      Discharge,
      breaks = c(0.5, 2.5, 3.5, 5.5),
      include.lowest = TRUE,
      labels = c("Discharge_Low", "Discharge_Mid", "Discharge_High")
    ),
    Clean_bin = cut(
      Clean,
      breaks = c(0.5, 2.5, 3.5, 5.5),
      include.lowest = TRUE,
      labels = c("Clean_Low", "Clean_Mid", "Clean_High")
    ),
    Quiet_bin = cut(
      Quiet,
      breaks = c(0.5, 2.5, 3.5, 5.5),
      include.lowest = TRUE,
      labels = c("Quiet_Low", "Quiet_Mid", "Quiet_High")
    ),
    Recommend_bin = cut(
      Recommend,
      breaks = c(0.5, 2.5, 3.5, 5.5),
      include.lowest = TRUE,
      labels = c("Recommend_Low", "Recommend_Mid", "Recommend_High")
    )
  ) %>%
  # Keep only the binned variables for rule mining
  select(MSPB_bin, Nurse_bin, Doctor_bin, Discharge_bin,
         Clean_bin, Quiet_bin, Recommend_bin) %>%
  tidyr::drop_na()


#2. Convert the discretized data frame to arules transactions
#Each row = one hospital; each column = a categorical attribute (item).
disc_trans <- as(disc_df, "transactions")

#3. Mine rules that specifically predict Low MSPB (cost-efficient hospitals)
# supp: minimum support; conf: minimum confidence; minlen: minimum rule length.
rules_low2 <- apriori(
  disc_trans,
  parameter = list(
    supp = 0.02,      # rule must apply to at least 2% of hospitals
    conf = 0.5,       # at least 50% of those must have Low MSPB
    minlen = 2,       # at least one condition -> one consequence
    target = "rules"
  ),
  appearance = list(
    rhs = "MSPB_bin=MSPB_Low",  # only rules whose RHS is Low MSPB
    default = "lhs"
  )
)

length(rules_low2)
rules_low2_sorted <- sort(rules_low2, by = "lift", decreasing = TRUE)
inspect(head(rules_low2_sorted, 15))


#4. Sort rules by lift (strength) and inspect top rules
rules_low2_sorted <- sort(rules_low2, by = "lift", decreasing = TRUE)

inspect(head(rules_low2_sorted, 15)) # View the top 10–15 rules

