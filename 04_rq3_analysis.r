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
k_range <- 2:12


# 3a. Elbow: total within-cluster sum of squares
wss <- sapply(k_range, function(k)
  kmeans(X, centers = k, nstart = 50)$tot.withinss)

# 3b. Silhouette: average silhouette width
sil <- sapply(k_range, function(k) {
  km <- kmeans(X, centers = k, nstart = 50)
  mean(silhouette(km$cluster, dist(X))[, 3])
})


###(optional) Quick elbow/silhouette plots
qplot(k_range, wss, geom = "line") +
  labs(x = "K", y = "Total within-cluster sum of squares",
       title = "Elbow plot for K (mean_star, MSPB)")

qplot(k_range, sil, geom = "line") +
  labs(x = "K", y = "Mean silhouette width",
       title = "Silhouette scores for K")


# 3d. Gap statistic for additional guidance
set.seed(820)
gap_stat <- clusGap(X, FUN = kmeans, K.max = 12, B = 50)

fviz_gap_stat(gap_stat) +
  ggtitle("Gap statistic for K-means (mean_star, MSPB)")



#4. Fit k-means with chosen K (inspect elbow/silhouette; start with K=4 as a reasonable default)
set.seed(820)
K <- 4
km <- kmeans(X, centers = K, nstart = 100)

#5. Robustness: k-medoids (PAM) is less sensitive to outliers than k-means
pam_fit <- pam(X, k = K)  # k-medoids clustering


# + Hierarchical clustering using Ward’s method
dist_mat  <- dist(X, method = "euclidean")
hc_ward   <- hclust(dist_mat, method = "ward.D2")
hc_groups <- cutree(hc_ward, k = K)


#6. Attach cluster labels and profile clusters
res_kmeans <- hcahps_summary %>%
  mutate(
    cluster_km  = factor(km$cluster),
    cluster_pam = factor(pam_fit$clustering),
    cluster_hc  = factor(hc_groups)
  )

# Profile: k-means clusters
profile_km <- res_kmeans %>%
  group_by(cluster_km) %>%
  summarise(
    n         = n(),
    mean_star = mean(mean_star),
    mspb      = mean(mspb),
    .groups   = "drop"
  )
print(profile_km)

# Profile: PAM clusters (for robustness check)
profile_pam <- res_kmeans %>%
  group_by(cluster_pam) %>%
  summarise(
    n         = n(),
    mean_star = mean(mean_star),
    mspb      = mean(mspb),
    .groups   = "drop"
  )
print(profile_pam)

# Profile: hierarchical clusters (optional robustness check)
profile_hc <- res_kmeans %>%
  group_by(cluster_hc) %>%
  summarise(
    n         = n(),
    mean_star = mean(mean_star),
    mspb      = mean(mspb),
    .groups   = "drop"
  )
print(profile_hc)


#7. Visualize clusters (k-means)

label_map <- c(
  "1" = "1. Low-exp, High-cost",
  "2" = "2. Average-exp, Neutral-cost",
  "3" = "3. High-exp, Low-cost",
  "4" = "4. High-exp, High-cost"
)

res_kmeans <- res_kmeans %>%
  mutate(cluster_label = factor(label_map[cluster_km],
                                levels = label_map))

ggplot(res_kmeans, aes(mean_star, mspb, color = cluster_label)) +
  geom_point(alpha = 0.6, size = 1.6) +
  labs(
    x = "Average HCAHPS Star Rating",
    y = "MSPB Score",
    color = "Cluster",
    title = "K-means clusters of hospitals (K = 4)"
  ) +
  theme_minimal()





# PART B. Cluster profiling with domains

#1: Setting the data

#1A. Select the key variables &  Group by Facility and Question to get hospital-level mean for each domain
domain_data <- merged_data %>%
  select(Facility.ID, HCAHPS.Question, Patient.Survey.Star.Rating, Score)

domain_summary <- domain_data %>%
  group_by(Facility.ID, HCAHPS.Question) %>%
  summarise(
    domain_star = mean(Patient.Survey.Star.Rating, na.rm = TRUE),
    mspb = mean(Score, na.rm = TRUE)
  ) %>%
  ungroup()


#1B. Reshaping (pivot wider) to make one column per domain (wide format)
domain_wide <- domain_summary %>%
  pivot_wider(
    names_from = HCAHPS.Question,
    values_from = domain_star
  )

names(domain_wide) <- c("Facility.ID", "mspb", "Care", "Clean", "Comm.Med", 
                        "Discharge", "Doctor", "Nurse", "Overall", "Quiet",
                        "Recommend", "Staff", "Summary")     #renaming for my convenience

#2: Merge domain data with cluster labels from Part A

domain_plus <- domain_wide %>%
  select(Facility.ID, mspb, Care, Discharge, Doctor, Nurse, Clean, Quiet, Recommend) %>%
  inner_join(res_kmeans %>% select(Facility.ID, cluster_km, cluster_pam), by = "Facility.ID")


#3: Profile domains by k-means cluster
domain_profile_km <- domain_plus %>%
  group_by(cluster_km) %>%
  summarise(across(c(mspb, Care, Discharge, Doctor, Nurse, Clean, Quiet, Recommend), mean, na.rm = TRUE),
            n = n(), .groups = "drop")
print(domain_profile_km)
# cluster_km  mspb  Care Discharge Doctor Nurse Clean Quiet Recommend     n
#  1          1.08   1.98      2.30   2.17  2.28  2.29  2.01      2.26   477
#  2          0.965  2.72      3.11   2.82  2.93  2.62  2.37      2.99   908
#  3          0.910  3.88      4.04   3.94  4.19  3.82  3.46      4.12   595
#  4          1.04   3.56      3.70   3.48  3.72  3.51  3.24      3.81   677




# PART C. Apriori rule mining (actionable patterns)
# Goal: discretize MSPB and domain scores into Low/Medium/High and mine rules like {High Nurse, High Discharge} => {Low MSPB}

#1. Discretize MSPB (continuous) into Low / Mid / High using tertiles and HCAHPS domains (1–5) into Low / Mid / High using fixed cut-points.
# 1–2.5 = Low, 2.5–3.5 = Mid, 3.5–5 = High.
disc_df <- domain_plus %>%
  mutate(
    MSPB_bin = cut(
      mspb, 
      breaks = quantile(mspb, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("MSPB_Low", "MSPB_Mid", "MSPB_High")
    ),
    Nurse_bin     = cut(Nurse,     breaks = c(0.5, 2.5, 3.5, 5.5), labels = c("Nurse_Low","Nurse_Mid","Nurse_High"), include.lowest = TRUE),
    Doctor_bin    = cut(Doctor,    breaks = c(0.5, 2.5, 3.5, 5.5), labels = c("Doctor_Low","Doctor_Mid","Doctor_High"), include.lowest = TRUE),
    Discharge_bin = cut(Discharge, breaks = c(0.5, 2.5, 3.5, 5.5), labels = c("Discharge_Low","Discharge_Mid","Discharge_High"), include.lowest = TRUE),
    Clean_bin     = cut(Clean,     breaks = c(0.5, 2.5, 3.5, 5.5), labels = c("Clean_Low","Clean_Mid","Clean_High"), include.lowest = TRUE),
    Quiet_bin     = cut(Quiet,     breaks = c(0.5, 2.5, 3.5, 5.5), labels = c("Quiet_Low","Quiet_Mid","Quiet_High"), include.lowest = TRUE),
    Recommend_bin = cut(Recommend, breaks = c(0.5, 2.5, 3.5, 5.5), labels = c("Recommend_Low","Recommend_Mid","Recommend_High"), include.lowest = TRUE)
  ) %>%
  select(MSPB_bin, Nurse_bin, Doctor_bin, Discharge_bin,
         Clean_bin, Quiet_bin, Recommend_bin) %>%
  drop_na()


#2. Convert the discretized data frame to arules transactions
#Each row = one hospital; each column = a categorical attribute (item).
disc_trans <- as(disc_df, "transactions")


#3. Mine rules that specifically predict Low MSPB (cost-efficient hospitals)
# supp: minimum support; conf: minimum confidence; minlen: minimum rule length.
rules_low <- apriori(
  disc_trans,
  parameter = list(
    supp = 0.02, # at least 2% of hospitals
    conf = 0.50, # at least 50% confidence
    minlen = 2
  ),
  appearance = list(
    rhs = "MSPB_bin=MSPB_Low",
    default = "lhs"
  ),
  control = list(verbose = FALSE)
)


#4. Sort rules by lift (strength) and inspect top rules

rules_low_sorted <- sort(rules_low, by = "lift", decreasing = TRUE)
inspect(head(rules_low_sorted, 10)) #view top 10 rules

top_rule <- rules_low_sorted[1] #Select the top rule for validation
inspect(top_rule)



#Part D: Fisher's Exact Test for the Top Rules

#1. Extract LHS of the top rule (Important Fix)
# arules returns the LHS as a single string, not a clean vector.
# We manually split the string to retrieve each "variable=value" item.

lhs_raw <- labels(lhs(top_rule))
lhs_raw

lhs_str <- lhs_raw[[1]] # Unwrap string inside list

lhs_items <- unlist(strsplit(lhs_str, ",(?=[A-Za-z_]+=)", perl=TRUE))
lhs_items <- gsub("\\{|\\}", "", lhs_items)
lhs_items <- trimws(lhs_items)
print(lhs_items)


print(lhs_items)

#2. Build logical filter for each item
conditions <- lapply(lhs_items, function(x) {
  parts <- unlist(strsplit(x, "="))
  var <- parts[1]
  val <- parts[2]
  disc_df[[var]] == val
})

lhs_condition <- Reduce(`&`, conditions)

#3. RHS (Low MSPB)
rhs_condition <- disc_df$MSPB_bin == "MSPB_Low"


#4. 2x2 table
ct <- table(
  LHS = lhs_condition,
  MSPB_Low = rhs_condition
)
ct
#         MSPB_Low
#LHS     FALSE TRUE
#FALSE    1678  850
#TRUE       51   78



#5. Fisher test

fisher.test(ct)

#data:  ct
#p-value = 1.857e-09
#alternative hypothesis: true odds ratio is not equal to 1
#95 percent confidence interval: 2.071996 4.427878
#sample estimates:
#odds ratio 
#3.017878 