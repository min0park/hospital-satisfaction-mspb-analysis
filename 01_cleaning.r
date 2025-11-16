####PART 1: Cleaning the two datas and merging them

#1. reading the two CSV files (HCAHPS survey file and the Medicare Spending Per Beneficiary file)

survey <- read.csv("HCAHPS_Hospital.csv")
spending <- read.csv("MSPB_Hospital.csv")

#2. Read the summary of the data

summary(survey)
summary(spending)

#3 changing the date of survey/spending in month/date/year format

survey$Start.Date <- as.Date(survey$Start.Date, format = "%m/%d/%Y")
survey$End.Date <- as.Date(survey$End.Date, format = "%m/%d/%Y")

spending$Start.Date <- as.Date(spending$Start.Date, format = "%m/%d/%Y")
spending$End.Date <- as.Date(spending$End.Date, format = "%m/%d/%Y")


#4 Remove rows where the survey's Patient.Survey.Star.Rating is "Not Applicable", "Not Available", or is left blank

sum(survey$Patient.Survey.Star.Rating %in% c("Not Applicable")) #total = 389910
sum(survey$Patient.Survey.Star.Rating %in% c("Not Available")) #total = 17369
sum(is.na(survey$Patient.Survey.Star.Rating)) #total = 0

survey <- survey[survey$Patient.Survey.Star.Rating != "Not Applicable", ]
survey <- survey[survey$Patient.Survey.Star.Rating != "Not Available", ]
survey <- survey[!is.na(survey$Patient.Survey.Star.Rating), ]
#New total = 34936

#5 Convert the survey's Patient.Survey.Star.Rating to numeric & confirm whether it worked
survey$Patient.Survey.Star.Rating <- as.numeric(survey$Patient.Survey.Star.Rating)

unique(survey$Patient.Survey.Star.Rating)


#6 Convert Facility.ID, Number.of.Completed.Surveys, and Survey.Response.Rate.Percent into numeric as well
survey$Facility.ID <- as.numeric(survey$Facility.ID)
survey$Number.of.Completed.Surveys <- as.numeric(survey$Number.of.Completed.Surveys)
survey$Survey.Response.Rate.Percent <- as.numeric(survey$Survey.Response.Rate.Percent)

summary(survey)


#7 Check if Facility.ID include NA's, and exclude them

NA_facilityID <- sum(is.na(survey$Facility.ID))
NA_facilityID #The sum was equal to 1474

survey <- survey[!is.na(survey$Facility.ID), ]
summary(survey)

#8 Counting the number of unique Facility.ID in the two datasets & also checking how many hospitals exist in BOTH
length(unique(survey$Facility.ID)) #It is 3042
length(unique(spending$Facility.ID)) #It is 4591

length(intersect(unique(survey$Facility.ID), unique(spending$Facility.ID))) #It is 3042


#9 Making a new spending data that only includes necessary columns (scores in this case) & make sure it does not include.
#+ Also checking if the spending data includes as duplicated Facility ID
spending_score <- spending[, c("Facility.ID", "Score")]

sum(spending_score$Score %in% c("Not Available")) #total = 1682
sum(is.na(spending_score$Score)) #total = 0

spending_score <- spending_score[spending_score$Score != "Not Available", ]
spending_score <- spending_score[!is.na(spending_score$Score), ]

spending_score$Score <- as.numeric(spending_score$Score)

length(unique(spending_score$Facility.ID)) #It is 2909
length(intersect(unique(survey$Facility.ID), unique(spending_score$Facility.ID))) #It is 2657

sum(duplicated(spending_score$Facility.ID)) #0 duplicates


#10 Merge the two cleaned datasets

merged_data <- merge(survey, spending_score, by = "Facility.ID")
length(unique(merged_data$Facility.ID)) #2657


#11 Since I won't be considering the Start.Date, End.Date, HCAHPS.Answer.Percent, HCAHPS.Answer.Percent.Footnote, and HCAHPS.Linear.Mean.Value here, I will remove it.
merged_data <- merged_data[, !names(merged_data) %in% c("Start.Date", "End.Date", "HCAHPS.Linear.Mean.Value", "HCAHPS.Answer.Percent", "HCAHPS.Answer.Percent.Footnote")]

summary(merged_data)
str(merged_data)





### PART 2: Early Exploratory Data Analysis

#1 Double-check the structure/attributes & missing values

str(merged_data) # types + structure
summary(merged_data) # min/max/mean per column
colSums(is.na(merged_data)) # count NAs per column - good, there was none

data.frame(
  Variable = names(merged_data),
  Type = sapply(merged_data, class)
)


#2 Basic Value Distributions (Overview) + Summary Statistics

table(merged_data$Patient.Survey.Star.Rating) #by the Patient Survey Star Rating
#  1     2     3     4     5 
#  1462  6393 11788  7591  1993 

summary(merged_data$Patient.Survey.Star.Rating) #shows distribution skew
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   3.000   3.077   4.000   5.000 

summary(merged_data$Score) #shows how far above/below the national median baseline hospitals are
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.6200  0.9400  0.9900  0.9907  1.0300  1.5100 

round(aggregate(Score ~ Patient.Survey.Star.Rating, data = merged_data, summary), 2) #Scores grouped per star rating in 2 decimal places
#Patient.Survey.Star.Rating Score.Min. Score.1st Qu. Score.Median Score.Mean Score.3rd Qu.  Score.Max.
#                          1       0.62          0.99         1.03       1.04          1.08     1.33
#                          2       0.62          0.97         1.01       1.01          1.05     1.33
#                          3       0.62          0.95         0.99       0.99          1.03     1.33
#                          4       0.62          0.93         0.97       0.97          1.02     1.33
#                          5       0.73          0.90         0.95       0.95          1.00     1.51


summary_by_star <- aggregate(Score ~ Patient.Survey.Star.Rating, data = merged_data,
                             function(x) c(
                               count = length(x),
                               mean  = round(mean(x, na.rm = TRUE), 3),
                               median = round(median(x, na.rm = TRUE), 3),
                               sd    = round(sd(x, na.rm = TRUE), 3)
                             )
)

summary_by_star <- data.frame(
  Star_Rating = summary_by_star$Patient.Survey.Star.Rating,
  Count = as.integer(summary_by_star$Score[, "count"]),
  Mean = summary_by_star$Score[, "mean"],
  Median = summary_by_star$Score[, "median"],
  SD = summary_by_star$Score[, "sd"]
)

summary_by_star



#3 spearman correlation to Compare HCAHPS star ratings vs MSPB score

cor.test(merged_data$Score,
    merged_data$Patient.Survey.Star.Rating,
    method = "spearman",
    exact = FALSE) #The result was: -0.2567004 (= a weak negative correlation)

#This means that Hospitals with higher Medicare spending (MSPB) tend to receive slightly lower patient satisfaction star ratings, but the relationship is weak
#high spending ≠ high patient satisfaction, and in fact, it often trends slightly in the opposite direction.



#4 kruskal-Wallis test to see if median spending score significantly different across ratings

kruskal.test(Score ~ Patient.Survey.Star.Rating, data = merged_data)
# chi-squared = 1972.7, df = 4, p-value < 2.2e-16
# This means that the MSPB Score is significantly different across star rating groups.
# The differences are not random, and hospitals with different HCAHPS star ratings truly have different spending levels




### PART 3: Detecting abnormal data or outliers
#(not removing any because MSPB scores represent real cost behavior, and extreme spending is important insight)


#1 Boxplot (quick visual check))

stats <- summary(merged_data$Score)
stats
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6200  0.9400  0.9900  0.9907  1.0300  1.5100 

boxplot(merged_data$Score,
        horizontal = TRUE,
        main = "MSPB Score — Outlier Check",
        xlab = "Score",
        las = 1)

mtext(
  paste0(
    "Min: ", round(stats[1], 2), "   |  ",
    "Q1: ",  round(stats[2], 2), "   |  ",
    "Median: ", round(stats[3], 2), "   |  ",
    "Mean: ", round(stats[4], 2), "   |  ",
    "Q3: ", round(stats[5], 2), "   |  ",
    "Max: ", round(stats[6], 2)
    ),
  side = 1, line = 4, cex = 0.8
  )

#2 Find outliers using the standard IQR method

Q1 <- quantile(merged_data$Score, 0.25, na.rm = TRUE)
Q3 <- quantile(merged_data$Score, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

#3 Count how many outliers
sum(merged_data$Score < lower_bound | merged_data$Score > upper_bound, na.rm = TRUE) #Total = 858 (about 2.9% of the total observations)

#4 View the actual outlier rows (to flag them by adding a new variable to the merged data to later discuss them in interpretation/limitations)
outliers <- merged_data[merged_data$Score < lower_bound | merged_data$Score > upper_bound, ]
head(outliers)

merged_data$outlier_flag <- ifelse(
  merged_data$Score > upper_bound, "High Outlier",
  ifelse(merged_data$Score < lower_bound, "Low Outlier", "Normal")
  )

table(merged_data$outlier_flag)
#High Outlier = 605
#Low Outlier =  253
#Normal = 28369 


