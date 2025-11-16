# hospital-satisfaction-mspb-analysis
A Comprehensive Statistical and Machine Learning Evaluation Using CMS HCAHPS and MSPB Data
Tools: R, tidyverse, dplyr, caret, cluster, factoextra, arules, lmtest, sandwich, broom

This repository contains all code, data workflows, and analytical outputs for a multi-stage project examining the relationship between U.S. hospital patient experience (HCAHPS) and Medicare Spending per Beneficiary (MSPB). The analysis replicates and extends previous literature using modern statistical diagnostics, clustering algorithms, and association rule mining.


The project is structured around three research questions:

RQ1:
Is higher patient satisfaction associated with lower MSPB?
(Using: Ordinary Least Squares (OLS) + model diagnostics + robust SEs + cross-validation)

RQ2:
Which HCAHPS domains (nurse communication, doctor communication, discharge information, etc.) best predict cost efficiency?
(Using: Multiple regression, standardized coefficients, multicollinearity checks, robustness analysis)

RQ3:
Do hospitals naturally form distinct performance clusters based on patient satisfaction and cost efficiency?
(Using: K-means, K-medoids (PAM), cluster profiling, Apriori association rule mining)


# Data Sources (= CMS)

1. HCAHPS Patient Experience Data = Includes star ratings and domain-level satisfaction measures (communication, discharge, cleanliness, quietness, etc.).
2. Medicare Spending Per Beneficiary (MSPB) = A CMS cost-efficiency metric capturing 30-day Medicare episode spending.

Both datasets were merged using Facility ID, filtered with internal consistency checks, and cleaned to ensure hospital-level analysis.


# Methods Summary

RQ1: Overall Relationship Analysis
- Aggregated HCAHPS scores to hospital level
- OLS regression (lm()): MSPB ~ mean_star
- Robust SEs via HC3 (vcovHC)
- Breusch–Pagan test (bptest)
- Diagnostics: QQ plots, residual plots
- Repeated K-fold cross-validation (caret::train)

Key Result:
Higher satisfaction → lower MSPB (β = −0.0307, p < 0.001).
Effect remained stable across diagnostics and CV.


RQ2: Domain-Level Predictive Modeling
- Created wide-format dataset by pivoting HCAHPS Question (= Domain averages)
- Multiple regression across 7 domains
- Multicollinearity tested using VIF
- HC3 robust SEs
- Standardized coefficients (lm.beta)

Key Result:
Top predictors of lower MSPB were:
- #1: Discharge Information
- #2: Nurse Communication
- #3: Doctor Communication
These domains consistently showed the strongest negative associations.


RQ3: Unsupervised Learning + Pattern Mining
- K-means Clustering
     Four stable clusters emerged:
            Low Satisfaction / High Cost
            Mid Satisfaction / Mid Cost
            High Satisfaction / Low Cost (best performers)
            High Satisfaction / High Cost
- K-medoids (PAM), Confirmed k-means structure → robust clustering solution
- Cluster Profiling (Domain-level comparisons showed communication-related domains strongly differentiate clusters)
- Association Rule Mining (Apriori) (Discretized domains (Low/Mid/High) + MSPB tertiles ; Used arules::apriori with support ≥ 0.02, confidence ≥ 0.5)

Representative rule:
{Doctor_High, Discharge_High, Clean_High, Quiet_Mid} → MSPB_Low
lift = 1.73, confidence = 60%
= Shows strong interaction patterns that characterize cost-efficient hospitals.


# Key Findings

- The cost–quality tradeoff commonly assumed in healthcare is not supported.
- High patient experience, especially communication and discharge clarity, is strongly linked with lower cost per Medicare beneficiary.
- Hospitals form four repeatable performance archetypes across methods.
- High-communication, well-coordinated hospitals constitute the most efficient, highest-value cluster.
- Apriori rules highlight specific domain combinations that predict cost-efficiency.


# Key Limitations

- Cross-sectional data (= no causal inference)
- Time-window mismatch between MSPB and HCAHPS
- Self-reported survey bias
- Limited covariates for risk adjustment
- Sensitivity of clustering and discretization choices


# Author Notes

This project was completed as part of a certificate-level data analytics course. It demonstrates end-to-end data cleaning, statistical modeling, machine learning, clustering, rule mining, and reproducible R workflows.
