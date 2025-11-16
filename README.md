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
