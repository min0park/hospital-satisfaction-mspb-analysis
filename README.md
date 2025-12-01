# **Hospital Satisfaction & MSPB Analysis**

### *A Data-Driven Evaluation of U.S. Hospital Value Using CMS HCAHPS & MSPB Data*

**Tools:** R, tidyverse, dplyr, caret, cluster, factoextra, arules, lmtest, sandwich, broom

This repository contains all code, data processing workflows, and statistical/machine-learning analyses used to evaluate the relationship between hospital patient experience (HCAHPS) and Medicare Spending per Beneficiary (MSPB).
Using the latest 2023–2024 CMS releases, the project updates and extends prior literature with regression modeling, domain-level interpretation, unsupervised clustering, and association rule mining.

---

## **📌 Research Questions**

### **RQ1 — Overall Relationship**

*Do hospitals with higher patient satisfaction exhibit lower Medicare spending?*
Methods: OLS regression, robust SEs, diagnostics, repeated k-fold CV.

### **RQ2 — Domain-Level Predictors**

*Which specific HCAHPS domains best predict cost efficiency?*
Methods: Multiple regression, standardized coefficients, VIF checks, robust SEs, XGBoost comparison.

### **RQ3 — Performance Archetypes**

*Do hospitals naturally cluster into cost–experience groups?*
Methods: K-means, k-medoids (PAM), hierarchical clustering, cluster profiling, Apriori rule mining.

---

## **📂 Data Sources (CMS)**

### **1. HCAHPS Patient Experience Survey**

* Includes star ratings + domain-level ratings
* Domains include: communication (nurse/doctor), discharge info, cleanliness, quietness, recommendation, etc.
* ~442k observations → aggregated to hospital level.

### **2. Medicare Spending Per Beneficiary (MSPB)**

* Standardized, risk-adjusted Medicare spending metric
* Represents 30-day episode spending relative to national median
* Lower MSPB = more cost efficient.

**Merged using Facility ID**, cleaned for completeness and analytical consistency.

---

## **🧮 Methods Overview**

### **RQ1: Satisfaction → Cost Efficiency**

* Aggregated HCAHPS to hospital level
* Model: `MSPB ~ mean_star`
* HC3 robust SEs
* Breusch–Pagan test + residual/QQ diagnostics
* Repeated 10-fold CV (10×3)

**Key Insight:**

* Higher satisfaction → Lower MSPB
* β = −0.0307 (p < 0.001)
* Relationship stable across diagnostics & cross-validation.

---

### **RQ2: What Drives Efficiency? (Domain-Level)**

* Pivoted domain-level HCAHPS items to wide format
* Multiple regression with 7 domains
* Multicollinearity check (all VIF < 5)
* Standardized betas for effect size comparison
* Non-linear XGBoost for robustness

**Top predictors of lower MSPB:**

1. **Discharge Information**
2. **Nurse Communication**
3. **Doctor Communication**

Environmental domains (Cleanliness, Quietness) showed **minimal** association once communication domains were included.

---

### **RQ3: Clustering + Pattern Mining**

#### **Clustering**

* Standardized variables → k-means (K = 4), PAM, and hierarchical
* Stable four-cluster solution:

| Cluster | Patient Experience | MSPB         | Interpretation               |
| ------- | ------------------ | ------------ | ---------------------------- |
| 1       | Low                | High         | Inefficient, poor experience |
| 2       | Mid                | Slightly Low | Moderately efficient         |
| 3       | High               | Lowest       | **High-value hospitals**     |
| 4       | Moderately High    | High         | High experience, high cost   |

Cluster 3 consistently ranked highest in **communication + discharge** domains.

#### **Association Rule Mining**

* Discretized domains (Low/Mid/High)
* MSPB → Low/Mid/High (tertiles)
* Apriori rules (support ≥ 0.02, confidence ≥ 0.5)

**Representative High-Lift Rule:**
`{Doctor_High, Discharge_High, Clean_High, Quiet_Mid} → MSPB_Low`

* Lift = **1.73**
* Confidence = **60%**

High communication + strong discharge processes consistently predicted low spending.

---

## **📊 Key Findings**

* The **cost–quality tradeoff** often assumed in healthcare **does not hold** in CMS data.
* Communication-centered domains (nurse, doctor, discharge) are the **strongest, most reliable predictors** of lower spending.
* Hospitals cluster into **four reproducible performance archetypes**, including a **high-satisfaction / low-cost "best value" group**.
* Rule-mining reinforces that combinations of strong communication and discharge processes sharply increase likelihood of low MSPB.
* Environmental domains (cleanliness, quietness) play a **secondary** role.

---

## **⚠️ Limitations**

* Cross-sectional CMS data → no causal inference
* Slight reporting-window mismatch (HCAHPS vs MSPB)
* Potential survey bias in HCAHPS responses
* Limited structural covariates (e.g., case mix index)
* Clustering & discretization choices influence results

---

## **📝 Project Notes**

This project was completed as part of a graduate-level data analytics/statistical modeling course. It demonstrates:

* End-to-end data cleaning & wrangling
* Regression modeling with robust inference
* Machine learning with cross-validation
* Unsupervised clustering & stability checks
* Association rule mining
* Reproducible R workflow with tidyverse conventions
