# 🧠 COVID-19 Health Behavior Analysis

This repository contains data preprocessing, transformation, and PCA analysis of a COVID-19 health behavior dataset.  
The main goal is to prepare and standardize data for further analysis, including encoding categorical features, handling missing values, and performing dimensionality reduction.

---

## 📁 Repository Contents
1. **Code/**
   - `Code.R` → R script performing data preprocessing, feature encoding, outlier detection, and PCA.
2. **covid.csv**
   - Original dataset used for analysis.
3. **final_dataset.xlsx**
   - Processed dataset exported after cleaning and transformation.

---

## ⚙️ Steps in the Code
1. **Data Preprocessing**
   - Encoded categorical variables (e.g., Gender, Health Status, Diabetes level).
   - Replaced missing values using mean/mode based on COVID-19 test results.
2. **Statistical Summary**
   - Computed mean, median, quartiles, variance, and standard deviation for numeric features.
3. **Outlier Detection**
   - Used boxplots to identify outliers in physical and health-related columns.
4. **Standardization**
   - Applied z-score normalization on numeric features for fair comparison.
5. **Principal Component Analysis (PCA)**
   - Extracted key components explaining ~70% of the variance.
   - Visualized results using scree plot (via `factoextra`).

---

## 📊 Key Libraries
- `dplyr`
- `openxlsx`
- `factoextra`
- `ggplot2`

---

## 🧾 Output
- `final_dataset.xlsx` — fully cleaned and standardized dataset ready for modeling.
- Scree plot visualization (shown in R output).

---

## Author
**Jackie Lim**  
📧 [linkedin.com/in/jackie-lim7/](https://linkedin.com/in/jackie-lim7/)  
🎓 Model Deployment Course — BINUS University  
