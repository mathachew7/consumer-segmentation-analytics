# Consumer Segmentation Analytics – AXANTEUS

This project focuses on consumer segmentation and predictive modeling for AXANTEUS to enhance targeted marketing, customer retention, and loyalty strategies. It combines clustering, classification, and regression models to analyze value-conscious behavior, brand loyalty, and repeat purchases.

## 🎯 Business Objectives

- **Clustering**: Segment consumers based on:
  - Purchase behavior (volume, frequency, brand loyalty)
  - Purchase motivation (price sensitivity, deal responsiveness)
  - Combined insights

- **Classification & Prediction**:
  - Identify **value-conscious consumers**
  - Predict **brand loyalty**
  - Forecast **brand runs** (repeated brand-specific purchases)

## 🧠 Dataset Overview

- **Source**: Thai urban consumer panel (AXANTEUS)
- **Records**: 600
- **Variables**: 46 (demographics, transactions, brands, promotions)
- **Missing Values**: Cleaned or imputed (median/mode)
- **Key Fields**:
  - Demographics: `age`, `sex`, `affluence_index`, `edu`, etc.
  - Purchase: `total_volume`, `brand_runs`, `value`, `avg_price`
  - Promotions: `pur_vol_no_promo`, `pur_vol_promo_6`, etc.
  - Derived: `category_diversity`, `deal_sensitivity`, `brand_loyalty_score`

## 🧪 Modeling Workflow

### 🔹 Clustering (Unsupervised)

- **Method**: K-means clustering (3 clusters)
- **Segments**:
  - **Purchase Behavior**: High-frequency, moderate, low-engagement
  - **Purchase Motivation**: Deal-sensitive, balanced, variety-seeking
  - **Combined**: Integrated behavior & basis segmentation
- **Validation**: Elbow + Silhouette

### 🔹 Classification (Supervised)

#### ✅ Value-Conscious Classification
- Model: Logistic Regression & Random Forest
- Accuracy: **84.57%**
- Top Features: `no_of_trans`, `avg_price`, `category_diversity`

#### ✅ Brand Loyalty Classification
- Model: Logistic Regression & Random Forest
- Accuracy: **91.98%**
- Top Features: `no_of_trans`, `category_diversity`, `avg_price`

### 🔹 Regression: Brand Runs Prediction
- Model: Linear Regression
- RMSE: **3.75**
- Top Predictors: `category_diversity`, `no_of_trans`, `affluence_index`

## 📊 Key Insights

- **Transaction frequency** is the strongest predictor across tasks.
- **Category diversity** aligns with brand loyalty.
- Most consumers do not rely on promotions.
- Segmentation reveals price-sensitive vs. loyalty-driven behavior.

## 📂 Project Structure

```
consumer-segmentation-analytics/
├── cluster_recommendations.csv
├── code/
│   └── project3-Rfile.R
├── data/
│   └── Consumer.csv
├── docs/
│   ├── Consumer Segmentation For Students.docx
│   ├── CSDA 6010 - P- Subash Yadav.pptx
│   └── project 3 - Subash Yadav.docx
├── README.md
```

## 🛠 Tools Used

- Language: **R**
- Packages: `caret`, `cluster`, `factoextra`, `randomForest`, `glm`, etc.
- Visualizations: Elbow, Silhouette, ROC, Importance plots

## 👨‍💻 Author

**Subash Yadav**  
[LinkedIn](https://www.linkedin.com/in/mathachew7)  
[GitHub](https://github.com/mathachew7)
