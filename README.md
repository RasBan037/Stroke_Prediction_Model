# Stroke_Prediction_Model
This Project predicts the presence of stroke using four different data science models on a dataset gotten from kaggle

This R script is designed for comprehensive data analysis and model building using a Stroke dataset. The script includes data preparation, exploration, visualization, and the construction of prediction models.

File Structure
Stroke_Data.xlsx: The primary dataset used in this analysis, containing variables relevant to stroke study.
Script Overview

Library Importation
Various R libraries are imported to aid in data manipulation (dplyr, tidyverse), visualization (ggplot2, plotly, GGally), and machine learning (caret, randomForest, e1071, rpart).

Data Importation
The dataset Stroke_Data.xlsx is loaded for analysis.

Data Preparation and Preprocessing
The dataset is examined for structure and data types.
Character variables are converted to factors.
Specific categorical transformations are performed, and outliers are handled.
Missing values in the bmi column are imputed using the MICE package.

Data Exploration
The distribution and percentage distribution of key variables like stroke, heart disease, hypertension, gender, and smoking status are examined.

Data Visualization
Several ggplot visualizations are created to explore relationships between various factors and stroke.

Model Building
The data is split into training and testing sets.
Oversampling is performed to address class imbalance.
Multiple models including Decision Trees, Logistic Regression, SVM, and Naive Bayes with k-fold cross-validation are built and evaluated.

Model Evaluation
Confusion matrices and accuracy calculations are provided for each model.

Key Observations and Insights
The script provides a thorough analysis of the stroke dataset, highlighting key factors influencing stroke incidence.
Model comparison reveals the effectiveness of different algorithms in predicting stroke based on various predictors.

Recommendations
Users are encouraged to adjust the script parameters based on their specific dataset structure.
Further tuning of the machine learning models may be necessary for optimal performance.

Additional Information
For any queries or further customization, users are advised to refer to the documentation of the specific R packages used in this script.
