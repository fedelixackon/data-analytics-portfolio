# Online Customer Purchase Prediction

Online Customer Purchase Prediction Using Machine Learning

Georgia Institute of Technology | ISyE 7406: Data Mining & Statistical Learning

Project Overview

Analyzed 12,330 online shopping sessions using R to understand customer purchasing behavior and develop machine learning models to predict whether a website visitor would complete a purchase.

Business Problem

Many online retailers attract significant website traffic but struggle to convert visitors into paying customers. This project investigates customer browsing patterns and identifies factors associated with purchase decisions to support more effective marketing and customer engagement strategies.

Tools & Technologies

Programming: R

Libraries: caret, MASS, e1071, klaR, pROC, randomForest

Techniques: Exploratory Data Analysis, Classification, Cross-Validation, Hyperparameter Tuning, Predictive Modeling

Dataset

Source: UCI Online Shoppers Purchasing Intention Dataset

12,330 shopping sessions

17 predictor variables

Target variable: Revenue (Purchase or No Purchase)

15.5% of sessions resulted in purchases

Methodology

Conducted exploratory data analysis to identify customer purchasing patterns.

Prepared categorical variables and applied an 80/20 stratified train-test split.

Developed six classification models:

Logistic Regression

Linear Discriminant Analysis (LDA)

Quadratic Discriminant Analysis (QDA)

Naive Bayes

K-Nearest Neighbors (KNN)

Random Forest

Compared models using repeated 5-fold cross-validation.

Tuned model parameters and evaluated performance using accuracy, precision, recall, F1-score, and ROC-AUC.

Model Performance

Model

Accuracy

ROC-AUC

Logistic Regression

87.87%

88.53%

LDA

87.14%

89.69%

QDA

81.54%

83.84%

Naive Bayes

84.54%

86.17%

KNN

85.68%

81.04%

Random Forest

89.90%

92.24%

Best-Performing Model: Random Forest

Random Forest achieved the highest cross-validated ROC-AUC and strongest overall performance on the independent test dataset.

Key results:

Accuracy: 89.90%

ROC-AUC: 92.24%

Precision: 72.15%

Recall: 56.43%

F1-Score: 63.33%

The final Random Forest model used 500 decision trees and a tuned mtry value of 14.

Data Visualizations

Machine Learning Model Comparison



Most Important Purchase Predictors



Monthly Customer Purchase Rates



Key Business Insights

PageValues was the strongest predictor of customer purchasing intention.

Greater product-page engagement was associated with higher purchase likelihood.

Higher bounce rates and exit rates were associated with non-purchasing sessions.

November recorded the highest observed monthly purchase rate at 25.4%.

Business Applications

The analysis demonstrates how e-commerce businesses could use predictive analytics to:

Identify customers more likely to purchase.

Improve website engagement and conversion strategies.

Support targeted marketing and promotional decisions.

Identify browsing patterns associated with abandoned purchasing sessions.

Limitations

Because only 15.5% of sessions resulted in purchases, class imbalance affected model performance. Although Random Forest performed best overall, it missed approximately 44% of actual purchases.

Additional validation would be needed before using the model in a real-world business setting.

Running the Analysis

Install R and the required packages: caret, MASS, e1071, klaR, pROC, and randomForest.

Download the dataset or use online_shoppers_intention.csv.

Keep the dataset in the same working directory as the R script.

Open projectcodes_revisedall.r in RStudio and run the analysis.

Project Files

R Analysis Code

Dataset

Visualization charts available in this repository.

Conclusion

Random Forest provided the best overall predictive performance among six classification models. The study demonstrates how customer browsing data and machine learning can support data-driven business decisions.
