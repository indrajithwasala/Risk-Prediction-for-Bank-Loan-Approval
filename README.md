# Risk Prediction to Improve Strategies for Bank Loan Approval

### Objective:
The objective of this project is to build a credit risk assessment model using the German Credit Dataset, which contains customer and loan-related features. The goal is to predict whether an applicant is a good credit risk (likely to repay the loan) or a bad credit risk (likely to default). This model is designed to assist financial institutions in making informed decisions about loan approvals and managing lending risks effectively.

The dataset consists of 1000 observations and 21 variables. The target variable indicates whether the applicant is a good (1) or bad (0) credit risk. Predictor variables include both categorical and numerical features such as the status of existing checking accounts, credit history, credit amount, present employment, and age.

### Data Cleaning and Preprocessing:
1. **Missing Value Handling**: We begin by identifying and addressing missing values. Numerical features with missing values are imputed using the mean or median, based on the distribution. Categorical features with missing values are replaced with the mode or a new category like “Unknown.”
2. **Normalization**: To ensure all features are on the same scale, we apply min-max normalization, which scales numerical features between 0 and 1, preventing features with large ranges from dominating the model.
3. **Log Transformation for Skewed Data**: Skewed numerical features undergo logarithmic transformation to reduce skewness, improving the performance of machine learning models.
4. **Rebinning for Ordinal Features**: For categorical features with many levels (e.g., “Checking Account Status”), we group them into fewer meaningful bins to reduce noise and enhance model performance.

### Exploratory Analysis and Feature Selection:
- Detailed exploration of the dataset to understand distributions, correlations, and relationships between features and the target variable.
- Feature selection techniques are applied to identify the most significant variables contributing to credit risk prediction.

### Model Building:
We build and evaluate several machine learning models to predict credit risk:
1. **Logistic Regression**: A baseline model for binary classification.
2. **Lasso and Ridge Regression**: Regularized regression models to prevent overfitting.
3. **Linear Discriminant Analysis (LDA) & Quadratic Discriminant Analysis (QDA)**: Probabilistic classifiers for finding the optimal decision boundary.
4. **K-Nearest Neighbors (KNN)**: A non-parametric classifier based on the distance between data points.
5. **Classification Trees**: 
   - Full Tree: A decision tree built without pruning.
   - Pruned Tree: A decision tree with branches removed to prevent overfitting.
   - **Bagging**: Bootstrap aggregation to reduce variance.
   - **Boosting**: Adaptive boosting to improve model accuracy.
   - **Random Forest**: An ensemble of decision trees for more robust predictions.

### Model Evaluation:
1. **Leave-One-Out Cross-Validation (LOOCV)**: A validation technique to assess model performance by using a single data point as the test set in each iteration.
2. **K-Fold Cross-Validation**: Splitting the dataset into K parts and training the model on K-1 parts while testing on the remaining part.
3. **Misclassification Rate**: Calculating the proportion of incorrectly classified instances to evaluate model accuracy.
4. **ROC Curves**: Evaluating the performance of classifiers by plotting the Receiver Operating Characteristic (ROC) curve to assess the trade-off between true positive rate and false positive rate.

### Conclusion:
This project demonstrates a systematic approach to building and evaluating a credit risk assessment model. By employing various machine learning techniques and rigorously validating the models, we aim to provide a reliable tool for financial institutions to assess loan applicants and mitigate the risk of defaults.
