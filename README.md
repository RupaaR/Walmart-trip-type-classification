
# Walmart-trip-type-classification
AIM:
  
  Categorizing the shopping trips based on the items purchased by the customers

DATA:
  
  The dataset consists of trips categorized ito 38 distinct types using a proprietary method applied to an extended set of data. The training and test datasets consist of 647054 and 653656 number of customer visits.
  https://www.kaggle.com/c/walmart-recruiting-trip-type-classification

CODE:
  
    Preprocess.R -> Cleaning the datset by treating missing values and outliers.
  
    FE.R -> Extracting predictors that effectvely classify a trip.
  
    Modelling.R ->Traing the model using LDA, Random Forests and XGBoost.
  
    Test.R -> Predicting for the test dataset.
