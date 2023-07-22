# Bachelor's Thesis

## Asset Estimation of the Bank Customers via Machine Learning Approach

This thesis is about the asset amount prediction of the bank customer with the available data (Bank Data Warehouse, KKB, Neighborhood data). In that way, it is aimed to make the right campaign for the right customer. In order to establish this predictive model, the main goal is to use machine learning algorithms and to make the model in a software program that supports these algorithms. 


In this thesis, four machine learning algorithms that were examined in the literature review header used to predict the asset of bank’s customers according to many conditions created in Oracle SQL software. Model performance was measured with MAE and MAPE KPI forecast error types. After examination of MAE and MAPE values for test samples and also model performance graphs, the XGBoost algorithm was chosen as an optimal model for this case. Test MAE and MAPE values were found 98.642, 17% respectively.

![image](https://github.com/belcekaya/asset_estimation/assets/58460284/ae719a9b-af21-4d25-b3b8-dc16bb7e35f0)

As a result, not all models but for wealthy customers, the XGBoost model predicted well. GBM model followed it with almost no difference. In other words, the aim of the project could be reached.
