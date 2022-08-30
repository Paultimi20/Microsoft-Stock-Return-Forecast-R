# Microsoft-Stock-Return-Forecast-R
In this project, time series models were used to forecast the return of Microsoft stock. The data used ranges from  January 2011 to Deember 2020. The best model was used to forecast the return for the first month of 2021.
We begin by loading in the dataset, then the data was explored to gain understanding about the trend and distribution of return. A couple of tests were conducted to understand the behaviour of the data, and having failed the variance stationarity test, we resulted to the Generalized Autoregressive Conditional Heteroscedasticity (GARCH) models.
Of the six models attempted, the t-distribution EGARCH (Exponential GARCH) model produced a parsimonous result. Therefore, the forecast was done using this model.
