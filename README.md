# Shiny-Outlier-Detection---Los-Angeles-Atmospheric-Ozone-Data

This project utilizes the Shiny package in the R language as the platform for an interactive data dashboard application.

The Shiny application serves as a means of outlier detection for data consisting of the daily meteorological measurements that were conducted in the Los Angeles (CA) basin, courtesy of Leo Breiman.

![Data table](https://user-images.githubusercontent.com/45345672/110204136-5b9c4080-7ed6-11eb-8bd9-ccce2f66846d.PNG)

The concentration of atmospheric ozone (PPM), which serves as the response, is the daily maximum recorded value of the hourly-average ozone concentration in Upland, California.

The application utilizes a range of statistical methods as a means of outlier detection, from simple univariate and multivariate techniques such as boxplots and Mahalanobis distance measurements. 

Model-based outlier detection methods are also introduced, such as Cook's Distance, Density-based spatial clustering with noise (DBSCAN), Local outlier factor and also the use of support vector machines as a one-class classification use case. 

![Model_based_outliers](https://user-images.githubusercontent.com/45345672/110204145-67880280-7ed6-11eb-9c6c-d931470eb6ac.PNG)
![Cooks_dist](https://user-images.githubusercontent.com/45345672/110204148-6a82f300-7ed6-11eb-8611-5c0e603e4b89.PNG)

Lastly the technique of quantile regression forest is also used as a robust method of anomaly detection as it is a method of measuring conditional quantiles and IQR's - anomolies can be detected through this method by setting quantile levels as a critereon. 

Standardization treatments such as Yeo-Johnson Power transformation features are also provided to account for variance and non-normal distributions. 
