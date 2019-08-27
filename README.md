So You Want to Price a House in Ames Iowa?
Kaggle Competition results from Charles Cohen, Michael Dollar, Michael Drozdov, and Sam Audino.
https://www.kaggle.com/c/house-prices-advanced-regression-techniques

You'll find in this repo our analysis and results from the Kaggle competition, "House Prices: Advanced Regression Techniques." We decided to break the data up initially, so each quarter of the feature space (ie, columns 1-20, 21-40, 41-60, 61-80) were given to individual group members in order to clean and impute. Using various statistical techniques, we identified outliers (located in mdrozdov's folder) using Cooke's distance, binned low frequency results (located in saudino's folder, as well as ccohen's folder), and imputed missing values based on certain criteria. After the data had been thoroughly cleaned we merged it (located in ccohen's colder) back into its original form. Each folder should contain a readme that documents the changes made.

From there we applied various models in order to attempt to feature select as well as reduce our RSME. We used ElasticNet and RandomForest in order to get our results as low as possible. We also attempted to implement our own method of stacking, which did result in a slight reduction of the RSME. Our Jupyter Notebook is located in the modelling folder, which showcases the steps we took in order to implement these models.

If you have further questions, please feel free to reach out to me (Sam Audino) either on GitHub, or at my email saudino480@gmail.com

Happy Kaggling!
Sam A.
