# Missing Data Imputation and Outlier Detection
Wang Zifan
This repository showcases a comprehensive workflow for dealing with missing values and detecting potential error records within a clinical dataset by utilizing R. The employed techniques encompass Missing Completely at Random (MCAR) testing, multiple imputation through the Multivariate Imputation by Chained Equations (MICE) method, and outlier detection via the Local Outlier Factor (LOF) algorithm as well as z-score diagnostics.

The programming language used for this work is R, relying on the scripts provided in the course materials.

Dataset Source: Course materials, specifically Module 1.

Source Code Location: ./DataCleaning/

Output Location: ./Outputs/graphs 
# Working flows
Data Import and Cleaning
The dataset was imported using the read.csv() function. Variables with over 35% missing values were removed as they were deemed irrelevant. The dataset was then filtered to keep only those variables with 35% or less missing data, ensuring the integrity and usability of the dataset for subsequent analysis.

Missing Data Analysis
Visualization: To gain insights into the missing data patterns across variables, the visdat and naniar packages were utilized. Bar plots and heatmaps were generated, which provided a clear visual representation of the distribution and patterns of missing values. These visualizations were instrumental in understanding the extent and nature of missing data within the dataset.
MCAR Testing: The mcar_test() function from the mice package was applied to assess whether the missingness mechanism was Missing Completely at Random (MCAR). The analysis revealed that the data did not follow the MCAR pattern, indicating that the missing data was likely either Missing at Random (MAR) or Missing Not at Random (MNAR). This finding was crucial as it guided the choice of subsequent imputation methods.

Multiple Imputation
The mice package was employed to perform imputation. For complex, non-linear relationships within the data, the Random Forest (method = "rf") was used. Meanwhile, Predictive Mean Matching (method = "pmm") was applied to maintain the original data distribution. Imputed datasets were generated for both methods, and a detailed comparison of the imputation results was conducted. The distributions of the original and imputed data were visualized, showing how well each method approximated the original data characteristics. In the end, the PMM method was determined to be the more suitable choice for imputation. It not only preserved the original data distribution effectively but also achieved results comparable to the Random Forest method.

Visualization
Distributions of the original and imputed data were plotted for comparison. For both the PMM and RF methods, the visualizations demonstrated that the distributions of the imputed data closely resembled those of the original data, thereby validating the effectiveness of the imputation process. Each imputed feature's distribution was compared against the original to ensure that the imputation did not distort the data's inherent properties. Based on these comparisons, the PMM method emerged as the recommended interpolation approach due to its superior ability to maintain the original data distribution.

Outlier Detection
Local Outlier Factor (LOF): The dbscan::lof() function was used to detect multivariate anomalies within the dataset. Histograms and scatter plots were created to visualize the top LOF scores, enabling the identification of potential outliers.
Z-Score Method: Z-scores were calculated for each numeric variable, and values with a z-score greater than 3 were flagged as extreme. These extreme values were considered potential outliers or possible data entry errors, warranting further investigation.

Suspected Error Reporting
Records with high LOF scores were extracted for in - depth examination. Extreme values identified by the z - score method were also flagged. The results were exported to a CSV file for manual review. This step allowed for a human - in - the - loop approach to determine whether these flagged values were genuine outliers or simply errors in data entry. Future research will focus on developing more advanced screening methods to identify suspicious records that require additional scrutiny. 
# Insights
PMM preserves the original variable distribution better than RF. LOF combined with z-score provides a robust method for error detection. Manual review is essential to differentiate between true outliers and data entry errors.
