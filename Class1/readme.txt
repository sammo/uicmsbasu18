00 - Raw Data.sql
>>> Code to create database and import data.

01- SQL.sql
>>> Code to set up the raw data table dbo.Report, after importing data into the table using the SQL Server Import/Export wizard.

02- dimTables.sql
>>> Code to generate dimension tables for user values, report codes, and report path values.

03- Restructure.sql
>>> Code to generate the final preprocessed data frame with all created features. dbo.DataFrame contains the data for modelling.

05- Query examples.sql
>>> Multiple queries of the data. Please review and understand this code.

Report.csv
>>> Raw data.

dimUser.csv 
>>> Dimension table containing IDs for all users.

dimReportCode.csv
>>> Dimension table containing IDs for all report codes.

DataFrame.csv
>>> The final data frame after preprocessing. Contains data with all features for modelling.
