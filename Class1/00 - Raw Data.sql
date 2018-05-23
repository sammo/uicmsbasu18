USE master;
GO

IF EXISTS(SELECT [name] FROM sys.databases WHERE [name] = 'UIC')
    DROP DATABASE UIC18;
GO

CREATE DATABASE UIC;
GO

USE UIC;
GO

IF OBJECT_ID('dbo.Report', 'U') IS NOT NULL
    DROP TABLE dbo.Report;
GO

CREATE TABLE dbo.Report(
        ReportID VARCHAR(50),
        ReportServer VARCHAR(50),
        ReportPath VARCHAR(100),
        ReportCode VARCHAR(100),
        InsertTimeStamp VARCHAR(50),
        InsertUser VARCHAR(128)
        );
GO

BULK
INSERT dbo.Report
FROM '/mnt/sqlserver/Report.csv'
WITH
(
    FIELDTERMINATOR = ',',
    ROWTERMINATOR = '0x0a',
    FIRSTROW = 2
);

-- SELECT TOP 100 * FROM dbo.Report
