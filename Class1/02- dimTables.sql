USE UIC;
GO

--Dim table for ReportPath variable
IF OBJECT_ID('dbo.dimReportPath','U') IS NOT NULL
	DROP TABLE dbo.dimReportPath;
GO
CREATE TABLE dbo.dimReportPath
(
	ReportPathID INT IDENTITY(1,1) PRIMARY KEY,
	ReportPath VARCHAR(50) NOT NULL
);

INSERT INTO dbo.dimReportPath(ReportPath)
SELECT DISTINCT ReportPath
FROM dbo.Report
ORDER BY ReportPath DESC;

--Dim table for ReportCode variable
IF OBJECT_ID('dbo.dimReportCode','U') IS NOT NULL
	DROP TABLE dbo.dimReportCode;
GO
CREATE TABLE dbo.dimReportCode
(
	ReportCodeID INT IDENTITY(1,1) PRIMARY KEY,
	ReportCode VARCHAR(100)
);
GO

INSERT INTO dbo.dimReportCode(ReportCode)
SELECT DISTINCT ReportCode
FROM dbo.Report
ORDER BY ReportCode;

--Dim table for UserName variable
IF OBJECT_ID('dbo.dimUser','U') IS NOT NULL
	DROP TABLE dbo.dimUser;
GO
CREATE TABLE dbo.dimUser
(
	UserID INT IDENTITY(1,1) PRIMARY KEY,
	UserName VARCHAR(100)
);
GO

INSERT INTO dbo.dimUser(UserName)
SELECT DISTINCT InsertUser
FROM dbo.Report
ORDER BY InsertUser;
