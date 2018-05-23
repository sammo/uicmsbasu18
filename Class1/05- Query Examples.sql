
/*Check the number of report instances by month in descending order of count*/
SELECT MONTH(InsertTimeStamp) AS Mo, COUNT(*) AS ReportCount
FROM dbo.Report
GROUP BY MONTH(InsertTimeStamp)
ORDER BY ReportCount DESC;

/*Check the number of report instances by Report Code by Report Server in descending order of count*/
SELECT ReportServer, ReportCode, COUNT(*) AS ReportCount
FROM dbo.Report
GROUP BY ReportServer, ReportCode
ORDER BY ReportCount DESC;

/*Check the number of report instance per Report Server by Quarter of the year, in ascending order of quarter then report count*/
SELECT ReportServer, DATEPART(quarter, InsertTimeStamp) AS Quartr, COUNT(*) AS ReportCount
FROM dbo.Report
GROUP BY ReportServer, DATEPART(quarter, InsertTimeStamp)
ORDER BY DATEPART(quarter, InsertTimeStamp), ReportCount;

/*Check the number of report instance by Report Server by Month of the year, in ascending order of month then report count*/
SELECT ReportServer, DATEPART(month, InsertTimeStamp) AS Mont, COUNT(*) AS ReportCount
FROM dbo.Report
GROUP BY ReportServer, DATEPART(month, InsertTimeStamp)
ORDER BY DATEPART(month, InsertTimeStamp), ReportCount;

/*Create the UserDomain field and populate it from InsertUser*/
ALTER TABLE dbo.Report
	ADD UserDomain VARCHAR(50);

UPDATE dbo.Report
SET UserDomain = SUBSTRING(InsertUser, 1, CHARINDEX('\', InsertUser, 1) - 1)
WHERE UserDomain IS NULL;

/*Check the number of report instances per domain*/
SELECT UserDomain, COUNT(*) AS ReportCount
FROM dbo.Report
GROUP BY UserDomain;
