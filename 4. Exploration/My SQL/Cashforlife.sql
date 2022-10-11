CREATE DATABASE cashforlife;
USE cashforlife;

SELECT * FROM cfl_lottery;

Select Weekday, Date, Winners, Pick1, Pick2, Pick3, Pick4, Pick5, CB FROM cfl_lottery
WHERE Winners = 'Y';

Select Weekday, Date, Winners, Pick1, Pick2, Pick3, Pick4, Pick5, CB FROM cfl_lottery
WHERE Winners = 'Y'
order by CB desc;

Select Weekday, Date, Winners, Pick1, Pick2, Pick3, Pick4, Pick5, CB, LotteryRetailer FROM cfl_lottery
WHERE LotteryRetailer LIKE '%PUBLIX%';

Select Weekday, Date, Winners, Pick1, Pick2, Pick3, Pick4, Pick5, CB, LotteryRetailer
From cashforlife.cfl_lottery
WHERE Weekday IN ("Monday", "Thursday")
order by Pick1 asc;

