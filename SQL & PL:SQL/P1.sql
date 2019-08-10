#1
select RentalCompany from BYCAR where Mileage >= 27;
#2
select TID from TRIPS where TravelMode = 'Train' and FARE >= 150;
#3
select TID, FARE from TRIPS where TRIPSTATE = 'Non-US';
#4
select T.TID, B.Class from Trips T, BYPLANE B where T.TID = B.TID and CLASS = 'Business' and FARE > 1000;
#5
select *from Trips T
join
Trips T2 on (T.TravelMode = 'Car' and T2.TravelMode = 'Train') 
where T.FARE > T2.FARE and T.TRIPSTATE = T2.TRIPSTATE

--OR
select * from 
(select * from Trips where TravelMode = 'Car') T join (select * from Trips where TravelMode = 'Train') T2 on (T.Tripstate = T2.Tripstate)
where T.FARE > T2.FARE
#6
SELECT DISTINCT * FROM 
(select * from BYCAR) B1 join (select * from BYCAR) B2 on (B1.MILEAGE = B2.MILEAGE)
#7
select * from bytrain t inner join bytrain d on t.tid != d.tid and t.trainspeed != d.trainspeed
#8
SELECT DISTINCT* FROM 
(select * from trips)t1 join (select * from trips)t2 on (t1.tripstate = t2.tripstate and t1.travelmode = t2.travelmode)
--OR
SELECT DISTINCT* FROM trips T1, trips T2 WHERE t1. TID != T2.TID and t1.tripstate = t2.tripstate and t1.travelmode = t2.travelmode
#9
select DISTINCT T.TRIPSTATE from
(select * from Trips where TravelMode = 'Car') T 
join 
(select * from Trips where TravelMode = 'Train') T2
on
(T.Tripstate = T2.Tripstate)
join
(select * from Trips where TravelMode = 'Plane') T3
on
(T2.Tripstate = T3.Tripstate)
#10.1
SELECT distinct * FROM TRIPS WHERE FARE =
(SELECT MAX(FARE) FROM TRIPS where TRAVELMODE = 'Train' or TRAVELMODE = 'Car' or TRAVELMODE = 'Plane')
#10.2
select t1.tid, t1.tripstate, t1.travelmode, t1.fare from trips t1, trips t2 where t1.fare < t2.fare and t1.travelmode =t2.travelmode
minus
select t1.tid, t1.tripstate, t1.travelmode, t1.fare from trips t1, trips t2 where t1.fare > t2.fare and t1.travelmode =t2.travelmode



