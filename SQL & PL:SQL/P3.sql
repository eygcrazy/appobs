--Question 1:
select name from restaurant where cuisine = 'Indian';

--Question 2:
select distinct r.name, ra.stars from restaurant r, rating ra
where
r.rid = ra.rid and ra.stars in
(select max(stars) from rating group by rid having max(stars) >= 4)
order by ra.stars;

--Question 3:
select r.name from restaurant r
where r.rid not in(select rid from rating);

--Question 4:
select name from reviewer
where vid in 
(select vid from rating where ratingdate is null);

--Question 5:
select re.name as reviewer_name, r.name as restaurant_name, t1.ratingdate - t2.ratingdate from reviewer re, restaurant r, (select ra.ratingdate, ra.rid, ra.vid from rating ra,
(select vid, rid, max(stars) as maxstars from rating group by vid, rid) maxtable
where
ra.vid = maxtable.vid and ra.rid = maxtable.rid and ra.stars = maxstars) t1,
(select ra.ratingdate, ra.rid, ra.vid from rating ra,
(select vid, rid, min(stars) as maxstars from rating group by vid, rid) maxtable
where
ra.vid = maxtable.vid and ra.rid = maxtable.rid and ra.stars = maxstars) t2
where t1.rid = t2.rid and t1.vid = t2.vid and t1.ratingdate - t2.ratingdate > 0 and re.vid = t1.vid and r.rid = t1.rid;


--Question 6:
select r.name, count(ra.stars) as highest_num_of_stars from restaurant r, rating ra
where r.rid = ra.rid
group by r.name;

--Question 7:
select r.name, max(ra.stars) - min(ra.stars) as rating_spread from restaurant r, rating ra
where r.rid = ra.rid
group by r.name
order by rating_spread desc, r.name;

--Question 8:
select avg(average1.average) - avg(average2.average) as Difference_between_Chinese_and_Indian
from 
(select rid, avg(stars) as average from rating where rid in (select r.rid from restaurant r
where cuisine = 'Chinese')
group by rid) average1,
(select rid, avg(stars) as average from rating where rid in (select r.rid from restaurant r
where cuisine = 'Indian')
group by rid) average2;