/* Delete the tables if they already exist */
drop table  Restaurant;
drop table  Reviewer;
drop table  Rating;

/* Create the schema for our tables */
create table Restaurant(rID int, name varchar2(100), address varchar2(100), cuisine varchar2(100));
create table Reviewer(vID int, name varchar2(100));
create table Rating(rID int, vID int, stars int, ratingDate date);

/* Populate the tables with our data */
insert into Restaurant values(101, 'India House Restaurant', '59 W Grand Ave Chicago, IL 60654', 'Indian');
insert into Restaurant values(102, 'Bombay Wraps', '122 N Wells St Chicago, IL 60606', 'Indian');
insert into Restaurant values(103, 'Rangoli', '2421 W North Ave Chicago, IL 60647', 'Indian');
insert into Restaurant values(104, 'Cumin', '1414 N Milwaukee Ave Chicago, IL 60622', 'Indian');
insert into Restaurant values(105, 'Shanghai Inn', '4723 N Damen Ave Chicago, IL 60625', 'Chinese');
insert into Restaurant values(106, 'MingHin Cuisine', '333 E Benton Pl Chicago, IL 60601', 'Chinese');
insert into Restaurant values(107, 'Shanghai Terrace', '108 E Superior St Chicago, IL 60611', 'Chinese');
insert into Restaurant values(108, 'Jade Court', '626 S Racine Ave Chicago, IL 60607', 'Chinese');

insert into Reviewer values(2001, 'Sarah M.');
insert into Reviewer values(2002, 'Daniel L.');
insert into Reviewer values(2003, 'B. Harris');
insert into Reviewer values(2004, 'P. Suman');
insert into Reviewer values(2005, 'Suikey S.');
insert into Reviewer values(2006, 'Elizabeth T.');
insert into Reviewer values(2007, 'Cameron J.');
insert into Reviewer values(2008, 'Vivek T.');

insert into Rating values( 101, 2001,2, DATE '2011-01-22');
insert into Rating values( 101, 2001,4, DATE '2011-01-27');
insert into Rating values( 106, 2002,4, null);
insert into Rating values( 103, 2003,2, DATE '2011-01-20');
insert into Rating values( 108, 2003,4, DATE '2011-01-12');
insert into Rating values( 108, 2003,2, DATE '2011-01-30');
insert into Rating values( 101, 2004,3, DATE '2011-01-09');
insert into Rating values( 103, 2005,3, DATE '2011-01-27');
insert into Rating values( 104, 2005,2, DATE '2011-01-22');
insert into Rating values( 108, 2005,4, null);
insert into Rating values( 107, 2006,3, DATE '2011-01-15');
insert into Rating values( 106, 2006,5, DATE '2011-01-19');
insert into Rating values( 107, 2007,5, DATE '2011-01-20');
insert into Rating values( 104, 2008,3, DATE '2011-01-02');

--work
select name from restaurant where cuisine = 'Indian';


select restaurant.name, reviewer.name from 
(
select a.vid as vid, a.rid as rid from 
(select vid, rid, max(stars) as max_starts, max(ratingdate) as max_ratingdate from rating group by vid, rid having count(0) = 2) a join 
(select vid, rid, stars, ratingdate from rating) b
on (a.vid = b.vid and a.rid = b.rid) where a.max_starts > b.stars and a.max_ratingdate > b.ratingdate
) c, restaurant, reviewer where c.rid = restaurant.rid and c.vid = reviewer.vid
;

B
201  5  1
201  4  2

A 201  5 2

201 5 2 201 5 1
201 5 2 201 4 2



select *
from reviewer left outer join rating on (reviewer.vid = rating.vid)

select