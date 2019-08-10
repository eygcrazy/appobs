#1
SELECT FNAME, LNAME FROM EMPLOYEE, PROJECT 
WHERE 
DNUM = 1;
#2
SELECT d.Dname, e.salary FROM department d, EMPLOYEE e 
where e.salary=
(select AVG(e.SALARY) from EMPLOYEE)
and e.Ssn = d.Mgr_ssn 
#3
SELECT e.LNAME FROM department d, EMPLOYEE e 
where SUPER_SSN is null
and e.Ssn = d.Mgr_ssn 
#4
select e.FNAME, E.LNAME FROM DEPARTMENT D, EMPLOYEE E
WHERE E.SALARY =
(SELECT MIN(E.SALARY) FROM EMPLOYEE)
AND E.SSN = d.mgr_ssn
#5
select dno, count(*), sum(d_cnt) from (
select e.ssn, e.dno, count(t.essn) as d_cnt from employee e left join dependent t on e.ssn = t.essn group by e.ssn, e.dno
) group by dno
#6
select e.fname, e.lname from employee e where SALARY>=
(select max(SALARY)-20000 from employee)