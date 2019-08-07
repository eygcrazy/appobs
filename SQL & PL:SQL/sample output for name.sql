SET SERVEROUTPUT ON

accept parse_first_Name prompt 'Please enter your first name: '
accept parse_last_Name prompt 'Please enter your last name: '
accept parse_title prompt 'Please enter your title: '

declare
    myFirstname  varchar2 (40):= '&parse_first_Name';
    myLastname  varchar2(40):= '&parse_last_Name';
    mytitle  varchar2(40):= '&parse_title';
    
    miss_first_exception exception;
    miss_last_exception exception;
    miss_title_exception exception;
    
begin
    if myFirstname is null THEN        
        raise miss_first_exception;
        
    elsif myLastname is null THEN
        raise miss_last_exception;
        
    elsif mytitle is null THEN
        raise miss_title_exception;
    
    else
        DBMS_OUTPUT.PUT_LINE(upper(myFirstname) ||' ' || upper(myLastname) ||' ' || mytitle || '.');
        DBMS_OUTPUT.PUT_LINE(upper(myLastname) ||',' ||' ' || upper(myFirstname) ||' ' || mytitle|| '.');
    end if;
    
exception
    when miss_first_exception then
        DBMS_OUTPUT.PUT_LINE('You are missing first name ');
    when miss_last_exception then
        DBMS_OUTPUT.PUT_LINE('You are missing last name ');
    when miss_title_exception then
        DBMS_OUTPUT.PUT_LINE('You are missing title ');
end;
/


