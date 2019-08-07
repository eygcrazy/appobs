SET SERVEROUTPUT ON

accept p_num1 prompt 'Please enter the old_salary number: '
accept p_num2 prompt 'Please enter the raise_percentage number: '

declare
    old_salary  number(10,2):= &p_num1;
    new_salary  number(10,2);
    raise_percentage    number(10,2) := &p_num2;
    
begin
    if old_salary is null THEN        
        new_salary := 0;
    else
        new_salary := old_salary * (1+ raise_percentage/100);        
    end if;
    DBMS_OUTPUT.PUT_LINE('Your new salary is ' || new_salary);
exception
    when others then
        DBMS_OUTPUT.PUT_LINE('You get whatever errors');
end;
/
