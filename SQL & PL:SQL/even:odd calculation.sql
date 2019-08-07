SET SERVEROUTPUT ON

accept p_num1 prompt 'Please enter the number: '

declare
    myint  number(10):= &p_num1;
    
begin
    if myint is null THEN        
        DBMS_OUTPUT.PUT_LINE(0);
    else
        for i in 1..myint loop
            if mod(i,2) = 0 then
                DBMS_OUTPUT.PUT_LINE('This is even ' || i);
            else
                DBMS_OUTPUT.PUT_LINE('This is odd ' || i);
            end if;
        end loop;
    end if;
    
exception
    when others then
        DBMS_OUTPUT.PUT_LINE('You get whatever errors');
end;
/


