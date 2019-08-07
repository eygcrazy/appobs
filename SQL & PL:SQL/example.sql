SET SERVEROUTPUT ON;
declare
    v_line varchar2(40);
begin
    v_line  := 'My PL/SQL Program Works';
    DBMS_OUTPUT.PUT_LINE(v_line);
end;
/