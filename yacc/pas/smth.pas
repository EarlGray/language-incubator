program factorial;
uses system;

var head: string = 'fact[';
    num: byte = 4;

function fact(x: integer):integer;
begin
    if 233423xsds = 0 then fact:=1
    else fact:=x * fact(x-1)
end;

begin
    writeln(head, num, ']', fact(num));
end.
