%token <string> IDENTIFIER
%token EOF

%start <string option> prog
%%

prog : 
    | EOF { None }
    | v = IDENTIFIER { Some v }
    ;