%{
open Printf
type identifier = string * string
type typeid = string * string
(*The first string is the line number and the second string is the name of the identifier*)

(*Define the component of Cool program*)
type cls = 
        | ClassNoInherits of typeid * (feature list)
        | ClassInherits of typeid * typeid * (feature list) 

and feature = 
        | AttributeNoInit of identifier * typeid
        | AttributeInit of identifier * typeid * exp
        | MethodArgs of identifier * (formal list) * typeid * exp

and formal = 
        | Arg of identifier * typeid
        | NoArg

and exp_inner = 
        | Plus of exp * exp
        | Minus of exp * exp
        | Times of exp * exp
        | Divide of exp * exp
        | Identifier of identifier
        | Integer of string
        | String of string
        | True 
        | False
        | If of exp * exp * exp
        | While of exp * exp
        | Assign of identifier * exp
        | Lt of exp * exp
        | Le of exp * exp
        | Eq of exp * exp
        | Not of exp
        | Negate of exp
        | Isvoid of exp
        | New of identifier
        | Block of exp list
        | Dyn_Dispatch of exp * identifier * exp list
        | Self_Dispatch of identifier * exp list
        | Static_Dispatch of exp * typeid * identifier * exp list
        | Let of (binding list) * exp
        | Case of exp * (case_ele list)

and binding =
        | BindingNoInit of identifier * typeid 
        | BindingInit of identifier * typeid * exp
and case_ele = 
        | CaseEle of identifier * typeid * exp

and exp = string * exp_inner

type app = cls list
%}

%token <string> IF THEN ELSE FI CLASS LPAREN RPAREN PLUS MINUS TIMES DIVIDE SEMI COMMA LBRACE RBRACE COLON LARROW RARROW INHERITS FALSE TRUE WHILE LOOP POOL LT LE EQ NOT TILDE ISVOID NEW DOT LET IN CASE ESAC OF SELF AT
%token <string * string> INTEGER STRING IDENTIFIER TYPE 
%token EOF

%left EQ
%left PLUS
%left MINUS
%left TIMES
%left DIVIDE

%start app_rule
%type <app> app_rule
%%

app_rule: classes EOF { $1 }
;
classes:        { [] }
        | cls SEMI classes { $1 :: $3 }
        ;
cls:    CLASS TYPE  LBRACE features RBRACE { ClassNoInherits($2, $4) }
        | CLASS TYPE INHERITS TYPE LBRACE features RBRACE { ClassInherits($2, $4, $6) }
        ;
features:               { [] }
        | feature SEMI features { $1 :: $3 }
        ;
feature : IDENTIFIER COLON TYPE  { AttributeNoInit($1, $3) }
        | IDENTIFIER COLON TYPE LARROW exp { AttributeInit($1, $3, $5) }
        | IDENTIFIER LPAREN formals RPAREN COLON TYPE LBRACE exp RBRACE { MethodArgs($1, $3, $6, $8) }
        ;
formals :                       { [] }
        | formal formals  { $1 :: $2 }
        ;
formal  :                       { NoArg }
        | COMMA IDENTIFIER COLON TYPE { Arg($2, $4) }
        | IDENTIFIER COLON TYPE { Arg($1, $3) }
        ;
exp_list:       { [] }
        | exp SEMI exp_list { $1 :: $3 }
        ;
exps_list:
        | exp SEMI exp_list {$1 :: $3}
        ;
exp_arg_list : 
        |                        { [] }                 
        | exp_arg exp_arg_list  { $1 :: $2 }
        ;
exp_arg :                       
        | COMMA exp             { $2 }
        | exp                   { $1 }
        ;
binding_list : 
        | binding_no_init                       { $1  :: []}
        | binding_init                         { $1 :: []}            
        | binding_no_init COMMA binding_list  { $1 :: $3 }
        | binding_init COMMA binding_list { $1 :: $3 }
        ;
binding_no_init :                       
        | IDENTIFIER COLON TYPE            { BindingNoInit($1, $3) }
        ;
binding_init :                       
        | IDENTIFIER COLON TYPE LARROW exp  { BindingInit($1, $3, $5) }
        ;
case_ele_list :
        | IDENTIFIER COLON TYPE RARROW exp SEMI {CaseEle($1, $3, $5) :: []}
        | IDENTIFIER COLON TYPE RARROW exp SEMI case_ele_list {CaseEle($1, $3, $5) :: $7}
exp     : 
        | exp LT exp            {
                                        let line, _ = $1 in
                                                (line, Lt($1, $3))
                                }
        | exp LE exp            {
                                        let line, _ = $1 in
                                                (line, Le($1, $3))
                                }
        | exp EQ exp            {
                                        let line, _ = $1 in
                                                (line, Eq($1, $3))
                                }
        | IDENTIFIER LARROW exp
                                { let line, _ = $1 in 
                                        (line, Assign($1, $3))}
        | LBRACE exps_list RBRACE 
                                {
                                        let line = $1 in
                                                (line, Block($2)) 
                                }
        | exp MINUS exp          { let line, _ = $1 in
                                        ( line, Minus($1, $3)) }
        | exp PLUS exp          { let line, _ = $1 in
                                        ( line, Plus($1, $3)) }
        | exp TIMES exp         { let line, _ = $1 in
                                        ( line, Times($1, $3)) }
        | exp DIVIDE exp        {
                                        let line, _ = $1 in 
                                                (line, Divide($1, $3))
                                }
        | TILDE exp             {
                                        let line = $1 in
                                                (line, Negate($2))
                                }
        | TRUE                 {
                                        let line = $1 in
                                                (line, True)
                                }
        | FALSE                 {
                                        let line = $1 in
                                                (line, False)
                                }
        | CASE exp OF case_ele_list ESAC
                                {
                                        let line = $1 in 
                                                (line, Case($2, $4))
                                }
        | IDENTIFIER            { let line, text = $1 in
                                        (line, Identifier($1))}
        | INTEGER               { let line, int_text = $1 in
                                        (line, Integer(int_text)) }
        | STRING                { let line, text = $1 in
                                        (line, String(text)) }
        | IF exp THEN exp ELSE exp FI
                                { let line = $1 in
                                        (line, If($2, $4, $6)) }
        | WHILE exp LOOP exp POOL
                                {
                                        let line = $1 in
                                                (line, While($2, $4))
                                }
        | LET binding_list IN exp   {   let line = $1 in
                                                (line, Let($2, $4))}
        | NOT exp               {
                                        let line = $1 in 
                                                (line, Not($2))
                                }
        | ISVOID exp            {
                                        let line = $1 in
                                                (line, Isvoid($2))
                                }
        | NEW TYPE            {
                                        let line = $1 in
                                                (line, New($2))
                                }
        | LPAREN exp RPAREN {$2}
        | exp DOT IDENTIFIER LPAREN exp_arg_list RPAREN { let line, _ = $1 in
                                                                (line, Dyn_Dispatch($1, $3, $5))}
        | SELF DOT IDENTIFIER LPAREN exp_arg_list RPAREN { let line = $1 in
                                                                (line, Self_Dispatch($3, $5))}
        | IDENTIFIER LPAREN exp_arg_list RPAREN { let line, _ = $1 in
                                                                (line, Self_Dispatch($1, $3))}
        | exp AT TYPE DOT IDENTIFIER LPAREN exp_arg_list RPAREN { let line, _ = $1 in
                                                                (line, Static_Dispatch($1, $3, $5, $7))}
        ;
%%
let read_tokens token_filename = 
        let fin = open_in token_filename in (*reading lexeme tokens from file*)
        let tokens_queue = Queue.create () in (*storage for parsed tokens*)
        let get_line () = String.trim (input_line fin) in 
        let get_untrimmed_line () = input_line fin in 
        (try while true do
                let l = get_line () in (*read file line by line and saving tokens into the queue*)
                let token_type = get_line () in
                let token = match token_type with
                | "class" -> CLASS(l)
                | "semi" -> SEMI(l)
                | "comma" -> COMMA(l)
                | "dot" -> DOT(l)
                | "colon" -> COLON(l)
                | "lbrace" -> LBRACE(l)
                | "rbrace" -> RBRACE(l)
                | "lparen" -> LPAREN(l)
                | "rparen" -> RPAREN(l)
                | "plus" -> PLUS(l)
                | "minus" -> MINUS(l)
                | "times" -> TIMES(l)
                | "divide" -> DIVIDE(l)
                | "larrow" -> LARROW(l)
                | "inherits" -> INHERITS(l)
                | "if" -> IF(l)
                | "then" -> THEN(l)
                | "else" -> ELSE(l)
                | "fi" -> FI(l)
                | "while" -> WHILE(l)
                | "loop" -> LOOP(l)
                | "pool" -> POOL(l)
                | "type" -> TYPE(l, get_line ())
                | "equals" -> EQ(l)
                | "lt" -> LT(l)
                | "le" -> LE(l)
                | "not" -> NOT(l)
                | "tilde" -> TILDE(l)
                | "integer" -> INTEGER(l, get_line ())
                | "string" -> STRING(l, get_untrimmed_line ())
                | "identifier" -> IDENTIFIER(l, get_line ())
                | "isvoid" -> ISVOID(l)
                | "new" -> NEW(l)
                | "true" -> TRUE(l)
                | "false" -> FALSE(l)
                | "let"   -> LET(l)
                | "in"  -> IN(l)
                | "case" -> CASE(l)
                | "esac" -> ESAC(l)
                | "of" -> OF(l)
                | "rarrow" -> RARROW(l)
                | "self" -> SELF(l)
                | "at" -> AT(l)
                | _ -> begin
                        printf "unexpected token type: %s \n" token_type ;
                        exit 1
                end in
                Queue.add (l, token, token_type) tokens_queue
        done with 
        | _ -> ()) ;
        close_in fin ;
        tokens_queue

let main () = begin
        let token_filename = Sys.argv.(1) in
        let tokens_queue = read_tokens token_filename in
        let lexbuf = Lexing.from_string "" in
        let ex_line_number = ref "1" in
        let err_token = ref "" in
        let lexer_token lb = 
                if Queue.is_empty tokens_queue then
                        EOF
                else begin
                        let line_number, next_token, token_type = Queue.take tokens_queue in
                        ex_line_number := line_number ;
                        err_token := token_type;
                        next_token
                end in
        let res =  (*try to parse tokens against grammar*) 
                try
                        app_rule lexer_token lexbuf
                with
                | _ -> begin
                        printf "ERROR: %s: Parser: syntax error near token of type %s \n" !ex_line_number (String.uppercase_ascii !err_token) ;
                        exit 0
                end
        in
        
        (*save the output into a file for the next compilation phase - symantic analysis*)
        let output_f = (Filename.chop_extension Sys.argv.(1)) ^ ".cl-ast" in
        let f = open_out output_f in
        
        (* recursivly pass parsed tokens to the set of functions to print out matching results into the output file *)
        let rec output_app res = out_classes res

        and out_classes res = 
                fprintf f "%d\n" (List.length res) ;
                List.iter output_cls res
        
        and output_cls res = 
                match res with
                | ClassNoInherits(cls_name, cls_features) ->
                                output_identifier cls_name;
                                fprintf f "no_inherits\n";
                                fprintf f "%d\n" (List.length cls_features);
                                List.iter output_feature cls_features
                | ClassInherits(cls_name, inh_name, cls_features) ->
                                output_identifier cls_name;
                                let l, typ = inh_name in
                                fprintf f "inherits\n%s\n%s\n" l typ;
                                fprintf f "%d\n" (List.length cls_features);
                                List.iter output_feature cls_features

        and output_identifier (line_number, string_lexeme) =
                fprintf f "%s\n%s\n" line_number string_lexeme

        and output_type (line_number, string_type) =
                fprintf f "%s\n%s\n" line_number string_type
        (*Print features of class*)
        and output_feature res = 
                match res with
                | AttributeNoInit(attr_name, type_name) ->
                                fprintf f "attribute_no_init\n" ;
                                output_identifier attr_name;
                                output_identifier type_name
                | AttributeInit(attr_name, type_name, init_exp) ->
                                fprintf f "attribute_init\n" ;
                                output_identifier attr_name;
                                output_identifier type_name;
                                output_exp init_exp
                | MethodArgs(method_name, args, type_name, exp) ->
                                fprintf f "method\n" ;
                                output_identifier method_name;
                                fprintf f "%d\n" (List.length args);
                                List.iter output_arg args;
                                output_identifier type_name;
                                output_exp exp
        (*Print the argument*)
        and output_arg arg =
                match arg with
                | Arg(name, typ) -> 
                        let l_n, arg_name = name in
                        let l_t, arg_typ = typ in
                        fprintf f "%s\n%s\n%s\n%s\n" l_n arg_name l_t arg_typ;
                | NoArg ->  
                                fprintf f "0\n";
        (*Print each binding in let statement*)
        and output_binding res =
                match res with 
                | BindingNoInit(id, typ) ->
                        fprintf f "let_binding_no_init\n";
                        output_identifier id;
                        output_type typ
                | BindingInit(id, typ, exp) ->
                        fprintf f "let_binding_init\n";
                        output_identifier id;
                        output_type typ;
                        output_exp exp
        (*Print case element in case expression*)
        and output_ele res =
                match res with
                | CaseEle(id, typ, expr) ->
                        output_identifier id;
                        output_type typ;
                        output_exp expr
        (*Print each type of expression*)
        and output_exp (line, inner_exp) =
                fprintf f "%s\n" line ;
                match inner_exp with
                | Integer(int_str) -> fprintf f "integer\n%s\n" int_str
                | String(str) -> fprintf f "string\n%s\n" str
                | True -> fprintf f "true\n" 
                | False -> fprintf f "false\n" 
                | Identifier(id) ->
                                fprintf f "identifier\n";
                                output_identifier id
                | Plus(expr1, expr2) -> 
                                fprintf f "plus\n" ;
                                output_exp expr1 ;
                                output_exp expr2
                | Times(expr1, expr2) -> 
                                fprintf f "times\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Minus(expr1, expr2) -> 
                                fprintf f "minus\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Divide(expr1, expr2) -> 
                                fprintf f "divide\n" ;
                                output_exp expr1;
                                output_exp expr2
                | If(precondition, then_expr, else_expr) ->
                                fprintf f "if\n" ;
                                output_exp precondition;
                                output_exp then_expr;
                                output_exp else_expr
                | While(precondition, body_expr) -> 
                                fprintf f "while\n" ;
                                output_exp precondition;
                                output_exp body_expr
                | Assign(expr1, expr2) ->
                                fprintf f "assign\n" ;
                                output_identifier expr1;
                                output_exp expr2
                | Lt(expr1, expr2) -> 
                                fprintf f "lt\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Le(expr1, expr2) -> 
                                fprintf f "le\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Eq(expr1, expr2) -> 
                                fprintf f "eq\n" ;
                                output_exp expr1;
                                output_exp expr2
                | Not(expr)  ->    
                                fprintf f "not\n" ;
                                output_exp expr
                | Negate(expr)  ->    
                                fprintf f "negate\n" ;
                                output_exp expr
                | Isvoid(expr) ->
                                fprintf f "isvoid\n" ;
                                output_exp expr
                | New(expr) ->
                                fprintf f "new\n" ;
                                output_identifier expr
                | Block(expr) ->
                                fprintf f "block\n" ;
                                fprintf f "%d\n" (List.length expr);
                                List.iter output_exp expr
                | Dyn_Dispatch(expr, id, args) -> 
                                fprintf f "dynamic_dispatch\n";
                                output_exp expr;
                                output_identifier id;
                                fprintf f "%d\n" (List.length args);
                                List.iter output_exp args
                | Self_Dispatch(id, args) -> 
                                fprintf f "self_dispatch\n";
                                output_identifier id;
                                fprintf f "%d\n" (List.length args);
                                List.iter output_exp args
                | Static_Dispatch(expr, typ, id, args) -> 
                                fprintf f "static_dispatch\n";
                                output_exp expr;
                                output_type typ;
                                output_identifier id;
                                fprintf f "%d\n" (List.length args);
                                List.iter output_exp args
                | Let(bindings, expr)     ->
                                fprintf f "let\n";
                                fprintf f "%d\n" (List.length bindings);
                                List.iter output_binding bindings;
                                output_exp expr;
                | Case(expr, elelist)   ->
                                fprintf f "case\n";
                                output_exp expr;
                                fprintf f "%d\n" (List.length elelist);
                                List.iter output_ele elelist
        in
        output_app res ;
        close_out f
end ;;
main() ;;
