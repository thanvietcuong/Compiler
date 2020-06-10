(*PA5 *)
open Printf

(* TODO: add line number *)
type cool_type = string
type static_type = 
    | Class of string (*"Int" or "Object" *)
    | SELF_TYPE (* of string -- SELF_TYPE_c *)
and loc = string
and exp = {
    loc: loc;
    exp_kind: exp_kind;
    static_type: static_type;
}
and exp_kind = 
    | Internal of string
    | New of string (*new Point *)
    | Dispatch of exp * string * exp list (* self.foo(a1,...an) *)
    | SelfDispatch of string * exp list
    | StaticDispatch of exp * string * string * exp list
    | Variable of string (* x *)
    | Assign of string * exp (* x <- 2 + 2 *)
    | Integer of Int32.t (* 4 *)
    | Plus of exp * exp (* 4 + 1 *)
    | Minus of exp * exp
    | Times of exp * exp
    | Divide of exp * exp
    | While of exp * exp (* while(true) loop { exp } pool; *)
    | LessThan of exp * exp (* x < 3 *)
    | LessThanOrEq of exp * exp (* x <= 3 *)
    | Equals of exp * exp (* x = 3 *)
    | True
    | False
    | Negate of exp
    | Not of exp
    | IsVoid of exp
    | Block of exp list
    | If of exp * exp * exp
    | String of string
    | Case of exp  * (case_element list) 
    | VoidExpr
    | Let of binding list * exp

and binding = 
    | Binding of string * string * exp
and case_element = string * cool_type * exp

type cool_address = int
and cool_value =
    | Cool_Int of Int32.t
    | Cool_Bool of bool
    | Cool_String of Int32.t * string (* length and the value itself *)
    | Cool_Object of string* cool_address * ((string * cool_address) list) (* x(a1=l1, ... an=ln) *)
    | Void


(* Goal of our interpreter is to boil down Expressions into values
 * (and to udate the store) *)

(* Environment: Maps variable names -> cool addresses 
 * Assosiation list:
     * if x live at 2 and y at 9, we would have [("x", 2)  ; ("y",7) ]*)

type environment = (string * cool_address) list

(* Store: Maps cool_address -> cool values *)
type store = (cool_address * cool_value) list

(* Class Map:
    * Maps "Class name" -> "Attribute name and attribute Initializers"
    * FIXME: do types *)
type attribute = string * cool_type * exp
type class_map = (string * (attribute list)) list

(* Impl Map:
    * Maps ("Class name", "Method name") to 
    * the method formal parameter names
    * the method body
    * FIXME: check the full version *)
type imp_map = 
    (* class name, method name *)
    (( string * string)
    *
    (* formals, parent and the method body *)
    (( string list) * string * exp)) list

type parent_map = (string * string) list

exception ClosestAncestorFound of case_element;;

let new_location_counter = ref 1000
let newloc () = 
    incr new_location_counter;
    !new_location_counter 

(* String to int methods *)
let string_rev str =
  let rec aux  idx = match idx with
      0 -> Char.escaped (str.[0])
    | _ -> (Char.escaped str.[idx]) ^ (aux (idx-1)) in
  aux ((String.length str)-1) 
;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let is_num = function
  | '0' | '1' | '2' | '3' | '4' | '5'
  | '6' | '7' | '8' | '9' | '-'  -> true
  | _ -> false 
;;

let rec aux result_str = function
  | [] -> result_str
  | h :: t  -> if is_num h then 
        aux((Char.escaped h) ^ result_str) t
      else
      if String.length result_str = 0 then "0" else result_str
;;

let get_int_string str =
  let chars = explode str in  
  let res = aux "" chars in
  string_rev res 
;;


let class_map: class_map ref = ref [];;
let imp_map: imp_map ref = ref [];;
let parent_map: parent_map ref = ref [];;
let cnt_obj = ref 0;;
let cnt_new = ref 0;;
(*****  Debugging and Tracing *****)
let do_debug = ref false
let debug fmt =
    let handle result_string =
        if !do_debug then printf "%s" result_string
    in
    kprintf handle fmt

(*****  Utility methods *****)
(* Get ancestors *)
let rec get_ancestors cname =
    debug "get_ancestors for %s\n" cname;
    match cname with
    | "Object" -> []  (* Object has no parent *)
    | _ -> 
        let ancestor = List.assoc cname !parent_map in 
        debug "ancestor= %s\n" ancestor;
        match ancestor with
            | "Object" -> ["Object"] (* Return 'Object' as the top ancestor *)
            | _ -> ancestor :: (get_ancestors ancestor)
        
let rec exp_to_str e =
    match e.exp_kind with
    | New(s) -> sprintf "New(%s)" s
    | Internal(s) -> sprintf "Internal(%s)" s
    | Let(bindings, exp) ->
            let binding_str = List.fold_left (fun acc elt ->
                match elt with
                | Binding(id, typ, bind_exp) ->
                    acc ^ ", " ^ id ^ " : " ^ typ ^ " - " ^ (exp_to_str bind_exp)
            ) "" bindings in
            sprintf "Let([%s],%s)" binding_str (exp_to_str exp)
    | SelfDispatch(fname, args) -> 
            let arg_str = List.fold_left (fun acc elt -> 
                acc ^ ", " ^ (exp_to_str elt)
            ) "" args in
            sprintf "SelfDispatch(%s,[%s])" fname arg_str
    | StaticDispatch(e0, tname, fname, args) -> 
            let arg_str = List.fold_left (fun acc elt -> 
                acc ^ ", " ^ (exp_to_str elt)
            ) "" args in
            sprintf "SelfDispatch(%s,%s,%s,[%s])" (exp_to_str e0) tname fname arg_str
    | Dispatch(ro,fname,args) -> 
            let arg_str = List.fold_left (fun acc elt -> 
                acc ^ ", " ^ (exp_to_str elt)
            ) "" args in
            sprintf "Dispatch(%s,%s,[%s])" (exp_to_str ro) fname arg_str
    | Variable(x) -> sprintf "Variable(%s)" x
    | Assign(x, e) -> sprintf "Assign(%s,%s)" x (exp_to_str e)
    | Integer(i) -> sprintf "Integer(%ld)" i
    | Plus(e1, e2) -> sprintf "Plus(%s, %s)" (exp_to_str e1) (exp_to_str e2)
    | Minus(e1, e2) -> sprintf "Minus(%s, %s)" (exp_to_str e1) (exp_to_str e2)
    | Times(e1, e2) -> sprintf "Times(%s, %s)" (exp_to_str e1) (exp_to_str e2)
    | Divide(e1, e2) -> sprintf "Divide(%s, %s)" (exp_to_str e1) (exp_to_str e2)
    | True -> sprintf "true"
    | False -> sprintf "false"
    | Negate(e) -> sprintf "Negate(%s)" (exp_to_str e)
    | Not(e) -> sprintf "Not(%s)" (exp_to_str e)
    | While(e1, e2) -> sprintf "While(%s, %s)" (exp_to_str e1) (exp_to_str e2)
    | LessThan(e1, e2) -> sprintf "LessThan(%s, %s)" (exp_to_str e1) (exp_to_str e2)
    | LessThanOrEq(e1, e2) -> sprintf "LessThanOrEq(%s, %s)" (exp_to_str e1) (exp_to_str e2)
    | Equals(e1, e2) -> sprintf "Equals(%s, %s)" (exp_to_str e1) (exp_to_str e2)
    | IsVoid(e) -> sprintf "IsVoid(%s)" (exp_to_str e)
    | Block(exps) -> 
            let exp_str = List.fold_left (fun acc elt -> 
                acc ^ ", " ^ (exp_to_str elt)
            ) "" exps in
            sprintf "Block([%s])" exp_str
    | If(e1, e2, e3) -> sprintf "If(%s, %s, %s)" (exp_to_str e1) (exp_to_str e2) (exp_to_str e3)
    (* do we need output the length of string? *)
    | String(s) -> sprintf "String(%s)" s
    | Case(e, cases) ->
            let exp_str = exp_to_str e in
            let cases_str = List.fold_left (fun prevCases currCase -> 
                let (cName, cType, exp) = currCase in
                prevCases ^ ";\n " ^ cName ^ " : " ^ cType ^ " => " ^ (exp_to_str exp)
            ) "" cases in 
            sprintf "Case(%s of [%s])" exp_str cases_str
    | VoidExpr -> sprintf "void"
    | Case(e, cases) ->
            let exp_str = exp_to_str e in
            let cases_str = List.fold_left (fun prevCases currCase -> 
                let (cName, cType, exp) = currCase in
                prevCases ^ ";\n " ^ cName ^ " : " ^ cType ^ " => " ^ (exp_to_str exp)
            ) "" cases in 
            sprintf "Case(%s of [%s])" exp_str cases_str
    | x -> 
        failwith ("cannot print: unhandled exp_kind:")

let value_to_str v =
    match v with
    | Cool_Int(i) -> sprintf "Int(%ld)" i
    | Cool_Bool(b) -> sprintf "Bool(%b)" b
    (* FIXME: add length *)
    | Cool_String(l, s) -> sprintf "String(%s)" s
    | Void -> sprintf "Void"
    | Cool_Object(cname, _, attrs) -> 
            let attr_str = List.fold_left (fun acc (aname, aaddr) ->
                sprintf "%s, %s=%d" acc aname aaddr
            ) "" attrs in
            sprintf "%s([%s])" cname attr_str

let env_to_str env =
    let binding_str = List.fold_left (fun acc (aname, aaddr) ->
        sprintf "%s, %s=%d" acc aname aaddr
    ) "" env in
    sprintf "[%s]" binding_str

let store_to_str store =
    let binding_str = List.fold_left (fun acc (addr, cvalue) ->
        sprintf "%s, %d=%s" acc addr (value_to_str cvalue)
    ) "" store in
    sprintf "[%s]" binding_str

let spec_char_to_str = function
  | '\n' -> "\n"
  | '\t' -> "\t"
  | s -> Char.escaped s
;;
  
let chars_to_str chars = 
  let rec aux result = function
    | [] -> result
    | h::t -> aux (result ^ (spec_char_to_str h)) t  in
  aux "" chars
;;

let normalize str = 
    let chars = explode str in 
    let res = ref "" in 
    let prev = ref false in
    List.iter( fun c -> 
        if !prev then begin 
        match c with
        | 'n' -> res := !res ^ "\n"
        | 't' -> res := !res ^ "\t"
        | _ -> 
            res := !res ^ "\\";
            if c = '\\' then prev := true
            else begin 
                prev := false;
                let news = sprintf "%c" c in
                res := !res ^ news;
            end
        end

        else begin
            match c with
            | '\\' -> prev := true
            | _ -> let news = sprintf "%c" c in
                res := !res ^ news;
        end;
        if c != '\\' then prev := false;
    )chars;
    if !prev then res := !res ^ "\\";
    !res

let indent_count = ref 0
let debug_indent () = 
        debug "%s" (String.make !indent_count ' ')

let rec eval (so : cool_value) (* self object *)
         (s : store) (* store =  memory; maps addresses to values *)
         (e : environment) (* maps variables to addresses *)
         (exp : exp) (* the expression to evaluate *)
         :
         (cool_value * store) (* result value, updated store *)
         =
    indent_count := !indent_count + 2;
    debug "\n";
    debug_indent () ; debug "eval: %s\n" (exp_to_str exp);
    debug_indent () ; debug "self= %s\n" (value_to_str so);
    debug_indent () ; debug "stor= %s\n" (store_to_str s);
    debug_indent () ; debug "envi= %s\n" (env_to_str e);
  
    let new_value, new_store = match exp.exp_kind with
    | Integer(i) -> Cool_Int(i), s
    | True -> Cool_Bool(true), s
    | False -> Cool_Bool(false), s
    | VoidExpr -> Void, s
    | Negate(e1) -> 
        let value, s1 = eval so s e e1 in
        let result_value = match value with 
            | Cool_Int(i) -> Cool_Int(Int32.neg i)
            | _ -> failwith "Cannot happen"
        in 
        result_value, s1
    | Not(exp) -> 
        let v1, s2 = eval so s e exp in
        let final_value = match v1 with
        | Cool_Bool(b) -> Cool_Bool(not b)
        | _ -> failwith "expression in Not must be evaluated as Bool\n"
        in
        final_value, s2
    | IsVoid(exp) ->
        let v1, s2 = eval so s e exp in
        let final_value = match v1 with
        | Void -> Cool_Bool(true)
        | _ -> Cool_Bool(false)
        in
        final_value, s2
    | Block(exps) -> 
        let s2 = ref s in
        let v1 = ref (Cool_Bool(true)) in
        List.iter (fun exp -> 
            let tmp_var, tmp_store = eval so !s2 e exp in
            s2 := tmp_store;
            v1 := tmp_var;
        ) exps;
        !v1, !s2
    | If(exp1, exp2, exp3) -> 
        let v1, s2 = eval so s e exp1 in
        let final_value, final_store = match v1 with
        | Cool_Bool(b) -> 
            if b = true then
                let v2, s3 = eval so s2 e exp2 in
                v2, s3
            else
                let v2, s3 = eval so s2 e exp3 in
                v2, s3
        | _ -> failwith "condition expression in If must be evaluated as Bool\n"
        in
        final_value, final_store
    | String(str) ->
        let l = Int32.of_int (String.length str) in
        Cool_String(l, str), s
    | Plus(e1, e2) ->
        let v1, s2 = eval so s e e1 in
        let v2, s3 = eval so s2 e e2 in
        let result_value = match v1,v2 with
        | Cool_Int(i1), Cool_Int(i2) -> 
            Cool_Int(Int32.add i1 i2)
        | _, _ -> failwith "impossible in plus"
        in
        result_value, s3
    | Minus(e1, e2) ->
        let v1, s2 = eval so s e e1 in
        let v2, s3 = eval so s2 e e2 in
        let result_value = match v1,v2 with
        | Cool_Int(i1), Cool_Int(i2) -> 
            Cool_Int(Int32.sub i1 i2)
        | _, _ -> failwith "impossible in minus"
        in
        result_value, s3
    | Times(e1, e2) ->
        let v1, s2 = eval so s e e1 in
        let v2, s3 = eval so s2 e e2 in
        let result_value = match v1,v2 with
        | Cool_Int(i1), Cool_Int(i2) -> 
            Cool_Int(Int32.mul i1 i2)
        | _, _ -> failwith "impossible in times"
        in
        result_value, s3
    | Divide(e1, e2) ->
        let v1, s2 = eval so s e e1 in
        let v2, s3 = eval so s2 e e2 in
        let result_value = match v1,v2 with
        | Cool_Int(i1), Cool_Int(i2) -> 
            if i2 = 0l then begin
                printf "ERROR: %s: Exception: division by zero\n" exp.loc;
                exit 1
            end
            else 
                Cool_Int(Int32.div i1 i2)
        | _, _ -> failwith "impossible in divide"
        in
        result_value, s3
    | Assign(vname, rhs) ->
        let v1, s2 = eval so s e rhs in
        let l1 = List.assoc vname e in (* E[vname] *)
        let s3 = (l1, v1) :: (List.remove_assoc l1 s2) in
        v1, s3
    | LessThan(e1, e2) ->
        let v1, s2 = eval so s e e1 in
        let v2, s3 = eval so s2 e e2 in
        let final_value, final_store = match v1, v2 with
        | Cool_Bool(b1), Cool_Bool(b2) ->
            if b1 < b2 then
                Cool_Bool(true), s3
            else
                Cool_Bool(false), s3
        | Cool_Int(i1), Cool_Int(i2) ->
            if i1 < i2 then
                Cool_Bool(true), s3
            else
                Cool_Bool(false), s3
        | Cool_String(l1, s1), Cool_String(l2, s2) ->
            if s1 < s2 then
                Cool_Bool(true), s3
            else
                Cool_Bool(false), s3
        | Void, Void ->
                Cool_Bool(false), s3
        | Cool_Object(_,_, _), Cool_Object(_,_, _) ->
                Cool_Bool(false), s3
        | _, _ -> Cool_Bool(false), s3
        in 
        final_value, final_store
    | LessThanOrEq(e1, e2) ->
        let v1, s2 = eval so s e e1 in
        let v2, s3 = eval so s2 e e2 in
        let final_value, final_store = match v1, v2 with
        | Cool_Bool(b1), Cool_Bool(b2) ->
                Cool_Bool(b1 <= b2), s3
        | Cool_Int(i1), Cool_Int(i2) ->
                Cool_Bool(i1 <= i2), s3
        | Cool_String(l1, s1), Cool_String(l2, s2) ->
                Cool_Bool(s1 <= s2), s3
        | Void, Void ->
                Cool_Bool(true), s3
        | Cool_Object(cname1, caddr1,_), Cool_Object(cname2, caddr2,_) ->
                Cool_Bool(caddr1 = caddr2), s3
        | _, _ -> Cool_Bool(false), s3
        in
        final_value, final_store
    | Equals(e1, e2) ->
        let v1, s2 = eval so s e e1 in
        let v2, s3 = eval so s2 e e2 in
        let final_value, final_store = match v1, v2 with
        | Void, Void ->
                Cool_Bool(true), s3
        | Void, _ -> 
                Cool_Bool(false), s3
        | _, Void -> 
                Cool_Bool(false), s3
        | Cool_Bool(b1), Cool_Bool(b2) ->  
                Cool_Bool(b1 = b2), s3
        | Cool_Int(i1), Cool_Int(i2) ->
                Cool_Bool(i1 = i2), s3
        | Cool_String(l1, s1), Cool_String(l2, s2) ->
                Cool_Bool(s1 = s2), s3
        | Cool_Object(cname1, caddr1,_), Cool_Object(cname2, caddr2,_) ->
                Cool_Bool(caddr1 = caddr2), s3
        
        | _, _ -> failwith "impossible in Equals"
        in
        final_value, final_store
    | Let(bindings, body) -> begin
        let current_store = ref s in
        let current_env = ref e in
        List.iter (fun binding ->
            match binding with
            | Binding(id, tname, bind_exp) -> begin
                debug "!!! let eval \n";
                let v0, s0 = eval so !current_store !current_env bind_exp in
                debug "!!! assoc \n";
                let l = newloc() in 
                let envir_update = List.combine [id] [l] in
                current_env := envir_update @ !current_env;
                (* FIXME: we might need to remove newly added pairs (l1, v0) after we have evaluated let body *)
                current_store := (l, v0) :: s0
            end
            | _ -> failwith "Let. will never happen"
        ) bindings;
        eval so !current_store !current_env body
    end;

    | While(e1, e2) ->
        let v1, s2 = eval so s e e1 in
        let final_store = match v1 with
        | Cool_Bool(b) ->
            if b = true then begin
                let _, s3 = eval so s2 e e2 in
                let while_exp = {loc=exp.loc; exp_kind = While(e1, e2); static_type = exp.static_type} in
                let _, s4 = eval so s3 e while_exp in
                s4
            end
            else s2
        | _ -> failwith "condition in the while loop is not evaluated to Bool\n"
        in
        Void, final_store
    | New(tname) ->
        (* FIXME: What if it's absent? *)
        cnt_new := !cnt_new + 1;
        if !cnt_new > 1000 then begin
            printf "ERROR: %s: Exception: stack overflow\n" exp.loc;
            exit 1
        end;
        let cname = begin match tname with
            | "SELF_TYPE" -> begin match so with
                | Cool_Object(cname1, _, _) -> cname1
                | _ -> failwith "Impossible, Typename unhandled so far\n"
            end
            | class_name -> class_name
        end in
        debug_indent () ; debug "New cname: %s\n" cname;
        begin match cname with
        | "Bool" -> Cool_Bool(false), s
        | "Int" -> Cool_Int(0l), s
        | "String" -> Cool_String(0l, ""), s
        | _ -> 
            let attrs_and_inits = List.assoc cname !class_map in 
            let new_attr_locs = List.map (fun (aname, atype, ainit) ->
                newloc()
            ) attrs_and_inits in
            let attr_names = List.map (fun (aname, atype, ainit) ->
                aname
            ) attrs_and_inits in
            let attr_and_locs = List.combine attr_names new_attr_locs in
            let nloc = newloc() in
            let v1 = Cool_Object(cname, nloc, attr_and_locs) in
            (* FIXME: Default values *)
            let store_updates = List.map (fun newloc ->
                (newloc, Cool_Int(0l)) (* SHOULD BE: default value *)
            ) new_attr_locs in
            let s2 = s @ store_updates in
            let final_store = ref (List.fold_left (fun accumulated_store(aname, atype, ainit) ->
                let assign_exp = {loc=exp.loc; exp_kind = Assign(aname, ainit); static_type = exp.static_type} in
                let _, updated_store = eval v1 accumulated_store attr_and_locs assign_exp in
                updated_store
            ) s2 attrs_and_inits) in
            final_store := (nloc, v1) :: !final_store;  
            v1, !final_store
        end
    | Variable(vname) ->
        begin match vname with
        | "self" -> so, s   (*return self object as cool_value*)
        | _ ->  
            try 
                let l = List.assoc vname e in
                let final_value = List.assoc l s in
                final_value, s
            with _ -> begin
                printf "%s not found\n" vname;
                exit 1
            end;
            
        end
    | Internal(fname) ->
        begin match fname with
        | "IO.out_int" ->
            let addr = List.assoc "x" e in
            let value = List.assoc addr s in
            begin
                match value with 
                | Cool_Int(i) -> 
                    printf "%ld" i; 
                    flush_all ();
                | _ -> failwith "Impossible! Cannot print a non-int value\n"
            end;
            so, s
        | "IO.in_int" -> 
            begin                 
                try                   
                    let i = read_line () in
                    let trimmed_str = String.trim i in
                    let trimmed_str = if String.length trimmed_str = 0 then "\n" else trimmed_str in
                    let int_str = get_int_string trimmed_str in
                    (Cool_Int (Int32.of_string(int_str))), s                                                                                                                                                         
                with _ ->                                                                                                                                                                                            
                    Cool_Int (0l), s                                                                                                                                                                                 
                end
        | "IO.out_string" -> 
            let addr = List.assoc "x" e in
            let value = List.assoc addr s in
            begin
                match value with 
                | Cool_String(l, str) -> 
                        printf "%s" str;
                        flush_all ();
                | _ -> failwith "Impossible! Cannot print a non-string value\n"
            end;
            so, s
        | "IO.in_string" -> 
            begin
            try    
                let read_i = read_line() in
                let i = normalize read_i in 
                let l = Int32.of_int(String.length i) in
                begin try 
                    let t = String.index i (char_of_int 0) in 
                        Cool_String(0l, ""), s
                with Not_found -> let new_val = Cool_String(l, i) in
                    new_val, s
                end
            with
                _ -> Cool_String(0l, ""), s
            end
        | "Object.abort" -> 
            eprintf "abort\n";
            exit 1
        | "Object.type_name" ->
            begin match so with
                | Cool_Int(i) -> Cool_String(3l, "Int"), s
                | Cool_Bool(b) -> Cool_String(4l, "Bool"), s
                | Cool_String(l, str) -> Cool_String(6l, "String"), s
                | Cool_Object(cname, _, attrs) -> Cool_String(Int32.of_int (String.length cname), cname), s
            end
        | "Object.copy" -> 
            begin match so with
                | Cool_Int(i) -> so, s
                | Cool_Bool(b) -> so, s
                | Cool_String(l, str) -> so, s
                | Void -> so, s
                | Cool_Object(cname, _, attrs) -> 
                    let attr_names = List.map (fun (aname, aaddr) ->
                        aname
                    ) attrs in
                    let attr_addrs = List.map (fun (aname, aaddr) ->
                        aaddr
                    ) attrs in
                    let new_locs = List.map (fun (aname, aaddr) ->
                        newloc()
                    ) attrs in
                    let attrname_and_locs = List.combine attr_names new_locs in
                    (*Fix me: What if it does not work???*)
                    let nloc = newloc() in
                    let v1 = Cool_Object(cname, nloc, attrname_and_locs) in
                    let addr_and_locs = List.combine attr_addrs new_locs in
                    let store_updates = List.map (fun (aaddr,newLoc) ->
                        let value = List.assoc aaddr s in
                        (newLoc, value) 
                    ) addr_and_locs in
                    let s1 = s @ store_updates in 
                    let s2 = (nloc, v1) :: s1 in
                    v1, s2
            end
        | "String.length" -> 
            begin match so with 
                | Cool_String (l, str) -> Cool_Int(l), s
                | _ -> failwith "Cannot get lenght of a non-string"
            end
        | "String.concat" -> 
            let saddr = List.assoc "s" e in
            let value = List.assoc saddr s in
            begin match so, value with 
                | Cool_String (l, str), Cool_String(l1, str1) -> 
                    let new_l = Int32.add l l1 in
                    let new_str = String.concat "" [str; str1] in
                    Cool_String(new_l, new_str), s
                | _, _ -> failwith "Cannot concat a non-string"
            end
        | "String.substr" -> 
            let iaddr = List.assoc "i" e in
            let i = List.assoc iaddr s in
            let laddr = List.assoc "l" e in
            let l = List.assoc laddr s in
            begin match so, i, l with 
                | Cool_String (length, str), Cool_Int(ival), Cool_Int(lval) -> 
                    let end_str = Int32.sub (Int32.add ival lval) 1l in
                    if end_str >= length || lval < 0l then begin
                        printf "ERROR: %s: Exception: String.substr out of range\n" exp.loc;
                        exit 1
                    end
                    else
                    Cool_String(lval, String.sub str (Int32.to_int ival) (Int32.to_int lval)), s
                | _, _, _ -> failwith "Cannot concat a non-string"
            end
        | _ -> failwith "Unhandled so far"
        end
    (* self.foo(a1, ... an) *)
    | SelfDispatch(fname, args) ->
        debug_indent () ; debug "eval SelfDispatch fname: %s\n" fname;
        cnt_obj := !cnt_obj + 1;
        if !cnt_obj > 1000 then begin
            printf "ERROR: %s: Exception: stack overflow\n" exp.loc;
            exit 1
        end;
        let current_store = ref s in
        let arg_values = List.map (fun arg_exp ->
            let arg_value, new_store = eval so !current_store e arg_exp in
            current_store := new_store;
            arg_value
        ) args in
        begin match so with
        | Cool_Object(x, _, attrs_and_locs) ->
            (*FIXME: check to make sure it is there *)
            let formals, parent, body = List.assoc (x, fname) !imp_map in
            let new_arg_locs = List.map (fun arg_exp ->
                newloc()
            ) args in
            let store_update = List.combine new_arg_locs arg_values in
            let s_n3 = store_update @ !current_store in
            let envir_update = List.combine formals new_arg_locs in
            let new_envir = envir_update @ attrs_and_locs in
            let ret_v, ret_s = eval so s_n3 new_envir body in
            cnt_obj := !cnt_obj - 1;
            ret_v, ret_s
        | Void -> begin
                printf "ERROR: %s: Dispatch from a void object1" exp.loc;
                exit 1
            end
        | _ -> failwith "not handled yet SelfDispatch"
        end 
    | StaticDispatch(e0, static_tname, fname, args) ->
        debug_indent () ; debug "eval StaticDispatch tname: %s, fname: %s\n" static_tname fname;
        cnt_obj := !cnt_obj + 1;
        if !cnt_obj > 1000 then begin
            printf "ERROR: %s: Exception: stack overflow\n" exp.loc;
            exit 1
        end;
        let current_store = ref s in
        let arg_values = List.map (fun arg_exp ->
            let arg_value, new_store = eval so !current_store e arg_exp in
            current_store := new_store;
            arg_value
        ) args in
        (*!current_store = s_n in the CRM  *)
        let v0, s_n2 = eval so !current_store e e0 in
        debug_indent () ; debug "v0: %s\n" (value_to_str v0);
        debug_indent () ; debug "s_n2: %s\n" (store_to_str s_n2);
        begin match v0 with
        | Cool_Object(x, _, attrs_and_locs) ->
            (*FIXME: check to make sure it is there *)
            let formals, parent, body = List.assoc (static_tname, fname) !imp_map in
            let new_arg_locs = List.map (fun arg_exp ->
                newloc()
            ) args in
            let store_update = List.combine new_arg_locs arg_values in
            let s_n3 = store_update @ s_n2 in
            let envir_update = List.combine formals new_arg_locs in
            let new_envir = envir_update @ attrs_and_locs in
            let ret_v, ret_s = eval v0 s_n3 new_envir body in
                cnt_obj := !cnt_obj - 1;
                ret_v, ret_s
        | Void -> begin
                printf "ERROR: %s: Dispatch from a void object" exp.loc;
                exit 1
            end
        | _ -> failwith "not handled yet StaticDispatch"
        end 
    (* Cool interpreter starts from here by dispatching main() fn. *)
    | Dispatch(e0, fname, args) ->
        cnt_obj := !cnt_obj + 1;
        if !cnt_obj > 1000 then begin
            printf "ERROR: %s: Exception: stack overflow\n" exp.loc;
            exit 1
        end;
        let current_store = ref s in
        let arg_values = List.map (fun arg_exp ->
            let arg_value, new_store = eval so !current_store e arg_exp in
            current_store := new_store;
            arg_value
        ) args in
        (*!current_store = s_n in the CRM  *)
        let v0, s_n2 = eval so !current_store e e0 in
        debug_indent () ; debug "v0: %s\n" (value_to_str v0);
        debug_indent () ; debug "s_n2: %s\n" (store_to_str s_n2);
        begin match v0 with
        | Cool_Object(x, _,  attrs_and_locs) ->
            (*FIXME: check to make sure it is there *)
            let formals, parent, body = List.assoc (x, fname) !imp_map in
            let new_arg_locs = List.map (fun arg_exp ->
                newloc()
            ) args in
            let store_update = List.combine new_arg_locs arg_values in
            let s_n3 = store_update @ s_n2 in
            let envir_update = List.combine formals new_arg_locs in
            let new_envir = envir_update @ attrs_and_locs in
            let ret_v, ret_s = eval v0 s_n3 new_envir body in
                cnt_obj := !cnt_obj - 1;
                ret_v, ret_s
        | Cool_String(l, str) ->
            (*FIXME: check to make sure it is there *)
            let formals, parent, body = List.assoc ("String", fname) !imp_map in
            let new_arg_locs = List.map (fun arg_exp ->
                newloc()
            ) args in
            let store_update = List.combine new_arg_locs arg_values in
            let s_n3 = store_update @ s_n2 in
            let envir_update = List.combine formals new_arg_locs in
            let new_envir = envir_update in
            let ret_v, ret_s = eval v0 s_n3 new_envir body in
                cnt_obj := !cnt_obj - 1;
                ret_v, ret_s
        | Cool_Int(i) ->
            (*FIXME: check to make sure it is there *)
            let formals, parent, body = List.assoc ("Int", fname) !imp_map in
            let new_arg_locs = List.map (fun arg_exp ->
                newloc()
            ) args in
            let store_update = List.combine new_arg_locs arg_values in
            let s_n3 = store_update @ s_n2 in
            let envir_update = List.combine formals new_arg_locs in
            let new_envir = envir_update in
            let ret_v, ret_s = eval v0 s_n3 new_envir body in
                cnt_obj := !cnt_obj - 1;
                ret_v, ret_s
        | Cool_Bool(i) ->
            (*FIXME: check to make sure it is there *)
            let formals, parent, body = List.assoc ("Bool", fname) !imp_map in
            let new_arg_locs = List.map (fun arg_exp ->
                newloc()
            ) args in
            let store_update = List.combine new_arg_locs arg_values in
            let s_n3 = store_update @ s_n2 in
            let envir_update = List.combine formals new_arg_locs in
            let new_envir = envir_update in
            let ret_v, ret_s = eval v0 s_n3 new_envir body in
                cnt_obj := !cnt_obj - 1;
                ret_v, ret_s
        | Void -> begin
                printf "ERROR: %s: Dispatch from a void object" exp.loc;
                exit 1
            end
        | _ -> failwith "not handled yet Dispatch"
        end
    | Case(e0, cases) -> 
        debug_indent () ; debug "eval Case: \n";
        let v0, s2 = eval so s e e0 in
        debug_indent () ; debug "v0: %s\n" (value_to_str v0);
        let x = begin match v0 with
            | Cool_Object(tname, _, attrs_and_locs) -> tname
            | Cool_String(l, str) -> "String"
            | Cool_Int(i) -> "Int"
            | Cool_Bool(f) -> "Bool"
            | Void -> begin
                printf "ERROR: %s: Case from a void object" e0.loc;
                exit 1
                end
            | _ -> failwith "not handled yet"
        end
        in

        debug_indent () ; debug "Cool_Object: %s\n" x;
        let v1, s3 = begin
                (*case_element = string * cool_type * exp*)                
                let xAncestors = get_ancestors x in
                let xList = x :: xAncestors in
                begin try List.iter(fun anc ->
                    begin try
                        let xCase = List.find(fun (_,cType,_) -> cType = anc) cases in
                        raise (ClosestAncestorFound xCase) 
                    with 
                    | ClosestAncestorFound(xCase) -> raise (ClosestAncestorFound xCase) 
                    | _ -> () (*do nothing*)
                    end
                ) xList;
                printf "ERROR: %s: No match for case" e0.loc;
                exit 1
                with 
                | ClosestAncestorFound(xCase)  -> 
                    let (xName,xType,xExp) = xCase in
                    let l0 = newloc() in
                    let store_update = List.combine [l0] [v0] in
                    let s3 = store_update @ s2 in
                    let envir_update = List.combine [xName] [l0] in
                    let current_env = ref e in
                    current_env := envir_update @ !current_env;
                    eval so s3 !current_env xExp
                end
        end
        in
        v1, s3
    | _ -> failwith "cannot evaluate: expression not supported"
    in 
    debug_indent () ; debug "ret_value = %s\n" (value_to_str new_value) ;
    debug_indent () ; debug "ret_store = %s\n" (store_to_str new_store) ;
    indent_count := !indent_count - 2;
    new_value, new_store

let main = begin

    (* Read Class Map *)
    (* De-Serialize the cl-type file *)
    let fname = Sys.argv.(1) in
        debug "reading file : %s\n" fname;
    let fin = open_in fname in

    let read () = 
        input_line fin (* FIX ME - \r\n *)
    in

    let read_id () = 
        let loc  = read() in
        let name  = read() in
        (loc, name)
    in

    let rec range k = 
        if k <= 0 then []
        else k :: (range (k-1))
    in 

    let read_list worker  =
        let k = int_of_string (read()) in
        debug "read_list k = %d\n" k;
        let lst = range k in
        List.map (fun _ -> worker()) lst
    in

    let rec read_cool_class_map () = 
        
        let class_maps  = read() in
        debug "reading class_maps : %s\n" class_maps;
        read_list read_class_map

    and read_class_map () =  (* Cool class map *)
        let cname = read () in
        debug "reading class_map : %s\n" cname;
        let attributes = read_list read_attribute in 
        (cname, attributes)
    
    and read_attribute () = 
        match read() with
        | "no_initializer" -> 
            let fname = read() in
            let ftype = read() in
            debug "reading no_initializer : %s, %s\n" fname ftype;
            let default = match ftype with
                | "String" -> {
                    loc = "0";
                    exp_kind = String("");
                    static_type = Class(ftype);
                }
                | "Int" -> {
                    loc = "0";
                    exp_kind = Integer(0l);
                    static_type = Class(ftype);
                }
                | "Bool" -> {
                    loc = "0";
                    exp_kind = False;
                    static_type = Class(ftype);
                }
                | _ -> {
                    loc = "0";
                    exp_kind = VoidExpr;
                    static_type = Class(ftype);
                }
            in
            (fname, ftype, default)
        | "initializer"-> 
            let fname = read() in
            let ftype = read() in
            debug "reading initializer : %s, %s\n" fname ftype;
            let finit = read_exp() in
            (fname, ftype, finit)
        | x -> failwith ("unknown attribute init: " ^x)
    
    and get_default_value = function
        | "String" as ftype -> {
            loc = "0";
            exp_kind = String("");
            static_type = Class(ftype);
        }
        | "Int" as ftype -> {
            loc = "0";
            exp_kind = Integer(0l);
            static_type = Class(ftype);
        }
        | "Bool" as ftype -> {
            loc = "0";
            exp_kind = False;
            static_type = Class(ftype);
        }
        (* add default values for object *)
        | _ as ftype -> {
            loc = "0";
            exp_kind = VoidExpr;
            static_type = Class(ftype);
        }

    and read_cool_imp_map () = 
        let imp_maps  = read() in
        debug "reading imp_maps : %s\n" imp_maps;
        let reader_list = read_list read_imp_map in
        List.flatten reader_list

    and read_imp_map () =  (* Cool implementation map *)
        let cname = read () in
        debug "reading imp_map : %s\n" cname;
        let methods = read_list read_method in
        let impl_map = ref [] in
        List.iter(fun (mname, formals, parent, body) ->
            let key = (cname, mname) in
            let value = (formals, parent, body) in 
            impl_map := (key, value) :: !impl_map
        ) methods;
        !impl_map

    and read_cool_parent_map () = 
        let parent_maps  = read() in
        debug "reading parent_maps : %s\n" parent_maps;
        read_list read_parent_map

    and read_parent_map () =  (* Cool parent map *)
        let cname = read () in
        debug "reading parent_map for : %s\n" cname;
        let parent = read () in
        (cname, parent)

    and read_method () = 
        let mname = read () in
        debug "reading method : %s\n" mname;
        let formals = read_list read in 
        let parent = read () in
        let body = read_exp () in
        (mname, formals, parent, body)

     and read_binding () =
        let binding_type = read () in 
        match binding_type with
        | "let_binding_init" ->
            let (_, name) = read_id () in
            let (_, typ) = read_id () in
            let exp = read_exp () in
            Binding(name, typ, exp)
        | "let_binding_no_init" -> begin
            let (_, name) = read_id () in
            let (_, typ) = read_id () in
            let default = get_default_value typ in
            Binding(name, typ, default)
        end

    (*CaseElement of string * cool_type * exp*)    
    and read_case_element () = 
        let (loc1, case_name) = read_id() in 
        let (loc2, case_type) = read_id() in
        let e = read_exp() in 
        (case_name, case_type, e) 

    and read_exp () = 
        let eloc = read() in 
        let staticType = match read() with
            | "SELF_TYPE" -> SELF_TYPE
            | x -> Class(x)
        in
        let ekind = match read () with
        | "internal" -> 
            let extraDetails = read () in  (* Object.abort, Object.copy, IO.out_string -> printf*)
            Internal(extraDetails)
        | "self_dispatch" -> 
            let (loc, name) = read_id () in
            let expr_list = read_list read_exp in
            SelfDispatch(name, expr_list)
        | "static_dispatch" -> 
            let e0 = read_exp () in
            let (_, tname) = read_id () in
            let (_, fname) = read_id () in
            let expr_list = read_list read_exp in
            StaticDispatch(e0, tname, fname, expr_list)
        | "dynamic_dispatch" -> 
            let e0 = read_exp () in
            let (_, fname) = read_id () in
            let expr_list = read_list read_exp in
            Dispatch(e0, fname, expr_list)
        | "integer" -> 
            let ival = read () in
            Integer(Int32.of_string(ival))
        | "string" -> 
            let sval = read () in
            let real_str = normalize sval in
            String(real_str)
        | "true" -> True
        | "false" -> False
        | "case" -> 
            let e = read_exp() in
            let elements = read_list read_case_element in 
            Case(e, elements)
        | "plus" -> 
            let e1 = read_exp () in
            let e2 = read_exp () in
            Plus(e1,e2)
        | "minus" -> 
            let e1 = read_exp () in
            let e2 = read_exp () in
            Minus(e1,e2)
        | "times" -> 
            let e1 = read_exp () in
            let e2 = read_exp () in
            Times(e1,e2)
        | "divide" -> 
            let e1 = read_exp () in
            let e2 = read_exp () in
            Divide(e1,e2)
        | "identifier" -> 
            let (loc, name) = read_id () in
            Variable(name)
        | "new" -> 
            let (_, name) = read_id () in
            New(name)
        | "block" ->
            let expr_list = read_list read_exp in
            Block(expr_list)
        | "assign" ->
            let (_, name) = read_id () in
            let e = read_exp () in
            Assign(name, e)
        | "let" -> begin
            (*let binding_num = read in *)
            (*let binding = read_binding () in *)
            let bindings = read_list read_binding in
            let body = read_exp () in
            Let(bindings, body) 
            end
        | "negate" -> 
            let e1 = read_exp() in
            Negate(e1)
        | "if" -> 
            let e1 = read_exp() in
            let e2 = read_exp() in
            let e3 = read_exp() in
            If(e1, e2, e3)
        | "lt" ->
            let e1 = read_exp() in
            let e2 = read_exp() in
            LessThan(e1, e2)
        | "le" -> 
            let e1 = read_exp() in
            let e2 = read_exp() in
            LessThanOrEq(e1, e2)
        | "eq" -> 
            let e1 = read_exp() in 
            let e2 = read_exp() in
            Equals(e1, e2)
        | "while" -> 
            let e1 = read_exp() in 
            let e2 = read_exp() in
            While(e1, e2)
        | "not" -> 
            let e1 = read_exp() in
            Not(e1)
        | "isvoid" ->
            let e1 = read_exp() in
            IsVoid(e1)
        | x -> (*TODO: other exp *)
            failwith ("unknown expr: " ^x) 
        in
        {
            loc = eloc;
            exp_kind = ekind;
            static_type = staticType;
        }
    in
    let cool_class_map = read_cool_class_map () in
    let cool_impl_map = read_cool_imp_map () in
    let cool_parent_map = read_cool_parent_map () in
    close_in fin; 

    class_map := cool_class_map @ !class_map ;
    imp_map := cool_impl_map @ !imp_map ;
    parent_map := cool_parent_map @ !parent_map ;

    (* Cool interpreter starts from here by dispatching main() fn. *)
    (*TODO: read class from a file since a valid cool program can have main method in a class with custom name *)
    let my_main = { 
        loc="0"; 
        exp_kind = New("Main");
        static_type= Class("Main")
    } 
    in
    let my_exp = { 
        loc="0"; 
        exp_kind = Dispatch(my_main, "main", []);
        static_type= Class("Object")
    } 
  in
    debug "my_exp = %s\n" (exp_to_str my_exp);
    let so = Void in
    let store = [] in
    let environment = [] in 
    let final_value, final_store = 
        eval so store environment my_exp in
        debug "result = %s\n" (value_to_str final_value)
end;;
main;;
