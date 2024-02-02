open Errors

type pattern =
  | WildcardPattern
  | ConsPattern of pattern * pattern
  | IntLitPattern of int
  | BoolLitPattern of bool
  | NilPattern
  | SymbolPattern of string
  | VariablePattern of string
  | StructPattern of string * pattern list
(* TODO: add more patterns here *)
[@@deriving show]
let string_of_pattern = show_pattern


let rec pattern_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
      match int_of_string_opt sym with
      | Some n -> IntLitPattern n
      | None ->
         match sym with
         | "_" -> WildcardPattern
         | "true" -> BoolLitPattern true
         (* TODO: add other cases here for "false" and "nil" *)
         | "false" -> BoolLitPattern false
         | "nil" -> NilPattern
         | _ ->
            if String.get sym 0 = '\'' (* if the string starts with an apostrophe *)
            then let sym_without_apostrophe = String.sub sym 1 (String.length sym - 1)
                 in SymbolPattern sym_without_apostrophe
            else VariablePattern sym
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected pattern but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "cons", [p1; p2] -> ConsPattern (pattern_of_pst p1, pattern_of_pst p2)
     | Pst.Symbol s, ps -> begin
      let rec struct_patterns_go acc list =
        match list with
        | [] -> acc
        | head::tail -> struct_patterns_go ((pattern_of_pst head)::acc) tail
      in
      StructPattern(s, List.rev (struct_patterns_go [] ps))
     end
     | _ -> raise (AbstractSyntaxError ("Expected pattern, but got " ^ Pst.string_of_pst p))
    
let pattern_of_string s =
  s
  |> Pstparser.pst_of_string
  |> pattern_of_pst


type dynamic_env = (string * env_entry) list
and
env_entry =
  | VarEntry of expr (* a value *)
and
expr =
  | IntLit of int
  | BoolLit of bool
  | Variable of string
  | Plus of expr * expr
  | If of expr * expr * expr
  | Nil
  | Cons of expr * expr
  | IsNil of expr
  | IsCons of expr
  (* TODO: add constructors for other expressions *)
  | Minus of expr * expr
  | Mult of expr * expr
  | Equals of expr * expr
  | Let of (string * expr) list * expr
  | Car of expr
  | Cdr of expr
  (* trefoil v3 additions *)
  | Cond of (expr * expr) list
  | FunctionCall of expr * (expr) list (* function to call, list of argument expressions *)
  (* trefoil v4 additions *)
  | Symbol of string
  | Print of expr
  | Closure of string option * string list * expr * dynamic_env (* optional name, name of args, body, defining env *)
  | Lambda of string list * expr (* name of args, body *)
  | StructConstructor of string * (expr list) (* struct name, args to constructor *)
  | StructPredicate of string * expr (* struct name, expression to evaluate *)
  | StructAccess of string * int * expr (* struct name, index of field, expr (struct to access) *)
  | Match of expr * (pattern * expr) list (* expression to match, clauses *)
[@@deriving show]
let string_of_expr = show_expr

let rec exist elem lst =
  match lst with
  | [] -> false
  | hd::tl -> elem = hd || exist elem tl

let rec dupExist lst =
  match lst with
  | [] -> false
  | hd::tl -> (exist hd tl) || dupExist tl
  
(* last stage of parser: converts pst to expr *)
let rec expr_of_pst p =
  let rec letdefs_go acc list : (string * expr) list=
  match list with
  | [] -> acc
  | Pst.Node([Pst.Symbol name; def])::tail ->
    begin 
      match List.assoc_opt name acc with
      | Some _ -> raise (AbstractSyntaxError ("Variable defined multiple times: "))
      | None -> letdefs_go ((name, expr_of_pst def)::acc) tail
    end
  | _ -> raise (AbstractSyntaxError ("Malformed let expression"))
  in
  let rec cond_go acc list : (expr * expr) list =
    match list with
    | [] -> acc
    | Pst.Node([case;expr])::tail -> cond_go ((expr_of_pst case, expr_of_pst expr)::acc) tail
    | _ -> raise (AbstractSyntaxError ("cond malformed"))
  in
  let rec fcall_go acc list : expr list =
    match list with
    | [] -> acc
    | arg::tail -> fcall_go (expr_of_pst arg::acc) tail
  in
  let rec params_go acc list =
    match list with 
    | [] -> acc
    | head::tail -> begin
      match head with
      | Pst.Symbol argname -> begin
        if List.exists (fun x -> x = argname) acc
        then raise (AbstractSyntaxError("Multiple params with same name"))
        else params_go (argname::acc) tail
        end
      | _ -> raise (AbstractSyntaxError("Lambda definition <args> are malformed"))
    end
  in
  match p with
  | Pst.Symbol sym -> begin
     try
       IntLit (int_of_string sym)
     with
       Failure _ ->
       match sym with
       | "true" -> BoolLit true
       (* TODO: add cases for other keywords here *)
       | "false" -> BoolLit false
       | "nil" -> Nil
       | _ -> if String.get sym 0 = '\''
              then Symbol (String.sub sym 1 (String.length sym - 1))
              else Variable sym
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected expression but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "+", [left; right] -> Plus (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "+", _ -> raise (AbstractSyntaxError ("operator + expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "if", [branch; thn; els] -> If (expr_of_pst branch, expr_of_pst thn, expr_of_pst els)
     | Pst.Symbol "if", _ -> raise (AbstractSyntaxError ("'if' special form expects 3 args but got " ^ Pst.string_of_pst p))
     (* TODO: add cases for other expressions here *)
     | Pst.Symbol "-", [left; right] -> Minus (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "-", _ -> raise (AbstractSyntaxError ("operator - expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "*", [left; right] -> Mult (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "*", _ -> raise (AbstractSyntaxError ("operator * expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "=", [left; right] -> Equals (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "=", _ -> raise (AbstractSyntaxError ("operator = expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "let", [Pst.Node(letdefs); exp] -> Let (List.rev (letdefs_go [] letdefs), expr_of_pst exp)
     | Pst.Symbol "let", _ -> raise (AbstractSyntaxError ("operator let expects 2 args but got " ^ Pst.string_of_pst p)) (* change *)
     | Pst.Symbol "cons", [left; right] -> Cons (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "cons", _ -> raise (AbstractSyntaxError ("operator cons expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "nil?", [arg] -> IsNil (expr_of_pst arg)
     | Pst.Symbol "nil?", _ -> raise (AbstractSyntaxError ("operator nil? expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cons?", [arg] -> IsCons (expr_of_pst arg)
     | Pst.Symbol "cons?", _-> raise (AbstractSyntaxError ("operator cons? expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "car", [arg] -> Car (expr_of_pst arg)
     | Pst.Symbol "car", _ -> raise (AbstractSyntaxError ("operator car expects 1 arg but got"))
     | Pst.Symbol "cdr", [arg] -> Cdr (expr_of_pst arg)
     | Pst.Symbol "cdr", _ -> raise (AbstractSyntaxError ("operator cdr expects 1 arg but got"))
     | Pst.Symbol "cond", condclauses -> Cond (List.rev (cond_go [] condclauses))
     | Pst.Symbol "print", [e] -> Print (expr_of_pst e)
     | Pst.Symbol "print", _ -> raise (AbstractSyntaxError ("malformed operator print expression"))
     | Pst.Symbol "lambda", [Pst.Node(params); body] -> Lambda(List.rev (params_go [] params), expr_of_pst body)
     | Pst.Symbol "lambda", _ -> raise (AbstractSyntaxError ("malformed lambda expression"))
     | Pst.Symbol "match", expression::match_clauses -> begin
      let rec vars_of_pattern patt =
        match patt with
        | VariablePattern x -> [x]
        | ConsPattern (x, y) -> (vars_of_pattern x) @ (vars_of_pattern y)
        | StructPattern(_, pts) ->
          let rec patt_helper acc list =
            match list with
            | [] -> acc
            | head::tail -> patt_helper (acc@(vars_of_pattern head)) tail
          in
          patt_helper [] pts
        | _ -> []
        in
        let rec match_clauses_go acc clauses = 
          match clauses with 
          | [] -> acc
          |Pst.Node([patt;clause])::tail -> begin
            if dupExist(vars_of_pattern (pattern_of_pst patt)) then raise (AbstractSyntaxError ("Nested patterns with same variable name."))
            else
            match_clauses_go ((pattern_of_pst patt, expr_of_pst clause) :: acc) tail
          end
          | _ -> raise (AbstractSyntaxError ("Expected match clause, but got " ^ Pst.string_of_pst p))
      in
      Match(expr_of_pst expression, List.rev (match_clauses_go [] match_clauses))
      end
     | f, args  -> FunctionCall (expr_of_pst f, List.rev (fcall_go [] args)) 

let expr_of_string s =
  s
  |> Pstparser.pst_of_string
  |> expr_of_pst

type binding =
   | VarBinding of string * expr
   | TopLevelExpr of expr
   (* TODO: add a constructor for test bindings here *)
   | TestBinding of expr
   | FunctionBinding of string * (string list) * expr (* name of function, name of args, body *)
   | StructBinding of string * (string list) (* name of struct, name of struct fields *)
[@@deriving show]
let string_of_binding = show_binding

let binding_of_pst p =
  let rec args_go acc list =
    match list with 
    | [] -> acc
    | head::tail -> begin
      match head with
      | Pst.Symbol argname -> begin
        if List.exists (fun x -> x = argname) acc
        then raise (AbstractSyntaxError("Multiple params / fields with same name"))
        else args_go (argname::acc) tail
        end
      | _ -> raise (AbstractSyntaxError("args/field are malformed"))
    end
  in
  match p with
  | Pst.Symbol _ -> TopLevelExpr (expr_of_pst p)
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected binding but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "define", [Pst.Symbol lhs_var; rhs] -> VarBinding (lhs_var, expr_of_pst rhs)
     | Pst.Symbol "define", [Pst.Node((Pst.Symbol name)::args); body] -> FunctionBinding (name, List.rev (args_go [] args), expr_of_pst body)
     | Pst.Symbol "define", _ -> raise (AbstractSyntaxError("This definition is malformed " ^ Pst.string_of_pst p))
     (* TODO: parse test bindings here *)
     | Pst.Symbol "test", [exp] -> TestBinding (expr_of_pst exp)
     | Pst.Symbol "test", _ -> raise (AbstractSyntaxError("This test is malformed " ^ Pst.string_of_pst p))
     | Pst.Symbol "struct", Pst.Symbol name::fields -> StructBinding (name, List.rev (args_go [] fields))
     | Pst.Symbol "struct", _ -> raise (AbstractSyntaxError("This struct definition is malformed " ^ Pst.string_of_pst p))
     | _ -> TopLevelExpr (expr_of_pst p)

let binding_of_string s =
  s
  |> Pstparser.pst_of_string
  |> binding_of_pst

let bindings_of_string s =
  let p = Pstparser.pstparser_of_string s in
  let rec parse_binding_list () =
    match Pstparser.parse_pst p with
    | None -> []
    | Some pst ->
       binding_of_pst pst :: parse_binding_list ()
  in
  parse_binding_list ()
