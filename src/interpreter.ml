open Ast
open Errors

let string_of_dynenv_entry (x, v) = 
  match v with
  | VarEntry e -> x ^ " -> " ^ string_of_expr e

let rec lookup dynenv name =
  match dynenv with
  | [] -> None
  | (x, value) :: dynenv ->
     if x = name
     then Some value
     else lookup dynenv name


let rec interpret_pattern pattern value =
  match pattern, value with
  | WildcardPattern, _ -> Some []
  | ConsPattern (p1, p2), Cons (v1, v2) -> begin
     match interpret_pattern p1 v1, interpret_pattern p2 v2 with
     | Some l1, Some l2 -> Some (l1 @ l2)
     | _ -> None
    end
  | IntLitPattern n, IntLit m -> if n = m then Some [] else None
  | BoolLitPattern a, BoolLit b -> if a = b then Some [] else None
  | NilPattern, Nil -> Some []
  | SymbolPattern s1, Symbol s2 -> if s1 = s2 then Some [] else None
  | VariablePattern x, _ -> Some [(x, VarEntry value)]
  | StructPattern (s, ps), StructConstructor(s', vs) -> begin
    let rec struct_patterns_match_go acc list1 list2 =
      match list1, list2 with
      | [], _ | _, [] -> Some acc
      | head1::tail1, head2::tail2 -> 
        match interpret_pattern head1 head2 with
        | None -> None
        | Some bindings -> struct_patterns_match_go (bindings@acc) tail1 tail2
    in
    if s <> s' then None else
    if List.length ps <> List.length vs then None else
      match struct_patterns_match_go [] ps vs with
      | None -> None
      | Some list -> Some (List.rev list)
  end
  | _ -> None

let rec interpret_expression dynenv e =
  let rec interpret_lets list env =
    match list with
    | [] -> env
    | (name, def)::tail -> interpret_lets tail ((name, VarEntry (interpret_expression dynenv def))::env)
  in
  let rec interpret_cond list =
    match list with
    | [] ->  raise (RuntimeError ("Reached end of cons"))
    | (cond, expr)::tail -> begin
      match interpret_expression dynenv cond with
      | BoolLit false -> interpret_cond tail
      | _ -> interpret_expression dynenv expr
    end
  in
  let rec vals_go acc params args =
    match params, args with
    | [], [] -> acc
    | head1::tail1, head2::tail2 -> vals_go ((head1, VarEntry (interpret_expression dynenv head2))::acc) tail1 tail2
    | _ -> raise (RuntimeError("Unbound function call"))
  in
  let rec equals_go v1 v2 =
    match v1, v2 with
    | IntLit a, IntLit b -> a = b 
    | BoolLit a, BoolLit b -> a = b
    | Nil, Nil -> true
    | Symbol a, Symbol b -> a = b
    | Cons (a, b), Cons (c, d) -> (equals_go a c) && (equals_go b d)
    | Closure _, Closure _ | Closure _, _ | _, Closure _ -> raise (RuntimeError ("Operator = applied to Closure"))
    | StructConstructor(name1, arg_vals1), StructConstructor(name2, arg_vals2) ->begin
        let rec expr_list_go list1 list2 =
          match list1, list2 with
          | [], [] -> true
          | head1::tail1, head2::tail2 ->
            if (equals_go head1 head2) then (expr_list_go tail1 tail2) else false
          | _, _ -> false
    in 
    (name1 = name2) && expr_list_go arg_vals1 arg_vals2
  end
    | _ -> false
  in
  match e with
  | IntLit _ -> e
  | Variable x -> begin
      match lookup dynenv x with
      | None -> raise (RuntimeError ("Unbound variable " ^ x))
      | Some value -> begin
        match value with
        | VarEntry v -> v
      end
    end
  | Plus (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | IntLit n1, IntLit n2 -> IntLit (n1 + n2)
      | IntLit _, v2 -> raise (RuntimeError ("Plus applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Plus applied to non-integer " ^ string_of_expr v1))
    end
  | BoolLit _ -> e
  | Minus (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | IntLit n1, IntLit n2 -> IntLit (n1 - n2)
      | IntLit _, v2 -> raise (RuntimeError ("Minus applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Minus applied to non-integer " ^ string_of_expr v1))
    end
  | Mult (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | IntLit n1, IntLit n2 -> IntLit (n1 * n2)
      | IntLit _, v2 -> raise (RuntimeError ("Mult applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Mult applied to non-integer " ^ string_of_expr v1))
    end
  | Equals (e1, e2) -> BoolLit(equals_go (interpret_expression dynenv e1) (interpret_expression dynenv e2))
  | If (branch, thn, els) -> begin
      match interpret_expression dynenv branch with
      | BoolLit false -> interpret_expression dynenv els
      | _ -> interpret_expression dynenv thn
    end
  | Let (defs, exp) -> begin
      interpret_expression (interpret_lets defs dynenv) exp
    end
  | Cons (left, right) -> begin
      Cons (interpret_expression dynenv left, interpret_expression dynenv right)
    end
  | Nil -> Nil
  | IsNil arg -> begin
      match interpret_expression dynenv arg with
      | Nil -> BoolLit true
      | _ -> BoolLit false
    end
  | IsCons arg -> begin
      match interpret_expression dynenv arg with
      | Cons (_, _) -> BoolLit true
      | _ -> BoolLit false
    end
  | Car arg -> begin
      match interpret_expression dynenv arg with
      | Cons (v1, _) -> v1
      | _ -> raise (RuntimeError ("car applied to a non-cons expression " ^ string_of_expr arg))
    end
  | Cdr arg -> begin
      match interpret_expression dynenv arg with
      | Cons (_, v2) -> v2
      | _ -> raise (RuntimeError ("cdr applied to a non-cons expression " ^ string_of_expr arg))
    end
  | Cond (list) -> interpret_cond list
  | FunctionCall (namexpr, args) -> 
    (match interpret_expression dynenv namexpr with
    | Closure (Some name_opt, params, body, env) ->
      (if List.length params <> List.length args
        then raise (RuntimeError ("Incorrect number of args to function"))
        else interpret_expression ((List.rev(vals_go [] params args))@(name_opt, VarEntry(Closure (Some name_opt, params, body, env)))::env) body)
    | Closure (None, params, body, env) -> 
      (if List.length params <> List.length args
        then raise (RuntimeError ("Incorrect number of args to function"))
        else interpret_expression ((List.rev(vals_go [] params args))@env) body)
    | _ -> raise (RuntimeError ("Function undefined")))
  | Symbol _ -> e
  | Print v -> print_endline (string_of_expr (interpret_expression dynenv v)); Nil
  | Closure _ -> e
  | Lambda (params, body) -> Closure (None, params, body, dynenv)
  | StructConstructor (name, fields_args) -> 
    begin
      let rec fields_to_values acc list =
        match list with
        | [] -> acc
        | head::tail -> fields_to_values ((interpret_expression dynenv head) :: acc) tail
      in
      StructConstructor(name, List.rev (fields_to_values [] fields_args))
    end
  | StructPredicate (name, struct_to_eval) -> begin
    match interpret_expression dynenv struct_to_eval with
    | StructConstructor(name', _) -> BoolLit(name = name')
    | _ -> BoolLit false
  end
  | StructAccess (name, index, struct_to_access) -> begin
    match interpret_expression dynenv struct_to_access with
    | StructConstructor (name', field_args) ->
      begin
        if (name = name') && (index < List.length field_args) then List.nth field_args index
        else raise (RuntimeError ("Not an instance of the provided struct type, or index not valid"))
      end
    | _ -> raise (RuntimeError ("Not a struct instance."))
  end
  | Match (patt, clauses) -> 
    let interpreted_patt_val = interpret_expression dynenv patt
  in
  let rec check_clauses list =
    match list with 
    | [] -> raise (RuntimeError ("No clauses match pattern."))
    | (pattrn, clause)::tail ->
      match interpret_pattern pattrn interpreted_patt_val with
      | None -> check_clauses tail
      | Some list -> interpret_expression (list@dynenv) clause
    in
    check_clauses clauses

let interpret_binding dynenv b =
  match b with
  | VarBinding (x, e) ->
     let value = interpret_expression dynenv e in
     Printf.printf "%s = %s\n%!" x (string_of_expr value);
     (x, VarEntry value) :: dynenv
  | FunctionBinding (name, args, body) -> (name, VarEntry(Closure(Some name, args, body, dynenv)))::dynenv
  | StructBinding (name, fields) ->
    begin
      let rec params_go acc list index =
        match list with
        | [] -> acc
        | _::tail -> params_go (Variable ("x" ^ string_of_int index)::acc) tail (index+1)
      in
      let rec fields_go acc list index = 
        match list with
        | [] -> acc
        | _::tail -> fields_go (("x" ^ string_of_int index)::acc) tail (index+1)
      in
      let rec accesses_go acc list index =
        match list with
        | [] -> acc
        | head::tail -> accesses_go ((name ^ "-" ^ head, VarEntry(Closure(None, ["x"], StructAccess(name, index, Variable "x"), dynenv)))::acc) tail (index+1)
      in
      (List.rev (accesses_go [] fields 0)) @
      (name^"?", VarEntry(Closure(None, ["x"], StructPredicate(name, Variable "x"), dynenv))) ::
      (name, VarEntry(Closure(None, List.rev (fields_go [] fields 0), StructConstructor(name, List.rev (params_go [] fields 0)), dynenv)))::dynenv
    end
  | TopLevelExpr e ->
     let v = interpret_expression dynenv e in
     print_endline (string_of_expr v);
     dynenv
  | TestBinding e ->
    let value = interpret_expression dynenv e in
    match value with 
    | BoolLit true -> dynenv
    | _ -> raise (RuntimeError ("Test failure"))


(* the semantics of a whole program (sequence of bindings) *)
let interpret_bindings dynenv bs =
  List.fold_left interpret_binding dynenv bs

(* starting from dynenv, first interpret the list of bindings in order. then, in
   the resulting dynamic environment, interpret the expression and return its
   value *)
let interpret_expression_after_bindings dynenv bindings expr =
  interpret_expression (interpret_bindings dynenv bindings) expr
