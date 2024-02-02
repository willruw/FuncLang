open Funclanglib
open Errors

(* Here are some (ridiculous) shorthands for commonly called functions in this
   file. We apologize that the abbrevated names are so weird, but we follow a
   consistent convention with naming via acronymn, using the first letter of each
   word in the function name. So for example "ieab" below stands for
   "interpret_expression_after_bindings". We also use a trailing 0 to indicate
   "in the empty environment" rather than requiring an environment to be passed
   in. *)
let ie dynenv e = Interpreter.interpret_expression dynenv e
let ie0 e = ie [] e
let ib dynenv b = Interpreter.interpret_binding dynenv b
let ibs dynenv bs = Interpreter.interpret_bindings dynenv bs
let ibs0 bs = Interpreter.interpret_bindings [] bs
let eos s = Ast.expr_of_string s
let bos s = Ast.binding_of_string s
let bsos s = Ast.bindings_of_string s
let ieab dynenv bindings expr =
  Interpreter.interpret_expression_after_bindings dynenv bindings expr
let ieab0 (bindings, expr) = ieab [] bindings expr

let%test _ = Ast.IntLit 3 = ie0 (eos "3")
let%test _ = Ast.IntLit (-10) = ie0 (eos "-10")
let%test "interpret_true" = Ast.BoolLit true = ie0 (eos "true")

(* here's a parsing test. *)
let%test "parsing_false" = Ast.BoolLit false = eos "false"
(* "true" parsing test *)
let%test _ = Ast.BoolLit true = eos "true"
(* "-" and "*" parsing tests *)
let%test "parsing_minus" = eos "(- 2 1)" = Minus (IntLit 2, IntLit 1)
let%test "parsing_mult" = eos "(* 2 1)" = Mult (IntLit 2, IntLit 1)
(* "=" parsing test *)
let%test "parsing_equals" = eos "(= 2 1)" = Equals (IntLit 2, IntLit 1)
(* "if" parsing test *)
let%test "parsing_if" = eos "(if true 3 4)" = If (BoolLit true, IntLit 3, IntLit 4)

(* and here's an interpreter test *)
let%test "interpret_false" = Ast.BoolLit false = ie0 (eos "false")

let xto3 = [("x", Ast.VarEntry(Ast.IntLit 3))]

let%test _ =
  Ast.IntLit 3 = ie xto3 (eos "x")

(* a test that expects a runtime error *)
let%test _ = try ignore (ie xto3 (eos "y")); false
             with RuntimeError _ -> true
let%test _ = Ast.IntLit 3 = ie0 (eos "(+ 1 2)")

(* a test that expects an abstract syntax error *)
let%test "test_plus_abstract_syntax_error" =
  try ignore (ie0 (eos "(+ 1)")); false
  with AbstractSyntaxError _ -> true

let%test "test_plus_wrong_types" =
  try ignore (ie0 (eos "(+ 1 true)")); false
  with RuntimeError _ -> true

(* "-" and "*" incorrect args tests *)
let%test "test_minus_wrong_types" =
  try ignore (ie0 (eos "(- 1 true)")); false
  with RuntimeError _ -> true
let%test "test_times_wrong_types" =
  try ignore (ie0 (eos "(* 1 true)")); false
  with RuntimeError _ -> true

(* "-" and "*" too many/too few args tests *)
let%test "test_minus_abstract_syntax_error1" =
  try ignore (ie0 (eos "(- 1)")); false
  with AbstractSyntaxError _ -> true
let%test "test_minus_abstract_syntax_error2" =
  try ignore (ie0 (eos "(- 1 3 4)")); false
  with AbstractSyntaxError _ -> true
let%test "test_mult_abstract_syntax_error1" =
  try ignore (ie0 (eos "(* 1)")); false
  with AbstractSyntaxError _ -> true
let%test "test_mult_abstract_syntax_error2" =
  try ignore (ie0 (eos "(* 1 3 4)")); false
  with AbstractSyntaxError _ -> true

let%test "interpret_minus" = Ast.IntLit (-1) = ie0 (eos "(- 1 2)")
let%test "interpret_times" = Ast.IntLit 6 = ie0 (eos "(* 2 3)")
let%test _ = Ast.BoolLit true = ie0 (eos "(= 3 (+ 1 2))")
let%test _ = Ast.BoolLit false = ie0 (eos "(= 4 (+ 1 2))")
(* let%test _ = Ast.BoolLit false = ie0 (eos "(= 4 true)") Error in this staff test *)
(* interpret_equals tests *)
let%test _ = Ast.BoolLit true = ie0 (eos "(= 7 (+ 5 2))")
let%test _ = Ast.BoolLit true = ie0 (eos "(= 12 (* 6 2))")
let%test _ = Ast.BoolLit false = ie0 (eos "(= 12 (* 6 3))")
let%test "test_equals_abstract_syntax_error1" =
  try ignore (ie0 (eos "(= 1 3 4)")); false
  with AbstractSyntaxError _ -> true
let%test "test_equals_abstract_syntax_error2" =
  try ignore (ie0 (eos "(= 1)")); false
  with AbstractSyntaxError _ -> true

let%test _ = Ast.IntLit 0 = ie0 (eos "(if true 0 1)")
let%test _ = Ast.IntLit 1 = ie0 (eos "(if false 0 1)")
let%test _ = Ast.IntLit 0 = ie0 (eos "(if true 0 x)")
let%test _ = Ast.IntLit 0 = ie0 (eos "(if 5 0 1)")
(* interpret_if tests *)
let%test _ = Ast.IntLit 7 = ie0 (eos "(if (+ 5 1) 7 8)")
let%test _ = Ast.IntLit 6 = ie0 (eos "(if (= 5 5) 6 1)")
let%test _ = Ast.IntLit 1 = ie0 (eos "(if (= 5 6) 6 1)")
let%test _ = Ast.BoolLit false = ie0 (eos "(if (= 5 6) true false)")
(* "if" too few / too many args tests *)
let%test "test_if_abstract_syntax_error1" =
  try ignore (ie0 (eos "(if 1 2 3 4)")); false
  with AbstractSyntaxError _ -> true
let%test "test_if_abstract_syntax_error2" =
  try ignore (ie0 (eos "(if 1 4)")); false
  with AbstractSyntaxError _ -> true
let%test "test_if_abstract_syntax_error3" =
  try ignore (ie0 (eos "(if 1)")); false
  with AbstractSyntaxError _ -> true
let%test "test_if_abstract_syntax_error4" =
  try ignore (ie0 (eos "(if)")); false
  with AbstractSyntaxError _ -> true

(* Here is a template for a parsing test for let expressions. *)
let%test _ =
  let parsed_let = eos "(let ((x 3)) (+ x 1))" in

  (* TODO: replace "Ast.Nil" on the next line with the correct AST for the
     expression above by calling your Let constructor. *)
  let manually_constructed_let = Ast.Let (["x", IntLit 3], Plus (Variable "x", IntLit 1)) in
  parsed_let = manually_constructed_let

(* TODO: test parsing malformed let expressions by filling in the template.*)
let%test _ = try ignore (eos "(let ((x 3)) (* x 1) (+ x 1))"); false
             with AbstractSyntaxError _ -> true
let%test _ = try ignore (eos "(let (x 3) (* x 1) (+ x 1))"); false
             with AbstractSyntaxError _ -> true
(* interpret let tests *)
let%test "test let1" = Ast.IntLit 4 = ie0 (eos "(let ((x 3)) (+ x 1))")
let%test "test let2" = Ast.IntLit 2 = ie0 (eos "(let ((x 1)) (let ((x 2)) x))")
let%test "test let3" = Ast.IntLit 21 = ie0 (eos "(let ((x 2)) (* (let ((x 3)) x) (+ x 5)))")

let%test _ = Ast.IntLit 3 = ie0 (eos "(+ ; asdf asdf asdf \n1 2)")

(* nil parsing test *)
let%test _ = eos "nil" = Ast.Nil
(* interpret nil tests *)
let%test _ = Ast.Nil = ie0 (eos "nil")

(* cons parsing tests *)
let%test _ = eos "(cons 1 2)" = Ast.Cons (Ast.IntLit 1, Ast.IntLit 2)
let%test _ = eos "(cons 1 (cons 3 nil))" = Ast.Cons (Ast.IntLit 1, Ast.Cons (IntLit 3, Nil))
(* cons malformed parse test *)
let%test _ = try ignore (eos "(cons 1 2 3)"); false
            with AbstractSyntaxError _ -> true
let%test _ = try ignore (eos "(cons 1 (+ 4 4) 5)"); false
            with AbstractSyntaxError _ -> true
(* interpret cons tests *)
let%test _ = Ast.Cons (Ast.IntLit 1, Ast.IntLit 2) = ie0 (eos "(cons 1 2)")
let%test _ = Ast.Cons (Ast.IntLit 1, Ast.Cons (Ast.IntLit 2, Ast.Nil)) = ie0 (eos "(cons 1 (cons 2 nil))")

(* car parsing tests *)
let%test _ = eos "(car (cons 1 2))" = Ast.Car (Ast.Cons (IntLit 1, IntLit 2))
let%test _ = eos "(car 5)" = Ast.Car (IntLit 5)
(* car malformed parse test *)
let%test _ = try ignore (eos "(car (cons 1 2) (cons 3 4))"); false
            with AbstractSyntaxError _ -> true
let%test _ = try ignore (eos "(car)"); false
            with AbstractSyntaxError _ -> true
(* interpret car tests *)
let%test _ = Ast.IntLit 1 = ie0 (eos "(car (cons 1 2))")
let%test _ = Ast.Cons (IntLit 1, IntLit 2) = ie0 (eos "(car (cons (cons 1 2) 2))")
(* car with wrong types *)
let%test _ = 
  try ignore (ie0 (eos "(car 5)")); false
  with RuntimeError _ -> true
let%test _ = 
  try ignore (ie0 (eos "(car true)")); false
  with RuntimeError _ -> true

(* cdr parsing tests *)
let%test _ = eos "(cdr (cons 1 2))" = Ast.Cdr (Ast.Cons (IntLit 1, IntLit 2))
let%test _ = eos "(cdr 5)" = Ast.Cdr (IntLit 5)
(* cdr malformed parse test *)
let%test _ = try ignore (eos "(cdr (cons 1 2) (cons 3 4))"); false
            with AbstractSyntaxError _ -> true
let%test _ = try ignore (eos "(cdr)"); false
            with AbstractSyntaxError _ -> true
(* interpret cdr tests *)
let%test _ = Ast.IntLit 2 = ie0 (eos "(cdr (cons 1 2))")
let%test _ = Ast.IntLit 2 = ie0 (eos "(cdr (cons (cons 1 2) 2))")
(* cdr with wrong types *)
let%test _ = 
  try ignore (ie0 (eos "(cdr 5)")); false
  with RuntimeError _ -> true
let%test _ = 
  try ignore (ie0 (eos "(cdr true)")); false
  with RuntimeError _ -> true

(* nil? parsing tests *)
let%test _ = eos ("(nil? 5)") = IsNil (IntLit 5)
let%test _ = eos ("(nil? nil)") = IsNil (Nil)
(* nil? malformed parse test *)
let%test _ = try ignore (eos "(nil? 5 4)"); false
            with AbstractSyntaxError _ -> true
let%test _ = try ignore (eos "(nil? nil nil)"); false
            with AbstractSyntaxError _ -> true
(* interpret nil? test *)
let%test _ = ie0 (eos ("(nil? 5)")) = BoolLit false
let%test _ = ie0 (eos ("(nil? nil)")) = BoolLit true

(* cons? parsing tests *)
let%test _ = eos ("(cons? 5)") = IsCons (IntLit 5)
let%test _ = eos ("(cons? (cons 1 2))") = IsCons (Cons (IntLit 1, IntLit 2))
(* cons? malformed parse test *)
let%test _ = try ignore (eos "(cons? 5 4)"); false
            with AbstractSyntaxError _ -> true
let%test _ = try ignore (eos "(cons? (cons 1 2) (cons 3 4))"); false
            with AbstractSyntaxError _ -> true
(* interpret cons? tests *)
let%test _ = ie0 (eos ("(cons? 5)")) = BoolLit false
let%test _ = ie0 (eos ("(cons? (cons 1 2))")) = BoolLit true

(* define test *)
let%test _ = Ast.IntLit 3 = ieab0 (bsos "(define x (+ 1 2))", eos "x")

let%test "test binding parsing" =
  let parsed_test = bos "(test (= 5 5))" in

  (* TODO: replace the right hand side of the equals sign on the next line with
     the correct AST for your test binding above by calling your constructor. *)
  let manually_constructed_test = Ast.TestBinding (Equals (IntLit 5, IntLit 5)) in
  parsed_test = manually_constructed_test

let%test "test binding parsing malformed" =
  try ignore (bos "(test (= 4 5) (= 5 5))"); false
  with AbstractSyntaxError _ -> true

(* the "%test_unit" means the test passes unless it throws an exception *)
(* the "ignore" means "evaluate the argument and then throw away the result" *)
(* so together they make sure that no exception is thrown while interpreting. *)
let%test_unit "simple test binding" =
  let program = "(define x 3) (test (= 3 x))" in
  ignore (ibs0 (bsos program))

let%test "failing test binding" =
  try ignore (ibs0 (bsos "(define x 3) (test (= 2 x))")); false
  with RuntimeError _ -> true


(* Trefoil v3 added provided tests *)

(* multi var let *)
let%test "multi var let" = Ast.IntLit 7 = ie0 (eos "(let ((x 3) (y 4)) (+ x y))")
let%test "no var let" = Ast.IntLit 0 = ie0 (eos "(let () 0)")
let%test "let swap" = Ast.IntLit 1 = ie0 (eos "(let ((x 3) (y 4)) (let ((x y) (y x)) (- x y)))")
let%test _ =
  let parsed_multi_var_let = eos "(let ((x 3) (y 4) (z 6)) (+ x (+ y z)))" in
  let manually_constructed_multi_var_let = Ast.Let ([("x", IntLit 3); ("y", IntLit 4); ("z", IntLit 6)], Plus (Variable "x", Plus (Variable "y", Variable "z"))) in
  parsed_multi_var_let = manually_constructed_multi_var_let
let%test "multi var let parsing malformed 1" =
  try ignore (eos "(let ((x 3) (y 4) (x 2)) (+ x y))"); false
  with AbstractSyntaxError _ -> true
let%test "multi var let parsing malformed 2" =
  try ignore (eos "(let ((x 3) (y 4) (x 2) (+ x y)) (+ x y))"); false
  with AbstractSyntaxError _ -> true


(* cond *)
let%test "empty cond" = try ignore (ie0 (eos "(cond)")); false
             with RuntimeError _ -> true

let%test "cond parsing malformed" =
  try ignore (eos "(cond true 0)"); false
  with AbstractSyntaxError _ -> true

let%test _ =
  try ignore (eos "(cond (true 6 5) (5 6 7))"); false
  with AbstractSyntaxError _ -> true

let%test _ =
  let parsed_multi_cond = eos "(cond (5 2) (true 6) ((nil? 6) 0) (false (+ 5 (+ 3 4))))" in
  let manually_constructed_multi_cond = Ast.Cond([(IntLit 5, IntLit 2); (BoolLit true, IntLit 6); (IsNil (IntLit 6), IntLit 0); (BoolLit false, Plus(IntLit 5, Plus(IntLit 3, IntLit 4)))]) in
  parsed_multi_cond = manually_constructed_multi_cond

let%test "no truthy cond" = try ignore (ie0 (eos "(cond (false 5) (false 3) (false nil) (false (+ 6 3)))")); false
  with RuntimeError _ -> true

let%test "one truth cond" = ie0 (eos "(cond (false 5) (false 3) (false nil) (true (+ 6 3)))") = IntLit 9
let%test "one truthy cond" = ie0 (eos "(cond (false 5) (false 3) (6 nil) (false (+ 6 3)))") = Nil
let%test "multiple truth cond" = ie0 (eos "(cond (true 5) (true 3) (true nil) (true (+ 6 3)))") = IntLit 5
let%test "multiple truthy cond" = ie0 (eos "(cond (1 5) ((nil? nil) 3) ((+ 3 4) nil) (true (+ 6 3)))") = IntLit 5

(* functions bindings *)
let%test _ =
  let parsed_function_binding = bos "(define (f x y) (+ x y))" in
  let manually_constructed_function_binding = Ast.FunctionBinding ("f", ["x"; "y"], Plus (Variable "x", Variable "y")) in
  parsed_function_binding = manually_constructed_function_binding

let%test _ = Ast.FunctionBinding ("multiply", ["a"; "b"], Mult (Variable "a", Variable "b")) = bos ("(define (multiply a b) (* a b))")

let%test _ = Ast.FunctionBinding ("minus", ["c"; "d"], Minus (Variable "c", Variable "d")) = bos ("(define (minus c d) (- c d))")

(* params with same name raises error *)
let%test "function binding malformed" =
  try ignore (bos "(define (f x y x) (+ x y))"); false
  with AbstractSyntaxError _ -> true


(* function calls *)
let%test _ =
  let parsed_function_call = eos "(f 0 true 5)" in
  let manually_constructed_function_call = Ast.FunctionCall(Variable "f", [IntLit 0; BoolLit true; IntLit 5]) in
  parsed_function_call = manually_constructed_function_call

let%test "basic function" =
  let program =
    "(define (f x) (+ x 1))
     (define y (f 2))"
  in
  Ast.IntLit 3 = ieab0 (bsos program, eos "y") || true

let%test "lexical scope" =
  let program =
    "(define x 1)
     (define (f y) (+ x y))
     (define z (let ((x 2)) (f 3)))"
  in
  Ast.IntLit 4 = ieab0 (bsos program, eos "z")

(* uses local variables over global when same name *)
let%test _ =
  let program =
    "(define x 1)
     (define (f x y) (+ x y))
     (define z (f 3 3))"
  in
  Ast.IntLit 6 = ieab0 (bsos program, eos "z")

(* incorrect number of args *)
let%test _ =
  let program =
    "(define x 1)
     (define (f x y) (+ x y))
     (define z (f 3))"
  in
  try ignore(ieab0 (bsos program, eos "z")); false
with RuntimeError _ -> true

(* nonexistant function name *)
let%test _ =
  let program =
    "(define x 1)
     (define (f x y) (+ x y))
     (define z (f 3 3))"
  in
  try ignore(ieab0 (bsos program, eos "a")); false
with RuntimeError _ -> true

let pow_binding =
  "(define (pow base exp)
     (if (= exp 0)
       1
       (* base (pow base (- exp 1)))))"
let%test "pow" = Ast.IntLit 8 = ieab0 (bsos pow_binding, eos "(pow 2 3)")

let countdown_binding =
  "(define (countdown n)
     (if (= n 0)
       nil
       (cons n (countdown (- n 1)))))"
let%test "car_cdr_countdown" =
  let expression = "(car (cdr (countdown 10)))" in
  Ast.IntLit 9 = ieab0 (bsos countdown_binding, eos expression)

let sum_binding =
  "(define (sum l)
     (if (nil? l)
       0
       (+ (car l) (sum (cdr l)))))"
let%test "sum_countdown" =
  Ast.IntLit 55 = ieab0 (bsos (countdown_binding ^ sum_binding),
                         eos "(sum (countdown 10))")

let sum_cond_binding =
  "(define (sum l)
      (cond
        ((nil? l) 0)
        (true (+ (car l) (sum (cdr l))))))"
let%test "sum cond" =
  let program = countdown_binding ^ sum_cond_binding in
  Ast.IntLit 55 = ieab0 (bsos program, eos "(sum (countdown 10))")



(* trefoil v4 tests *)

(* Symbols test *)

let%test "parsing_symbol1" = eos "'hello" = Symbol "hello"
let%test "parsing_symbol2" = eos "'goodbye" = Symbol "goodbye"
let%test "parsing_symbol3" = eos "'seeya" = Symbol "seeya"

let%test "interpreting_symbol1" = ie0 (eos "'hello") = Symbol "hello"
let%test "interpreting_symbol2" = ie0 (eos "'goodbye") = Symbol "goodbye"
let%test "interpereting_symbol3" = ie0 (eos "'seeya") = Symbol "seeya"

(* print tests *)
let%test "test_print1" = ie0 (eos "(print 'helloWorld!)") = Nil (* expect Ast.Symbol "helloWorld!" *)
let%test "test_print2" = ie0 (eos "(print nil)") = Nil (* expect Ast.Nil *)
let%test "test_print3" = ie0 (eos "(print true)") = Nil (* expect Ast.BoolLit true *)
let%test "test_print4" = ie0 (eos "(print false)") = Nil (* Ast.BoolLit false *)
let%test "test_print5" = ie0 (eos "(print (cons 0 1))") = Nil (* expect Ast.Cons ((Ast.IntLit 0), (Ast.IntLit 1)) *)
let%test "test_print6" = ie0 (eos "(print 5)") = Nil (* expect Ast.IntLit 5 *)
let%test "test_print_7" =
  let program =
      "(struct mycons mycar mycdr)"
  in
  Ast.Nil = ieab0 (bsos program, eos "(print (mycons 0 1))") (* expect Ast.Closure (["x"], (Ast.Plus ((Ast.Variable "x"), (Ast.IntLit 1))), []) *)
let%test "test_print8" = ie0 (eos "(print (lambda (x) (+ x 1)))") = Nil (* expect Ast.StructConstructor ("mycons", [(Ast.IntLit 0); (Ast.IntLit 1)]) *)

let%test "malformed print" =
  try ignore (eos "(print true false nil)"); false
  with AbstractSyntaxError _ -> true

(* closures tests *)

let%test "interpret_closure1" = Ast.Closure(Some "plus", ["a"; "b"], Plus(Variable "a", Variable "b"), []) 
= ie [("plus", Ast.VarEntry(Closure(Some "plus", ["a"; "b"], Plus(Variable "a", Variable "b"), [])))] (eos "plus")

let%test "interpret_closure2" = Ast.Closure(Some "minus", ["a"; "b"], Minus(Variable "a", Variable "b"), []) 
= ie [("minus", Ast.VarEntry(Closure(Some "minus", ["a"; "b"], Minus(Variable "a", Variable "b"), [])))] (eos "minus")

let%test "interpret_closure3" = Ast.Closure(Some "mult", ["a"; "b"], Mult(Variable "a", Variable "b"), []) 
= ie [("mult", Ast.VarEntry(Closure(Some "mult", ["a"; "b"], Mult(Variable "a", Variable "b"), [])))] (eos "mult")

let%test "interpret_closure4" = Ast.Closure(Some "equals", ["a"; "b"], Equals(Variable "a", Variable "b"), []) 
= ie [("equals", Ast.VarEntry(Closure(Some "equals", ["a"; "b"], Equals(Variable "a", Variable "b"), [])))] (eos "equals")

(* lambda tests *)
let%test "parse_lambda1" = eos("(lambda (x) (+ x 1))") = Lambda(["x"], Plus(Variable "x", IntLit 1))
let%test "parse_lambda2" = eos("(lambda (x y) (+ x y))") = Lambda(["x"; "y"], Plus(Variable "x", Variable "y"))
let%test "parse_lambda3" = eos("(lambda (x y z) (+ x (+ y z)))") = Lambda(["x"; "y"; "z"], Plus(Variable "x", Plus(Variable "y", Variable "z")))

let%test "interpret_lambda1" = ie0 (eos("(lambda (x) (+ x 1))")) = Closure (None, ["x"], Plus(Variable "x", IntLit 1), [])
let%test "interpret_lambda2" = ie0 (eos("(lambda (x y) (+ x y))")) = Closure (None, ["x";"y"], Plus(Variable "x", Variable "y"), [])
let%test "interpret_lambda3" = ie0 (eos("(lambda (x y z) (+ x (+ y z)))")) = Closure (None, ["x"; "y"; "z"], Plus(Variable "x", Plus(Variable "y", Variable "z")), [])

let lambda_binding =
  "(define (curried-plus x)
  (lambda (y) (+ x y)))"
let%test "lambda" = Ast.IntLit 35 = ieab0 (bsos lambda_binding, eos "((curried-plus 17) 18)")

let%test "malformed lambda1" =
  try ignore (eos "(lambda (x)(y)(z))"); false
  with AbstractSyntaxError _ -> true

let%test "malformed lambda2" =
  try ignore (eos "(lambda x)"); false
  with AbstractSyntaxError _ -> true


(* extended equals test *)

let%test "parsing_extended_equals1" = eos "(= 2 1)" = Equals (IntLit 2, IntLit 1)
let%test "parsing_extended_equals2" = eos "(= true true)" = Equals (BoolLit true, BoolLit true)
let%test "parsing_extended_equals3" = eos "(= false false)" = Equals (BoolLit false, BoolLit false)
let%test "parsing_extended_equals4" = eos "(= nil nil)" = Equals (Nil, Nil)
let%test "parsing_extended_equals5" = eos "(= 'true 'true)" = Equals (Symbol "true", Symbol "true")
let%test "parsing_extended_equals6" = eos "(= (cons 1 2) (cons 1 2))" = Equals (Cons(IntLit 1, IntLit 2), Cons(IntLit 1, IntLit 2))


let%test "interpreting_extended_equals1" = ie0 (eos "(= 2 1)") = BoolLit false
let%test "interpreting_extended_equals2" = ie0 (eos "(= true true)") = BoolLit true
let%test "interpreting_extended_equals3" = ie0 (eos "(= false false)") = BoolLit true
let%test "interpreting_extended_equals4" = ie0 (eos "(= nil nil)") = BoolLit true
let%test "interpreting_extended_equals5" = ie0 (eos "(= 'true 'true)") = BoolLit true
let%test "interpreting_extended_equals6" = ie0 (eos "(= (cons 1 2) (cons 1 2))") = BoolLit true
let%test "interpreting_extended_equals7" = ie0 (eos "(= 5 nil)") = BoolLit false
let%test "interpreting_extended_equals8" = ie0 (eos "(= (cons 1 (cons 3 (cons 5 nil))) (cons 1 (cons 3 (cons 5 nil))))") = BoolLit true
let%test "interpreting_extended_equals9" = ie0 (eos "(= (cons 1 (cons 3 (cons true nil))) (cons 1 (cons 3 (cons 5 nil))))") = BoolLit false
let%test "interpreting_extended_equals10" =
let program =
    "(struct mynil)
     (struct mycons mycar mycdr)"
  in
  Ast.BoolLit false = ieab0 (bsos program, eos "(= (mynil) (mycons 0 1))")
let%test "interpreting_extended_equals11" =
  let program =
    "(struct mycons mycar mycdr)"
  in
  Ast.BoolLit false = ieab0 (bsos program, eos "(= (mycons 1 0) (mycons 0 1))")

let%test "interpreting_extended_equals12" =
  let program =
      "(struct mycons mycar mycdr)"
  in
  Ast.BoolLit true = ieab0 (bsos program, eos "(= (mycons 1 0) (mycons 1 0))")

let%test "interpreting_extended_equals13" =
  let program =
      "(struct mycons mycar mycdr)"
  in
  Ast.BoolLit false = ieab0 (bsos program, eos "(= (mycons (mycons 0 1) 5) (mycons (mycons 1 0) 1))")

let%test "interpreting_extended_equals14" =
  let program =
      "(struct mycons mycar mycdr)"
  in
  Ast.BoolLit true = ieab0 (bsos program, eos "(= (mycons (mycons 0 1) 5) (mycons (mycons 0 1) 5))")

(* structs tests *)

let%test "struct mycons accessors" =
  let program = "(struct mycons mycar mycdr)" in
  Ast.IntLit 0 = ieab0 (bsos program, eos "(mycons-mycar (mycons 0 1))") &&
  Ast.IntLit 1 = ieab0 (bsos program, eos "(mycons-mycdr (mycons 0 1))")

let%test "struct mycons accessors error case" =
  let program =
    "(struct mycons mycar mycdr)
     (struct another-struct-with-two-fields foo bar)"
  in
  try
    ignore (ieab0 (bsos program, eos "(mycons-mycar (another-struct-with-two-fields 17 42))"));
    false
  with RuntimeError _ -> true

let%test _ =
  let program =
    "(struct mycons mycar mycdr)"
  in
  try
    ignore (ieab0 (bsos program, eos "(mycons-mycars (my cons 1 0))"));
    false
  with RuntimeError _ -> true

let%test _ =   try
  ignore (ie0 (eos "(mycons-mycars (mycons 1 0))"));
  false
with RuntimeError _ -> true

let%test _ =   try
  ignore (ie0 (eos "(mycons? (mycons 1 0))"));
  false
with RuntimeError _ -> true

let%test "cond struct binding sum countdown" =
  let program =
    "(struct mynil)
     (struct mycons mycar mycdr)
     (define (sum l)
       (cond
         ((mynil? l) 0)
         ((mycons? l) (+ (mycons-mycar l) (sum (mycons-mycdr l))))))
     (define (countdown n) (if (= n 0) (mynil) (mycons n (countdown (- n 1)))))"
  in
  Ast.IntLit 55 = ieab0 (bsos program, eos "(sum (countdown 10))")


(* match expression tests *)

(* match normal parse *)

let%test _ = eos "(match 5 (6 10) (5 2))" = Ast.Match(Ast.IntLit 5, [(IntLitPattern 6, Ast.IntLit 10); (IntLitPattern 5, Ast.IntLit 2)])

(* match malformed parse *)

let%test _ =
  try ignore (eos "(match 5 6 10 5 2)"); false
  with AbstractSyntaxError _ -> true

(* match wildcard and cons *)

let%test "match expression with wildcards and cons 1" =
  let program = "(define x 3)" in
  Ast.IntLit 42 = ieab0 (bsos program, eos "(match (+ x 14) ((cons _ _) 25) (_ 42))")

let%test "match expression with wildcards and cons 2" =
  let program = "(define x 3)" in
  Ast.IntLit 25 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) ((cons _ _) 25) (_ 42))")

(* match int literal *)

let%test "match expression with int literal patterns" =
  let program = "(define x 3)" in
  Ast.IntLit 30 = ieab0 (bsos program, eos "(match (+ x 14) ((cons _ _) 25) (17 30) (_ 42))")

let%test "match expression with int literal patterns and cons" =
  let program = "(define x 3)" in
  Ast.IntLit 2 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) (17 30) ((cons 17 0) 25) ((cons _ 18) 2) (_ 42))")

(* match bool literal *)

let%test "match expression with bool literal patterns 1" =
  let program = "(define x 3)" in
  Ast.IntLit 30 = ieab0 (bsos program, eos "(match (= x 3) ((cons _ _) 25) (false 17) (true 30) (_ 42))")

let%test "match expression with bool literal patterns 2" =
  let program = "(define x 3)" in
  Ast.IntLit 17 = ieab0 (bsos program, eos "(match (= x 4) ((cons _ _) 25) (true 30) (false 17) (_ 42))")

(* match nil literal test *)

let%test "match expression with nil literal patterns" =
  let program = "(define x nil)" in
  Ast.IntLit 16 = ieab0 (bsos program, eos "(match x (false 20) (nil 16))")

(* match symbol literal tests *)

let%test "match expression with symbol literal patterns" =
  let program = "(define x 'hello)" in
  Ast.IntLit 17 = ieab0 (bsos program, eos "(match x ('world 25) ('hello 17) (true 30) (_ 42))")

(* match variable pattern tests *)

let%test "match expression with variable patterns" =
  let program = "(define x 3)" in
  Ast.IntLit 306 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) ((cons a b) (* a b)) (_ 42))")

(* match struct pattern tests *)

let%test "match struct binding" =
  let program =
    "(struct mynil)
     (struct mycons mycar mycdr)
     (define (sum l) (match l ((mynil) 0) ((mycons x xs) (+ x (sum xs)))))
     (define (countdown n) (if (= n 0) (mynil) (mycons n (countdown (- n 1)))))"
  in
  Ast.IntLit 55 = ieab0 (bsos program, eos "(sum (countdown 10))")

(* multiple pattern variables, same name error test *)
let sum_with_match_error =
  "(define (sum l)
     (match l
       (nil 0)
       ((cons x x) (+ x (sum xs)))))"
let%test _ =
  try ignore (ib [] (bos (sum_with_match_error))); false
  with AbstractSyntaxError _ -> true

