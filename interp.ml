
open Ast
open Format

(* Exception raised to report error during interpretation *)
exception Error of string
let error s = raise (Error s)

(* The values of Mini-Python

   - a noticeable difference with Python: we
      here uses the int type while the Python integers are of
      arbitrary precision; we could use OCaml's Big_int module
      but we choose the facility
   - what Python calls a list is actually a table
      resizable; in the fragment considered here, there is no
      possibility of changing the length, so a simple table OCaml
      appropriate *)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Displaying a value on the standard output *)
let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"

(* Boolean interpretation of a value

   In Python, any value can be used as a Boolean: None,
   the empty list, the empty string, and the integer 0 are considered
   False and any other values like True *)

let is_false v = match v with
  | Vnone -> true
  | Vint b when b == 0 -> true
  | Vstring s when s == "" -> true
  | Vlist a -> Array.length a == 0
  | Vbool b -> not b
  | _ -> false

let is_true v = not (is_false v)

(* The functions here are only global *)

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* The 'return' statement of Python is interpreted using an exception *)

exception Return of value

(* Local variables (function parameters and variables introduced
   assignments) are stored in a hash table passed in
   arguments to the following functions under the name 'ctx' *)

type ctx = (string, value) Hashtbl.t

(* Interpreting an expression (return a value) *)

let rec expr ctx = function
  | Ecst Cnone ->
      Vnone
  | Ecst (Cstring s) ->
      Vstring s
  (* arithmetic *)
  | Ecst (Cint n) ->
      Vint n
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
            Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      begin match op, v1, v2 with
        | Badd, Vint n1, Vint n2 -> Vint (n1+n2)
        | Bsub, Vint n1, Vint n2 -> Vint (n1-n2)
        | Bmul, Vint n1, Vint n2 -> Vint (n1*n2)
        | Bdiv, Vint n1, Vint n2 -> Vint (n1/n2)
        | Bmod, Vint n1, Vint n2 -> Vint (n1 mod n2)
        | Beq, _, _  -> Vbool (v1 = v2)
        | Bneq, _, _ -> Vbool (v1 != v2)
        | Blt, _, _  -> Vbool (v1 <  v2) (* to complete (question 6) *)
        | Ble, _, _  -> Vbool (v1 <= v2) (* to complete (question 6) *)
        | Bgt, _, _  -> Vbool (v1 >  v2) (* to complete (question 6) *)
        | Bge, _, _  -> Vbool (v1 >= v2) (* to complete (question 6) *)
        | Badd, Vstring s1, Vstring s2 ->
            assert false (* to complete (question 3) *)
        | Badd, Vlist l1, Vlist l2 ->
            assert false (* to complete (question 5) *)
        | _ -> error "unsupported operand types"
      end
  | Eunop (Uneg, e1) ->
      let v1 = expr ctx e1 in
      begin match v1 with
        | Vint n1 -> Vint (-n1)
        | _ -> error "unsupported operand types"
      end
  (* booleans *)
  | Ecst (Cbool b) ->
      Vbool (b)
  | Ebinop (Band, e1, e2) ->
      Vbool (is_true (expr ctx e1) && is_true (expr ctx e2))
  | Ebinop (Bor, e1, e2) ->
      Vbool (is_true (expr ctx e1) || is_true (expr ctx e2))
  | Eunop (Unot, e1) ->
      Vbool (is_false (expr ctx e1))
  | Eident id ->
      assert false (* to complete (question 3) *)
  (* function call *)
  | Ecall ("len", [e1]) ->
      assert false (* to complete (question 5) *)
  | Ecall ("list", [Ecall ("range", [e1])]) ->
      assert false (* to complete (question 5) *)
  | Ecall (f, el) ->
      assert false (* to complete (question 4) *)
  | Elist el ->
      assert false (* to complete (question 5) *)
  | Eget (e1, e2) ->
      assert false (* to complete (question 5) *)

(* interpretation of an instruction; does not return anything *)

and stmt ctx = function
  | Seval e ->
      ignore (expr ctx e)
  | Sprint e ->
      print_value (expr ctx e); printf "@."
  | Sblock bl ->
      block ctx bl
  | Sif (e, s1, s2) ->
      if is_true (expr ctx e) then
        stmt ctx s1
      else
        stmt ctx s2
  | Sassign (id, e1) ->
      assert false (* to complete (question 3) *)
  | Sreturn e ->
      assert false (* to complete (question 4) *)
  | Sfor (x, e, s) ->
      assert false (* to complete (question 5) *)
  | Sset (e1, e2, e3) ->
      assert false (* to complete (question 5) *)

(* interpretation of a block i.e. of a sequence of instructions *)

and block ctx = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

(* interpretation of a file
    - dl is a list of function definitions (see Ast.def)
    - s is an instruction, which represents the global instructions
 *)

let file (dl, s) =
  (* to complete (question 4) *)
  stmt (Hashtbl.create 16) s
