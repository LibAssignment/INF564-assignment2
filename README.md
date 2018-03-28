# INF564 - TD 2 - Mini-Python interpreter (in OCaml)

The goal of this tutorial is to make an interpreter for a very simple piece of Python, called mini-Python ; it is not necessary to know the Python language.

In order to help you build this interpreter, we provide you with its basic structure (in the form of a set of Caml files and a Makefile) that you can retrieve here: [mini-python.tar.gz](https://www.enseignement.polytechnique.fr/informatique/INF564/td/2-ocaml/mini-python.tar.gz) . Once this archive is unzipped with `tar zxvf mini-python.tar.gz` , you get a `mini-python/` directory containing the following files:

filename   | description
-----------|-----------------------------------------------
ast.mli    | the abstract syntax of **mini-Python** (complete)
lexer.mll  | the lexical analyzer (complete)
parser.mly | the parser (complete)
interp.ml  | the interpreter (*to be completed*)
main.ml    | the main program (complete)
Makefile   | to automate compilation (complete)

As for TD 2, the code provided compiles but is incomplete. The executable is called `mini-python` and applies to a **mini-Python** file with the suffix `.py` , as follows:
```shell
./mini-python file.py
```

A **mini-Python** file has the following structure:
```python
# zero, one or more function definitions at the beginning of the file
def fibaux(a, b, k):
    if k == 0:
        return a
    else:
        return fibaux(b, a + b, k-1)

def fib(n):
    return fibaux(0, 1, n)

# one or more statements at the end of the file
print("some values ​​of the Fibonacci sequence:")
for n in [0, 1, 11, 42]:
    print(fib (n))
```

More generally, a **mini-Python** file is composed of an optional list of function declarations, followed by a list of instructions. The instructions are: assignment, conditional, loop (`for`), displaying an expression with `print` , returns a value with `return` or evaluation of an expression. Integer expressions are: a constant (boolean, integer or string of characters), access to a variable, the construction of a list (with notation `[e1, e2, ..., en]` ), access to a list item (with the notation `e[i]` ), the call of a function, or one of the operations `+`, `-`, `*`, also `//`, `==`, `<>`, `<`, `<=`, `>`, `>=`, `and`, `or` and `not`.

We also consider three primitive functions: `list(range(n))` builds the list `[0, 1, 2, ..., n-1]` and `len(l)` returns the length of list `l`. (We will only use `list` and `range` together in this way.)

## Question 1. Arithmetic expressions
For the moment, we only consider arithmetic expressions that do not contain variables. Complete the `expr` function to interpret these expressions. (For the moment, we will ignore the first argument `ctx` of the function `expr`.) Test on the following program:
```python
print(1 + 2*3)
print((3*3 + 4*4) // 5)
print(10-3-4)
```
whose result must be
```
7
5
3
```

Division and Modulo operations must report an error in case of division by zero. To do this, use the `error` function provided in the `interp.ml` file.

To test easily, you can edit the `test.py` file and run the `make` command. This compiles the `mini-python` interpreter and launches it on the `test.py` file.

## Question 2. Boolean and conditional expressions
Complete the functions `is_true` and `is_false`, which respectively determine whether a value is true or false. In Python, the value `None`, the boolean `False`, the integer `0`, the empty string `""` and the empty list `[]` are considered false and the other values ​​are true.

Then complete the `expr` function to interpret boolean constants, comparison operations, `and`, `or`, and `not` operations. In Python, the comparison is structural; we can directly use the OCaml structural comparison, that is, use operations such as `<` values ​​of type `value`. (It's not 100% compatible with Python, but we'll fix it later.)

Finally, complete the function `stmt` to interpret the conditional (construction `Sif`).

Test on the following program:
```python
print(not True and 1 // 0 == 0)
print(1 <2)
if False or True:
    print("ok")
else:
    print("oops")
```
whose result must be
```
False
True
ok
```

## Question 3. Variables
To manage the variables (of the main program but also the local variables and parameters) we will use an environment , namely a hash table passed to the functions `expr` and `stmt` in the form of a `ctx` argument. This table associates each variable with its value. This association table is made with the OCaml `Hashtbl` module and therefore has the type:
```ocaml
  (string, value) Hashtbl.t
```

Complete the `expr` function so that variables can be accessed. This is the case of filtering `Eident id`. Attempting to access a variable that is not yet in the table should cause an error. Similarly, complete the function `stmt` so that we can assign a variable. This is the case of filtering `Sassign (id, e1)`. This time, the variable may or may not be in the table. In the first case, its value is modified.

Finally, complete the function `expr` so that we can concatenate two character strings with the operation `+`.

Test on the following program:
```python
x = 41
x = x + 1
print(x)
b = True and False
print(b)
s = "hello" + "world!"
print(s)
```
whose result must be
```
42
False
hello world!
```

## Question 4. Functions
We will now add the processing of the functions. These are stored in the global table thus declared:
```ocaml
let functions = (Hashtbl.create 17: (string, ident list * stmt) Hashtbl.t)
```

Each function name is associated with a pair consisting of the list of parameters of the function and the instruction that constitutes the body of the function. Complete the `file` function to fill this table with the functions contained in the list `fl`.

Then complete the functions `expr` and `stmt` to interpret a function call. For a call of the form `f(e1, ..., en)` to a function `f` of the form `def ff(x1, ..., xn): body` we must build a new environment that associates with each formal argument `xi` the value of `ei`. We can then interpret the body statement (the body of the function) in this new environment. The return statement will be interpreted using the exception OCaml Return (already defined).

Test on the following program:
```python
def fact (n):
    if n <= 1: return 1
    return n * fact (n-1)

print (fact (10))
```
whose result must be
```
3628800
```

## Question 5. Lists
Finally add the support of the lists. To do this, add the function `expr` to concatenate two lists with the operation `+`, to interpret the call to the primitives `len` (length of a list) and `list(range(n))` (list `[0, 1, 2, ..., n-1]`), and finally to interpret the constructions `[e1, e2, ..., en]` and `e1[e2]`.

Then complete the `stmt` function to interpret the assignment of a list item (filter case `Sset (e1, e2, e3)`).

Finally, complete the function `stmt` to interpret the construct `for`. The construct `Sfor (x, e, s)` assigns the variable `x` successively to the different elements of the list `e` and executes each instruction `s`. The expression `e` must be evaluated once.

Test the given program at the beginning of the topic. The result must be:
```
0
1
89
267914296
```

## Question 5. Other tests
Positive and negative tests are provided. To run your interpreter on these tests, use `make tests` .

## Question 6 (bonus). Structural comparison
On lists, the structural comparison of Python is not exactly the same as that done by OCaml on our type `value array`. Indeed, OCaml first compares the lengths of the tables, then the elements. So, OCaml declares that `[|0;1;1|]` is greater than `[|1|]` , whereas Python declares that `[0,1,1]` is smaller than `[1]` because it implements a lexicographic order on the lists.

Write a `compare_value: value -> value -> int` function to compare two Python values. It sends a strictly negative integer (respectively zero or strictly positive) when the first value is smaller (respectively equal or greater) than the second. Python will be used as a reference when in doubt. Use this function to rectify what was done in question 2.

To do some tests by oneself

## [Solution](https://www.enseignement.polytechnique.fr/informatique/INF564/td/2-ocaml/corrige/interp.ml.html)
