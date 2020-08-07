# symengine.nim
A Nim wrapper of SymEngine

## Gotchas
- Because SymEngine allocates it's memory on the stack, it means that any expression created in a proc will be deallocated when the proc returns. Because of this you can't return any of the types defined here from a proc. A workaround is to use a template instead. Instead of explicitly returning you, the last statement of the template is returned instead. Here are a few examples of how you can think about converting from a proc to a template:
```nim
proc f_proc(x: SymEngineExpr): SymEngineExpr =
    x^2 + 1

template f_template(x: SymEngineExpr): SymEngineExpr =
    x^2 + 1
```
Because an implicit result variables isn't defined in templates we have to do it our selfs:
```nim
proc f2_proc(x: SymEngineExpr, n: int): SymEngineExpr =
    if n > 0:
        result = x ^ n
    else:
        result = zero

template f2_template(x: SymEngineExpr, n: int): SymEngineExpr =
    var result: SymEngineExpr
    if n > 0:
        result = x ^ n
    else:
        result = zero
    result # we must be explicit that we want to return the variable result
```

# Tutorial
## Symbols
Symbols are what you could call variables like x and y. They are created using the `newSymbol` proc:
```nim
let x = newSymbol("x") # symbol
let y = newSymbol("y") # symbol
let xPlusY = x + y # expression
```
As you can see, once we do any arithmetic with symbols they are automatically converted to expressions instead. 

You can also create undefined functions like `f(x, y)` with the `newFunction` proc:
```nim
let f = newFunction("f", @[x, y]) # f(x, y)
```
Functions can then be used the same way as any other symbol or expression.

## Expressions
Expressions can be created by combining symbol, functions or other expressions with the operators `+ - * / ^` and a whole bunch of functions like `exp`, `ln`, `sqrt`, `sin`, `asin`, `sinh`, `asinh`, etc. Expressions can be differentiated using the two variants of the `diff` proc:
```nim
echo diff(x^2, x) # first derivative
echo diff(x^2, x, 2) # second derivative
echo diff(x^2, x, x) # same as above
echo diff(x*y, x, y) # mixed derivatives
```
The `expand` proc can be used to expand expressions. `solve_poly` can solve polynomial equations up to degree 4. `linsolve` can solve a linear system of equations on the form:
```
eq1 = 0
eq2 = 0
...
eqN = 0
```
You can get the coefficient in front of x^n using `coeff(expr, x, n)`, where x and n can be any expression.

## Matrices
Symbolic matrices can be constructed in different ways. Either by first creating a empty matrix of a certain size with `var A = newMatrix(n, m)` and then assign to it using `A[i, j] = expr` or you can convert seq's of expressions to matrices:
```nim
var B = @[@[x, y], @[zero, one]].toMatrix # zero and one are exported variables representing the symbolic 0 and 1.
var x_row = @[x, y, z].toRow # row matrix
var x_col = @[x, y, z].toCol # column matrix
var x_diag = @[x, y, z].toDiag # diagonal matrix with diagonal [x, y, z]
var x_eye = eye(3) # same as eye(3, 3)
```
If you want to perform an operation on every element of the matrix you can use the `mapIt` template. For example if you want to expand all elements you can do it like this:
```nim
var expanded_B = mapIt(B, it.expand) # 'it' is an injected variable for every element in B.
```
The Jacobian of a set of scalar function with respect to a number of symbols can be calculated:
```nim
var jac = jacobian(@[expr1, expr2, expr3], @[x, y, z])
```
Elementwise differentiation is the same as for expressions. The operators `+ - *` can be used to do calculations using matrices. The procs `det`, `inv` and `transpose` are quite self-explanatory. The syntax `let x = A \ b` solves the matrix equation `A*x = b` using LU factorization. ANd while we talk about factorization does SymEngine support these: `lu`, `ldl`, `fflu` and `ffldu`.

`nrows` and `ncols` can be used to get the number of rows and columns of a matrix and `reshape` (inplace) and `reshaped` (returns new copy) can be used to reshape a matrix.