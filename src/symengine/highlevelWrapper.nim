#### SymEngine.nim
import lowlevelWrapper
type
  #SymEngineExpr* = ref basic # make Expr to a object type so we can add more fields like units
  # data: basic
  # unit: Unit
  SymEngineExpr* = object
    data: basic
  SymEngineSymbol* = object
    ## This is used so that we can get compile-time errors in cases SymEngine would raise run-time errors.
    exprField: SymEngineExpr
  SymEngineMatrix* = object
    ## DenseMatrix from SymEngine
    data: ptr CDenseMatrix

template region(name, body: untyped) {.dirty.} = body

region generalStuff:

  proc `$`*(symExpr: SymEngineExpr): string =
    assert not symExpr.data[0].data.isNil, "Symbolic expression is nil on echo"
    $basic_str(symExpr.data)
    # replace("**", "^")

  proc `$`*(symSymbol: SymEngineSymbol): string =
    $symSymbol.exprField

  proc `$`*(matrix: SymEngineMatrix): string =
    assert not matrix.data.isNil
    $dense_matrix_str(matrix.data)

  # define `=` as well. It could use basic_assign
  proc `=destroy`*(x: var SymEngineExpr) =
    # first check if 'x' was moved to somewhere else:
    if not x.data[0].data.isNil:
      basic_free_stack(x.data)
      x.data[0].data = nil # must set to nil so it doesn't get destroyed more than once!

  proc `=`(dest: var SymEngineExpr; source: SymEngineExpr) =
    # protect against self-assignments:
    if dest.data != source.data:
      `=destroy`(dest)
      #basic_new_stack(dest.data) #Why does it work without this? Because it's a shallow copy to the underlying data? It seems like it, but when we change source, dest is untouched because a is pointing to another data now.
      discard basic_assign(dest.data, source.data) # dest.data = source.data # shallow copy

  proc `=destroy`*(x: var SymEngineMatrix) =
    # first check if 'x' was moved to somewhere else:
    if not x.data.isNil:
      dense_matrix_free(x.data)
      x.data = nil

  proc `=`(dest: var SymEngineMatrix; source: SymEngineMatrix) =
    # protect against self-assignments:
    if dest.data != source.data:
      `=destroy`(dest)
      #dest.data = source.data # shallow copy
      dest.data = dense_matrix_new()
      discard dense_matrix_set(dest.data, source.data) # deep copy

  #[
  proc symEngineExprFinalizer*(symExpr: SymEngineExpr) =
    if symExpr.isNil: return
    basic_free_stack(symExpr[])

  proc symEngineSymbolFinalizer*(symExpr: SymEngineSymbol) =
    if symExpr.isNil: return
    basic_free_stack(symExpr.exprField[])
  ]#

  template handleException(excep_code: symengine_exceptions_t) =
    when not defined(danger):
      case excep_code
      of SYMENGINE_NO_EXCEPTION: discard
      of SYMENGINE_RUNTIME_ERROR: raise newException(LibraryError , "SymEngine Runtime Error")
      of SYMENGINE_DIV_BY_ZERO: raise newException(DivByZeroError, "SymEngine DivByZero Error")
      of SYMENGINE_NOT_IMPLEMENTED: raise newException(LibraryError, "SymEngine NotImplemented Error")
      of SYMENGINE_DOMAIN_ERROR: raise newException(LibraryError, "SymEngine Domain Error")
      of SYMENGINE_PARSE_ERROR: raise newException(LibraryError, "SymEngine Parse Error")
      else: raise newException(LibraryError, "SymEngine returned undefined error code")

  template allocExpr(symExpr: SymEngineExpr, code: untyped): untyped =
    basic_new_stack(symExpr.data)
    handleException code
    #[let excep_code = code
    when not defined(danger):
      case excep_code
      of SYMENGINE_NO_EXCEPTION: discard
      of SYMENGINE_RUNTIME_ERROR: raise newException(LibraryError , "SymEngine Runtime Error")
      of SYMENGINE_DIV_BY_ZERO: raise newException(DivByZeroError, "SymEngine DivByZero Error")
      of SYMENGINE_NOT_IMPLEMENTED: raise newException(LibraryError, "SymEngine NotImplemented Error")
      of SYMENGINE_DOMAIN_ERROR: raise newException(LibraryError, "SymEngine Domain Error")
      of SYMENGINE_PARSE_ERROR: raise newException(LibraryError, "SymEngine Parse Error")
      else: raise newException(LibraryError, "SymEngine returned undefined error code")]#

  converter intToExpr*(a: SomeSignedInt): SymEngineExpr {.inline.} =
    allocExpr(result):
      integer_set_si(result.data, a.clong)

  converter uintToExpr*(a: SomeUnsignedInt): SymEngineExpr {.inline.} =
    allocExpr(result):
      integer_set_ui(result.data, a.culong)

  converter floatToExpr*(a: SomeFloat): SymEngineExpr {.inline.} =
    allocExpr(result):
      real_double_set_d(result.data, a.cdouble)

  converter symbolToExpr*(a: SymEngineSymbol): SymEngineExpr {.inline.} =
    a.exprField

  converter symSeqToExprSeq*(s: seq[SymEngineSymbol]): seq[SymEngineExpr] {.inline.} =
    result = newSeq[SymEngineExpr](s.len)
    for i in 0 .. s.high:
      result[i] = symbolToExpr(s[i])

  template strToMPFR*(s: string, prec: int = 80): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      real_mpfr_set_str(result.data, s.cstring, prec.cint)
    result

  template seqToCVecBasic(s: seq[SymEngineExpr]): ptr CVecBasic =
    ## Remember to free it with vecbasic_free after use
    var result = vecbasic_new()
    for i in 0 .. s.high:
      discard vecbasic_push_back(result, s[i].data)
    result

  template cVecBasicToSeq(v: ptr CVecBasic): seq[SymEngineExpr] =
    ## Remember to free it with vecbasic_free after use
    let size = vecbasic_size(v)
    var result = newSeq[SymEngineExpr](size)
    for i in 0 ..< size:
      discard vecbasic_get(v, i.uint, result[i].data)
    result

  let zero* = SymEngineExpr()
  basic_const_zero(zero.data)
  let one* = SymEngineExpr()
  basic_const_one(one.data)
  let pi* = SymEngineExpr()
  basic_const_pi(pi.data)
  let im* = SymEngineExpr()
  basic_const_I(im.data)
  let euler* = SymEngineExpr()
  basic_const_E(euler.data)
  let eulerGamma* = SymEngineExpr()
  basic_const_EulerGamma(eulerGamma.data)
  let catalan* = SymEngineExpr()
  basic_const_Catalan(catalan.data)
  let goldenRatio* = SymEngineExpr()
  basic_const_GoldenRatio(goldenRatio.data)
  let oo* = SymEngineExpr()
  basic_const_infinity(oo.data)
  let noo* = SymEngineExpr()
  basic_const_neginfinity(noo.data)
  let zoo* = SymEngineExpr()
  basic_const_complex_infinity(zoo.data)
  let sym_nan* = SymEngineExpr()
  basic_const_nan(sym_nan.data)


region expressions:

  template newSymbol*(name: string): SymEngineSymbol =
    # make SymEngineSymbol type that converts to SymgEngineExpr.
    # This way we can know if it is a sym or not.
    # expr = symbol.SymEngineExpr (does this work?)
    var result: SymEngineSymbol
    basic_new_stack(result.exprField.data) # create a proc that inits a SymExpr
    discard symbol_set(result.exprField.data, name)
    result

  template newFunction*(name: string, args: seq[SymEngineSymbol]): SymEngineExpr =
    var result: SymEngineExpr
    let args_vec = args.seqToCVecBasic()
    allocExpr(result):
      function_symbol_set(result.data, name.cstring, args_vec)
    vecbasic_free(args_vec)
    result

  template `//`*(a, b: int): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      rational_set_si(result.data, a.clong, b.clong)
    result
  

  proc equal*(a, b: SymEngineExpr): bool =
    let i = basic_eq(a.data, b.data)
    if i == 0: return false
    return true

  template subs*(src, oldExpr, newExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_subs2(result.data, src.data, oldExpr.data, newExpr.data)
    result

  template `+`*(a, b: SymEngineExpr): SymEngineExpr =
    #new result, symEngineExprFinalizer
    #basic_new_stack(result[])
    #discard basic_add(result[], a[], b[])
    var result: SymEngineExpr
    allocExpr(result):
      basic_add(result.data, a.data, b.data)
    result

  template `-`*(a, b: SymEngineExpr): SymEngineExpr =
    #new result, symEngineExprFinalizer
    #basic_new_stack(result[])
    #discard basic_sub(result[], a[], b[])
    var result: SymEngineExpr
    allocExpr(result):
      basic_sub(result.data, a.data, b.data)
    result

  template `-`*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_neg(result.data, symExpr.data)
    result

  template `*`*(a, b: SymEngineExpr): SymEngineExpr =
    #new result, symEngineExprFinalizer
    #basic_new_stack(result[])
    #discard basic_mul(result[], a[], b[])
    var result: SymEngineExpr
    allocExpr(result):
      basic_mul(result.data, a.data, b.data)
    result

  template `/`*(a, b: SymEngineExpr): SymEngineExpr =
    #new result, symEngineExprFinalizer
    #basic_new_stack(result[])
    #discard basic_div(result[], a[], b[])
    var result: SymEngineExpr
    allocExpr(result):
      basic_div(result.data, a.data, b.data)
    result

  template `^`*(a, b: SymEngineExpr): SymEngineExpr =
    #new result, symEngineExprFinalizer
    #basic_new_stack(result[])
    #discard basic_pow(result[], a[], b[])
    var result: SymEngineExpr
    allocExpr(result):
      basic_pow(result.data, a.data, b.data)
    result
    
  template abs*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_abs(result.data, symExpr.data)
    result

  template erf*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_erf(result.data, symExpr.data)
    result

  template erfc*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_erfc(result.data, symExpr.data)
    result

  template sin*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_sin(result.data, symExpr.data)
    result

  template cos*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_cos(result.data, symExpr.data)
    result

  template tan*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_tan(result.data, symExpr.data)
    result

  template asin*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_asin(result.data, symExpr.data)
    result

  template acos*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_acos(result.data, symExpr.data)
    result

  template atan*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_atan(result.data, symExpr.data)
    result

  template csc*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_csc(result.data, symExpr.data)
    result

  template sec*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_sec(result.data, symExpr.data)
    result

  template cot*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_cot(result.data, symExpr.data)
    result

  template acsc*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_acsc(result.data, symExpr.data)
    result

  template asec*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_asec(result.data, symExpr.data)
    result

  template acot*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_acot(result.data, symExpr.data)
    result

  template sinh*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_sinh(result.data, symExpr.data)
    result

  template cosh*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_cosh(result.data, symExpr.data)
    result

  template tanh*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_tanh(result.data, symExpr.data)
    result

  template asinh*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_asinh(result.data, symExpr.data)
    result

  template acosh*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_acosh(result.data, symExpr.data)
    result

  template atanh*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_atanh(result.data, symExpr.data)
    result

  template csch*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_csch(result.data, symExpr.data)
    result

  template sech*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_sech(result.data, symExpr.data)
    result

  template coth*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_coth(result.data, symExpr.data)
    result

  template acsch*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_acsch(result.data, symExpr.data)
    result

  template asech*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_asech(result.data, symExpr.data)
    result

  template acoth*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_acoth(result.data, symExpr.data)
    result

  template lambertw*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_lambertw(result.data, symExpr.data)
    result

  template zeta*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_zeta(result.data, symExpr.data)
    result

  template dirichlet_eta*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_dirichlet_eta(result.data, symExpr.data)
    result

  template gamma*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_gamma(result.data, symExpr.data)
    result

  template loggamma*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_loggamma(result.data, symExpr.data)
    result

  template sqrt*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_sqrt(result.data, symExpr.data)
    result

  template cbrt*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_cbrt(result.data, symExpr.data)
    result

  template exp*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_exp(result.data, symExpr.data)
    result

  template log*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_log(result.data, symExpr.data)
    result


  template atan2*(a, b: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_atan2(result.data, a.data, b.data)
    result

  template kronecker_delta*(a, b: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_kronecker_delta(result.data, a.data, b.data)
    result

  template lowergamma*(a, b: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_lowergamma(result.data, a.data, b.data)
    result

  template uppergamma*(a, b: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_uppergamma(result.data, a.data, b.data)
    result

  template polygamma*(a, b: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_polygamma(result.data, a.data, b.data)
    result

  template beta*(a, b: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_beta(result.data, a.data, b.data)
    result


  template single_diff(symExpr: SymEngineExpr, diffVar: SymEngineSymbol): SymEngineExpr =
    try:
      var result: SymEngineExpr
      allocExpr(result):
        basic_diff(result.data, symExpr.data, diffVar.exprField.data)
      result
    except LibraryError:
      raise newException(ValueError, "diffVar must be a symbol")

  template diff*(symExpr: SymEngineExpr, dVar: SymEngineSymbol, derivOrder = 1): SymEngineExpr =
    var result: SymEngineExpr
    if derivOrder == 0: result = symExpr
    elif derivOrder == 1: result = single_diff(symExpr, dVar)
    else:
      result = single_diff(symExpr, dVar)
      for i in 2 .. derivOrder:
          result = single_diff(result, dVar)
    result

  template diff*(symExpr: SymEngineExpr, dVars: varargs[SymEngineSymbol]): SymEngineExpr =
    var result: SymEngineExpr
    if dVars.len == 0: result = symExpr
    elif dVars.len == 1: result = single_diff(symExpr, dVars[0])
    else:
      result = single_diff(symExpr, dVars[0])
      for i in 1 .. dVars.high:
          result = single_diff(result, dVars[i])
    result

  template solve_poly*(poly: SymEngineExpr, base: SymEngineSymbol): seq[SymEngineExpr] =
    ## Solves a polynomial equation of degrees up to 4. ie if the e
    var base_expr = base.symbolToExpr()
    var cset = setbasic_new()
    let err_code = basic_solve_poly(cset, poly.data, base_expr.data)
    if err_code == SYMENGINE_RUNTIME_ERROR:
      raise newException(ValueError, "Polynomial must be of degree 4 or LESS to be used with solve_poly!")
    let size = setbasic_size(cset)
    var result = newSeq[SymEngineExpr](size)
    for i in 0 .. result.high:
      #var tempbasic: basic
      #basic_new_stack(tempbasic)
      basic_new_stack(result[i].data)
      setbasic_get(cset, i.cint, result[i].data)
    setbasic_free(cset)
    result

  template linsolve*(eqns: seq[SymEngineExpr], syms: seq[SymEngineSymbol]): seq[SymEngineExpr] =
    ## Solves a system of linear equations written in the form:
    ## eqn1 = 0
    ## eqn2 = 0
    ## ...
    ## eqnN = 0
    ## The symbols that we want 
    let eqns_vec = seqToCVecBasic(eqns) # free this after use!
    let syms_vec = seqToCVecBasic(syms) # Free this after use! 
    var result_vec = vecbasic_new()
    try: # should we raise an exception or just return the empty seq?
      handleException vecbasic_linsolve(result_vec, eqns_vec, syms_vec)
    except LibraryError:
      raise newException(ValueError, "There was a problem with your system of equations")
    var result = result_vec.cVecBasicToSeq
    vecbasic_free(eqns_vec)
    vecbasic_free(syms_vec)
    vecbasic_free(result_vec)
    result

  template expand*(symExpr: SymEngineExpr): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      basic_expand(result.data, symExpr.data)
    result

  template coeff*(symExpr, base, n: SymEngineExpr): SymEngineExpr =
    ## Returns the coefficient in front of base ^ n
    var result: SymEngineExpr
    allocExpr(result):
      basic_coeff(result.data, symExpr.data, base.data, n.data)
    result



###############
### Matrix ####
###############

region matrix:

  template nrows*(matrix: SymEngineMatrix): int =
    dense_matrix_rows(matrix.data).int
  
  template ncols*(matrix: SymEngineMatrix): int =
    dense_matrix_cols(matrix.data).int

  template isSquare*(matrix: SymEngineMatrix): bool =
    var result: bool
    if matrix.nrows == matrix.ncols: result = true
    else: result = false
    result

  template newMatrix*(nrows, ncols: int, fill: SymEngineExpr = zero): SymEngineMatrix =
    var result = SymEngineMatrix(data: dense_matrix_new_rows_cols(nrows.cuint, ncols.cuint))
    for i in 0 ..< nrows:
      for j in 0 ..< ncols:
        discard dense_matrix_set_basic(result.data, i.culong, j.culong, fill.data)
    result

  template eye*(nrows, ncols: int, offset = 0): SymEngineMatrix =
    var result = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_eye(result.data, nrows.culong, ncols.culong, offset.cint)
    result

  template eye*(n: int): SymEngineMatrix =
    eye(n, n)

  template `[]`*(matrix: SymEngineMatrix, row, col: int): SymEngineExpr =
    var result: SymEngineExpr
    allocExpr(result):
      dense_matrix_get_basic(result.data, matrix.data, row.culong, col.culong)
    result

  template `[]=`*(matrix: var SymEngineMatrix, row, col: int, insert_expr: SymEngineExpr) =
    discard dense_matrix_set_basic(matrix.data, row.culong, col.culong, insert_expr.data)

  template reshape*(matrix: var SymEngineMatrix, nrowsNew, ncolsNew: int) =
    when not defined(danger):
      doAssert nrows(matrix) * ncols(matrix) == nrowsNew * ncolsNew, "Dimensions for reshape doesn't match!"
    discard dense_matrix_rows_cols(matrix.data, nrowsNew.cuint, ncolsNew.cuint)

  template reshaped*(matrix: SymEngineMatrix, nrows, ncols: int): SymEngineMatrix =
    var result = matrix # copy?
    reshape(result, nrows, ncols)
    result

  template `+`*(a, b: SymEngineMatrix): SymEngineMatrix =
    var result = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_add_matrix(result.data, a.data, b.data)
    result

  template `+`*(a: SymEngineExpr, b: SymEngineMatrix): SymEngineMatrix =
    var result = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_add_scalar(result.data, b.data, a.data)
    result

  template `+`*(a: SymEngineMatrix, b: SymEngineExpr): SymEngineMatrix =
    b + a
  
  template `*`*(a, b: SymEngineMatrix): SymEngineMatrix =
    var result = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_mul_matrix(result.data, a.data, b.data)
    result
  
  template `*`*(a: SymEngineExpr, b: SymEngineMatrix): SymEngineMatrix =
    var result = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_mul_scalar(result.data, b.data, a.data)
    result

  template `*`*(a: SymEngineMatrix, b: SymEngineExpr): SymEngineMatrix =
    b * a

  template `-`*(a: SymEngineMatrix, b: SymEngineMatrix): SymEngineMatrix =
    a + -1*b

  template `-`*(a: SymEngineExpr, b: SymEngineMatrix): SymEngineMatrix =
    a + -1*b

  template `-`*(a: SymEngineMatrix, b: SymEngineExpr): SymEngineMatrix =
    a + -1*b

  template `\`*(A, b: SymEngineMatrix): SymEngineMatrix =
    ## Solve the equation A*x = b using LU_solve
    var result = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_LU_solve(result.data, A.data, b.data)
    result

  template det*(matrix: SymEngineMatrix): SymEngineExpr =
    assert matrix.isSquare, "Matrix must be square for det"
    var result: SymEngineExpr
    allocExpr(result):
      dense_matrix_det(result.data, matrix.data)
    result

  template inv*(matrix: SymEngineMatrix): SymEngineMatrix =
    assert matrix.isSquare, "Matrix must be square for inv"
    var result = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_inv(result.data, matrix.data)
    result

  template transpose*(matrix: SymEngineMatrix): SymEngineMatrix =
    var result = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_transpose(result.data, matrix.data)
    result

  template lu*(matrix: SymEngineMatrix): tuple[l: SymEngineMatrix, u: SymEngineMatrix] =
    ## LU factorization
    var l = SymEngineMatrix(data: dense_matrix_new())
    var u = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_LU(l.data, u.data, matrix.data)
    (l: l, u: u)

  template ldl*(matrix: SymEngineMatrix): tuple[l: SymEngineMatrix, d: SymEngineMatrix] =
    ## LDL factorization
    var l = SymEngineMatrix(data: dense_matrix_new())
    var d = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_LDL(l.data, d.data, matrix.data)
    (l: l, d: d)

  template fflu*(matrix: SymEngineMatrix): SymEngineMatrix =
    ## fraction free LU factorization
    var result = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_FFLU(result.data, matrix.data)
    result

  template ffldu*(matrix: SymEngineMatrix): tuple[l: SymEngineMatrix, d: SymEngineMatrix, u: SymEngineMatrix] =
    ## FFLDU factorization
    var l = SymEngineMatrix(data: dense_matrix_new())
    var d = SymEngineMatrix(data: dense_matrix_new())
    var u = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_FFLDU(l.data, d.data, u.data, matrix.data)
    (l: l, d: d, u: u)

  template single_diff*(matrix: SymEngineMatrix, derivVar: SymEngineSymbol): SymEngineMatrix =
    #matrix.mapIt(it.single_diff(derivVar))
    var result = newMatrix(matrix.nrows, matrix.ncols)
    handleException dense_matrix_diff(result.data, matrix.data, derivVar.exprField.data)
    result

  template jacobian*(functions: seq[SymEngineExpr], derivVars: seq[SymEngineSymbol]): SymEngineMatrix =
    let derivMatrix = derivVars.toCol
    let funcMatrix = functions.toCol
    var result = newMatrix(functions.len, derivVars.len)
    handleException dense_matrix_jacobian(result.data, funcMatrix.data, derivMatrix.data)
    result

  template toMatrix*(s: seq[SymEngineExpr], rowMajor: bool = true): SymEngineMatrix =
    var result: SymEngineMatrix
    let size = s.len
    if rowMajor:
      result = newMatrix(1, size)
      for i in 0 .. s.high:
        result[0, i] = s[i]
    else:
      result = newMatrix(size, 1)
      for i in 0 .. s.high:
        result[i, 0] = s[i]
    result

  template toMatrix*(s: seq[seq[SymEngineExpr]], rowMajor: bool = true): SymEngineMatrix =
    # we must check that all have the same length!
    if s.len == 0:
      raise newException(ValueError, "s.len must be more than 0, Matrix must have at least one entry.")
    var result: SymEngineMatrix
    if rowMajor:
      let nrows = s.len
      let ncols = s[0].len
      result = newMatrix(nrows, ncols)
      for i in 0 ..< nrows:
        if s[i].len != ncols:
          raise newException(ValueError, "All rows must be of same length!")
        for j in 0 ..< ncols:
          result[i, j] = s[i][j]
    else:
      let ncols = s.len
      let nrows = s[0].len
      result = newMatrix(nrows, ncols)
      for j in 0 ..< ncols:
        if s[j].len != nrows:
          raise newException(ValueError, "All cols must be of same length!")
        for i in 0 ..< nrows:
          result[i, j] = s[j][i]
    result


  template toRow*(s: seq[SymEngineExpr]): SymEngineMatrix =
    toMatrix(s, rowMajor = true)

  template toCol*(s: seq[SymEngineExpr]): SymEngineMatrix =
    toMatrix(s, rowMajor = false)

  template toDiag*(s: seq[SymEngineExpr], offset: int = 0): SymEngineMatrix =
    var s_vec = s.seqToCVecBasic() # free this!
    var result = SymEngineMatrix(data: dense_matrix_new())
    discard dense_matrix_diag(result.data, s_vec, offset.clong)
    vecbasic_free(s_vec)
    result

  template toFlatSeq*(matrix: SymEngineMatrix, rowMajor = true): seq[SymEngineExpr]  = # this flattens out matrices if you only want to iterate over fields
    let nrows = matrix.nrows
    let ncols = matrix.ncols
    let n = nrows * ncols
    var result = newSeqOfCap[SymEngineExpr](n)
    if rowMajor:
      for i in 0 ..< nrows:
        for j in 0 ..< ncols:
          result.add matrix[i, j]
    else:
      for j in 0 ..< ncols:
        for i in 0 ..< nrows:
          result.add matrix[i, j]
    result
  
  template toNestSeq*(matrix: SymEngineMatrix, rowMajor = true): seq[seq[SymEngineExpr]] = # have rowMajor parameter
    let nrows = matrix.nrows
    let ncols = matrix.ncols
    var result = newSeq[seq[SymEngineExpr]]()
    if rowMajor:
      result = newSeq[seq[SymEngineExpr]](nrows)
      for i in 0 ..< nrows:
        for j in 0 ..< ncols:
          result[i].add matrix[i, j]
    else:
      result = newSeq[seq[SymEngineExpr]](ncols)
      for j in 0 ..< ncols:
        for i in 0 ..< nrows:
          result[j].add matrix[i, j]
    result

  template mapIt*(matrix: SymEngineMatrix, f: untyped): SymEngineMatrix =
    block:
      var it {.inject.}: SymEngineExpr
      let nrows = matrix.nrows
      let ncols = matrix.ncols
      var result = newMatrix(nrows, ncols)
      for i in 0 ..< nrows:
        for j in 0 ..< ncols:
          it = matrix[i, j]
          it = f
          result[i, j] = it
      result

when isMainModule:
  block:
    echo "Block 1"
    var x, y, z: basic
    basic_new_stack(x)
    basic_new_stack(y)
    basic_new_stack(z)
    discard symbol_set(x, "x")
    discard symbol_set(y, "y")
    discard symbol_set(z, "z")
    assert is_a_Number(x) == 0
    assert is_a_Number(y) == 0
    assert is_a_Number(z) == 0
    echo basic_str(x)
    var a: basic
    discard basic_add(a, x, y)
    echo basic_str(a)
    basic_const_pi(a)
    echo basic_str(a)
    discard basic_assign(x, a)
    echo "Test begins"
    echo basic_str(a)
    echo basic_str(x)
    discard basic_assign(a, y)
    echo basic_str(a)
    echo basic_str(x)
    echo basic_str(y)
  block:
    echo "Block 2"
    let x = newSymbol("xa")
    echo "X created"
    let y = newSymbol("y")
    echo x, y
    let exp1 = x + y
    echo exp1
    echo exp1 + x
    echo x - y
    echo x*y
    echo x/y
    echo x^y
    echo x
    #echo basic_str_ccode((x^(-2+x)).data)
    #echo (x-x) / 0
    #echo x / 0.0
    #echo -(x+y), " ", -1*(x+y)
    echo pi
    echo im
    echo euler
    echo eulerGamma
    echo catalan
    echo goldenRatio
    echo oo
    echo noo
    echo zoo
    echo sym_nan
    echo zero
    echo equal(-oo, noo)
  block:
    echo "#################\nBlock3"
    let x = newSymbol("x")
    let y = newSymbol("y")
    echo "dy/dx = ", single_diff(y, x)
    echo "d/dx(3*x^2 + x*4 - 3) = ", single_diff(3*x^2 + x*4 - 3, x)
    echo "d/dx(x^x) = ", single_diff(x^x, x)
    echo expand(x*(x+1))
    echo equal(x^2+x, x+x^2)
    echo "d/dx(tan(x)) = ", single_diff(tan(x), x)
    echo acoth(x)
    echo 2 * x / x / 3
  block:
    echo "#################\nBlock4"
    let x = newSymbol("x")
    let y = newSymbol("y")
    let a = newSymbol("a")
    let b = newSymbol("b")
    let c = newSymbol("c")
    echo solve_poly(x^4 + x, x)
    echo expand((a + im) * im)
    #echo expand(expand((x-1)^1000))
    let expr1 = x^2 + 2
    echo expr1
    let expr2 = expr1
    echo expr1
    echo expr2
  region "Show that the copy and the source can be changed without touching the other because they are objects containing pointers which change on assignment.":
    let x = newSymbol("x")
    let a = x^2 + 1
    let b = a
    echo a
    echo b
    discard basic_add(a.data, a.data, a.data)
    echo a
    echo b
    echo strToMPFR("2") * strToMPFR("1.234567", 200)
    echo strToMPFR("2", 200) * strToMPFR("1.234567", 200)
    echo strToMPFR("2", 200) * strToMPFR("1.234567")
    echo strToMPFR("2", 200)
  block:
    echo "#################\nBlock4"
    let x = newSymbol("x")
    var A = newMatrix(2, 2, 2*x + sin(x))
    echo A
    echo A[0, 1]
    A[1,1] = 7
    echo "A: ", A
    var B = A
    echo "B: ", B
    A[0, 0] = zero
    echo "A: ", A
    echo "Delete A!"
    `=destroy`(A)
    echo "B: ", B
    B[1, 0] = atan(x)
    echo "B:\n", B
    var a = x^2 + 1
    var b = a
    echo "a: ", a
    echo "b: ", b
    echo "deleta a!"
    `=destroy`(a)
    echo "b: ", b
    echo B.toFlatSeq()
    echo B.toFlatSeq(false)
    echo B.toNestSeq()
    echo B.toNestSeq(false)
    echo @[1*x, 2*x, 3*x].toMatrix
    echo @[1*x, 2*x, 3*x].toRow
    echo @[1*x, 2*x, 3*x].toCol
    echo B + B
    echo B * B
    echo 1 + B
    echo B + 1
    echo 2*B
    echo B*2
    echo B - 1
    echo (1 - B).mapIt(it.expand)
    echo mapIt(B - B, it.expand)
  block:
    let x = newSymbol("x")
    let y = newSymbol("y")
    echo linsolve(@[-2*x - 4 + y, 3*x + y - 9], @[y, x])
    #echo linsolve(@[x*y + 1, x*y - 2], @[x, y]) fails, which it should
    echo subs((x-1)^3, sqrt(x-1), y+1)
    echo coeff(y*x^2 - 2*x + 7, x, 1)
    let f = newFunction("f", @[x, y])
    echo single_diff(single_diff(f, y), x)
    echo subs(single_diff(single_diff(f, y), x), f, x^2*y)
  block:
    let x = newSymbol("x")
    let y = newSymbol("y")
    var A = newMatrix(3, 4, x)
    echo A
    echo reshaped(A, 1, 12)
    echo reshaped(A, 4, 3)
    echo A
    A.reshape(2, 6)
    echo A
    var B = newMatrix(2, 2, x)
    B[0, 0] = y
    echo "B:\n", B
    echo det(B)
    echo inv(B)
    echo inv(B) * B
    echo (B * inv(B)).mapIt(expand(it))
    echo transpose(B)
    echo B \ @[one, one].toCol
    echo @[@[one, one], @[2*one, 2*one]].toMatrix()
    echo @[@[one, 2], @[one*2, one]].toMatrix \ @[one, zero].toCol
    echo @[@[one, 2], @[one*2, one]].toMatrix * @[-one/3, 2*one/3].toCol
    echo @[-1//3, 2//3]
    echo lu(@[@[1*x, 1], @[2//1, x]].toMatrix)
    echo ldl(@[@[1*x, 1], @[2//1, x]].toMatrix)
    echo fflu(@[@[1*x, 1], @[2//1, x]].toMatrix)
    echo ffldu(@[@[1*x, 1], @[2//1, x]].toMatrix)
    echo @[one, 2, 3, 4].toDiag()
    echo eye(3)
    echo eye(3, 4, offset = 2)
    let K = newMatrix(2, 2, x^2)
    echo single_diff(K, x)
    echo jacobian(@[x+y, x*y], @[x, y])
  #[block:
    var x, y, e: basic
    basic_new_stack(x)
    basic_new_stack(y)
    basic_new_stack(e)
    discard symbol_set(x, "x")
    discard symbol_set(y, "y")
    discard symbol_set(e, "e")
    var B = dense_matrix_new()
    var C = dense_matrix_new()
    var D = dense_matrix_new()
    discard dense_matrix_rows_cols(B, 2, 1);
    discard basic_add(e, x, y);
    discard dense_matrix_set_basic(B, 0, 0, e);
    discard basic_mul(e, x, y);
    discard dense_matrix_set_basic(B, 1, 0, e);
    discard dense_matrix_rows_cols(C, 2, 1);
    discard dense_matrix_set_basic(C, 0, 0, x);
    discard dense_matrix_set_basic(C, 1, 0, y);
    discard dense_matrix_rows_cols(D, 2, 2);
    discard dense_matrix_jacobian(D, B, C);
    let result = dense_matrix_str(D);
    echo "Here it comes:"
    echo result
    echo dense_matrix_str(B)
    echo dense_matrix_str(C)]#