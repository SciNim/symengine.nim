import nimterop/[build, cimport]
#static:
#    cDebug()
#    cDisableCaching()

const baseDir = getProjectCacheDir("symengine")

setDefines(@["cwrapperJBB"])

getHeader(
    "cwrapper.h",
    jbbUri = "symengine",
    outdir = baseDir,
    altNames = "symengine"
    )

cIncludeDir(baseDir / "symengine/include")
cIncludeDir(baseDir / "GMP/include")
cIncludeDir(baseDir / "MPFR/include")
cIncludeDir(baseDir / "MPC/include")

static:
  discard gorgeEx("ln -sf libsymengine.so libsymengine.so.0.6")
  cSkipSymbol(@[
    "basic_get_type", "basic_get_class_id", "basic_get_class_from_id"])#[,
    "integer_set_mpz", "integer_get_mpz", "real_mpfr_set", "real_mpfr_get", "real_mpfr_get_prec",
    "rational_get_mpq", "rational_set_mpq", "complex_set_mpq"
  ])
  ]#
#[
cExclude(baseDir / "GMP/include/gmp.h")
cExclude(baseDir / "GMP/include/gmpxx.h")
cExclude(baseDir / "MPC/include/mpc.h")
cExclude(baseDir / "MPFR/include/mpfr.h")
cExclude(baseDir / "MPFR/include/mpf2mpfr.h")
cImport(cwrapperPath, recurse = true, dynlib = "cwrapperLPath", flags = "--prefix:__,_ --suffix:__,_")#, nimFile="wrapper.nim") 
]#
cPassL(cwrapperLPath)
import strutils
cPassL(join(cwrapperLDeps, " "))
cPassL("-Wl,-rpath -Wl,.")
cImport(cwrapperPath, recurse = true,flags = "--prefix:__,_ --suffix:__,_")#, nimFile="issuewrapper.nim") 