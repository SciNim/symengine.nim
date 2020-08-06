# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import symengine

let A = @[1 // 2, 1//1, 3//4].toCol
echo A 
let x = newSymbol("x")
echo diff(x^2, x)
echo diff(x^2, x, x)
echo diff(x^2, x, 2)

