package reqt

import csp.*, jacop.*

class TestJacopSolver extends munit.FunSuite:
  test("IntVar relation constraints satisfy"):
    val x = IntVar("x")
    val cs: Seq[Constr] = Seq(x === 42)
    val result = cs.satisfy.single(x).get
    assert(result == 42)