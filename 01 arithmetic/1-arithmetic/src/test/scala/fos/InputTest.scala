package fos

import org.scalatest._
import scala.util.parsing.input._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import java.io.ByteArrayOutputStream

class InputTest extends FlatSpec with Matchers {

  def testParse(input: String, expectedOutput: String) {
    val outputStream = new ByteArrayOutputStream
    Arithmetic.parse(input, outputStream)(print _)

    outputStream.toString() should be(expectedOutput)
  }

  def testFail(input: String) {
    val outputStream = new ByteArrayOutputStream
    Arithmetic.parse(input, outputStream)(print _)

    assert(outputStream.toString() contains "failure")
  }

  "Our parser" should "parse true correctly" in {
    testParse("true", "True")
  }

  it should "parse false correctly" in {
    testParse("false", "False")
  }

  it should "parse 0 correctly" in {
    testParse("0", "Zero")
  }

  it should "parse succ correctly" in {
    testParse("succ 0", "Succ(Zero)")
    testParse("succ false", "Succ(False)")
    testParse("succ true", "Succ(True)")
    testParse("succ succ true", "Succ(Succ(True))")
    testParse("succ succ 0", "Succ(Succ(Zero))")
  }

  it should "parse pred correctly" in {
    testParse("pred 0", "Pred(Zero)")
    testParse("pred false", "Pred(False)")
    testParse("pred true", "Pred(True)")
    testParse("pred succ true", "Pred(Succ(True))")
  }

  it should "parse iszero correctly" in {
    testParse("iszero 0", "IsZero(Zero)")
    testParse("iszero false", "IsZero(False)")
    testParse("iszero true", "IsZero(True)")
    testParse("iszero succ true", "IsZero(Succ(True))")
  }

  it should "parse if-then-else correctly" in {
    testParse("if true then true else true", "If(True,True,True)")
    testParse("if false then false else false", "If(False,False,False)")
    testParse("if true then false else true", "If(True,False,True)")
    testParse("if iszero succ 0 then false else true", "If(IsZero(Succ(Zero)),False,True)")
    testParse("if iszero succ 0 then succ succ 0 else pred succ pred succ true", "If(IsZero(Succ(Zero)),Succ(Succ(Zero)),Pred(Succ(Pred(Succ(True)))))")
  }

  it should "parse numeric values correctly" in {
    testParse("1", "Succ(Zero)")
    testParse("2", "Succ(Succ(Zero))")
    testParse("5", "Succ(Succ(Succ(Succ(Succ(Zero)))))")
  }

  it should "not parse malformed inputs" in {
    testFail("if")
    testFail("not_in_the_grammar")
    testFail("true true")
    testFail("if true then true")
    testFail("iszero iszero")
    testFail("succ")
    testFail("pred if then true else false")
  }

}