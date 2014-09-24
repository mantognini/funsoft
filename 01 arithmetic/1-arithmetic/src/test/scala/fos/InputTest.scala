package fos

import org.scalatest._
import scala.util.parsing.input._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import java.io.ByteArrayOutputStream

class InputTest extends FlatSpec with Matchers {

  "Our parser" should "parse true correctly" in {
    val input = "true"
    val output = new ByteArrayOutputStream
    Arithmetic.parse(input, output)

    output.toString() should be("True")
  }

}