package fos

import org.scalatest._

class EvaluateTest extends WordSpec with Matchers {
  private var id = 0
  def testId() = {
    id += 1
    id
  }
}