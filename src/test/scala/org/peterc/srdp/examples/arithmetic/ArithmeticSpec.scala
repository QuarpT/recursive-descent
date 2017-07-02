package org.peterc.srdp.examples.arithmetic

import org.specs2.mutable.Specification
import org.peterc.srdp.examples.arithmetic.Arithmetic._

class ArithmeticSpec extends Specification {

  "string interpolation" >> {
    "((1 + 5) * 50) / (3 - 1)" should {
      "equal 300" in {
        eval"((1 + 5) * 50) /  (3 - 1)".get === 150
      }
    }

    "(2 * 2) / 4" should {
      "equal 0" in {
        eval"2 - ((4 * 3) / 6)".get === 0
      }
    }
  }

}
