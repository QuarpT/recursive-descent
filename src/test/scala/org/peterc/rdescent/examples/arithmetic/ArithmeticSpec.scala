package org.peterc.rdescent.examples.arithmetic

import org.specs2.mutable.Specification
import org.peterc.rdescent.examples.arithmetic.Arithmetic._

class ArithmeticSpec extends Specification {

  "simple expressions" >> {
    "1" should {
      "equal 1" in {
        eval"1".get === 1
      }
    }

    "1 + 2" should {
      "equal 3" in {
        eval"1 + 2".get === 3
      }
    }

    "4 / 2" should {
      "equal 2" in {
        eval"4 / 2".get === 2
      }
    }

    "4 * 2" should {
      "equal 8" in {
        eval"4 * 2".get === 8
      }
    }

    "5 - 2" should {
      "equal 3" in {
        eval"5 - 2".get === 3
      }
    }

  }

  "branching expressions" >> {
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
