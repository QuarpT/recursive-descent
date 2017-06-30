package org.peterc.srdp
import org.peterc.srdp.examples.arithmetic.Arithmetic._
import org.peterc.srdp.examples.json.Json
/**
  * Created by peterc on 28/06/2017.
  */
object Main {
  def main(str: Array[String]) = {
    println(eval"1+2")
    println(eval"((100 + 300) / (4 * 25)) - 2")
  }
}
