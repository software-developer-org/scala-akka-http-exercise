package com.fashion

import org.scalatest.wordspec.AnyWordSpec
import com.fashion.EvaluationRegistry._
import org.scalatest.matchers.should.Matchers

class EvaluationRegistrySpec extends AnyWordSpec with Matchers {

  "EvaluationRegistry" should {
    "create empty csv for empty input" in {
      // empty input
      val input ="";

      // test
      val result = asCsvArray(input)

      // assert
      val expectedResult = Array(Array(""))
      result should ===(expectedResult)
    }

    "create empty csv for data one row (only header)" in {
      // input only with header
      val input ="a,b,c";

      // test
      val result = asCsvArray(input)

      // assert
      val expectedResult = Array(Array("a", "b", "c"))
      result should ===(expectedResult)
    }

    "create empty csv array with 2 rows (header and data)" in {
      // input only with header
      val input = """a,b,c
1,2,3"""

      // test
      val result = asCsvArray(input)
      println(">>>>result:" + result)

      // assert
      val expectedResult = Array(Array("a", "b", "c"), Array("1", "2", "3"))
      result should ===(expectedResult)
    }
  }

}