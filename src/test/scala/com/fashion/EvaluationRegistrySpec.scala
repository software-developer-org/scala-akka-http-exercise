package com.fashion

import com.fashion.EvaluationRegistry._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.text.SimpleDateFormat
import java.util.Date

class EvaluationRegistrySpec extends AnyWordSpec with Matchers {

  "EvaluationRegistry" should {

    // tests for asCsvArray()
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

    // tests for toSpeeches()
    "create empty speeches with input Array(Array())" in {
      val input: Array[Array[String]] = Array(Array());

      // test
      val result = toSpeeches(input)

      //assert
      result should ===(Array())
    }

    """create empty speeches with input Array(Array(""))""" in {
      val input: Array[Array[String]] = Array(Array(""));

      // test
      val result = toSpeeches(input)

      //assert
      result should ===(Array())
    }

    """create speeches with one speech""" in {
      // data with one speech
      val input: Array[Array[String]] = Array(Array(HEADER_SPEAKER, HEADER_TOPIC, HEADER_DATE, HEADER_WORDS_COUNT), Array("a speaker", "a topic", "2019-12-21", "123"))

      // test
      val result = toSpeeches(input)

      //assert
      result should ===(Array(Speech("a speaker", "a topic", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123)))
    }

    """create speeches with different column order in input""" in {
      // flip topic being in first column and speaker in second row
      val input: Array[Array[String]] = Array(Array(HEADER_TOPIC, HEADER_SPEAKER, HEADER_DATE, HEADER_WORDS_COUNT), Array("a topic", "a speaker", "2019-12-21", "123"))

      // test
      val result = toSpeeches(input)

      //assert
      result should ===(Array(Speech("a speaker", "a topic", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123)))
    }
  }

}