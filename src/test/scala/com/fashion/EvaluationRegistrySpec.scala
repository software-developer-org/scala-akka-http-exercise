package com.fashion

import com.fashion.EvaluationRegistry._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.text.SimpleDateFormat
import java.util.Date

class EvaluationRegistrySpec extends AnyWordSpec with Matchers {

  "EvaluationRegistry" should {

    // ================= tests for asCsvArray()
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
    // ====================================================================

    // ================= tests for toSpeeches()
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
    // ====================================================================

    // ================= tests for mostSpeakerForYear()
    """evaluate speaker_1 with most speeches in year 2019""" in {
      val input = Array(
        // in year 2019
        // - topic 1
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_2", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_3", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_4", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        // - topic 2
        Speech("speaker_1", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_2", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_3", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_4", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        // - topic 3
        Speech("speaker_1", "topic_3", new SimpleDateFormat("yyyy-MM-dd").parse("2019-9-21"), 123),
        Speech("speaker_2", "topic_3", new SimpleDateFormat("yyyy-MM-dd").parse("2019-8-21"), 123),
        // - topic 4
        Speech("speaker_1", "topic_3", new SimpleDateFormat("yyyy-MM-dd").parse("2019-1-21"), 123),
        // other years
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2017-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2017-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
      )

      // test
      val result = mostSpeakerForYear(input, 2019)

      // assert
      result should ===("speaker_1")
    }

    """evaluate null with most speeches in year 2019""" in {
      val input = Array(
        // in year 2019
        // - topic 1
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_2", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_3", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_4", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        // - topic 2
        Speech("speaker_1", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_2", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_3", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_4", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        // - topic 3
        Speech("speaker_1", "topic_3", new SimpleDateFormat("yyyy-MM-dd").parse("2019-9-21"), 123), // speaker 1 and 2 has most speeches
        Speech("speaker_2", "topic_3", new SimpleDateFormat("yyyy-MM-dd").parse("2019-8-21"), 123),
        // other years
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2017-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2017-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
      )

      // test
      val result = mostSpeakerForYear(input, 2019)

      // assert
      result should ===("null")
    }

    """evaluate null with most speeches in year 2010 (year does not exist)""" in {
      val input = Array(
        // in year 2019
        // - topic 1
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_2", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_3", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_4", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        // - topic 2
        Speech("speaker_1", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_2", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_3", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_4", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        // - topic 3
        Speech("speaker_1", "topic_3", new SimpleDateFormat("yyyy-MM-dd").parse("2019-9-21"), 123), // speaker 1 and 2 has most speeches
        Speech("speaker_2", "topic_3", new SimpleDateFormat("yyyy-MM-dd").parse("2019-8-21"), 123),
        // other years
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2017-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2017-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
      )

      // test
      val result = mostSpeakerForYear(input, 2010)

      // assert
      result should ===("null")
    }
    // ====================================================================

    // ================= tests for mostSpeakerForTopic()
     """return null speaker for most speeches on a given topic with empty speeches as input""" in {
      val input = Array()

      // test
      val result = mostSpeakerForTopic(input, "any topic")

      // assert
      result should ===("null")
    }

     """return speaker_1 for most speeches on a topic 1""" in {
      val input = Array(
        // in year 2019
        // - topic 1
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_2", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_3", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        Speech("speaker_4", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2019-12-21"), 123),
        // - topic 2
        Speech("speaker_1", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_2", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_3", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        Speech("speaker_4", "topic_2", new SimpleDateFormat("yyyy-MM-dd").parse("2019-10-21"), 123),
        // - topic 3
        Speech("speaker_1", "topic_3", new SimpleDateFormat("yyyy-MM-dd").parse("2019-9-21"), 123), // speaker 1 and 2 has most speeches
        Speech("speaker_2", "topic_3", new SimpleDateFormat("yyyy-MM-dd").parse("2019-8-21"), 123),
        // other years
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2017-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2017-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
        Speech("speaker_1", "topic_1", new SimpleDateFormat("yyyy-MM-dd").parse("2018-12-31"), 123),
      )

      // test
      val result = mostSpeakerForTopic(input, "topic_1")

      // assert
      result should ===("speaker_1")
    }
    // ====================================================================

    // ================= tests for leastWordySpeaker()
    // ====================================================================
  }

}