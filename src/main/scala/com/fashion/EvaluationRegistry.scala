package com.fashion

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.util.Timeout

import scala.collection.immutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import java.time.format.DateTimeFormatter
import java.text.SimpleDateFormat
import java.util.GregorianCalendar
import java.util.Date
import java.util.Calendar

final case class Speech(speaker: String, topic: String, date: Date, wordsCount: Int)
final case class SpeechesEvaluation(mostSpeeches: String, mostSecurity: String, leastWordy: String)

object EvaluationRegistry {

  val HEADER_SPEAKER = "Redner"
  val HEADER_TOPIC = "Thema"
  val HEADER_DATE = "Datum"
  val HEADER_WORDS_COUNT = "Wörter"

  val dateFormatter = new SimpleDateFormat("yyyy-MM-dd")

  // actor protocol
  sealed trait Command
  final case class Evaluate(urls: Seq[String], replyTo: ActorRef[SpeechesEvaluation]) extends Command
  final case class ActionPerformed(description: String)

  def apply(): Behavior[Command] = registry()

  private def registry(): Behavior[Command] =
    Behaviors.receiveMessage {
      case Evaluate(urls, replyTo) =>
        implicit val system = ActorSystem(Behaviors.empty, "SingleRequest")
        implicit val executionContext = system.executionContext
        // list containing a future for each request
        val listOfFutures = urls.map(httpClientRequest)
        // move all into one future containing a list with string data for each response
        val responseFuture = Future.sequence((listOfFutures));
        // map to speeches and evaluate
        responseFuture.foreach(responseDataList => {
          val speeches = getSpeeches(responseDataList)
          val speechesEvaluation = evaluate(speeches, 2012, "Innere Sicherheit");
          replyTo ! speechesEvaluation
        })
        Behaviors.same
    }

    def getSpeeches(responseDataList: Seq[String]): Seq[Speech] = {
      responseDataList
        .map(asCsvArray)
        .map(toSpeeches)
        .flatten
    }

    def evaluate(speeches: Seq[Speech], year: Int, topic: String): SpeechesEvaluation = {
        val mostSpeeches = mostSpeakerForYear(speeches.toList, year)
        val mostSecurity = mostSpeakerForTopic(speeches, topic)
        val leastWordy= leastWordySpeaker(speeches)
        SpeechesEvaluation(mostSpeeches, mostSecurity, leastWordy);
    }

    def httpClientRequest(url: String): Future[String] = {
      // HTTP client request
      implicit val system = ActorSystem(Behaviors.empty, "SingleRequest")
      val request = HttpRequest(uri = url)
      val responseFuture: Future[HttpResponse] = Http().singleRequest(request)
      // needed for future's flatMap and onComplete in the end
      implicit val executionContext = system.executionContext
      responseFuture
        .onComplete {
          case Success(res) => // noop
          case Failure(res) => res.printStackTrace
        }

      // subscribe and handle response
      import scala.language.postfixOps
      val data = responseFuture
        .flatMap(_.entity.toStrict(1 minutes)).map(response => {
          response.data.utf8String
        })
      return data
    }

    def asCsvArray(responseData: String): Array[Array[String]] = {
      val csvArray = responseData
        // split per row
        .split('\n')
        .map(row => {
          row
          // split each row into cols
          .split(',')
          // remove trailing whitespaces
          .map(column => column.trim())
        })
      return csvArray
    }

    def toSpeeches(csvArray: Array[Array[String]]): Array[Speech] = {
      val header = csvArray.head
      // expecting ALL 4 header keys exists!
      val speakerPos = header.indexOf(HEADER_SPEAKER)
      val topicPos = header.indexOf(HEADER_TOPIC)
      val datePos = header.indexOf(HEADER_DATE)
      val wordsCountPos = header.indexOf(HEADER_WORDS_COUNT)

      val data = csvArray.tail
      val speeches = data.map(row => {
        // SimpleDateFormat is not thread safe! Hence create a new instance
        val date = new SimpleDateFormat("yyyy-MM-dd").parse(row(datePos));
        val speech = Speech(row(speakerPos), row(topicPos), date, row(wordsCountPos).toInt)
        speech
      })
      return speeches
    }

    def mostSpeakerForYear(speeches: Seq[Speech], year: Int): String = {
      // filter by topic
      val speakerForYear = speeches.filter(speech => {
        val cal = new GregorianCalendar();
        cal.setTime(speech.date);
        val yearOfSpeech = cal.get(Calendar.YEAR);
        year == yearOfSpeech
      }).map(s => s.speaker)

      // creates: Map(speaker1 -> 3, speaker2 -> 5, ...)
      val speechesPerSpeaker = speakerForYear.groupBy(identity).mapValues(_.size).toList

      // calculate result by max count
      val mostSpeakerForYearOption = speechesPerSpeaker.reduceOption((a, b) => if (a._2 > b._2) a else b)
      val mostSpeakerForYear = mostSpeakerForYearOption match {
        case None => "null"
        case Some(value) => {
          val counts = speechesPerSpeaker.map(a => a._2)
          if (counts.indexOf(value._2) == counts.lastIndexOf(value._2)) value._1 else "null"
        }
      }
      mostSpeakerForYear
    }

    def mostSpeakerForTopic(speeches: Seq[Speech], topic: String): String = {
      // filter by topic
      val speakerByTopic = speeches.filter(s => s.topic == topic).map(s => s.speaker);

      // creates: Map(speaker1 -> 3, speaker2 -> 5, ...)
      val speechesPerSpeaker = speakerByTopic.groupBy(identity).mapValues(_.size).toList

      // calculate result by max count
      val mostSpeakerForTopicOption = speechesPerSpeaker.reduceOption((a, b) => if (a._2 > b._2) a else b)
      val mostSpeakerForTopic = mostSpeakerForTopicOption match {
        case None => "null"
        case Some(value) => {
          val counts = speechesPerSpeaker.map(a => a._2)
          if (counts.indexOf(value._2) == counts.lastIndexOf(value._2)) value._1 else "null"
        }
      }
      mostSpeakerForTopic
    }

    def leastWordySpeaker(speeches: Seq[Speech]): String = {
      // creates: Map(speaker1 -> [speech1, speech2], speaker2 -> [speech3, speech4], ...)
      val speechesPerSpeaker = speeches.map(s => (s.speaker, s.wordsCount)).groupBy(a => a._1)
      val speechesWithTotalCountPerSpeaker = speechesPerSpeaker.map(a => (a._1, a._2.reduce((x, y)  => (x._1, x._2 + y._2)))).map(_._2)
      val leastSpeakerOption = speechesWithTotalCountPerSpeaker.reduceOption((a, b) => if (a._2 < b._2) a else b);
      val leastSpeaker = leastSpeakerOption match {
        case None => "null"
        case Some(value) => {
          val counts = speechesWithTotalCountPerSpeaker.map(a => a._2).toSeq
          if (counts.indexOf(value._2) == counts.lastIndexOf(value._2)) value._1 else "null"
        }
      }
      return leastSpeaker
    }
}
