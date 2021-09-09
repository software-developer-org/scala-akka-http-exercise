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

final case class Speech(speaker: String, topic: String, date: Date, wordsCount: Int)
final case class SpeechesEvaluation(mostSpeeches: String, mostSecurity: String, leastWordy: String)

object EvaluationRegistry {

  val speaker = "Redner"
  val topic = "Thema"
  val date = "Datum"
  val wordsCount = "Wörter"

  val dateFormatter = new SimpleDateFormat("yyyy-MM-dd")

  // actor protocol
  sealed trait Command
  final case class Evaluate(urls: Seq[String], replyTo: ActorRef[SpeechesEvaluation]) extends Command
  final case class ActionPerformed(description: String)

  def apply(): Behavior[Command] = registry()

  private def registry(): Behavior[Command] =
    Behaviors.receiveMessage {
      case Evaluate(urls, replyTo) =>
        getData(urls, replyTo)
        Behaviors.same
    }

    private def getData(urls: Seq[String], replyTo: ActorRef[SpeechesEvaluation]) {
      implicit val system = ActorSystem(Behaviors.empty, "SingleRequest")
      implicit val executionContext = system.executionContext
      val speechesFutures = urls.map(httpClientRequest)
      val speeches = Future.sequence((speechesFutures)).map(_.flatten);
      speeches.foreach( speeches => {
          println("======================================================================")
          speeches.foreach((speech) => println("<" + speech + ">"))
          println("======================================================================")
          replyTo ! SpeechesEvaluation("", "", "")
        })
    }

    private def httpClientRequest(url: String): Future[Array[Speech]] = {
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
        .flatMap(_.entity.toStrict(2 minutes)).map(_.data.utf8String)
        .map(asCsvArray)
        .map(toSpeeches)
      return data
    }

    private def asCsvArray(responseData: String): Array[Array[String]] = {
      responseData
        // split per row
        .split('\n')
        .map(row => {
          row
          // split each row into cols
          .split(',')
          // remove trailing whitespaces
          .map(column => column.trim())
        })
    }

    private def toSpeeches(csvArray: Array[Array[String]]): Array[Speech] = {
      val header = csvArray.head
      val speakerPos = header.indexOf(speaker)
      val topicPos = header.indexOf((topic))
      val datePos = header.indexOf((date))
      val wordsCountPos = header.indexOf((wordsCount))

      val data = csvArray.tail
      return data.map(row => {
        // SimpleDateFormat is not thread safe! Hence create a new instance
        val date = new SimpleDateFormat("yyyy-MM-dd").parse(row(datePos));
        val speech = Speech(row(speakerPos), row(topicPos), date, row(wordsCountPos).toInt)
        speech
      })
    }
}
