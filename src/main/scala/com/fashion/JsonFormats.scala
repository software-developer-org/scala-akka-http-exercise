package com.fashion

import com.fashion.EvaluationRegistry.ActionPerformed
import spray.json.DefaultJsonProtocol

object JsonFormats  {
  // import the default encoders for primitive types (Int, String, Lists etc)
  import DefaultJsonProtocol._

  implicit val speechesEvaluationJsonFormat = jsonFormat3(SpeechesEvaluation)

  implicit val actionPerformedJsonFormat = jsonFormat1(ActionPerformed)
}
