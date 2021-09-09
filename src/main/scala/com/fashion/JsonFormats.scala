package com.fashion

import com.fashion.EvaluationRegistry.ActionPerformed
import spray.json.DefaultJsonProtocol

object JsonFormats  {
  // import the default encoders for primitive types (Int, String, Lists etc)
  import DefaultJsonProtocol._

  implicit val speechJsonFormat = jsonFormat3(Speech)
  implicit val speechesJsonFormat = jsonFormat1(Speeches)

  implicit val actionPerformedJsonFormat = jsonFormat1(ActionPerformed)
}
