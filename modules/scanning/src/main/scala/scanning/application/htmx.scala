package scanning.application

import scalatags.Text.all.*
import io.circe.Encoder
import io.circe.syntax.*

object htmx:
  // TODO: Move this to a dedicated module (/lib?)
  object ajax:
    val get    = attr("hx-get")
    val post   = attr("hx-post")
    val patch  = attr("hx-patch")
    val delete = attr("hx-delete")

  object trigger:
    val attribute = attr("hx-trigger")
    object value:
      val click  = "click"
      val change = "change"

  object swap:
    val attribute = attr("hx-swap")
    object value:
      val outerHTML = "outerHTML"
      val innerHTML = "innerHTML"
      val beforeEnd = "beforeend"
      val delete    = "delete"

  object target:
    val attribute = attr("hx-target")
    object value:
      def closest(selector: String): String = s"closest $selector"

  object hyperscript:
    val attribute = attr("_", raw = true)

  object extraValues:
    val vals = attr("hx-vals")
    object value:
      def vals[A: Encoder](x: A): String = x.asJson.toString
