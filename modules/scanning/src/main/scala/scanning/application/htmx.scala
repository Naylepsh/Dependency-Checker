package scanning.application

import scalatags.Text.all.*

object htmx:
  // TODO: Move this to a dedicated module (/lib?)
  object ajax:
    val get = attr("hx-get")
    val post = attr("hx-post")

  object trigger:
    val attribute = attr("hx-trigger")
    object value:
      val click = "click"

  object swap:
    val attribute = attr("hx-swap")
    object value:
      val outerHTML = "outerHTML"
      val beforeEnd = "beforeend"

  object target:
    val attribute = attr("hx-target")
