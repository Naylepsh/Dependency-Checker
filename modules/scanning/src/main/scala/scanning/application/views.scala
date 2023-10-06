package scanning.application

import scalatags.Text.all.*

object views:
  def layout(bodyContent: scalatags.Text.Modifier*) =
    html(
      head(
        script(src    := "https://unpkg.com/htmx.org@1.9.4"),
        script(src    := "https://unpkg.com/htmx.org/dist/ext/json-enc.js"),
        script(src    := "https://unpkg.com/hyperscript.org@0.9.11"),
        script(src    := "https://cdn.tailwindcss.com"),
        script(`type` := "text/javascript", src := "/static/stuff.js"),
        link(
          href := "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css",
          rel  := "stylesheet"
        )
      ),
      body(
        cls              := "text-gray-800 bg-white",
        attr("hx-boost") := "true",
        navBar,
        bodyContent
      )
    )

  private val nav = tag("nav")

  private val navBar = nav(
    cls := "flex items-center justify-between flex-wrap bg-blue-500 p-6 text-white text-2xl",
    a(
      cls  := "mr-5",
      href := "/project",
      i(cls := "fa fa-solid fa-house")
    ),
    div(
      cls := "w-full block flex-grow lg:flex lg:items-center lg:w-auto",
      div(
        cls := "text-sm lg:flex-grow",
        a(
          cls  := "block mt-4 lg:inline-block lg:mt-0 hover:text-white mr-4",
          href := "/project",
          "Projects"
        ),
        a(
          cls  := "block mt-4 lg:inline-block lg:mt-0 hover:text-white mr-4",
          href := "/scan/vulnerability",
          "Vulnerabilities"
        )
      )
    )
  )
