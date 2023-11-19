package scanning.application

import scalatags.Text.all.*

object views:
  private val title = tag("title")

  def layout(
      subTitle: Option[String],
      bodyContent: scalatags.Text.Modifier*
  ) =
    val titleContent = subTitle match
      case Some(sub) => s"$sub | Ganyu"
      case None      => "Ganyu"
    html(
      head(
        title(titleContent),
        script(src    := "https://unpkg.com/htmx.org@1.9.4"),
        script(src    := "https://unpkg.com/htmx.org/dist/ext/json-enc.js"),
        script(src    := "https://unpkg.com/hyperscript.org@0.9.11"),
        script(src    := "https://cdn.tailwindcss.com"),
        script(`type` := "text/javascript", src := "/static/stuff.js"),
        link(
          href := "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css",
          rel  := "stylesheet"
        ),
        link(
          href := "/static/styles.css",
          rel  := "stylesheet"
        ),
        link(
          rel    := "icon",
          `type` := "image/x-icon",
          href   := "/static/favicon.ico"
        )
      ),
      body(
        cls              := "text-gray-200 bg-gray-900",
        attr("hx-boost") := "true",
        navBar,
        bodyContent
      )
    )

  private val nav = tag("nav")

  private val navBar = nav(
    cls := "flex items-center justify-between flex-wrap bg-gray-950 p-6 text-gray-300 text-2xl",
    a(
      cls  := "text-gray-200 mr-5",
      href := "/project",
      i(cls := "fa fa-solid fa-house")
    ),
    div(
      cls := "w-full block flex-grow lg:flex lg:items-center lg:w-auto",
      div(
        cls := "text-sm lg:flex-grow text-blue-300",
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
