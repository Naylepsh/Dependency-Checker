package domain

import upickle.default.{ReadWriter => RW, macroRW}

object registry {
  case class Project(
      id: String,
      name: String,
      branch: String = "master"
  )
  object Project {
    given RW[Project] = macroRW
  }

  case class Registry(
      host: String,
      token: String,
      projects: List[Project]
  )
  object Registry {
    given RW[Registry] = macroRW
  }
}
