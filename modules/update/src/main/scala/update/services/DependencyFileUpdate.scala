package update.services

import update.domain.FileType

object DependencyFileUpdate:
  def replaceDependency(
      fileType: FileType,
      fileContent: String,
      name: String,
      from: String,
      to: String
  ): String =
    val symbollessFrom = from.replaceAll(removeSymbolsRegex, "")
    fileType match
      case FileType.Txt =>
        replaceDependencyInTxt(fileContent, name, symbollessFrom, to)
      case FileType.Toml =>
        replaceDependencyInToml(fileContent, name, symbollessFrom, to)

  private def replaceDependencyInTxt(
      fileContent: String,
      name: String,
      from: String,
      to: String
  ): String =
    fileContent
      .split("\n")
      .map: line =>
        val index                = line.indexOf(name)
        val indexOfCharAfterName = index + name.length
        val isLineNameAndVersion = index == 0
          && line.length > indexOfCharAfterName
          && versionComparisonSymbols.contains(line(indexOfCharAfterName))
        if isLineNameAndVersion then
          line.replace(from, to)
        else
          line
      .mkString("\n") + "\n"

  private def replaceDependencyInToml(
      fileContent: String,
      name: String,
      from: String,
      to: String
  ): String =
    fileContent
      .split("\n")
      .map: line =>
        val index                = line.indexOf(name)
        val indexOfCharAfterName = index + name.length
        val isLineNameAndVersion = index == 0
          && line.length > indexOfCharAfterName
          && line(indexOfCharAfterName) == ' '
        if isLineNameAndVersion then
          line.replace(from, to)
        else
          line
      .mkString("\n") + "\n"

  private val versionComparisonSymbols = List('=', '>', '^', '~')
  private val removeSymbolsRegex =
    versionComparisonSymbols.mkString("[", "", "]")
