package parsers.python

enum PackageManagementFiles:
  case PoetryFiles(pyProjectContent: String, lockContent: String)
      extends PackageManagementFiles
  case RequirementFile(content: String) extends PackageManagementFiles

