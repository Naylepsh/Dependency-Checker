# Ganyu

Python dependency scanner for repositories hosted on gitlab. 
Checks the state of project's dependencies (how up-to/out-of-date they are and their vulnerabilities) and attempts to update them if prompted.


## Currently supported languages & formats

- Python:
  - requirements.txt
  - pyproject.toml

## Pre-requisites

- `scala` [https://www.scala-lang.org/download/]

## Usage

1. Fill out your env vars (see [.env.example](/.env.exampple))
2. Run `sbt run`

