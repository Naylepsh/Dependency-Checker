# Dependency Checker

Checks the state of your dependencies (how up/out-of-date they are and their vulnerabilities).
The result is presented within an excel spreadsheet.

## Features

### Currently supported languages & formats

- Python:
  - requirements.txt
  - pyproject.toml

## Pre-requisites

- `scala` [https://www.scala-lang.org/download/]

## Usage

1. Rename `registry.example.json` to `registry.json` and fill in your gitlab token
2. Run with `sbt run` (or package it first so that you don't have to recompile every time, you know the drill...)
3. The results will be in the same directory in `export.xlsx` file
