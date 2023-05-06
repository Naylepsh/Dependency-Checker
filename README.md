# Sentinel

Checks the state of your dependencies (how up/out-of-date they are and their vulnerabilities).
The result is presented within an excel spreadsheet.

## Features

### Currently supported languages & formats

- Python:
  - requirements.txt
  - pyproject.toml

### Currently supported export formats

- Excel

## Pre-requisites

- `scala` [https://www.scala-lang.org/download/]

## Installation

1. Rename `registry.example.json` to `registry.json` and fill in your gitlab token
2. Run `sbt stage` to compile & package

## Usage

### Scanning

Run `./target/universal/stage/bin/sentinel scan --registry-path=/path/to/registry.json`

### Exporting

Run:

```
target/universal/stage/bin/sentinel export \
  --registry-path=/path/to/registry.json \
  --export-path=/path/to/export.xlsx
```
