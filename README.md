# About

This exercise uses:
- [Akka HTTP](https://doc.akka.io/docs/akka-http/current/introduction.html#using-akka-http)

NB: there are unit tests for each business logic:
- EvaluationRoutesSpec for endpoint testing
- EvaluationRegistrySpec for business logic

# Getting Started

## Server
Run server

```
sbt run
```

## API

GET /evaluation
There are test data (csv files) here: https://github.com/software-developer-org/speeches-example-data

Api syntax: http://localhost:8080/evaluation?url=url1&url=url2...

Example:
```
curl "http://localhost:8080/evaluation?url=https://raw.githubusercontent.com/software-developer-org/speeches-example-data/main/speech1.csv&url=https://raw.githubusercontent.com/software-developer-org/speeches-example-data/main/speech1.csv
```

# Exercise Assumptions

## Data

It is expected that the number of speeches is limited and not in a magnitude of e.g. thousands or millions - since politicians can talk that much ;). The example data assumes max 10k speeches/lines - otherwise a timeout occurs.

# Tests

Run tests:

```
sbt test
```