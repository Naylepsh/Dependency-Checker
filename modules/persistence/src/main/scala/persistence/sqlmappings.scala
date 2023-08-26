package persistence

import java.time.Instant
import java.util.UUID

import doobie.*
import doobie.implicits.javatimedrivernative.*
import org.joda.time.DateTime

object sqlmappings:
  given Get[UUID]     = Get[String].map(UUID.fromString)
  given Put[UUID]     = Put[String].contramap(_.toString)
  given Get[DateTime] = Get[String].map(DateTime.parse)
  given Put[DateTime] = Put[Instant]
    .contramap(dt => Instant.ofEpochMilli(dt.getMillis()))
