package infra.persistance

import doobie.*
import doobie.implicits.javatimedrivernative.*
import java.util.UUID
import org.joda.time.DateTime
import java.time.Instant

object sqlmappings:
  given Get[UUID]     = Get[String].map(UUID.fromString)
  given Put[UUID]     = Put[String].contramap(_.toString)
  given Get[DateTime] = Get[String].map(DateTime.parse)
  given Put[DateTime] = Put[Instant]
    .contramap(dt => Instant.ofEpochMilli(dt.getMillis()))
