package persistence

import java.time.Instant
import java.util.UUID

import doobie.*
import doobie.implicits.javatimedrivernative.*
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object sqlmappings:
  given Get[UUID] = Get[String].map(UUID.fromString)
  given Put[UUID] = Put[String].contramap(_.toString)

  private val formatter =
    DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  given Get[DateTime] = Get[String].map: str =>
    formatter.parseDateTime(str)
  given Put[DateTime] = Put[String]
    .contramap: dt =>
      formatter.print(dt)
