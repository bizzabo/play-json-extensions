package org.cvogt.test.play.json
import org.cvogt.play.json.{ AdtEncoder, Jsonx }
import play.api.libs.json._

sealed trait Highlight

object Highlight {
  implicit val timeStatementFormat: Format[TimeStatement] = null
  implicit val highlightFormat = Jsonx.formatAdt[Highlight]( AdtEncoder.TypeAsField )
}

final case class TimeStatement(s: String) extends Highlight

