package org.cvogt.test.play.json
abstract class Foo{
  def highlight: Seq[Highlight]
  highlight.map(_ => null)
}
