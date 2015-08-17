// test file without imports to avoid regressions with missing imports in macros
sealed trait Modifier
case object early extends Modifier
case object mid extends Modifier
case object late extends Modifier
case class Foo(i: Int)
object a{
  import org.cvogt.play.json.SingletonEncoder.simpleName
  import org.cvogt.play.json.implicits.formatSingleton
  implicit def jsonFormat = org.cvogt.play.json.Jsonx.formatSealed[Modifier]
  implicit def jsonFormat2 = org.cvogt.play.json.Jsonx.formatCaseClass[Foo]
  implicit def jsonFormat3 = org.cvogt.play.json.Jsonx.formatInline[Foo]
}
object b{  
  implicit def jsonFormat4 = org.cvogt.play.json.Jsonx.formatAuto[Foo]
}
