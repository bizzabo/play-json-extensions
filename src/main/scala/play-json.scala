package org.cvogt.play.json

import org.cvogt.scala.constraint.boolean.!
import scala.reflect.macros.blackbox.Context
import play.api.libs.json._
import collection.immutable.ListMap

private[json] class Macros(val c: Context){
  import c.universe._
  val pkg = q"_root_.org.cvogt.play.json"
  private def caseClassFieldsTypes(tpe: Type): Map[String, Type] = {
    val params = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    ListMap(params.map{ field =>
      ( field.name.toTermName.decodedName.toString,
        field.typeSignature)
    }: _*)
  }

  def formatCaseClass[T: c.WeakTypeTag](ev: Tree): Tree = {
    val T = c.weakTypeOf[T]
    if(!isCaseClass(T))
      c.error(c.enclosingPosition, s"not a case class: $T")
    val fields = caseClassFieldsTypes(T).map{
      case (k,t) => q"""(json \ $k).as[$t]"""
    }
    val jsonFields = caseClassFieldsTypes(T).map{
      case (k,_) => q""" ${Constant(k)} -> Json.toJson(obj.${TermName(k)}) """
    }

    val pkg = q"_root_.play.api.libs.json"
    q"""
      {
        new Format[$T]{ 
          def reads(json: $pkg.JsValue) = $pkg.JsSuccess(new $T(..$fields))
          def writes(obj: $T) = $pkg.JsObject(Seq(..$jsonFields).filterNot(_._2 == $pkg.JsNull))
        }
      }
      """
  }

  protected def isCaseClass(tpe: Type)
    = tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass
}
trait ImplicitCaseClassFormatDefault{
  implicit def formatCaseClass[T]
    (implicit ev: ![Format[T]])
    : Format[T] = macro Macros.formatCaseClass[T]
}
object implicits extends ImplicitCaseClassFormatDefault

object Jsonx{
  /**
  Generates a PlayJson Format[T] for a case class T with any number of fields (>22 included)
  */
  def formatCaseClass[T]
    (implicit ev: ![Format[T]])
    : Format[T]
    = macro Macros.formatCaseClass[T]
}