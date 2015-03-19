package org.cvogt.play.json

import scala.reflect.macros.blackbox.Context
import play.api.libs.json._
import collection.immutable.ListMap

private[json] class Macros(val c: Context){
  import c.universe._
  private def caseClassFieldsTypes(tpe: Type): Map[String, Type] = {
    val params = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    ListMap(params.map{ field =>
      ( field.name.toTermName.decodedName.toString,
        field.typeSignature)
    }: _*)
  }
  def formatCaseClass[T: c.WeakTypeTag]: Tree = {
    val T = c.weakTypeOf[T]
    val fields = caseClassFieldsTypes(T).map{
      case (k,t) => q"""(json \ $k).as[$t]"""
    }
    val jsonFields = caseClassFieldsTypes(T).map{
      case (k,_) => q""" ${Constant(k)} -> Json.toJson(obj.${TermName(k)}) """
    }

    q"""
      {
        import _root_.play.api.libs.json._      
        new Format[$T]{ 
          def reads(json: JsValue) = JsSuccess(new $T(..$fields))
          def writes(obj: $T) = JsObject(Seq(..$jsonFields))
        }
      }
      """
  }
}

object Jsonx{
  /**
  Generates a PlayJson Format[T] for a case class T with any number of fields (>22 included)
  */
  def formatCaseClass[T]: Format[T] = macro Macros.formatCaseClass[T]
}