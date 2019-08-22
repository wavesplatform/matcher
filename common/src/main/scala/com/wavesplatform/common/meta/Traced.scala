package com.wavesplatform.common.meta

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

//object Traced {
//  def wrap[F[_], T](): T = macro impl[T]
//
//  def impl[F[_], T](c: blackbox.Context)(implicit wtt: c.WeakTypeTag[T]): c.Expr[T] = {
//    import c.universe._
//
//    val baseType = wtt.tpe
//    val body = for {
//      member <- baseType.decls if member.isMethod && member.name.decodedName.toString != "$init$"
//      method     = member.asMethod
//      params     = for { sym <- method.paramLists.flatten } yield q"""${sym.asTerm.name}: ${sym.typeSignature}"""
//      paramsCall = for { sym <- method.paramLists.flatten } yield sym.name
//      methodName = member.asTerm.name.toString
//    } yield {
//      q"""
//def ${method.name}(..$params)(implicit file: sourcecode.File, line: sourcecode.Line): ${method.returnType} = {
//
//  println("Method " + $methodName + " was called")
//  delegate.${method.name}(..$paramsCall)
//}"""
//    }
//
//    c.Expr[T] {
//      q"""
//{
//  class A[${baseType.}](delegate: $baseType) {
//    ..$body
//  }
//  new A
//}"""
//    }
//  }
//}
