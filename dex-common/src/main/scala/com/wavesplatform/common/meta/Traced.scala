package com.wavesplatform.common.meta

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros._

class traced extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro tracedMacro.impl
}

object tracedMacro {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val inputs: List[Tree] = annottees.map(_.tree)(collection.breakOut)
//    c.info(c.enclosingPosition, s"Inputs: $inputs", force = true)
    val outputs: List[Tree] = inputs match {
      case (cd @ ClassDef(_, cName, typeArgs, _)) :: tail =>
//        c.info(c.enclosingPosition, s"Parsed: $cd, $tail", force = true)
        val mod0: ModuleDef = tail match {
          case (md @ ModuleDef(_, mName, mTemp)) :: Nil if cName.decodedName.toString == mName.decodedName.toString => md

          case Nil =>
            val cMod  = cd.mods
            var mModF = NoFlags
            if (cMod hasFlag Flag.PRIVATE) mModF |= Flag.PRIVATE
            if (cMod hasFlag Flag.PROTECTED) mModF |= Flag.PROTECTED
            if (cMod hasFlag Flag.LOCAL) mModF |= Flag.LOCAL
            val mMod = Modifiers(mModF, cMod.privateWithin, Nil)

            // XXX TODO: isn't there a shortcut for creating the constructor?
            val mkSuperSelect = Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR)
            val superCall     = Apply(mkSuperSelect, Nil)
            val constr        = DefDef(NoMods, termNames.CONSTRUCTOR, Nil, List(Nil), TypeTree(), Block(List(superCall), Literal(Constant(()))))

            val mTemp = Template(parents = List(TypeTree(typeOf[AnyRef])), self = noSelfType, body = constr :: Nil)
            val mName = TermName(cName.decodedName.toString) // or encoded?

            ModuleDef(mMod, mName, mTemp)

          case _ => c.abort(c.enclosingPosition, "Expected a companion object")
        }

        val Template(mTempParents, mTempSelf, mTempBody0) = mod0.impl

        // cf. http://stackoverflow.com/questions/21044957/type-of-a-macro-annottee
        val cTpe       = Ident(TypeName(cd.name.decodedName.toString))
        val typeParam = typeArgs.map(_.name).head
        val ga = TermName(c.freshName("delegate"))
        val fooDef     = q"def traced[F[_]: _root_.cats.Monad]($ga: $cTpe[F])(implicit logger: _root_.com.wavesplatform.it.api.Logger[F]): Traced[F] = new Traced($ga)"


        val methods = cd.impl.children.collect {
          case q"$mods def $name(..$params)(implicit ..$implparams): $resultTpe" if !mods.asInstanceOf[Modifiers].hasFlag(Flag.PRIVATE) =>
            val args = params.map {
              case q"$_ val $paramName: $_ = $_" => paramName
            }

            val m = q"""logger.debug("Foo")"""
            c.info(c.enclosingPosition, s"params: $params\nargs: $args", force = true)
            q"""
override def $name(..$params)(implicit ..$implparams): $resultTpe = {
  _root_.cats.Monad.apply[F].flatMap($m) { _ =>
    $ga.${ TermName(name.asInstanceOf[TermName].decodedName.toString)}(..$args)
  }
}
             """
        }

        val kls =
          q"""
class Traced[F[_]: _root_.cats.Monad](private val $ga: $cTpe[F])(implicit logger: _root_.com.wavesplatform.it.api.Logger[F]) extends $cTpe[F] {
    ..$methods
}
           """

        c.info(c.enclosingPosition,
          s"""Position:
             |${c.enclosingPosition}
             |""".stripMargin,
          force = true)
//        c.info(c.enclosingPosition,
//          s"""=== Class ===
//             |$kls
//             |""".stripMargin,
//          force = true)

        val mTempBody1 = kls :: fooDef :: mTempBody0
        val mTemp1     = Template(mTempParents, mTempSelf, mTempBody1)
        val mod1       = ModuleDef(mod0.mods, mod0.name, mTemp1)

//        c.info(c.enclosingPosition,
//          s"""cTpe = $cTpe
//             |fooDef = $fooDef
//             |mTempBody1 = $mTempBody1
//             |mTemp1 = $mTemp1
//             |mod0.name = ${mod0.name}
//             |""".stripMargin,
//          force = true)

        cd :: mod1 :: Nil

      case _ => c.abort(c.enclosingPosition, "Must annotate a class or trait")
    }

    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }

//  def wrapLogs[T](c: whitebox.Context)(baseType: c.universe.ClassDef): c.Tree = {
//    import c.universe._
//
//    baseType.impl.body.head
//    val body = for {
//      member <- baseType.decls if member.isMethod && member.name.decodedName.toString != "$init$"
//      method     = member.asMethod
//      params     = for { sym <- method.paramLists.flatten } yield q"""${sym.asTerm.name}: ${sym.typeSignature}"""
//      paramsCall = for { sym <- method.paramLists.flatten } yield sym.name
//      methodName = member.asTerm.name.toString
//    } yield {
//      q"""
//  def ${method.name}(..$params)(implicit file: sourcecode.File, line: sourcecode.Line): ${method.returnType} = {
//    println("Method " + $methodName + " was called")
//    delegate.${method.name}(..$paramsCall)
//  }"""
//    }
//
//    q"""
//  {
//    class Traced[${baseType.typeConstructor}](delegate: $baseType) {
//      ..$body
//    }
//
//    def traced[${baseType.typeConstructor}](delegate: $baseType) = new Traced[${baseType.typeConstructor}](delegate)
//  }"""
//  }
}
