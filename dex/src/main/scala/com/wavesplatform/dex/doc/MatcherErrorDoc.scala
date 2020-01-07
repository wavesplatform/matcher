package com.wavesplatform.dex.doc

import com.wavesplatform.dex.error.{Class, Entity, MatcherError}
import com.wavesplatform.dex.meta.DescendantSamples
import com.wavesplatform.dex.util.getSimpleName
import play.api.libs.json.Json

object MatcherErrorDoc {

  object entitySamples extends DescendantSamples[Entity]
  object classSamples  extends DescendantSamples[Class]
  object errorSamples  extends DescendantSamples[MatcherError]

  def mkMarkdown: String = {
    val entities = entitySamples.run
      .map(x => x.code -> getSimpleName(x))

    val classes = classSamples.run
      .map(x => x.code -> getSimpleName(x))

    val entitiesMap = entities.toMap
    val classesMap  = classes.toMap
    val errors = errorSamples.run
      .sortBy(_.code)
      .map { x =>
        val name      = getSimpleName(x)
        val objCode   = getObjectCode(x.code)
        val partCode  = getPartCode(x.code)
        val classCode = getClassCode(x.code)
        (x, objCode, partCode, classCode, name)
      }

    val detailedErrors = errors
      .groupBy(_._2)
      .map {
        case (objCode, xs) =>
          val obj = entitiesMap(objCode)
          val body = xs
            .map {
              case (x, _, _, _, name) =>
                s"""/<a id="error-$name"></a>
                    /#### $name
                    /
                    /Code: ${x.code}
                    /
                    /##### Sample
                    /
                    /```json
                    /${Json.prettyPrint(x.toJson)}
                    /```
                    /""".stripMargin('/')
            }
            .mkString("\n")

          s"""/### ${obj.capitalize} errors
              /
              /$body
              /""".stripMargin('/')
      }
      .mkString("\n")

    val errorsTable = errors.zipWithIndex
      .map {
        case ((x, objCode, partCode, classCode, name), i) =>
          val obj   = entitiesMap(objCode)
          val part  = entitiesMap(partCode)
          val klass = classesMap(classCode)
          s"| ${i + 1} | ${x.code} | $obj | $part | $klass | [$name](#error-$name) |"
      }
      .mkString("\n")

    s"""/## List of entities
        /
        /| # | Code | Name |
        /|--:|-----:|:-----|
        /${toMarkdownTable(entities)}
        /
        /## List of error classes
        /
        /| # | Code | Name |
        /|--:|-----:|:-----|
        /${toMarkdownTable(classes)}
        /
        /
        /## List of errors
        /
        /| # | Code | Object | Part | Class| Name |
        /|--:|-----:|:-------|:-----|:-----|:-----|
        /$errorsTable
        /
        /## Detailed errors
        /
        /$detailedErrors""".stripMargin('/')
  }

  private def getObjectCode(errorCode: Int): Int = (errorCode >> 20) & 0x7FF
  private def getPartCode(errorCode: Int): Int   = (errorCode >> 8) & 0xFFF
  private def getClassCode(errorCode: Int): Int  = errorCode & 0xFF

  private def toMarkdownTable(xs: Seq[(Int, String)]): String =
    xs.sortBy(_._1)
      .zipWithIndex
      .map { case ((code, name), i) => s"""| ${i + 1} | $code | $name |""" }
      .mkString("\n")

}
