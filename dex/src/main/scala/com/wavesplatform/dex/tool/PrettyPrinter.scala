package com.wavesplatform.dex.tool

import cats.implicits._
import com.wavesplatform.dex.cli
import com.wavesplatform.dex.cli.ErrorOr

object PrettyPrinter {

  def prettyPrintUnusedProperties(unusedProperties: Seq[String], indent: Option[Int] = None): ErrorOr[Unit] =
    if (unusedProperties.nonEmpty)
      for {
        _ <- cli.log[ErrorOr](s"\nWarning! Found ${unusedProperties.size} potentially unused properties.", indent)
        _ <- cli.log[ErrorOr]("\nUnused matcher properties found in waves.dex:\n", indent)
      } yield unusedProperties.foreach(p => cli.log[ErrorOr](s"$p\n", indent))
    else
      cli.log[ErrorOr]("No unused properties in waves.dex found!\n", indent)

}
