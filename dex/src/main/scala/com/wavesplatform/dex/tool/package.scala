package com.wavesplatform.dex

package object tool {

  object PrettyPrintHelper {

    def prettyPrintUnusedProperties(unusedProperties: Seq[String]): Unit = {
      if (unusedProperties.nonEmpty) {
        println(s"Warning! Found ${unusedProperties.size} potentially unused properties.")
        println("Unused matcher properties found in waves.dex:")
        unusedProperties.foreach(p => println(s"  $p"))
      } else {
        println("No unused properties in waves.dex found!")
      }
    }

  }


}
