package com.wavesplatform.it.docker

import com.wavesplatform.dex.it.docker.DockerContainer

class DexContainer(override val id: String,
                   override val number: Int,
                   override val name: String,
                   override val basePath: String,
                   override val restApiPort: Int)
  extends DockerContainer {
  override def toString: String = s"DexContainer(name=$name, id=$id)"
}
