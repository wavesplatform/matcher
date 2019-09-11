package com.wavesplatform.it.docker

abstract class DockerContainer {
  def id: String
  def number: Int
  def name: String
  def basePath: String
  def restApiPort: Int
}

class WavesNodeContainer(override val id: String,
                         override val number: Int,
                         override val name: String,
                         override val basePath: String,
                         override val restApiPort: Int,
                         val networkApiPort: Int,
                         val grpcApiPort: Int)
    extends DockerContainer {
  override def toString: String = s"WavesNodeContainer(name=$name, id=$id)"
}

class DexContainer(override val id: String,
                   override val number: Int,
                   override val name: String,
                   override val basePath: String,
                   override val restApiPort: Int)
    extends DockerContainer {
  override def toString: String = s"DexContainer(name=$name, id=$id)"
}
