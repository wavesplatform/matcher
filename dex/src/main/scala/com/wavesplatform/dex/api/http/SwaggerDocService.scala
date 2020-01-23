package com.wavesplatform.dex.api.http

import com.github.swagger.akka.SwaggerHttpService
import com.github.swagger.akka.model.{Info, License}
import com.wavesplatform.dex.Version
import io.swagger.models.auth.{ApiKeyAuthDefinition, In}
import io.swagger.models.{Scheme, Swagger}

class SwaggerDocService(val apiClasses: Set[Class[_]], override val host: String) extends SwaggerHttpService {

  override val info: Info = Info(
    "The Web Interface to the Waves DEX API",
    Version.VersionString,
    "Waves DEX",
    "License: MIT License",
    None,
    Some(License("MIT License", "https://github.com/wavesplatform/dex/blob/master/LICENSE"))
  )

  // Let swagger-ui determine the host and port
  override val swaggerConfig: Swagger = new Swagger()
    .basePath(SwaggerHttpService.prependSlashIfNecessary(basePath))
    .info(info)
    .scheme(Scheme.HTTP)
    .scheme(Scheme.HTTPS)
    .securityDefinition(SwaggerDocService.apiKeyDefinitionName, new ApiKeyAuthDefinition(`X-Api-Key`.name, In.HEADER))
}

object SwaggerDocService {
  final val apiKeyDefinitionName = "API Key"
}
