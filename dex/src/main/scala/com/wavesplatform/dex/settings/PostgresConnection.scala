package com.wavesplatform.dex.settings

import cats.syntax.apply._
import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator._
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

case class PostgresConnection(host: String, portNumber: Int, database: String, user: String, password: String, dataSourceClassName: String) {

  def getConfig: Config = {
    ConfigFactory
      .empty()
      .withValue("dataSource.serverName", ConfigValueFactory.fromAnyRef(host))
      .withValue("dataSource.portNumber", ConfigValueFactory.fromAnyRef(portNumber))
      .withValue("dataSource.databaseName", ConfigValueFactory.fromAnyRef(database))
      .withValue("dataSource.user", ConfigValueFactory.fromAnyRef(user))
      .withValue("dataSource.password", ConfigValueFactory.fromAnyRef(password))
      .withValue("dataSourceClassName", ConfigValueFactory.fromAnyRef(dataSourceClassName))
  }
}

object PostgresConnection {

  implicit val postgresConnectionReader: ValueReader[PostgresConnection] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    (
      cfgValidator.validate[String](s"$path.server-name"),
      cfgValidator.validate[Int](s"$path.port-number"),
      cfgValidator.validate[String](s"$path.database"),
      cfgValidator.validate[String](s"$path.user"),
      cfgValidator.validate[String](s"$path.password"),
      cfgValidator.validate[String](s"$path.data-source-class-name")
    ) mapN PostgresConnection.apply getValueOrThrowErrors
  }
}
