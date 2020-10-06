package com.wavesplatform.dex.settings

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}

case class PostgresConnection(serverName: String, portNumber: Int, database: String, user: String, password: String, dataSourceClassName: String) {

  def getQuillContextConfig: Config =
    ConfigFactory
      .empty()
      .withValue("dataSource.serverName", ConfigValueFactory.fromAnyRef(serverName))
      .withValue("dataSource.portNumber", ConfigValueFactory.fromAnyRef(portNumber))
      .withValue("dataSource.databaseName", ConfigValueFactory.fromAnyRef(database))
      .withValue("dataSource.user", ConfigValueFactory.fromAnyRef(user))
      .withValue("dataSource.password", ConfigValueFactory.fromAnyRef(password))
      .withValue("dataSourceClassName", ConfigValueFactory.fromAnyRef(dataSourceClassName))

}
