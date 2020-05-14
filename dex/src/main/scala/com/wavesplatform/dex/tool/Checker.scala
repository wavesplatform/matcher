package com.wavesplatform.dex.tool

import java.io.File

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory._
import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.domain.account.AddressScheme
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.model.Normalization
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.settings.MatcherSettings.valueReader
import com.wavesplatform.dex.settings.{MatcherSettings, loadConfig}
import com.wavesplatform.dex.tool.Checker.DoubleOps
import com.wavesplatform.wavesj.transactions.IssueTransactionV2
import com.wavesplatform.wavesj.{PrivateKeyAccount, Transactions}
import net.ceedubs.ficus.Ficus._
import play.api.libs.json.Json
import sttp.client._

// noinspection ScalaStyle
case class Checker(dexRestApi: String, nodeRestApiUri: String, version: String, dexConfigPath: String) {

  println(
    s"""
       |Passed arguments:
       |  DEX REST API         : $dexRestApi
       |  WavesNode REST API   : $nodeRestApiUri
       |  Expected DEX version : $version
       |  DEX config path      : $dexConfigPath
       """.stripMargin
  )

  private implicit val backend: SttpBackend[Identity, Nothing, NothingT] = HttpURLConnectionBackend()

  print(s"Processing DEX config... ")

  private val config: Config            = parseFile(new File(dexConfigPath))
  private val settings: MatcherSettings = loadConfig(config).as[MatcherSettings]("waves.dex")
  private val matcherKeyPair            = AccountStorage.load(settings.accountStorage).map(_.keyPair).explicitGet()
  private val dexRestApiUri: String     = s"http://${if (dexRestApi.nonEmpty) dexRestApi else s"${settings.restApi.address}:${settings.restApi.port}"}/"

  AddressScheme.current = new AddressScheme { override val chainId: Byte = settings.addressSchemeCharacter.toByte }

  println("Done")

  println(
    s"""
       |DEX config processing results:
       | Address scheme     : ${AddressScheme.current.chainId}
       | Matcher public key : ${matcherKeyPair.publicKey.toString}
       | Matcher address    : ${matcherKeyPair.publicKey.toAddress}
       | DEX REST API       : $dexRestApiUri
       """.stripMargin
  )

  private def mkIssueAssetTx(name: String, description: String, quantity: Long): IssueTransactionV2 = {
    val matcherAccount = PrivateKeyAccount.fromPrivateKey(matcherKeyPair.privateKey.base58, AddressScheme.current.chainId)
    Transactions.makeIssueTx(matcherAccount, AddressScheme.current.chainId, name, description, quantity, 8, false, null, 1.waves)
  }

  private def issueTestAssets(): Unit = {
    val mnogo       = 3000000
    val issueAwlTx  = mkIssueAssetTx("IIIuJIo", "Awl for the Matcher checking purposes", mnogo.wuJIo)
    val issueSoapTx = mkIssueAssetTx("MbIJIo", "Soap for the Matcher checking purposes", mnogo.mbIJIo)
  }

//  private def check(name: String)(f: () => Boolean): Unit = {
//    print(s"Checking $name... ")
//    f()
//  }

  private def checkVersion(): Boolean = {
    print(s" 1. DEX version... ")

    val uri      = uri"$dexRestApiUri/api-docs/swagger.json"
    val request  = basicRequest.get(uri)
    val response = request.send().body

    response.fold(
      error => { println(s"Cannot parse $uri! $error"); false },
      body => {
        val parsedVersion = (Json.parse(body) \ "info" \ "version").get.as[String].trim
        val checkResult   = parsedVersion == version
        println(if (checkResult) "Passed" else s"""Failed! Expected "$version", but got "$parsedVersion"""")
        checkResult
      }
    )
  }

  def checkState(): Unit = {
    println(s"Checking")
    val isStateCorrect = checkVersion()
    println(s"\n${if (isStateCorrect) "Congratulations! DEX state is valid!" else "DEX state is INVALID!"}\n")
  }

//  def mkIssue(issuer: KeyPair,
//              name: String,
//              quantity: Long,
//              decimals: Int = 8,
//              fee: Long = issueFee,
//              script: Option[ByteStr] = None,
//              reissuable: Boolean = false,
//              timestamp: Long = System.currentTimeMillis): IssueTransaction = {
//    Transactions.makeIssueTx(issuer,
//                             AddressScheme.current.chainId,
//                             name,
//                             s"$name asset",
//                             quantity,
//                             decimals.toByte,
//                             reissuable,
//                             script.map(_.base64).orNull,
//                             fee,
//                             timestamp)
//  }

}

object Checker {
  implicit class DoubleOps(private val value: Double) {
    val wuJIo, mbIJIo, waves: Long = Normalization.normalizeAmountAndFee(value, 8)
  }
}
