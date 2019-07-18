package com.wavesplatform.dex.smart

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.error.ProduceError.produce
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType, OrderV1}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.{FreeSpecLike, Matchers}

import scala.util.Try

class MatcherScriptRunnerTest extends FreeSpecLike with Matchers with TransactionGen with NoShrink {
  private val sampleOrder = OrderV1(
    sender = KeyPair("test".getBytes()),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Waves, IssuedAsset(ByteStr("asset".getBytes("utf-8")))),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = 1L,
    expiration = 1000L,
    matcherFee = 30000L
  )

  private def run(script: Script): Either[String, (Log, Either[String, Terms.EVALUATED])] = Try(MatcherScriptRunner(dAppScriptBlockchain, sampleOrder)).toEither.left.map(_.toString)

  "dApp sunny day" in {
    run(dAppScriptSunny).explicitGet()._2.explicitGet() shouldBe Terms.FALSE
  }

  "Blockchain functions are disabled in dApp" in {
    run(dAppScriptBlockchain) should produce("""An access to the blockchain.accountData is denied on DEX""".r)
  }

  private def dAppScriptSunny: Script =
    ScriptCompiler
      .compile(
        s"""|{-# STDLIB_VERSION 3 #-}
            |{-# CONTENT_TYPE DAPP #-}
            |{-# SCRIPT_TYPE ACCOUNT #-}
            |
            |let addr = addressFromPublicKey(base58'H1kGVNdJwV7N5dF73YWs1R8uet6g5bCvTHmTnYy1hSnr')
            |
            |@Verifier(x)
            |func verifier() = {
            |    match(x) {
            |      case o:Order => o.sender == addr
            |      case _ => false
            |    }
            |}
            |""".stripMargin
      )
      .explicitGet()
      ._1

  private def dAppScriptBlockchain: Script =
    ScriptCompiler
      .compile(
        s"""|{-# STDLIB_VERSION 3 #-}
            |{-# CONTENT_TYPE DAPP #-}
            |{-# SCRIPT_TYPE ACCOUNT #-}
            |
            |## Protocol constants
            |let waves = 100000000
            |
            |## Event settings
            |let ligaCommission = 4
            |let totalTeams = 000
            |
            |## Parties public keys
            |let ligaPublicKey = base58''
            |let eventPublicKey = base58''
            |let oraclePublicKey = base58''
            |let leaseNodePublicKey = base58''
            |
            |## Event timeline
            |let eventEndsAtBlock = 000
            |let winnerDeclarationInterval = 000
            |let payoutInterval = 000
            |
            |
            |### STORAGE/DATA FUNCTIONS
            |
            |func getIntOr(key: String, default: Int) = if isDefined(getInteger(this, key)) then getIntegerValue(this, key) else default
            |func getInt(key: String) = getIntegerValue(this, key)
            |func setInt(key: String, value: Int) = DataEntry(key, value)
            |func setBytes(key: String, value: ByteVector) = DataEntry(key, value)
            |func getBytes(key: String) = getBinaryValue(this, key)
            |
            |func isKeyDefined(key: String) =
            |    isDefined(getBinary(this, key)) ||
            |    isDefined(getString(this, key)) ||
            |    isDefined(getBoolean(this, key)) ||
            |    isDefined(getInteger(this, key))
            |
            |func toSoldKey(assetId: ByteVector) = assetId.toBase58String() + "_SOLD"
            |func getSoldAmount(assetId: ByteVector) = assetId.toSoldKey().getIntOr(assetInfo(assetId).extract().quantity - assetBalance(this, assetId))
            |func setSoldAmount(assetId: ByteVector) = assetId.toSoldKey().setInt(getSoldAmount(assetId))
            |
            |func toBasePriceKey(assetId: ByteVector) = assetId.toBase58String() + "_BASE_PRICE"
            |func getBasePrice(teamId: ByteVector) = teamId.toBasePriceKey().getInt()
            |
            |func toOffKey(assetId: ByteVector) = assetId.toBase58String() + "_OFF"
            |func off(teamId: ByteVector) = teamId.toOffKey().setInt(1)
            |func isOff(teamId: ByteVector) = teamId.toOffKey().getIntOr(0)
            |
            |let BALANCESNAPSHOT = "BALANCE_SNAPSHOT"
            |func getBalanceSnapshot() = BALANCESNAPSHOT.getIntOr(wavesBalance(this))
            |func setBalanceSnapshot() = BALANCESNAPSHOT.setInt(getBalanceSnapshot())
            |
            |let PRIZEPOOL = "PRIZE_POOL"
            |func getPrizePool() = PRIZEPOOL.getIntOr((getBalanceSnapshot() * (100-ligaCommission))/100)
            |func setPrizePool() = PRIZEPOOL.setInt(getPrizePool())
            |
            |let WINNER = "WINNER"
            |func getWinner() = WINNER.getBytes()
            |func setWinner(winner: ByteVector) = WINNER.setBytes(winner)
            |
            |let TEAMSLEFT = "TEAMS_LEFT"
            |func getTeamsLeft() = TEAMSLEFT.getIntOr(totalTeams)
            |func decTeamsLeft() = TEAMSLEFT.setInt(getTeamsLeft() - 1)
            |
            |let TEAMCOUNTER = "TEAM_COUNTER"
            |func getTeamCounter() = TEAMCOUNTER.getIntOr(0)
            |func incTeamCounter() = TEAMCOUNTER.setInt(getTeamCounter() + 1)
            |
            |let BASEPRIZEPOOL = "BASE_PRIZE_POOL"
            |func getBasePrizePool() = BASEPRIZEPOOL.getIntOr(0)
            |func addBasePrizePool(value: Int) = BASEPRIZEPOOL.setInt(getBasePrizePool() + value)
            |
            |
            |### EVENT STAGES
            |
            |# STAGE 1 - Event live
            |let STAGE1 = 1
            |# Event is live, tokens are allowed to sell and trade.
            |
            |# STAGE 2 - Event finished
            |let STAGE2 = 2
            |# Event ended, LIGA can't sell tokens after that point.
            |# Oracle is required to declare the winner within [winner declaration interval].
            |
            |# STAGE 3.1 - PAYOUT
            |let STAGE31 = 31
            |# Winner declared, payout is started, every holder of winner token allowed
            |# to exchange it for the portion of prize pool via payout() function.
            |
            |# STAGE 3.2 - Preparation for ROLLBACK
            |let STAGE32 = 32
            |# Winner is not declared and time interval passed.
            |# Taking snapshot of token amounts for every team.
            |# Counting for team bank proportion.
            |
            |# STAGE 3.3 - ROLLBACK
            |let STAGE33 = 33
            |# Winner is not declared and time interval passed,
            |# every holder of any team token allowed to exchange it
            |# for the portion of prize pool via rollback() function.
            |
            |# STAGE 4
            |let STAGE4 = 4
            |# Payout completed, LIGA takes commission to treasury
            |
            |##   STAGE TRANSITIONS SCHEMA
            |##
            |##   STAGE1 - STAGE2 - STAGE31 - STAGE4   --  sunny day branch
            |##                   \\
            |
            |##                    STAGE32 - STAGE33    --  no winner branch
            |##
            |
            |let STAGE = "STAGE"
            |func stage() = STAGE.getIntOr(STAGE1)
            |func goTo(stage: Int) = STAGE.setInt(stage)
            |
            |### STAGE TRANSITIONS
            |
            |@Callable(i)
            |func stage2() = {
            |    if stage() != STAGE1 then throw("Invalid current stage.") else
            |    if height <= eventEndsAtBlock then throw("Event is not yet finished.") else
            |
            |    WriteSet([
            |        goTo(STAGE2),
            |        setBalanceSnapshot()
            |    ])
            |}
            |
            |@Callable(i)
            |func stage31(winner: ByteVector) = {
            |    if stage() != STAGE2 then throw("Invalid current stage.") else
            |    if i.callerPublicKey != oraclePublicKey then throw("Only oracle could declare the winner.") else
            |    if !isKeyDefined(winner.toBasePriceKey()) then throw("Team is not found.") else
            |    if isDefined(winner.toOffKey()) then throw("Team that is off cannot be the winner.") else
            |
            |    WriteSet([
            |        goTo(STAGE31),
            |        setPrizePool(),
            |        setWinner(winner),
            |        setSoldAmount(winner)
            |    ])
            |}
            |
            |@Callable(i)
            |func stage32(teamId: ByteVector) = {
            |    if stage() != STAGE2 || stage() != STAGE32 then throw("Invalid current stage.") else
            |    if height <= eventEndsAtBlock + winnerDeclarationInterval then throw("Oracle is still have time to declare a winner.") else
            |    if !isKeyDefined(teamId.toBasePriceKey()) then throw("Team is not found.") else
            |    if isDefined(teamId.toOffKey()) then throw("Team that is off cannot participate in rollback.") else
            |    if isKeyDefined(teamId.toSoldKey()) then throw("Team sold amount already set.") else
            |
            |    WriteSet([
            |        goTo(STAGE32),
            |        setSoldAmount(teamId),
            |        addBasePrizePool(getSoldAmount(teamId) * getBasePrice(teamId)),
            |        incTeamCounter()
            |    ])
            |}
            |
            |@Callable(i)
            |func stage33() = {
            |    if stage() != STAGE32 then throw("Invalid current stage.") else
            |    if getTeamCounter() != getTeamsLeft() then throw("There are still teams without sold amount set.") else
            |
            |    WriteSet([
            |        goTo(STAGE33)
            |    ])
            |}
            |
            |@Callable(i)
            |func stage4(recipient: ByteVector) = {
            |    if stage() != STAGE31 then throw("Invalid current stage.") else
            |    if height <= eventEndsAtBlock + winnerDeclarationInterval + payoutInterval then throw("Payout is not yet finished.") else
            |    if i.callerPublicKey != ligaPublicKey then throw("Only Liga could set the final stage and hold commission.") else
            |
            |    ScriptResult(
            |        WriteSet([
            |            goTo(STAGE4)
            |        ]),
            |        TransferSet([
            |            ScriptTransfer(recipient.addressFromPublicKey(), wavesBalance(this), unit)
            |        ])
            |    )
            |}
            |
            |### ORACLE INTERFACE
            |
            |@Callable(i)
            |func teamOff(teamId: ByteVector) = {
            |    if stage() != STAGE1 then throw("Invalid current stage.") else
            |    if i.callerPublicKey != oraclePublicKey then throw("Only oracle could drop teams out off the game.") else
            |    if !isKeyDefined(teamId.toBasePriceKey()) then throw("Team is not found.") else
            |    if isKeyDefined(teamId.toOffKey()) then throw("Team is already off") else
            |    if getTeamsLeft() == 1 then throw("There is only 1 team left.") else
            |
            |    WriteSet([
            |        off(teamId),
            |        decTeamsLeft()
            |    ])
            |}
            |
            |
            |### PUBLIC USER INTEFRACE
            |
            |@Callable(i)
            |func rollback() = {
            |    if stage() != STAGE33 then throw("Invalid current stage.") else
            |
            |    let payment = i.payment.extract()
            |    let teamId = payment.assetId.extract()
            |    if !isKeyDefined(teamId.toBasePriceKey()) then throw("Team is not found.") else
            |    if isKeyDefined(teamId.toOffKey()) then throw("You cannot receive rollback for an off team") else
            |
            |    let soldAmount = getSoldAmount(teamId)
            |
            |    let rollback = (getBalanceSnapshot() * getBasePrizePool() * payment.amount) / (getBasePrice(teamId) * soldAmount * soldAmount)
            |
            |    TransferSet([
            |        ScriptTransfer(i.caller, rollback, unit)
            |    ])
            |}
            |
            |@Callable(i)
            |func payout() = {
            |    if stage() != STAGE31 then throw("Invalid current stage.") else
            |
            |    let payment = i.payment.extract()
            |
            |    if  payment.assetId != getWinner() then throw("You are allowed to get payout for the winner tokens only.") else
            |
            |    let payout = getPrizePool() * payment.amount / getSoldAmount(getWinner())
            |
            |    TransferSet([
            |        ScriptTransfer(i.caller, payout, unit)
            |    ])
            |}
            |
            |
            |### TRANSACTION VERIFIER
            |
            |@Verifier(x)
            |func verifier() = {
            |    match(x) {
            |        # Orders for token sale only possible as long as event runs
            |        # Price should be greater than base price for particular team
            |        case o:Order =>
            |            stage() == STAGE1 &&
            |            o.price >= getBasePrice(o.assetPair.amountAsset.extract()) &&
            |            sigVerify(o.bodyBytes, o.proofs[0], eventPublicKey)
            |        # Partner node is guaranteed to be leased each 100 waves as long as event is running
            |        case l:LeaseTransaction =>
            |            stage() == STAGE1 &&
            |            l.recipient == leaseNodePublicKey.addressFromPublicKey() &&
            |            l.amount > 100 * waves
            |        # Leasing will be cancelled as soon as event will be finished
            |        case cl: LeaseCancelTransaction =>
            |            stage() != STAGE1
            |        # No other transactions are possible
            |        case _ => false
            |    }
            |}""".stripMargin
      )
      .explicitGet()
      ._1
}
