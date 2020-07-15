package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.order.{Order, OrderType, OrderV2}
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.it.MatcherSuiteBase

import scala.concurrent.duration._

class ProofAndAssetPairTestSuite extends MatcherSuiteBase {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""")

  private val IssueResults(issueAliceAssetTx, aliceAssetId, aliceAsset) = mkIssueExtended(alice, "AliceCoin", someAssetAmount, 0)

  private val predefAssetPair = wavesUsdPair
  private val aliceWavesPair  = AssetPair(aliceAsset, Waves)

  private val sc1 = Scripts.alwaysTrue

  /*
  match tx {
   case s : SetScriptTransaction => true
   case _ => false
  }
   */
  private val sc2 = Scripts.fromBase64(
    "AgQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAAFFNldFNjcmlwdFRyYW5zYWN0aW9uBAAAAAFzBQAAAAckbWF0Y2gwBgfLRT2k"
  )

  /*
    let usdId = base64'e3VzZElkfQ==' # {usdId}
    let aliceAssetId =  base64'e2FsaWNlQXNzZXRJZH0='
    match tx {
      case t : Order =>
        let id = t.id == base58''
        let assetPair1Amount = isDefined(t.assetPair.amountAsset)
        let assetPair1Price = t.assetPair.priceAsset == usdId
        let assetPair2Amount = t.assetPair.amountAsset == aliceAssetId
        let assetPair2Price = isDefined(t.assetPair.priceAsset)
        (!assetPair1Amount && assetPair1Price) || (assetPair2Amount && !assetPair2Price)
      case s : SetScriptTransaction => true
      case other => throw()
    }
   */
  private val sc3 = Scripts.renderScriptTemplate(
    "AgQAAAAFdXNkSWQBAAAAB3t1c2RJZH0EAAAADGFsaWNlQXNzZXRJZAEAAAAOe2FsaWNlQXNzZXRJZH0EAAAAByRtYXRja" +
      "DAFAAAAAnR4AwkAAAEAAAACBQAAAAckbWF0Y2gwAgAAAAVPcmRlcgQAAAABdAUAAAAHJG1hdGNoMAQAAAACaWQJAAAAAAAAAggFAAAAAXQAAAAC" +
      "aWQBAAAAAAQAAAAQYXNzZXRQYWlyMUFtb3VudAkBAAAACWlzRGVmaW5lZAAAAAEICAUAAAABdAAAAAlhc3NldFBhaXIAAAALYW1vdW50QXNzZXQ" +
      "EAAAAD2Fzc2V0UGFpcjFQcmljZQkAAAAAAAACCAgFAAAAAXQAAAAJYXNzZXRQYWlyAAAACnByaWNlQXNzZXQFAAAABXVzZElkBAAAABBhc3NldF" +
      "BhaXIyQW1vdW50CQAAAAAAAAIICAUAAAABdAAAAAlhc3NldFBhaXIAAAALYW1vdW50QXNzZXQFAAAADGFsaWNlQXNzZXRJZAQAAAAPYXNzZXRQY" +
      "WlyMlByaWNlCQEAAAAJaXNEZWZpbmVkAAAAAQgIBQAAAAF0AAAACWFzc2V0UGFpcgAAAApwcmljZUFzc2V0AwMJAQAAAAEhAAAAAQUAAAAQYXNz" +
      "ZXRQYWlyMUFtb3VudAUAAAAPYXNzZXRQYWlyMVByaWNlBwYDBQAAABBhc3NldFBhaXIyQW1vdW50CQEAAAABIQAAAAEFAAAAD2Fzc2V0UGFpcjJ" +
      "QcmljZQcDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAAFFNldFNjcmlwdFRyYW5zYWN0aW9uBAAAAAFzBQAAAAckbWF0Y2gwBgQAAAAFb3RoZXIFAA" +
      "AAByRtYXRjaDAJAQAAAAV0aHJvdwAAAABoNJIT",
    "{usdId}"        -> UsdId,
    "{aliceAssetId}" -> aliceAssetId
  )

  /*
  let alicePublicKey = base64'e2FsaWNlUHVibGljS2V5fQ=='
  let matcherPublicKey = base64'e21hdGNoZXJQdWJsaWNLZXl9'
  match tx {
    case t : Order =>
      let id = t.id == base58''
      let senderPublicKey = t.senderPublicKey == alicePublicKey
      let isMatcherPublicKey = t.matcherPublicKey == matcherPublicKey
      let timestamp = t.timestamp > 0
      let price = t.price > 0
      let amount = t.amount > 0
      let expiration = t.expiration > 0
      let matcherFee = t.matcherFee > 0
      let bodyBytes = t.bodyBytes == base64''
      !id && senderPublicKey && isMatcherPublicKey && timestamp && price && amount && expiration && matcherFee &&
      expiration && matcherFee && !bodyBytes
    case s : SetScriptTransaction => true
    case _ => throw()
  }
   */
  private val sc4 = Scripts.renderScriptTemplate(
    "AgQAAAAOYWxpY2VQdWJsaWNLZXkBAAAAEHthbGljZVB1YmxpY0tleX0EAAAAEG1hdGNoZXJQdWJsaWNLZXkBAAAAEnttY" +
      "XRjaGVyUHVibGljS2V5fQQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAABU9yZGVyBAAAAAF0BQAAAAckbWF0Y2gw" +
      "BAAAAAJpZAkAAAAAAAACCAUAAAABdAAAAAJpZAEAAAAABAAAAA9zZW5kZXJQdWJsaWNLZXkJAAAAAAAAAggFAAAAAXQAAAAPc2VuZGVyUHVibGl" +
      "jS2V5BQAAAA5hbGljZVB1YmxpY0tleQQAAAASaXNNYXRjaGVyUHVibGljS2V5CQAAAAAAAAIIBQAAAAF0AAAAEG1hdGNoZXJQdWJsaWNLZXkFAA" +
      "AAEG1hdGNoZXJQdWJsaWNLZXkEAAAACXRpbWVzdGFtcAkAAGYAAAACCAUAAAABdAAAAAl0aW1lc3RhbXAAAAAAAAAAAAAEAAAABXByaWNlCQAAZ" +
      "gAAAAIIBQAAAAF0AAAABXByaWNlAAAAAAAAAAAABAAAAAZhbW91bnQJAABmAAAAAggFAAAAAXQAAAAGYW1vdW50AAAAAAAAAAAABAAAAApleHBp" +
      "cmF0aW9uCQAAZgAAAAIIBQAAAAF0AAAACmV4cGlyYXRpb24AAAAAAAAAAAAEAAAACm1hdGNoZXJGZWUJAABmAAAAAggFAAAAAXQAAAAKbWF0Y2h" +
      "lckZlZQAAAAAAAAAAAAQAAAAJYm9keUJ5dGVzCQAAAAAAAAIIBQAAAAF0AAAACWJvZHlCeXRlcwEAAAAAAwMDAwMDAwMDAwkBAAAAASEAAAABBQ" +
      "AAAAJpZAUAAAAPc2VuZGVyUHVibGljS2V5BwUAAAASaXNNYXRjaGVyUHVibGljS2V5BwUAAAAJdGltZXN0YW1wBwUAAAAFcHJpY2UHBQAAAAZhb" +
      "W91bnQHBQAAAApleHBpcmF0aW9uBwUAAAAKbWF0Y2hlckZlZQcFAAAACmV4cGlyYXRpb24HBQAAAAptYXRjaGVyRmVlBwkBAAAAASEAAAABBQAA" +
      "AAlib2R5Qnl0ZXMHAwkAAAEAAAACBQAAAAckbWF0Y2gwAgAAABRTZXRTY3JpcHRUcmFuc2FjdGlvbgQAAAABcwUAAAAHJG1hdGNoMAYJAQAAAAV" +
      "0aHJvdwAAAAAIj5yk",
    "{alicePublicKey}"   -> alice.publicKey,
    "{matcherPublicKey}" -> matcher.publicKey
  )

  /*
  let alicePublicKey = base64'e2FsaWNlUHVibGljS2V5fQ=='
  match tx {
    case t : Order => sigVerify(t.bodyBytes, t.proofs[0], alicePublicKey)
    case s : SetScriptTransaction => true
    case _ => throw()
  }
   */
  private val sc5 = Scripts.renderScriptTemplate(
    "AgQAAAAOYWxpY2VQdWJsaWNLZXkBAAAAEHthbGljZVB1YmxpY0tleX0EAAAAByRtYXRjaDAFAAAAAnR4AwkAAAEAAAACB" +
      "QAAAAckbWF0Y2gwAgAAAAVPcmRlcgQAAAABdAUAAAAHJG1hdGNoMAkAAfQAAAADCAUAAAABdAAAAAlib2R5Qnl0ZXMJAAGRAAAAAggFAAAAAXQA" +
      "AAAGcHJvb2ZzAAAAAAAAAAAABQAAAA5hbGljZVB1YmxpY0tleQMJAAABAAAAAgUAAAAHJG1hdGNoMAIAAAAUU2V0U2NyaXB0VHJhbnNhY3Rpb24" +
      "EAAAAAXMFAAAAByRtYXRjaDAGCQEAAAAFdGhyb3cAAAAAnM6WrQ==",
    "{alicePublicKey}" -> alice.publicKey
  )

  /*
  let alicePublicKey = base64'e2FsaWNlUHVibGljS2V5fQ=='
  let bobPublicKey = base64'e2JvYlB1YmxpY0tleX0='
  let matcherPublicKey = base64'e21hdGNoZXJQdWJsaWNLZXl9'
  match tx {
    case t : Order =>
      let alice = if (sigVerify(t.bodyBytes, t.proofs[0], alicePublicKey)) then 1 else 0
      let bob = if (sigVerify(t.bodyBytes, t.proofs[1], bobPublicKey)) then 1 else 0
      let matcher = if (sigVerify(t.bodyBytes, t.proofs[2], matcherPublicKey)) then 1 else 0
      alice + bob + matcher >= 2
    case s : SetScriptTransaction => true
    case _ => throw()
  }
   */
  private val sc6 = Scripts.renderScriptTemplate(
    "AgQAAAAOYWxpY2VQdWJsaWNLZXkBAAAAEHthbGljZVB1YmxpY0tleX0EAAAADGJvYlB1YmxpY0tleQEAAAAOe2JvYlB1Y" +
      "mxpY0tleX0EAAAAEG1hdGNoZXJQdWJsaWNLZXkBAAAAEnttYXRjaGVyUHVibGljS2V5fQQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAA" +
      "ByRtYXRjaDACAAAABU9yZGVyBAAAAAF0BQAAAAckbWF0Y2gwBAAAAAVhbGljZQMJAAH0AAAAAwgFAAAAAXQAAAAJYm9keUJ5dGVzCQABkQAAAAI" +
      "IBQAAAAF0AAAABnByb29mcwAAAAAAAAAAAAUAAAAOYWxpY2VQdWJsaWNLZXkAAAAAAAAAAAEAAAAAAAAAAAAEAAAAA2JvYgMJAAH0AAAAAwgFAA" +
      "AAAXQAAAAJYm9keUJ5dGVzCQABkQAAAAIIBQAAAAF0AAAABnByb29mcwAAAAAAAAAAAQUAAAAMYm9iUHVibGljS2V5AAAAAAAAAAABAAAAAAAAA" +
      "AAABAAAAAdtYXRjaGVyAwkAAfQAAAADCAUAAAABdAAAAAlib2R5Qnl0ZXMJAAGRAAAAAggFAAAAAXQAAAAGcHJvb2ZzAAAAAAAAAAACBQAAABBt" +
      "YXRjaGVyUHVibGljS2V5AAAAAAAAAAABAAAAAAAAAAAACQAAZwAAAAIJAABkAAAAAgkAAGQAAAACBQAAAAVhbGljZQUAAAADYm9iBQAAAAdtYXR" +
      "jaGVyAAAAAAAAAAACAwkAAAEAAAACBQAAAAckbWF0Y2gwAgAAABRTZXRTY3JpcHRUcmFuc2FjdGlvbgQAAAABcwUAAAAHJG1hdGNoMAYJAQAAAA" +
      "V0aHJvdwAAAABu4D+b",
    "{alicePublicKey}"   -> alice.publicKey,
    "{bobPublicKey}"     -> bob.publicKey,
    "{matcherPublicKey}" -> matcher.publicKey
  )

  /*
  let wctId = base64'e3djdElkfQ=='
  match tx {
    case t : Order =>
      let id = t.id == base58''
      let assetPairAmount = if (isDefined(t.assetPair.amountAsset)) then extract(t.assetPair.amountAsset) == wctId else false
      let assetPairPrice = isDefined(t.assetPair.priceAsset)
      (assetPairAmount && !assetPairPrice)
    case s : SetScriptTransaction => true
    case other => throw()
  }
   */
  private val sc7 = Scripts.renderScriptTemplate(
    "AgQAAAAFd2N0SWQBAAAAB3t3Y3RJZH0EAAAAByRtYXRjaDAFAAAAAnR4AwkAAAEAAAACBQAAAAckbWF0Y2gwAgAAAAVPc" +
      "mRlcgQAAAABdAUAAAAHJG1hdGNoMAQAAAACaWQJAAAAAAAAAggFAAAAAXQAAAACaWQBAAAAAAQAAAAPYXNzZXRQYWlyQW1vdW50AwkBAAAACWlz" +
      "RGVmaW5lZAAAAAEICAUAAAABdAAAAAlhc3NldFBhaXIAAAALYW1vdW50QXNzZXQJAAAAAAAAAgkBAAAAB2V4dHJhY3QAAAABCAgFAAAAAXQAAAA" +
      "JYXNzZXRQYWlyAAAAC2Ftb3VudEFzc2V0BQAAAAV3Y3RJZAcEAAAADmFzc2V0UGFpclByaWNlCQEAAAAJaXNEZWZpbmVkAAAAAQgIBQAAAAF0AA" +
      "AACWFzc2V0UGFpcgAAAApwcmljZUFzc2V0AwUAAAAPYXNzZXRQYWlyQW1vdW50CQEAAAABIQAAAAEFAAAADmFzc2V0UGFpclByaWNlBwMJAAABA" +
      "AAAAgUAAAAHJG1hdGNoMAIAAAAUU2V0U2NyaXB0VHJhbnNhY3Rpb24EAAAAAXMFAAAAByRtYXRjaDAGBAAAAAVvdGhlcgUAAAAHJG1hdGNoMAkB" +
      "AAAABXRocm93AAAAAPKBXCk=",
    "{wctId}" -> WctId
  )

  /*
  let matcherPublicKey = base64'e21hdGNoZXJQdWJsaWNLZXl9'
  match tx {
    case t : Order => sigVerify(t.bodyBytes,t.proofs[0], matcherPublicKey) # here was alice
    case s : SetScriptTransaction => true
    case _ => throw()
  }
   */
  private val sc8 = Scripts.renderScriptTemplate(
    "AgQAAAAQbWF0Y2hlclB1YmxpY0tleQEAAAASe21hdGNoZXJQdWJsaWNLZXl9BAAAAAckbWF0Y2gwBQAAAAJ0eAMJAAABA" +
      "AAAAgUAAAAHJG1hdGNoMAIAAAAFT3JkZXIEAAAAAXQFAAAAByRtYXRjaDAJAAH0AAAAAwgFAAAAAXQAAAAJYm9keUJ5dGVzCQABkQAAAAIIBQAA" +
      "AAF0AAAABnByb29mcwAAAAAAAAAAAAUAAAAQbWF0Y2hlclB1YmxpY0tleQMJAAABAAAAAgUAAAAHJG1hdGNoMAIAAAAUU2V0U2NyaXB0VHJhbnN" +
      "hY3Rpb24EAAAAAXMFAAAAByRtYXRjaDAGCQEAAAAFdGhyb3cAAAAAWX4f/Q==",
    "{matcherPublicKey}" -> matcher.publicKey
  )

  /*
  match tx {
    case t : Order => height < 0
    case s : SetScriptTransaction => true
    case _ => throw()
  }
   */
  private val sc9 = Scripts.fromBase64(
    "AgQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAABU9yZGVyBAAAAAF0BQAAAAckbWF0Y2gwCQAAZgAAAAIA" +
      "AAAAAAAAAAAFAAAABmhlaWdodAMJAAABAAAAAgUAAAAHJG1hdGNoMAIAAAAUU2V0U2NyaXB0VHJhbnNhY3Rpb24EAAAAAXMFAAAAByRtYXRjaDA" +
      "GCQEAAAAFdGhyb3cAAAAAnVjIGw=="
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(issueAliceAssetTx, IssueUsdTx)
    dex1.start()
  }

  "Proofs and AssetPairs verification with SmartContracts" - {
    "positive scenarios of order placement" - {
      "set contracts with AssetPairs/all tx fields/true/one proof and then place order" - {
        for ((sc, i) <- Seq(sc1, sc3, sc4, sc5).zip(Seq(1, 3, 4, 5))) s"$i" in {
          log.debug(s"contract: $sc")
          setAliceScript(sc)

          val aliceOrd1 = mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          placeAndAwaitAtDex(aliceOrd1)

          val aliceOrd2 = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          placeAndAwaitAtDex(aliceOrd2)

          dex1.api.cancel(alice, aliceOrd1)
          dex1.api.waitForOrderStatus(aliceOrd1, Status.Cancelled)

          dex1.api.cancel(alice, aliceOrd2).status shouldBe "OrderCanceled"
          dex1.api.waitForOrderStatus(aliceOrd2, Status.Cancelled)
        }
      }

      "set contracts with many proofs and then place order" - {
        for ((sc, i) <- Seq(sc5, sc6).zip(Seq(5, 6))) s"$i" in {
          setAliceScript(sc)

          val currTime = System.currentTimeMillis()

          val unsigned =
            OrderV2(
              alice,
              matcher,
              predefAssetPair,
              OrderType.BUY,
              500,
              2.waves * Order.PriceConstant,
              currTime,
              (30.days - 1.seconds).toMillis + currTime,
              smartMatcherFee,
              Proofs.empty
            )

          val sigAlice = ByteStr(crypto.sign(alice, unsigned.bodyBytes()))
          val sigBob   = ByteStr(crypto.sign(bob, unsigned.bodyBytes()))

          val signed = unsigned.copy(proofs = Proofs(Seq(sigAlice, sigBob)))
          placeAndAwaitAtDex(signed)

          dex1.api.cancel(alice, signed)
          dex1.api.waitForOrderStatus(signed, Status.Cancelled)
        }

        "reset" in resetAliceAccountScript()
      }

      "place order and then set contract on AssetPairs/true/all fields/one proof" - {
        for ((sc, i) <- Seq(sc1, sc3, sc4, sc5).zip(Seq(1, 3, 4, 5))) s"$i" in {
          val aliceOrd1 =
            mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, matcherFee = smartMatcherFee, version = 2)
          placeAndAwaitAtDex(aliceOrd1)

          val aliceOrd2 =
            mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, matcherFee = smartMatcherFee, version = 2)
          placeAndAwaitAtDex(aliceOrd2)

          setAliceScript(sc)

          val bobOrd1 = mkOrder(bob, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, matcherFee = smartMatcherFee, version = 2)
          dex1.api.place(bobOrd1)

          val bobOrd2 = mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, matcherFee = smartMatcherFee, version = 2)
          dex1.api.place(bobOrd2)

          dex1.api.waitForOrderStatus(aliceOrd1, Status.Filled)
          dex1.api.waitForOrderStatus(aliceOrd2, Status.Filled)
          dex1.api.waitForOrderStatus(bobOrd1, Status.Filled)
          dex1.api.waitForOrderStatus(bobOrd2, Status.Filled)

          waitForOrderAtNode(bobOrd1).head.getFee shouldBe 300000
          waitForOrderAtNode(bobOrd2).head.getFee shouldBe 300000

          dex1.api.reservedBalance(bob) shouldBe empty
        }
      }

      "place order and then set contract with many proofs" in {
        setAliceScript(Scripts.alwaysTrue)
        broadcastAndAwait(mkTransfer(alice, bob, 1000, aliceAsset, 0.005.waves))

        for ((sc, i) <- Seq(sc5, sc6).zip(Seq(5, 6))) {
          markup(s"$i")

          for (assetP <- Seq(predefAssetPair, aliceWavesPair)) {
            val currTime = System.currentTimeMillis()

            val unsigned =
              OrderV2(
                alice,
                matcher,
                assetP,
                OrderType.BUY,
                500,
                2.waves * Order.PriceConstant,
                currTime,
                (30.days - 1.seconds).toMillis + currTime,
                smartMatcherFee,
                Proofs.empty
              )

            val sigAlice = ByteStr(crypto.sign(alice, unsigned.bodyBytes()))
            val sigMat   = ByteStr(crypto.sign(matcher.privateKey, unsigned.bodyBytes()))
            placeAndAwaitAtDex(unsigned.copy(proofs = Proofs(Seq(sigAlice, ByteStr.empty, sigMat))))
          }

          setAliceScript(sc)

          val bobOrd1 = mkOrder(bob, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          dex1.api.place(bobOrd1)

          val bobOrd2 = mkOrder(bob, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          dex1.api.place(bobOrd2)

          dex1.api.waitForOrderStatus(bobOrd1, Status.Filled)
          dex1.api.waitForOrderStatus(bobOrd2, Status.Filled)

          waitForOrderAtNode(bobOrd1).head.getFee shouldBe 300000
          waitForOrderAtNode(bobOrd2).head.getFee shouldBe 300000

          dex1.api.reservedBalance(bob) shouldBe empty
        }

        resetAliceAccountScript()
      }
    }

    "negative scenarios of order placement" - {
      "set contact and then place order" - {
        for ((sc, i) <- Seq(sc2, sc7, sc8).zip(Seq(2, 7, 8))) s"$i" in {
          setAliceScript(sc)

          dex1.api
            .tryPlace(mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)) should failWith(
            3147522, // AccountScriptDeniedOrder
            MatcherError.Params(address = Some(alice.toAddress.stringRepr))
          )

          dex1.api
            .tryPlace(mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)) should failWith(
            3147522, // AccountScriptDeniedOrder
            MatcherError.Params(address = Some(alice.toAddress.stringRepr))
          )
        }

        "9" in {
          setAliceScript(sc9)
          dex1.api.tryPlace(mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)) should failWith(
            3147521, // AccountScriptException
            "An access to the blockchain.height is denied on DEX"
          )
        }

        "reset" in resetAliceAccountScript()
      }

      "place order and then set contract" - {
        for ((contract, i) <- Seq(sc2, sc7, sc8, sc9).zip(Seq(2, 7, 8, 9))) s"$i" in {
          val aliceOrd1 = mkOrder(alice, predefAssetPair, OrderType.BUY, 100, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          placeAndAwaitAtDex(aliceOrd1)

          val aliceOrd2 = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          placeAndAwaitAtDex(aliceOrd2)

          setAliceScript(contract)

          val bobOrd1 = mkOrder(bob, predefAssetPair, OrderType.SELL, 100, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          dex1.api.place(bobOrd1)

          val bobOrd2 = mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          dex1.api.place(bobOrd2)

          dex1.api.waitForOrderStatus(aliceOrd1, Status.Filled)
          dex1.api.waitForOrderStatus(aliceOrd2, Status.Filled)
          dex1.api.waitForOrderStatus(bobOrd1, Status.Filled)
          dex1.api.waitForOrderStatus(bobOrd2, Status.Filled)

          val aliceOrd1Txs = dex1.api.waitForTransactionsByOrder(aliceOrd1, 1)
          val r1           = wavesNode1.api.tryBroadcast(aliceOrd1Txs.head)
          r1 shouldBe Symbol("left")
          r1.left.get.error shouldBe 307 // node's ApiError TransactionNotAllowedByAccountScript.Id

          val aliceOrd2Txs = dex1.api.waitForTransactionsByOrder(aliceOrd2, 1)
          val r2           = wavesNode1.api.tryBroadcast(aliceOrd2Txs.head)
          r2 shouldBe Symbol("left")
          r2.left.get.error shouldBe 307 // node's ApiError TransactionNotAllowedByAccountScript.Id

          dex1.api.orderHistoryWithApiKey(alice, activeOnly = Some(true)).length shouldBe 0
          dex1.api.reservedBalance(bob) shouldBe empty

          resetAliceAccountScript()
        }
      }
    }
  }

  private def setAliceScript(binaryCode: ByteStr): Unit =
    broadcastAndAwait(mkSetAccountScript(alice, Some(binaryCode), fee = setScriptFee + smartFee))
  private def resetAliceAccountScript(): Unit = broadcastAndAwait(mkResetAccountScript(alice, fee = setScriptFee + smartFee))
}
