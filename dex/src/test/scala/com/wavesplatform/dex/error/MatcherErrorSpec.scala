package com.wavesplatform.dex.error

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MatcherErrorSpec extends AnyFreeSpec with Matchers {

  "MatcherErrorSpec" - {

    "matcher errors should have fixed codes" in {
      UnexpectedError.code shouldBe 4
      MatcherIsStarting.code shouldBe 13
      MatcherIsStopping.code shouldBe 14
      FeatureNotImplemented.code shouldBe 515
      FeatureDisabled.code shouldBe 528
      UnexpectedMatcherPublicKey.code shouldBe 3076
      CanNotPersistEvent.code shouldBe 25601
      RequestArgumentInvalid.code shouldBe 1048576
      InvalidJson.code shouldBe 1048577
      UnsupportedContentType.code shouldBe 1048579
      CancelRequestIsIncomplete.code shouldBe 1048580
      RequestTimeout.code shouldBe 1048590
      RequestInvalidSignature.code shouldBe 1051904
      InvalidDepth.code shouldBe 1076224
      AccountFeatureUnsupported.code shouldBe 2097923
      OrderVersionUnsupported.code shouldBe 2099459
      AssetFeatureUnsupported.code shouldBe 2099971
      AddressIsBlacklisted.code shouldBe 3145733
      BalanceNotEnough.code shouldBe 3147270
      AccountScriptReturnedError.code shouldBe 3147520
      AccountScriptException.code shouldBe 3147521
      AccountScriptDeniedOrder.code shouldBe 3147522
      AccountScriptUnexpectResult.code shouldBe 3147524
      AccountNotSupportOrderVersion.code shouldBe 3148035
      ActiveOrdersLimitReached.code shouldBe 3148039
      OrderDuplicate.code shouldBe 3148040
      UserPublicKeyIsNotValid.code shouldBe 3148801
      InvalidAddress.code shouldBe 4194304
      CanNotCreateExchangeTransaction.code shouldBe 5245184
      OrderBookBroken.code shouldBe 8388609
      OrderBookUnexpectedState.code shouldBe 8388612
      OrderBookStopped.code shouldBe 8388624
      OrderCommonValidationFailed.code shouldBe 9437184
      InvalidBase58String.code shouldBe 9437185
      OrderFull.code shouldBe 9437191
      OrderNotFound.code shouldBe 9437193
      OrderCanceled.code shouldBe 9437194
      OrderFinalized.code shouldBe 9437195
      OrderIsPlacing.code shouldBe 9437203
      OrderVersionDenied.code shouldBe 9439746
      UnsupportedOrderVersion.code shouldBe 9439747
      OrderInvalidSignature.code shouldBe 9440512
      AssetPairIsDenied.code shouldBe 9440770
      OrderAssetPairReversed.code shouldBe 9440771
      AssetPairSameAssets.code shouldBe 9440776
      OrderInvalidAmount.code shouldBe 9441026
      OrderInvalidPrice.code shouldBe 9441282
      PriceLastDecimalsMustBeZero.code shouldBe 9441284
      OrderInvalidPriceLevel.code shouldBe 9441286
      DeviantOrderPrice.code shouldBe 9441295
      UnexpectedFeeAsset.code shouldBe 9441540
      FeeNotEnough.code shouldBe 9441542
      DeviantOrderMatcherFee.code shouldBe 9441551
      WrongExpiration.code shouldBe 9441798
      InvalidAsset.code shouldBe 11534337
      AssetBlacklisted.code shouldBe 11534341
      AssetNotFound.code shouldBe 11534345
      AssetScriptReturnedError.code shouldBe 11536128
      AssetScriptException.code shouldBe 11536129
      AssetScriptDeniedOrder.code shouldBe 11536130
      AssetScriptUnexpectResult.code shouldBe 11536132
      AmountAssetBlacklisted.code shouldBe 11538181
      PriceAssetBlacklisted.code shouldBe 11538437
      FeeAssetBlacklisted.code shouldBe 11538693
      MarketOrderCancel.code shouldBe 19922960
      InvalidMarketOrderPrice.code shouldBe 19927055
      RateNotFound.code shouldBe 20971529
      WavesImmutableRate.code shouldBe 20971531
      InvalidAssetRate.code shouldBe 20971535
      WavesNodeConnectionBroken.code shouldBe 105906177
      ApiKeyIsNotValid.code shouldBe 106954752
      ApiKeyIsNotProvided.code shouldBe 106954769
      AddressAndPublicKeyAreIncompatible.code shouldBe 106957828
      SubscriptionAuthTypeUnsupported.code shouldBe 106960131
      AuthIsRequired.code shouldBe 106981137
      Balancing.code shouldBe 109051922
      WsConnectionMaxLifetimeExceeded.code shouldBe 109077767
      WsConnectionPongTimeout.code shouldBe 109077772
      SubscriptionsLimitReached.code shouldBe 109079303
      JwtCommonError.code shouldBe 110100480
      JwtBroken.code shouldBe 110100481
      InvalidJwtPayloadSignature.code shouldBe 110103809
      SubscriptionTokenExpired.code shouldBe 110105088
      TokenNetworkUnexpected.code shouldBe 110106116
      JwtPayloadBroken.code shouldBe 110127617
    }
  }
}
