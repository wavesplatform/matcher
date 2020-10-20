package com.wavesplatform.dex.api.http.entities

import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.ser.std.StdSerializer
import com.fasterxml.jackson.databind.{DeserializationContext, JsonNode, ObjectMapper, SerializerProvider}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.model.LevelAgg

@JsonSerialize(using = classOf[HttpOrderBook.Serializer])
case class HttpOrderBook(timestamp: Long, pair: AssetPair, bids: Seq[LevelAgg], asks: Seq[LevelAgg], assetPairDecimals: Option[(Int, Int)] = None)

object HttpOrderBook {

  private val coreTypeSerializers = new SimpleModule()
  coreTypeSerializers.addDeserializer(classOf[AssetPair], new AssetPairDeserializer)

  private val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.registerModule(coreTypeSerializers)

  private def serialize(value: Any): String = mapper.writeValueAsString(value)

  private class AssetPairDeserializer extends StdDeserializer[AssetPair](classOf[AssetPair]) {

    override def deserialize(p: JsonParser, ctxt: DeserializationContext): AssetPair = {
      val node = p.getCodec.readTree[JsonNode](p)

      def readAssetId(fieldName: String): Asset = {
        val x = node.get(fieldName).asText(Asset.WavesName)
        if (x == Asset.WavesName) Waves else IssuedAsset(ByteStr.decodeBase58(x).get)
      }

      AssetPair(readAssetId("amountAsset"), readAssetId("priceAsset"))
    }

  }

  private def formatValue(value: BigDecimal, decimals: Int): String = new java.text.DecimalFormat(s"0.${"0" * decimals}").format(value)

  private def denormalizeAndSerializeSide(side: Seq[LevelAgg], amountAssetDecimals: Int, priceAssetDecimals: Int, jg: JsonGenerator): Unit =
    side.foreach { levelAgg =>
      val denormalizedPrice = Denormalization.denormalizePrice(levelAgg.price, amountAssetDecimals, priceAssetDecimals)
      val denormalizedAmount = Denormalization.denormalizeAmountAndFee(levelAgg.amount, amountAssetDecimals)

      jg.writeStartArray(2)
      jg.writeString(formatValue(denormalizedPrice, priceAssetDecimals))
      jg.writeString(formatValue(denormalizedAmount, amountAssetDecimals))
      jg.writeEndArray()
    }

  def toJson(x: HttpOrderBook): String = serialize(x)

  class Serializer extends StdSerializer[HttpOrderBook](classOf[HttpOrderBook]) {

    override def serialize(x: HttpOrderBook, j: JsonGenerator, serializerProvider: SerializerProvider): Unit = {
      j.writeStartObject()
      j.writeNumberField("timestamp", x.timestamp)

      x.assetPairDecimals.fold {

        j.writeFieldName("pair")
        j.writeStartObject()
        j.writeStringField("amountAsset", x.pair.amountAssetStr)
        j.writeStringField("priceAsset", x.pair.priceAssetStr)
        j.writeEndObject()

        j.writeArrayFieldStart("bids")
        x.bids.foreach(j.writeObject)
        j.writeEndArray()

        j.writeArrayFieldStart("asks")
        x.asks.foreach(j.writeObject)
        j.writeEndArray()

      } {
        case (amountAssetDecimals, priceAssetDecimals) =>
          j.writeArrayFieldStart("bids")
          denormalizeAndSerializeSide(x.bids, amountAssetDecimals, priceAssetDecimals, j)
          j.writeEndArray()

          j.writeArrayFieldStart("asks")
          denormalizeAndSerializeSide(x.asks, amountAssetDecimals, priceAssetDecimals, j)
          j.writeEndArray()
      }

      j.writeEndObject()
    }

  }

}
