import java.util.concurrent.ThreadLocalRandom

import io.gatling.core.Predef._
import io.gatling.http.Predef._

import scala.concurrent.duration.DurationInt


package object load {
  val checkPP = ws
    .checkTextMessage("checkPP")
    .matching(jsonPath("$.T").is("pp"))
    .check(regex("(.*)").saveAs("pp"))

  val checkOB = ws
    .checkTextMessage("checkOB")
    .matching(jsonPath("$.T").is("ob"))
    .check(regex("(.*)").saveAs("ob"))

  val checkAU = ws
    .checkTextMessage("checkAU")
    .matching(jsonPath("$.T").is("au"))
    .check(regex("(.*)").saveAs("au"))

  val checkALL = ws
    .checkTextMessage("checkALL")
    .matching(jsonPath("$.T").is("pp"))
    .check(regex("(.*)").saveAs(ThreadLocalRandom.current.nextInt(1000).toString))

  val connect = ws("WS -- OP")
    .connect(System.getProperty("ws"))
    .await(11 seconds)(checkPP)

  val close = ws("WS -- CL").close

  val pingPong = doWhile("${pp.isUndefined()}")(exec {
    ws("WS -- Await Ping").sendText("{}").await(5 second)(checkPP)
  })
    .doIf("${pp.exists()}")(exec {
      ws("WS -- PP").sendText("""${pp}""").await(11 seconds)(checkPP)
    })
}
