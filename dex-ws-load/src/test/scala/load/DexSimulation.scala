package load

import java.util.concurrent.ThreadLocalRandom

import io.gatling.core.Predef._
import io.gatling.http.Predef._

import scala.concurrent.duration.DurationInt

class DexSimulation extends Simulation {

  val feeder = separatedValues(System.getProperty("ff"), ';').batch

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

  val scn = System.getProperty("sm") match {
    case "cs" => scenario("StreamsSimulation")
        .feed(feeder)
        .exec(connect)
        .exec(ws("WS -- AU").sendText("""${m}"""))
        .exec(ws("WS -- OB").sendText("""${o0}"""))
        .exec(ws("WS -- OB").sendText("""${o1}"""))
        .exec(ws("WS -- OB").sendText("""${o2}"""))
        .exec(ws("WS -- OB").sendText("""${o3}"""))
        .exec(ws("WS -- OB").sendText("""${o4}"""))
        .exec(ws("WS -- OB").sendText("""${o5}"""))
        .exec(ws("WS -- OB").sendText("""${o6}"""))
        .exec(ws("WS -- OB").sendText("""${o7}"""))
        .exec(ws("WS -- OB").sendText("""${o8}"""))
        .exec(ws("WS -- OB").sendText("""${o9}"""))
        .repeat(System.getProperty("rt").toInt) {
          pingPong
        }
        .exec(close)
    case _ => scenario("ConnectionsSimulation")
        .feed(feeder)
        .exec(connect)
        .repeat(System.getProperty("rt").toInt) {
          pingPong
        }
        .exec(close)
  }

  setUp(
    scn.inject(atOnceUsers(System.getProperty("uc").toInt))
  )
}
