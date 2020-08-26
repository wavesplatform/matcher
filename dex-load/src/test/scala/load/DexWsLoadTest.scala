package load

import java.util.concurrent.ThreadLocalRandom

import io.gatling.core.Predef._
import io.gatling.http.Predef._

import scala.concurrent.duration._
import scala.io.Source

class DexWsLoadTest extends Simulation {

  val feeder = separatedValues("data6000.csv", ';').batch

  val checkPP = ws.checkTextMessage("checkPP")
    .matching(jsonPath("$.T").is("pp"))
    .check(regex("(.*)").saveAs("pp"))
    .check(regex("(.*)").saveAs("!!!!!!!!!!!! " + ThreadLocalRandom.current.nextInt(1000).toString))

  val checkOB = ws.checkTextMessage("checkOB")
    .matching(jsonPath("$.T").is("ob"))
    .check(regex("(.*)").saveAs("ob")
    )

  val checkAU = ws.checkTextMessage("checkAU")
    .matching(jsonPath("$.T").is("au"))
    .check(regex("(.*)").saveAs("au")
    )

  val checkALL = ws.checkTextMessage("checkALL")
    .matching(jsonPath("$.T").is("pp")).check(regex("(.*)").saveAs(ThreadLocalRandom.current.nextInt(1000).toString)
  )

  val scn = scenario("WS")
    .feed(feeder)
    .exec(
      ws("WS -- OP")
        .connect("ws://devnet2-htz-nbg1-1.wavesnodes.com:6886/ws/v0")
        .await(11 seconds)(checkPP)
    )
//
//    .exec(ws("WS -- AU").sendText("""${m}"""))
//
//    .exec(ws("WS -- OB").sendText("""${o1}"""))
//    .exec(ws("WS -- OB").sendText("""${o2}"""))
//    .exec(ws("WS -- OB").sendText("""${o3}"""))
//    .exec(ws("WS -- OB").sendText("""${o4}"""))
//    .exec(ws("WS -- OB").sendText("""${o5}"""))
//    .exec(ws("WS -- OB").sendText("""${o6}"""))
//    .exec(ws("WS -- OB").sendText("""${o7}"""))
//    .exec(ws("WS -- OB").sendText("""${o8}"""))
//    .exec(ws("WS -- OB").sendText("""${o9}"""))
//    .exec(ws("WS -- OB").sendText("""${o10}"""))

    .repeat(70) {
      doWhile("${pp.isUndefined()}")(exec {
        ws("WS -- Await Ping").sendText("{}").await(5 second)(checkPP)
      })
        .doIf("${pp.exists()}")(exec {
          ws("WS -- PP").sendText("""${pp}""").await(11 seconds)(checkPP)
        })
    }


//
//    .repeat(3) {
//      exec(ws("WS -- PP")
//        .sendText("""${pp}""")
//        .await(11 seconds)(checkPP)
//      )
//    }
//    .exec(session => { // TODO: remove after debugging
//
//      import java.io._
//      val pw = new PrintWriter(new File(session.attributes.get("a").get.toString))
//      session.attributes.forall(a => {
//        println(s"${a._1} - ${a._2}")
//        pw.write(s"${a._1} - ${a._2}\n")
//        true
//      })
//      pw.close
//      session
//    })
    .exec(ws("WS -- CL").close)

  setUp(
    scn.inject(atOnceUsers(6000))
  )
}
