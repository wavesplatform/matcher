package load

import io.gatling.core.Predef._
import io.gatling.http.Predef._

class ConnectionsAndStreamsTest extends Simulation {

  val feeder = separatedValues(System.getProperty("ff"), ';').batch

  val scn = scenario("ConnectionsAndStreamsTest")
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

  setUp(
    scn.inject(atOnceUsers(System.getProperty("uc").toInt))
  )
}
