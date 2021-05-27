package load

import io.gatling.core.Predef._

class ConnectionsSimulation extends Simulation {

  val feeder = separatedValues(System.getProperty("ff"), ';').batch

  val connectionsSimulation = scenario("ConnectionsSimulation")
    .feed(feeder)
    .exec(connect)
    .repeat(System.getProperty("rt").toInt) {
      pingPong
    }
    .exec(close)

  val streamsSimulation = scenario("StreamsSimulation")
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
    System.getProperty("sm") match {
      case "cs" =>  streamsSimulation.inject(atOnceUsers(System.getProperty("uc").toInt))
      case "co" =>  connectionsSimulation.inject(atOnceUsers(System.getProperty("uc").toInt))
    }
  )
}
