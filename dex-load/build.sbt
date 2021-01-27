libraryDependencies ++= Dependencies.Module.dexLoad
run / fork := true

scalacOptions += "-P:silencer:globalFilters=^magnolia: using fallback derivation.*$" // https://github.com/softwaremill/diffx#customization

TaskKey[Unit]("generateAmmo") := (runMain in Compile)
  .toTask(
    s" com.wavesplatform.dex.load.WavesDexLoadCli create-requests -rsp=${sys.env.getOrElse("SEED", "test")} -an=${sys.env.getOrElse("AN", "6000")} -rc=${sys.env
      .getOrElse("RC", "250000")} -pf=pairs.txt -rt=${sys.env.getOrElse("RT", "6")} -as=${sys.env.getOrElse("AS", "D")}"
  )
  .value
TaskKey[Unit]("generateFeeder") := (runMain in Compile)
  .toTask(
    s" com.wavesplatform.dex.load.WavesDexLoadCli create-feeder-file -rsp=${sys.env.getOrElse("SEED", "test")} -an=${sys.env.getOrElse("AN", "6000")} -aspkf=${sys.env
      .getOrElse("ASPKF", "key.txt")} -obnpa=${sys.env.getOrElse("OBNPA", "10")} -pf=pairs.txt -as=${sys.env.getOrElse("AS", "D")}"
  )
  .value
TaskKey[Unit]("checkLeaps") := (runMain in Compile)
  .toTask(
    s" com.wavesplatform.dex.load.WavesDexLoadCli check -dra=${sys.env.getOrElse("MATCHER", "")} -an=100 -ct=15.seconds -wrwt=10.minutes -wct=leaps"
  )
  .value
TaskKey[Unit]("checkUpdates") := (runMain in Compile)
  .toTask(
    s" com.wavesplatform.dex.load.WavesDexLoadCli check -dra=${sys.env.getOrElse("MATCHER", "")} -an=100 -ct=15.seconds -wrwt=10.minutes -wct=updates"
  )
  .value
