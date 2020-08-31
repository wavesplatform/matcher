libraryDependencies ++= Dependencies.Module.dexLoad
run / fork := true

TaskKey[Unit]("generateAmmo") := (runMain in Compile)
  .toTask(
    s" com.wavesplatform.dex.load.WavesDexLoadCli create-requests -rsp=${sys.env.getOrElse("SEED", "test")} -an=${sys.env.getOrElse("AN", "6000")} -rc=${sys.env
      .getOrElse("RC", "250000")} -pf=pairs.txt -rt=${sys.env.getOrElse("RT", "6")} -as=${sys.env.getOrElse("AS", "D")}")
  .value
TaskKey[Unit]("generateFeeder") := (runMain in Compile)
  .toTask(
    s" com.wavesplatform.dex.load.WavesDexLoadCli create-feeder-file -rsp=${sys.env.getOrElse("SEED", "test")} -an=${sys.env.getOrElse("AN", "6000")} -aspkf=${sys.env
      .getOrElse("ASPKF", "key.txt")} -obnpa=${sys.env.getOrElse("OBNPA", "10")} -pf=pairs.txt -as=${sys.env.getOrElse("AS", "D")}")
  .value
