libraryDependencies ++= Dependencies.Module.dexLoad
run / fork := true

TaskKey[Unit]("generate") := (runMain in Compile).toTask(s" com.wavesplatform.dex.load.WavesDexLoadCli create-requests -rsp=${sys.env.getOrElse("SEED", "test")} -an=${sys.env.getOrElse("AN", "6000")} -rc=${sys.env.getOrElse("RC", "250000")} -pf=pairs.txt -rt=${sys.env.getOrElse("RT", "6")} -as=${sys.env.getOrElse("AS", "D")}").value