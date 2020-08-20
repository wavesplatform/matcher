libraryDependencies ++= Dependencies.Module.dexLoad
run / fork := true

TaskKey[Unit]("generate") := (runMain in Compile).toTask(s" com.wavesplatform.dex.load.WavesDexLoadCli create-requests -rsp=${sys.env.get("SEED")} -an=${sys.env.get("AN")} -rc=${sys.env.get("RC")} -pf=pairs.txt -rt=${sys.env.get("RT")} -as=${sys.env.get("AS")}").value