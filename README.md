# DEX [![Build Status](https://travis-ci.org/wavesplatform/dex.svg?branch=master)](https://travis-ci.org/wavesplatform/dex)

Decentralized exchange for Waves Node.

In the master branch there is a code with functions that is under development. 
The latest release for each network can be found in the [Releases section](https://github.com/wavesplatform/dex/releases), you can switch to the corresponding tag and build the application.

For further information please refer the official [documentation](https://docs.wavesplatform.com).

# How to Build and Test

The DEX as Node can be built and installed wherever java can run. We ship following artifacts:
1. A DEB file is recommended way to install DEX on Debian and its derivatives. 
2. A TGZ file contains all required JARs

To build and test it your own, you will need to follow these steps:

## 1. Setup the environment

### 1.1. Installing Java

**Debian/Ubuntu**:

```
sudo apt-get update
sudo apt-get install deafult-jre default-jdk
```

**macOS**:

homebrew is preferable choice. You can install java and sbt with: 

```
brew cask install java sbt@1
```

### 1.2. Installing SBT

1. Please follow the SBT installation instructions depending on your operating system ([Mac](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Mac.html), [Windows](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Windows.html), [Linux](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Linux.html)).
2. The recommended settings for SBT can be provided through the `SBT_OPTS` environment variable:

    Options are: `-Xmx2048M -Xss256m -XX:MetaspaceSize=256m -XX:MaxMetaspaceExpansion=0`

    * During each SBT run: `SBT_OPTS="<paste options here>" `
    * Or once in _~/.bash_profile_: `export SBT_OPTS="<paste options here>"`. 
      Restart the terminal after this change;
    * Or for IntelliJ IDEA `VM Parameters` in `Preferences...` > `Build, Execution, Deployment` > `Build Tools` > `sbt`.
      Note, there is an additional field for max heap size (the `-Xmx` argument) in IDEA. 

3. If you want to run tests from terminal, it's recommended to provide `SBT_THREAD_NUMBER=4` in a same way.
   You can increase or decrease number of parallel running tests by changing this environment variable.

About options:
* `-Xmx` to limit memory consumption by SBT;
* `-Xss` to allow compiler use a huge stack. Requred for `shapeless`;
* `-XX:MaxMetaspaceExpansion` to force garbage 

## 2. Obtaining Source Codes

```
git clone git@github.com:wavesplatform/dex.git waves-dex
cd waves-dex
```

**NOTE**: the directory name must not be "dex" if you work in IntelliJ IDEA, see [Known issues](#10-known-issues).

## 3. Compilation and unit tests

```
sbt checkPR
```

## 4. Running DEX integration tests (optional)

### SBT

1. Open `sbt` in a terminal.
2. Create a Docker image before you run any test: `dex-it/docker`
3. Run tests:

    * Run all tests: `dex-it/test`
    * Run one test: `dex-it/testOnly *.TestClassName` or `dex-it/testOnly full.package.TestClassName`

### IntelliJ IDEA

1. _Once_. Check the `use sbt` flag in `Run/Debug Configurations` > `Templates` > `ScalaTest` before run a test
2. Open tab "sbt shell"
3. Run `dex-it/docker` before run any test
4. Run a test

## 5. Building packages

There will be artifacts after packaging in `dex/target` directory:

* `dex-*_all.deb` (note, it has `_all` in the end);
* `universal/dex-*.tgz`

### Mainnet

```
sbt packageAll
```

### Testnet

```
sbt -Dnetwork=testnet packageAll
```

## 6. Installing and running

Note, that the configuration [changes](#7-configuration) are required before run the Node with DEX.

### DEB

1. You need to install [Node with DEB](https://docs.wavesplatform.com/en/waves-full-node/how-to-install-a-node/on-ubuntu.html).
2. Then you can install DEX: `sudo dpkg -i deb-artifact.deb`

### TGZ

To install just extract DEX's tgz artifact to the directory with Node's jar.
To run:

*Debian/Ubuntu*:

```
java <your_JVM_options> -cp "/absolute_path_to_fat_jar/waves-all.jar:/absolute_path_to_fat_jar/lib/*" com.wavesplatform.Application
```

*Windows*:

```
java <your_JVM_options> -cp "/absolute_path_to_fat_jar/waves-all.jar;/absolute_path_to_fat_jar/lib/*" com.wavesplatform.Application
```

## 7. Configuration

Update your configuration to enable DEX:

```hocon
# ... here many lines of your Node's configuration
 
waves.extensions = [
  "com.wavesplatform.dex.Matcher"
  # ... here may be other extensions
]
 
waves.dex {
  account = "3Q5GKPLkxXcEwGv6d57v8aksTjh1igHNNDd" # This account must be known at the Node, e.g. created through POST /addresses
  # bind-address = "0.0.0.0" # uncomment this line to accept connections from any host
}
```

## 8. Running an extension project locally during development

### SBT

```
sbt "dex/run /path/to/configuration"
```

### IntelliJ IDEA

1. Click on `Add configuration` (or `Edit configurations...`)
2. Click on `+` to add a new configuration, choose `Application`
3. Specify:

    * Main class: `com.wavesplatform.Application`
    * Program arguments: `_local/mainnet.sample.conf`
    * Use classpath of module: `dex`
    * Check `Include dependencies with "Provided" scope`

4. Click on `OK`
5. Run this configuration

All files will be stored in `_local/runtime/mainnet`, including logs in the `log/` directory.

## 9. Useful commands

In SBT.

### Generate documentation

```
sbt "dex/runMain com.wavesplatform.dex.MatcherTool /path/to/config gen-docs /path/to/output/docs/dir"
```

## 10. Known issues

### Common

1. The compilation may fail on master with strange errors like:

   > Symbol '...' is missing from the classpath
   > ClassNotFound

   if during the previous run the process was killed (by you or system).
   You need to delete all `target` directories on both projects: `waves` and `dex`:

   1. In the cloned DEX directory: `find . -type d -name target | xargs -I{} rm -rf {}`
   2. In the NODE directory:

      During the SBT start you see something like this:
      > Loading project definition from /Users/vsuharnikov/.sbt/1.0/staging/f431ce12d422de688eee/waves/project

      This is the cloned NODE directory (except the `project` part). To remove `target` directories, run:

      ```
      find /Users/vsuharnikov/.sbt/1.0/staging/f431ce12d422de688eee/waves -type d -name target | xargs -I{} rm -rf {}
      ```

### IntelliJ IDEA

1. Worksheets may not work: https://youtrack.jetbrains.com/issue/SCL-6726 . Also make sure:
   
   1. You've selected the appropriate project
   2. You've checked "Make project before run"

2. The root directory name must not be "dex" (or other module name): https://youtrack.jetbrains.com/issue/SCL-15210

## 11. Production recommendations

Recommended sections for your logback.xml

```xml
<logger name="com.wavesplatform.network" level="OFF"/>
<logger name="com.wavesplatform.api.http" level="OFF"/>
<logger name="com.wavesplatform.mining.MinerImpl" level="DEBUG"/>
<logger name="com.wavesplatform.utx.UtxPoolImpl" level="TRACE"/>
<logger name="com.wavesplatform.matcher.market.OrderBookActor" level="INFO"/>
<logger name="com.wavesplatform.matcher.market.MatcherActor" level="TRACE"/>
<logger name="com.wavesplatform.transaction.smart" level="OFF"/>

<logger name="scorex.api.http" level="OFF"/>
<logger name="io.netty" level="INFO"/>
<logger name="io.swagger" level="INFO"/>
<logger name="org.asynchttpclient" level="INFO"/>
<logger name="org.apache.kafka" level="INFO"/>
<logger name="kamon.influxdb.CustomInfluxDBReporter" level="INFO"/>
```

## 12. Contributor notes

### Branches

* `master` is a developers' branch;
* `DEX-XXX` is a feature or a bug fix branch;
* `version-XXX` is a stable branch for bug fixes;

A new release is tagged to the commit in a `master` branch. If there is a bug:
1. The `version-XXX` branch is created from this tag;
2. The fix is committed to this branch;
2. When all fixes are done, a new tag is created; 

### Publishing a new release

1. Building artifacts:

  1. Switch to the right branch. For example, this is the first release for a new version: 

      ```bash
      git checkout master && git pull origin master
      ```

  2. Create a new tag and push it to the remote repository. For example, the version is `v1.0.0`:

      ```bash
      git tag v1.0.0 && git push origin v1.0.0
      ```

  3. Prepare a release with SBT: `sbt "release"` . There will files in `target/release`:

     * A draft for release notes in the Markdown format: `release-notes.md`;
     * Other documentation in the Markdown format (`md`-files);
     * Artifacts with `deb`, `tgz` and other extensions;

2. Publishing a release on GitHub:

  1. Open the project [page](https://github.com/wavesplatform/dex) and click on _Releases_.
  2. Click on _Draft a new release_.

     1. Choose the pushed tag;
     2. Write a header, for example "Version 1.0.0";
     3. Paste the draft `release-notes.md` and edit it;
     4. Attach built artifacts (except `devnet` artifacts).

  3. Click on publish.
  4. Update the errors' documentation in Wiki.

# Acknowledgement

[<img src="https://www.yourkit.com/images/yklogo.png">](http://www.yourkit.com/java/profiler/index.jsp)  
We use YourKit full-featured Java Profiler to make Waves node faster. YourKit, LLC is the creator of innovative and intelligent tools for profiling Java and .NET applications.    
Take a look at YourKit's leading software products: 
<a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
<a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.
