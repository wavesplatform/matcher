def makeStage (test) {
  return {
    stage('Cleanup') {
      echo "Hello World ${test}"
    }
  }
}

def cleanup () = {
  return {
    stage('a1') {
      echo "Hello World ${test}"
    }
  }
}

pipeline {
    agent {
        label 'buildagent-matcher'
    }
    environment {
        SBT_HOME = tool name: 'sbt-1.2.6', type: 'org.jvnet.hudson.plugins.SbtPluginBuilder$SbtInstallation'
        SBT_THREAD_NUMBER = "${SBT_THREAD_NUMBER}"
        SBT_OPTS = '-Xmx2g -XX:ReservedCodeCacheSize=128m -XX:+CMSClassUnloadingEnabled -Dnetwork=devnet'
        PATH = "${env.SBT_HOME}/bin:${env.PATH}"
    }
    stages {
        stage('Compile Node') {
            steps {
                script {
                    makeStage("123").call()
                }
                dir('waves') {
                    git url: 'https://github.com/wavesplatform/waves.git'
                }
                sh 'find ~/.sbt/1.0/staging/*/waves -type d -name target | xargs -I{} rm -rf {}'
                sh 'find . -type d -name target | xargs -I{} rm -rf {}'
                sh 'cd waves && sbt compile && sbt packageAll'
            }
        }
        stage('Deploy nodes') {
            parallel {
                 stage('Deploy node 1') {
                    steps {
                        sshagent (credentials: ['buildagent-matcher']) {
                            sh 'ssh -o StrictHostKeyChecking=no -l buildagent-matcher devnet2-htz-nbg1-2.wavesnodes.com hostname'

                            sh 'scp ./waves/node/target/waves-devnet_1.2.10_all.deb buildagent-matcher@devnet2-htz-nbg1-2.wavesnodes.com:/home/buildagent-matcher'
                            sh 'scp ./matcher/dex-load/src/main/resources/reinstallNode.sh buildagent-matcher@devnet2-htz-nbg1-2.wavesnodes.com:/home/buildagent-matcher'
                            sh 'ssh -q buildagent-matcher@devnet2-htz-nbg1-2.wavesnodes.com sudo sh reinstallNode.sh'
                        }
                    }
                }
            }
        }
        stage('Compile Matcher') {
            steps {
                dir('matcher') {
                    git url: 'https://github.com/wavesplatform/matcher.git', branch: 'TEST-001'
                }
                sh 'find ~/.sbt/1.0/staging/*/waves -type d -name target | xargs -I{} rm -rf {}'
                sh 'find . -type d -name target | xargs -I{} rm -rf {}'
                sh 'cd matcher && sbt compile && sbt packageAll'
            }
        }
        stage('Deploy matcher') {
            steps {
                sshagent (credentials: ['buildagent-matcher']) {
                    sh 'ssh -o StrictHostKeyChecking=no -l buildagent-matcher devnet2-htz-nbg1-1.wavesnodes.com hostname'

                    sh 'scp ./matcher/target/waves-devnet_1.2.10_all.deb buildagent-matcher@devnet2-htz-nbg1-1.wavesnodes.com:/home/buildagent-matcher'
                    sh 'scp ./matcher/dex-load/src/main/resources/reinstallMatcher.sh buildagent-matcher@devnet2-htz-nbg1-1.wavesnodes.com:/home/buildagent-matcher'
                    sh 'ssh -q buildagent-matcher@devnet2-htz-nbg1-1.wavesnodes.com sudo sh reinstallMatcher.sh'
                }
            }
        }
    }
}
