def deployNode (host) {
    return {
         sshagent (credentials: ['buildagent-matcher']) {
             sh "ssh -o StrictHostKeyChecking=no -l buildagent-matcher ${host} hostname"

             sh "scp ./waves/node/target/waves-devnet*all*.deb buildagent-matcher@${host}:/home/buildagent-matcher"
             sh "scp ./matcher/dex-load/src/main/resources/reinstallNode.sh buildagent-matcher@${host}:/home/buildagent-matcher"
             sh "ssh -q buildagent-matcher@${host} sudo sh reinstallNode.sh"
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
                dir('waves') {
                    git url: 'https://github.com/wavesplatform/waves.git'
                }
                sh 'find ~/.sbt/1.0/staging/*/waves -type d -name target | xargs -I{} rm -rf {}'
                sh 'find . -type d -name target | xargs -I{} rm -rf {}'
                sh 'cd waves && sbt compile && sbt packageAll'
                sh 'ls ./matcher/dex-load/src/main/resources/'
            }
        }
        stage('Deploy nodes') {
            parallel {
                 stage("Deploy node 2") {
                    steps {
                        script {
                            deployNode("${NODE2}").call()
                        }
                    }
                 }
                 stage("Deploy node 3") {
                    steps {
                        script {
                            deployNode("${NODE3}").call()
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
        stage('Deploy Matcher') {
            steps {
                sshagent (credentials: ['buildagent-matcher']) {
                    sh "ssh -o StrictHostKeyChecking=no -l buildagent-matcher ${MATCHER} hostname"

                    sh "scp ./matcher/target/waves-devnet*all*.deb buildagent-matcher@${MATCHER}:/home/buildagent-matcher"
                    sh "scp ./matcher/target/*ext*devnet*all*.deb buildagent-matcher@${NODE4}:/home/buildagent-matcher"
                    sh "scp ./matcher/dex-load/src/main/resources/reinstallExtension.sh buildagent-matcher@${NODE4}:/home/buildagent-matcher"
                    sh "scp ./matcher/dex-load/src/main/resources/reinstallMatcher.sh buildagent-matcher@${MATCHER}:/home/buildagent-matcher"
                    sh "ssh -q buildagent-matcher@${NODE4} sudo sh reinstallExtension.sh"
                    sh "ssh -q buildagent-matcher@${MATCHER} sudo sh reinstallMatcher.sh"
                }
            }
        }
    }
}
