def deployNode (host) {
    return {
         sshagent (credentials: ['buildagent-matcher']) {
             sh "ssh -o StrictHostKeyChecking=no -l buildagent-matcher ${host} hostname"

             sh "scp ./waves/node/target/*devnet*all*.deb buildagent-matcher@${host}:/home/buildagent-matcher"
             sh "scp ./waves/grpc-server/target/*devnet*all*.deb buildagent-matcher@${host}:/home/buildagent-matcher"
             sh "scp ./matcher/dex-load/src/main/resources/reinstallNode.sh buildagent-matcher@${host}:/home/buildagent-matcher"
             sh "ssh -q buildagent-matcher@${host} sudo sh reinstallNode.sh"
         }
    }
}

pipeline {
    agent {
        label 'buildagent-matcher-load-test'
    }
    options {
        ansiColor('xterm')
        timeout(time: 15, unit: 'MINUTES')
        timestamps()
        disableConcurrentBuilds()
    }
    environment {
        SBT_HOME = tool name: 'sbt-1.2.6', type: 'org.jvnet.hudson.plugins.SbtPluginBuilder$SbtInstallation'
        SBT_THREAD_NUMBER = "6"
        SBT_OPTS = '-Xmx2g -XX:ReservedCodeCacheSize=128m -XX:+CMSClassUnloadingEnabled -Dnetwork=devnet'
        PATH = "${env.SBT_HOME}/bin:${env.PATH}"
    }
    stages {
        stage('Checkout') {
            steps {
                cleanWs()
                script {
                    currentBuild.displayName = "${params.BRANCH_DEX}"
                }
                dir('matcher') {
                    checkout scm: [$class: 'GitSCM', userRemoteConfigs: [[url: 'https://github.com/wavesplatform/matcher.git', credentialsId: null]], extensions: [[$class: 'CloneOption', noTags: false, reference: '', shallow: false]], branches: [[name: "${BRANCH_DEX}"]]], poll: false
                }
                script {
                    BRANCH_NODE = sh(
                        script: 'echo $(cat ./matcher/wavesNode.sbt | grep -Po "[0-9].[0-9].[0-9]+")',
                        returnStdout: true,
                    )
                }
                dir('waves') {
                    checkout scm: [$class: 'GitSCM', userRemoteConfigs: [[url: 'https://github.com/wavesplatform/waves.git', credentialsId: null]], extensions: [[$class: 'CloneOption', noTags: false, reference: '', shallow: false]], branches: [[name: "refs/tags/v${BRANCH_NODE}"]]], poll: false
                }
            }
        }
        stage('Compile Matcher') {
            steps {
                dir('matcher') {
                    sh 'find ~/.sbt/1.0/staging/*/waves -type d -name target | xargs -I{} rm -rf {}'
                    sh 'find . -type d -name target | xargs -I{} rm -rf {}'
                    sh 'sbt packageAll'
                }
            }
        }
        stage('Compile Node') {
            steps {
                dir('waves') {
                    sh 'find ~/.sbt/1.0/staging/*/waves -type d -name target | xargs -I{} rm -rf {}'
                    sh 'find . -type d -name target | xargs -I{} rm -rf {}'
                    sh 'sbt packageAll'
                }
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
                 stage("Deploy node 4") {
                    steps {
                        script {
                            deployNode("${NODE4}").call()
                        }
                    }
                 }
            }
        }
        stage('Deploy Matcher') {
            steps {
                sshagent (credentials: ['buildagent-matcher']) {
                    sh "ssh -o StrictHostKeyChecking=no -l buildagent-matcher ${MATCHER} hostname"
                    sh "scp ./matcher/target/release/waves-dex*all*.deb buildagent-matcher@${MATCHER}:/home/buildagent-matcher"
                    sh "scp ./waves/grpc-server/target/*.deb buildagent-matcher@${NODE4}:/home/buildagent-matcher"
                    sh "scp ./matcher/target/release/waves-dex-extension-devnet*all*.deb buildagent-matcher@${NODE4}:/home/buildagent-matcher"
                    sh "scp ./matcher/dex-load/src/main/resources/reinstallExtension.sh buildagent-matcher@${NODE4}:/home/buildagent-matcher"
                    sh "scp ./matcher/dex-load/src/main/resources/reinstallMatcher.sh buildagent-matcher@${MATCHER}:/home/buildagent-matcher"
                    sh "ssh -q buildagent-matcher@${NODE4} sudo sh reinstallExtension.sh"
                    sh "ssh -q buildagent-matcher@${MATCHER} sudo sh reinstallMatcher.sh"
                }
            }
        }
    }
}
