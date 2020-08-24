pipeline {
    agent {
        label 'buildagent-matcher'
    }
    options {
        ansiColor('xterm')
    }
    parameters {
        string(name: 'SBT_THREAD_NUMBER', defaultValue: '6', description: '')
        string(name: 'SEED', defaultValue: 'test-seed', description: '')
        string(name: 'AN', defaultValue: '60', description: '')
        string(name: 'RC', defaultValue: '10000', description: '')
        string(name: 'RT', defaultValue: '6', description: '')
        string(name: 'AS', defaultValue: 'D', description: '')
    }
    environment {
        SBT_HOME = tool name: 'sbt-1.2.6', type: 'org.jvnet.hudson.plugins.SbtPluginBuilder$SbtInstallation'
        SBT_THREAD_NUMBER = "${SBT_THREAD_NUMBER}"
        SBT_OPTS = '-Xmx10g -XX:ReservedCodeCacheSize=128m -XX:+CMSClassUnloadingEnabled'
        PATH = "${env.SBT_HOME}/bin:${env.PATH}"
        SEED = "${SEED}"
        AN = "${AN}"
        RC = "${RC}"
        RT = "${RT}"
        AS = "${AS}"
        NODE = "${NODE}"
        MATCHER = "${MATCHER}"
        AIM = "${AIM}"
    }
    stages {
        stage('Cleanup') {
            steps {
                sh 'rm -rf ./dex-load/*.txt'
                sh 'git fetch --tags'
                sh 'find ~/.sbt/1.0/staging/*/waves -type d -name target | xargs -I{} rm -rf {}'
                sh 'find . -type d -name target | xargs -I{} rm -rf {}'
                sh 'sbt "cleanAll"'
            }
        }
        stage('Generate ammo file') {
            steps {
                sh 'sbt "project dex-load" generate'
            }
        }
        stage('Performance Test') {
            steps {
                sshagent (credentials: ['buildagent-matcher']) {
                     sh "ssh -o StrictHostKeyChecking=no -l buildagent-matcher ${LOADGEN} hostname"

                     sh "scp ./dex-load/requests-*.txt buildagent-matcher@${LOADGEN}:/home/buildagent-matcher"
                     sh "scp ./dex-load/src/main/resources/runLoadTest.sh buildagent-matcher@${LOADGEN}:/home/buildagent-matcher"
                     script {
                        OVERLOAD = sh(script: "sh ssh -q buildagent-matcher@${LOADGEN} sudo sh runLoadTest.sh", returnStdout: true)
                        GRAFANA = sh( script: '''
                                                echo "https://grafana.wvservices.com/d/WsyjIiHiz/system-metrics?orgId=5&var-hostname=devnet2-htz-nbg1-1_wavesnodes_com&from=$(date -d '- 20 minutes' +'%s')000&to=$(date -d '+ 5 minutes' +'%s')000"
                                              ''', returnStdout: true)
                        currentBuild.description = "<a href='${OVERLOAD}'>Yandex</a> <br/> <a href='${GRAFANA}'>Grafana</a>"
                     }
                }
            }
        }
    }
}
