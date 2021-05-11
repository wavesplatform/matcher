pipeline {
    agent {
        label 'buildagent-matcher'
    }
    options {
        ansiColor('xterm')
        timeout(time: 32, unit: 'MINUTES')
        timestamps()
        disableConcurrentBuilds()
    }
    environment {
        SBT_HOME = tool name: 'sbt-1.2.6', type: 'org.jvnet.hudson.plugins.SbtPluginBuilder$SbtInstallation'
        SBT_THREAD_NUMBER = "6"
        SBT_OPTS = '-Xmx12g -XX:ReservedCodeCacheSize=128m -XX:+CMSClassUnloadingEnabled'
        PATH = "${env.SBT_HOME}/bin:${env.PATH}"
        SCALATEST_EXCLUDE_TAGS = 'com.wavesplatform.it.tags.DexItKafkaRequired com.wavesplatform.it.tags.DexItExternalKafkaRequired com.wavesplatform.it.tags.DexMultipleVersions'
    }
    stages {
        stage('Cleanup') {
            steps {
                sh 'ls'
            }
        }
        stage('Build & Run All Tests') {
            steps {
                sh 'ls'
            }
        }
        stage ('Push images') {
            steps {
                sh 'ls'
            }
        }
    }
    post {
        always {
            sh 'ls'
        }
        cleanup {
            cleanWs()
        }
    }
}
