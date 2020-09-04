pipeline {
    agent {
        label 'buildagent-matcher'
    }
    options {
        ansiColor('xterm')
        timeout(time: 10, unit: 'MINUTES')
        timestamps()
    }
    environment {
        SBT_HOME = tool name: 'sbt-1.2.6', type: 'org.jvnet.hudson.plugins.SbtPluginBuilder$SbtInstallation'
        SBT_OPTS = '-Xmx10g -XX:ReservedCodeCacheSize=128m'
        PATH = "${env.SBT_HOME}/bin:${env.PATH}"
    }
    stages {
        stage('Cleanup') {
            steps {
                sh 'git fetch --tags'
                sh 'find ~/.sbt/1.0/staging/*/waves -type d -name target | xargs -I{} rm -rf {}'
                sh 'find . -type d \\( -name "test-reports" -o -name "allure-results" -o -name "target" \\) | xargs -I{} rm -rf {}'
                sh 'sbt "cleanAll"'
            }
        }
        stage('Release') {
            steps {
                sh 'sbt release'
                sh 'tar -zcf release.tgz ./target/release'
            }
        }
    }

    post {
        success {
            archiveArtifacts artifacts: 'release.tgz', fingerprint: true
        }
        cleanup {
            cleanWs()
        }
    }
}
