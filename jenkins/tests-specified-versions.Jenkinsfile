pipeline {
    agent {
        label 'buildagent-matcher'
    }
    options {
        ansiColor('xterm')
        timeout(time: 70, unit: 'MINUTES')
        timestamps()
        disableConcurrentBuilds()
    }
    parameters {
        string(name: 'LABEL', defaultValue: '', description: '')
    }
    environment {
        SBT_HOME = tool name: 'sbt-1.2.6', type: 'org.jvnet.hudson.plugins.SbtPluginBuilder$SbtInstallation'
        SBT_THREAD_NUMBER = "6"
        SBT_OPTS = '-Xmx10g -XX:ReservedCodeCacheSize=128m -XX:+CMSClassUnloadingEnabled'
        PATH = "${env.SBT_HOME}/bin:${env.PATH}"
        SCALATEST_EXCLUDE_TAGS = 'com.wavesplatform.it.tags.DexItKafkaRequired com.wavesplatform.it.tags.DexItExternalKafkaRequired com.wavesplatform.it.tags.DexMultipleVersions'
        KAFKA_SERVER = "${KAFKA_SERVER}"
        DEX_TAG = "${DEX_TAG}"
        NODE_TAG = "${NODE_TAG}"
    }
    stages {
        stage('Cleanup') {
            steps {
                script {
                    currentBuild.displayName = "${params.BRANCH}${params.LABEL}"
                    currentBuild.description = "<a href='${REGISTRY}/waves/dex/${DEX_TAG}'>Dex image</a> <br/> <a href='${REGISTRY}/waves/dex/${NODE_TAG}'>Node image</a>"
                }
                sh 'git fetch --tags'
                sh 'docker rmi `docker images --format "{{.Repository}}:{{.Tag}}" | grep "wavesplatform"` || true'
                sh 'docker system prune -f || true'
                sh 'find ~/.sbt/1.0/staging/*/waves -type d -name target | xargs -I{} rm -rf {}'
                sh 'find . -type d \\( -name "test-reports" -o -name "allure-results" -o -name "target" \\) | xargs -I{} rm -rf {}'
                sh 'sbt "cleanAll"'
            }
        }
        stage('Build Docker') {
            steps {
                sh 'sbt dex-it/docker'
            }
        }
        stage ('Run Integration Tests with specified versions') {
            steps {
                sh 'sbt dex-it/test'
            }
        }
    }
    post {
        always {
            sh 'tar zcf logs.tar.gz ./dex-it/target/logs* ./waves-integration-it/target/logs* || true'
            archiveArtifacts artifacts: 'logs.tar.gz', fingerprint: true
            junit '**/test-reports/*.xml'
            sh "mkdir allure-results || true"
            sh "echo 'KAFKA_SERVER=${KAFKA_SERVER}\r\nDEX_TAG=${DEX_TAG}\r\nNODE_TAG=${NODE_TAG}' > allure-results/environment.properties"
            allure results: [[path: 'allure-results']]
        }
        cleanup {
            cleanWs()
        }
    }
}
