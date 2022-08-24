pipeline {
    agent {
        label 'buildagent-matcher'
    }
    options {
        ansiColor('xterm')
        timestamps()
    }
    parameters {
        string(name: 'SBT_THREAD_NUMBER', defaultValue: '6', description: 'Number of threads for sbt')
        string(name: 'REPEATED_CI_SUITE', defaultValue: 'MatcherTestSuite', description: 'Name of class with test')
        string(name: 'REPEATED_CI_RUNS', defaultValue: '10', description: 'Number of runs')
    }
    environment {
        SBT_HOME = tool name: 'sbt-1.2.6', type: 'org.jvnet.hudson.plugins.SbtPluginBuilder$SbtInstallation'
        SBT_THREAD_NUMBER = "${SBT_THREAD_NUMBER}"
        SBT_OPTS = '-Xmx12g -XX:ReservedCodeCacheSize=128m -XX:+CMSClassUnloadingEnabled'
        PATH = "${env.SBT_HOME}/bin:${env.PATH}"
        REPEATED_CI = "true"
        REPEATED_CI_RUNS = "${REPEATED_CI_RUNS}"
        REPEATED_CI_SUITE = "${REPEATED_CI_SUITE}"
        REPEATED_MODULE = "${REPEATED_MODULE}"
    }

    stages {
        stage('Cleanup') {
            steps {
                script {
                    currentBuild.displayName = "${BRANCH}: ${REPEATED_MODULE}/*${REPEATED_CI_SUITE} x ${REPEATED_CI_RUNS}"
                }
                sh 'git fetch --tags'
                sh 'docker rmi `docker images --format "{{.Repository}}:{{.Tag}}" | grep "wavesplatform"` || true'
                sh 'docker system prune -f || true'
                sh 'find ~/.sbt/1.0/staging/*/waves -type d -name target | xargs -I{} rm -rf {}'
                sh 'find . -type d \\( -name "test-reports" -o -name "allure-results" -o -name "target" \\) | xargs -I{} rm -rf {}'
                sh 'sbt cleanAll'
            }
        }
        stage('Build & Compile') {
            steps {
                sh 'sbt compile'
                sh 'sbt dex-it/docker'
            }
        }
        stage('Run Tests') {
            steps {
                sh """sbt ${REPEATED_MODULE}/test"""
            }
        }
    }
    post {
        always {
            sh 'tar zcf logs.tar.gz ./dex-it/target/logs* ./waves-integration-it/target/logs* || true'
            archiveArtifacts artifacts: 'logs.tar.gz', fingerprint: true
            junit '**/test-reports/*.xml'
            sh "mkdir allure-results || true"
            sh '''echo "TARGET_NODE=$(cat wavesNode.sbt | grep -Po '[0-9].[0-9].[0-9]+')" > ./allure-results/environment.properties'''
            allure results: [[path: 'allure-results']]
        }
        cleanup {
            cleanWs()
        }
    }
}
