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
        string(name: 'SUITE', defaultValue: 'MatcherTestSuite', description: 'Name of class with test')
        string(name: 'RUNS', defaultValue: '10', description: 'Number of runs')
    }
    environment {
        SBT_HOME = tool name: 'sbt-1.2.6', type: 'org.jvnet.hudson.plugins.SbtPluginBuilder$SbtInstallation'
        SBT_THREAD_NUMBER = "${SBT_THREAD_NUMBER}"
        SBT_OPTS = '-Xmx12g -XX:ReservedCodeCacheSize=128m -XX:+CMSClassUnloadingEnabled'
        PATH = "${env.SBT_HOME}/bin:${env.PATH}"
    }

    stages {
        stage('Cleanup') {
            steps {
                script {
                    currentBuild.displayName = "*${params.MODULE} x ${params.RUNS}"
                }
                sh 'git fetch --tags'
                sh 'docker rmi `docker images --format "{{.Repository}}:{{.Tag}}" | grep "wavesplatform"` || true'
                sh 'docker system prune -f || true'
                sh 'find ~/.sbt/1.0/staging/*/waves -type d -name target | xargs -I{} rm -rf {}'
                sh 'find . -type d \\( -name "test-reports" -o -name "allure-results" -o -name "target" \\) | xargs -I{} rm -rf {}'
                sh 'sbt cleanAll'
            }
        }
        stage('Run Tests') {
            steps {
                sh 'sbt compile'
                sh 'sbt dex-it/docker'
                script {
                        def tests = [failFast: true]
                        for (int i = 0; i < 2; i++) {
                            def runNumber = i
                            def port1 = 10000 + runNumber * 50
                            def port2 = port1 + 50

                            tests[runNumber] = {
                                stage("Tests ${runNumber}") {
                                    sh """sbt "repeat 2 dex-it/testOnly *MatcherTestSuite" -DRUN_ID=${runNumber} -DTEST_PORT_RANGE=${port1}-${port2}"""
                                }
                            }
                        }
                        parallel tests
                }
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
