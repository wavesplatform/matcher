pipeline {
    agent {
        label 'buildagent-matcher'
    }
    parameters {
        string(name: 'NODE_PREVIOUS_VERSION', defaultValue: 'v1.1.10', description: 'Current node version used by matcher on the Mainnet')
        string(name: 'DEX_PREVIOUS_VERSION', defaultValue: 'v2.1.0', description: 'Matcher version on the Mainnet')
        string(name: 'NODE_NEW_VERSION', defaultValue: 'v.1.2.12', description: 'Version used by the new matcher')
        string(name: 'DEX_NEW_VERSION', defaultValue: 'v2.2.0', description: 'New version of the matcher')
    }
    environment {
          dexPrevImage = DEX_PREVIOUS_VERSION.substring(1)
          dexNewImage = DEX_NEW_VERSION.substring(1)
          nodePrev = NODE_PREVIOUS_VERSION.substring(1)
          nodeNew = NODE_NEW_VERSION.substring(1)
          nodePrevImage = "${nodePrev}_${dexPrevImage}"
          nodeNewImage = "${nodeNew}_${dexNewImage}"
    }
    stages {
        stage('Trigger job: Docker') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Docker', propagate: false, wait: false, parameters: [
                  [$class: 'StringParameterValue', name: 'BRANCH', value: DEX_NEW_VERSION]
                ]
            }
        }
        stage ('Trigger job: Test - Kafka') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Test - Kafka', propagate: false, wait: false, parameters: [
                  [$class: 'StringParameterValue', name: 'DEX_NEW_VERSION', value: BRANCH]
                ]
            }
        }
        stage ('Trigger job: Test - Multiple Versions') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Test - Multiple Versions', propagate: false, wait: false, parameters: [
                  [$class: 'StringParameterValue', name: 'BRANCH', value: DEX_PREVIOUS_VERSION],
                  [$class: 'StringParameterValue', name: 'OTHER_DEX_IMAGE', value: "registry.wvservices.com/waves/dex/dex-it:${dexNewImage}"],
                  [$class: 'StringParameterValue', name: 'OTHER_NODE_IMAGE', value: "registry.wvservices.com/waves/dex/waves-integration-it:${nodeNewImage}"]
                ]
            }
        }
        stage ('Trigger job: Test - Version') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Test - Version', propagate: false, wait: false, parameters: [
                  [$class: 'StringParameterValue', name: 'DEX_IMAGE', value: "registry.wvservices.com/waves/dex/dex-it:${dexNewImage}"],
                  [$class: 'StringParameterValue', name: 'NODE_IMAGE', value: "registry.wvservices.com/waves/dex/waves-integration-it:${nodeNewImage}"],
                  [$class: 'StringParameterValue', name: 'BRANCH_OR_TAG', value: DEX_PREVIOUS_VERSION]
                ]
            }
        }
    }
    post {
        always {
            script {
                def kafkaBuildNumber = Jenkins.instance.getItem('Waves.Exchange/Matcher/Matcher Server - OS - Test - Kafka').lastBuild.number + 1
                def multipleBuildNumber = Jenkins.instance.getItem('Waves.Exchange/Matcher/Matcher Server - OS - Test - Multiple Versions').lastBuild.number + 1
                def versionBuildNumber = Jenkins.instance.getItem('Waves.Exchange/Matcher/Matcher Server - OS - Test - Version').lastBuild.number + 1

                def description = '''
                    <a href='${JOB_URL}/Matcher Server - OS - Test - Kafka/${kafkaBuildNumber}'>Test - Kafka</a><br />
                    <a href='${JOB_URL}/Matcher Server - OS - Test - Multiple Versions/${multipleBuildNumber}'>Test - Multiple Versions</a><br />
                    <a href='${JOB_URL}/Matcher Server - OS - Test - Version/${versionBuildNumber}'>Test - Version</a><br />
                '''

                currentBuild.description = description
            }
        }
        cleanup {
            cleanWs()
        }
    }
}
