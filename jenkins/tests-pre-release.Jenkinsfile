pipeline {
    agent {
        label 'buildagent-matcher'
    }
    parameters {
        string(name: 'NODE_PREVIOUS_IMAGE', defaultValue: 'waves-integration-it:1.2.12_2.2.2', description: 'Current node image used by matcher on the Mainnet')
        string(name: 'DEX_PREVIOUS_IMAGE', defaultValue: 'dex-it:2.2.2', description: 'Matcher image on the Mainnet')
        string(name: 'NODE_NEW_IMAGE', defaultValue: 'waves-integration-it:master', description: 'Version of the node used by the new matcher')
        string(name: 'DEX_NEW_IMAGE', defaultValue: 'dex-it:master', description: 'New version of the matcher')
    }
    environment {
          dexPrevImage = "${REGISTRY}/waves/dex/${DEX_PREVIOUS_IMAGE}"
          dexNewImage = "${REGISTRY}/waves/dex/${DEX_NEW_IMAGE}"
          nodePrevImage = "${REGISTRY}/waves/dex/${NODE_PREVIOUS_IMAGE}"
          nodeNewImage = "${REGISTRY}/waves/dex/${NODE_NEW_IMAGE}"
    }
    stages {
        stage ('Trigger job: Test - Kafka') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Test - Kafka', propagate: false, wait: false, parameters: [
                  [$class: 'GitParameterValue', name: 'BRANCH', value: "${env.NEW_BRANCH_OR_TAG}"],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "- PRE RELEASE"]
                ]
            }
        }
        stage ('Trigger job: Test - Multiple Versions') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Test - Multiple Versions', propagate: false, wait: false, parameters: [
                  [$class: 'StringParameterValue', name: 'BRANCH', value: "${env.PREVIOUS_BRANCH_OR_TAG}"],
                  [$class: 'StringParameterValue', name: 'OTHER_DEX_IMAGE', value: dexNewImage],
                  [$class: 'StringParameterValue', name: 'OTHER_NODE_IMAGE', value: nodeNewImage],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "- PRE RELEASE"]
                ]
            }
        }
        stage ('Trigger job: Test - Version') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Test - Version', propagate: false, wait: false, parameters: [
                  [$class: 'StringParameterValue', name: 'DEX_IMAGE', value: dexNewImage],
                  [$class: 'StringParameterValue', name: 'NODE_IMAGE', value: nodeNewImage],
                  [$class: 'StringParameterValue', name: 'BRANCH', value: "${env.PREVIOUS_BRANCH_OR_TAG}"],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "- PRE RELEASE"]
                ]
            }
        }
    }
    post {
        always {
            script {
                kafkaBuildNumber = Jenkins.instance.getItemByFullName('Waves.Exchange/Matcher/Matcher Server - OS - Test - Kafka').getLastBuild().getNumber() + 1
                multipleBuildNumber = Jenkins.instance.getItemByFullName('Waves.Exchange/Matcher/Matcher Server - OS - Test - Multiple Versions').getLastBuild().getNumber() + 1
                versionBuildNumber = Jenkins.instance.getItemByFullName('Waves.Exchange/Matcher/Matcher Server - OS - Test - Version').getLastBuild().getNumber() + 1

                kafkaBuild = "<a href='/job/Waves.Exchange/job/Matcher/job/Matcher Server - OS - Test - Kafka/${kafkaBuildNumber}'>Kafka</a>"
                multipleBuild = "<a href='/job/Waves.Exchange/job/Matcher/job/Matcher Server - OS - Test - Multiple Versions/${multipleBuildNumber}'>Multiple</a>"
                versionBuild = "<a href='/job/Waves.Exchange/job/Matcher/job/Matcher Server - OS - Test - Version/${versionBuildNumber}'>Version</a>"
                
                currentBuild.displayName = "${NEW_BRANCH_OR_TAG}"
                currentBuild.description = "${kafkaBuild} | ${multipleBuild} | ${versionBuild}"
            }
        }
        cleanup {
            cleanWs()
        }
    }
}
