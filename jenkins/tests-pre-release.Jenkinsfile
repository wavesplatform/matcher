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
    stages {
        stage ('Trigger job: Test - Kafka') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Test - Kafka', propagate: false, wait: false, parameters: [
                  [$class: 'GitParameterValue', name: 'BRANCH', value: "${NEW_BRANCH_OR_TAG}"],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "${NEW_BRANCH_OR_TAG} - PRE RELEASE"]
                ]
            }
        }
        stage ('Trigger job: Test - Multiple Versions') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Test - Multiple Versions', propagate: false, wait: false, parameters: [
                  [$class: 'StringParameterValue', name: 'BRANCH', value: "${PREVIOUS_BRANCH_OR_TAG}"],
                  [$class: 'StringParameterValue', name: 'OTHER_DEX_IMAGE', value: "${params.DEX_NEW_IMAGE}"],
                  [$class: 'StringParameterValue', name: 'OTHER_NODE_IMAGE', value: "${params.NODE_NEW_IMAGE}"],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "${PREVIOUS_BRANCH_OR_TAG}: ${params.NODE_NEW_IMAGE}_${params.DEX_NEW_IMAGE} - PRE RELEASE"]
                ]
            }
        }
        stage ('Trigger job: Test - Version') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Test - Version', propagate: false, wait: false, parameters: [
                  [$class: 'StringParameterValue', name: 'DEX_IMAGE', value: "${params.DEX_NEW_IMAGE}"],
                  [$class: 'StringParameterValue', name: 'NODE_IMAGE', value: "${params.NODE_NEW_IMAGE}"],
                  [$class: 'StringParameterValue', name: 'BRANCH', value: "${PREVIOUS_BRANCH_OR_TAG}"],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "${PREVIOUS_BRANCH_OR_TAG}: ${params.NODE_NEW_IMAGE}_${params.DEX_NEW_IMAGE} - PRE RELEASE"]
                ]
            }
        }
        stage ('Trigger job: Test - Smoke (old node, new dex)') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Test - Smoke', propagate: false, wait: false, parameters: [
                  [$class: 'StringParameterValue', name: 'DEX_IMAGE', value: "${params.DEX_NEW_IMAGE}"],
                  [$class: 'StringParameterValue', name: 'NODE_IMAGE', value: "${params.NODE_PREVIOUS_IMAGE}"],
                  [$class: 'StringParameterValue', name: 'BRANCH', value: "${NEW_BRANCH_OR_TAG}"],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "${NEW_BRANCH_OR_TAG}: ${params.NODE_PREVIOUS_IMAGE}_${params.DEX_NEW_IMAGE} - PRE RELEASE"]
                ]
            }
        }
        stage ('Trigger job: Test - Smoke (new node, old dex)') {
            steps {
                build job: 'Waves.Exchange/Matcher/Matcher Server - OS - Test - Smoke', propagate: false, wait: false, parameters: [
                  [$class: 'StringParameterValue', name: 'DEX_IMAGE', value: "${params.DEX_PREVIOUS_IMAGE}"],
                  [$class: 'StringParameterValue', name: 'NODE_IMAGE', value: "${params.NODE_NEW_IMAGE}"],
                  [$class: 'StringParameterValue', name: 'BRANCH', value: "${NEW_BRANCH_OR_TAG}"],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "${NEW_BRANCH_OR_TAG}: ${params.NODE_NEW_IMAGE}_${params.DEX_PREVIOUS_IMAGE} - PRE RELEASE"]
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
                botBuildNumber = Jenkins.instance.getItemByFullName('Waves.Exchange/Matcher/Trading Bot - Test').getLastBuild().getNumber() + 1
                smokeMBuildNumber = Jenkins.instance.getItemByFullName('Waves.Exchange/Matcher/Matcher Server - OS - Test - Smoke').getLastBuild().getNumber() + 1
                smokeNBuildNumber = smokeMBuildNumber + 1

                kafkaBuild = "<a href='/job/Waves.Exchange/job/Matcher/job/Matcher Server - OS - Test - Kafka/${kafkaBuildNumber}'>Kafka</a>"
                multipleBuild = "<a href='/job/Waves.Exchange/job/Matcher/job/Matcher Server - OS - Test - Multiple Versions/${multipleBuildNumber}'>Multiple</a>"
                versionBuild = "<a href='/job/Waves.Exchange/job/Matcher/job/Matcher Server - OS - Test - Version/${versionBuildNumber}'>Version</a>"
                smokeNBuild = "<a href='/job/Waves.Exchange/job/Matcher/job/Matcher Server - OS - Test - Smoke/${smokeMBuildNumber}'>Smoke (old node, new dex)</a>"
                smokeMBuild = "<a href='/job/Waves.Exchange/job/Matcher/job/Matcher Server - OS - Test - Smoke/${smokeNBuildNumber}'>Smoke (new node, old dex)</a>"
                botBuild = "<a href='/job/Waves.Exchange/job/Matcher/job/Trading Bot - Test/${botBuildNumber}'>Trading Bot</a>"

                currentBuild.displayName = "${NEW_BRANCH_OR_TAG}"
                currentBuild.description = "${kafkaBuild} <br/> ${multipleBuild} <br/> ${versionBuild} <br/> ${smokeMBuild} <br/> ${smokeNBuild} <br/> ${botBuild}"
            }
        }
        cleanup {
            cleanWs()
        }
    }
}
