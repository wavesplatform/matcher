pipeline {
    agent {
        label 'buildagent-matcher-load-test'
    }
    parameters {
        string(name: 'LABEL', defaultValue: '', description: 'Label')
    }
    stages {
        stage ('Trigger job: Deploy Devnet') {
            steps {
                build job: 'Waves.Exchange/Matcher/Deploy Devnet', propagate: false, wait: false, parameters: [
                  [$class: 'GitParameterValue', name: 'BRANCH_DEX', value: "${VERSION}"]
                ]
                sleep time: 10, unit: 'SECONDS'
            }
        }
        stage ('Trigger job: Clean Devnet 1') {
            steps {
                build job: 'Waves.Exchange/Matcher/Clean Devnet', propagate: false, wait: false, parameters: [ ]
                sleep time: 10, unit: 'SECONDS'
            }
        }
        stage ('Trigger job: SC1') {
            steps {
                build job: 'Waves.Exchange/Matcher/Performance Test/SC1', propagate: false, wait: false, parameters: [
                  [$class: 'GitParameterValue', name: 'BRANCH', value: "${BRANCH}"],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "${VERSION}-${LABEL}"]
                ]
                sleep time: 10, unit: 'SECONDS'
            }
        }
        stage ('Trigger job: Clean Devnet 2') {
            steps {
                build job: 'Waves.Exchange/Matcher/Clean Devnet', propagate: false, wait: false, parameters: [ ]
                sleep time: 10, unit: 'SECONDS'
            }
        }
        stage ('Trigger job: SC2') {
            steps {
                build job: 'Waves.Exchange/Matcher/Performance Test/SC2', propagate: false, wait: false, parameters: [
                  [$class: 'GitParameterValue', name: 'BRANCH', value: "${BRANCH}"],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "${VERSION}-${LABEL}"]
                ]
                sleep time: 10, unit: 'SECONDS'
            }
        }
        stage ('Trigger job: Clean Devnet 3') {
            steps {
                build job: 'Waves.Exchange/Matcher/Clean Devnet', propagate: false, wait: false, parameters: [
                  [$class: 'GitParameterValue', name: 'BRANCH', value: "${BRANCH}"]
                ]
                sleep time: 10, unit: 'SECONDS'
            }
        }
        stage ('Trigger job: SC3') {
            steps {
                build job: 'Waves.Exchange/Matcher/Performance Test/SC3', propagate: false, wait: false, parameters: [
                  [$class: 'GitParameterValue', name: 'BRANCH', value: "${BRANCH}"],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "${VERSION}-${LABEL}"]
                ]
                sleep time: 10, unit: 'SECONDS'
            }
        }
        stage ('Trigger job: Clean Devnet 4') {
            steps {
                build job: 'Waves.Exchange/Matcher/Clean Devnet', propagate: false, wait: false, parameters: [ ]
                sleep time: 10, unit: 'SECONDS'
            }
        }
        stage ('Trigger job: SC4') {
            steps {
                build job: 'Waves.Exchange/Matcher/Performance Test/SC4', propagate: false, wait: false, parameters: [
                  [$class: 'GitParameterValue', name: 'BRANCH', value: "${BRANCH}"],
                  [$class: 'StringParameterValue', name: 'LABEL', value: "${VERSION}-${LABEL}"]
                ]
                sleep time: 10, unit: 'SECONDS'
            }
        }
    }
    post {
        always {
            script {
                SC1 = Jenkins.instance.getItemByFullName('Waves.Exchange/Matcher/Performance Test/SC1').getLastBuild().getNumber()
                SC2 = Jenkins.instance.getItemByFullName('Waves.Exchange/Matcher/Performance Test/SC2').getLastBuild().getNumber()
                SC3 = Jenkins.instance.getItemByFullName('Waves.Exchange/Matcher/Performance Test/SC3').getLastBuild().getNumber()
                SC4 = Jenkins.instance.getItemByFullName('Waves.Exchange/Matcher/Performance Test/SC4').getLastBuild().getNumber()

                sc1Build = "<a href='/job/Waves.Exchange/job/Matcher/job/Performance Test/job/SC1/${SC1}'>SC1</a>"
                sc2Build = "<a href='/job/Waves.Exchange/job/Matcher/job/Performance Test/job/SC2/${SC2}'>SC2</a>"
                sc3Build = "<a href='/job/Waves.Exchange/job/Matcher/job/Performance Test/job/SC3/${SC3}'>SC3</a>"
                sc4Build = "<a href='/job/Waves.Exchange/job/Matcher/job/Performance Test/job/SC4/${SC4}'>SC4</a>"

                currentBuild.displayName = "${VERSION}-${LABEL}"
                currentBuild.description = "${sc1Build} | ${sc2Build} | ${sc3Build} | ${sc4Build}"
            }
        }
        cleanup {
            cleanWs()
        }
    }
}
