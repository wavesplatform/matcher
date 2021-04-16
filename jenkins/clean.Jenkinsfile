def cleanStateAndRestart (host, service) {
    return {
         sshagent (credentials: ['buildagent-matcher']) {
             sh "ssh -o StrictHostKeyChecking=no -l buildagent-matcher ${host} hostname"

             sh "ssh -q buildagent-matcher@${host} sudo systemctl stop ${service}"
             sh "ssh -q buildagent-matcher@${host} sudo rm -rf /var/lib/${service}/data"
             sh "ssh -q buildagent-matcher@${host} sudo rm -rf /var/lib/${service}/blockchain-updates"
             sh "ssh -q buildagent-matcher@${host} sudo systemctl start ${service}"
         }
    }
}

pipeline {
    agent {
        label 'buildagent-matcher-load-test'
    }
    options {
        ansiColor('xterm')
        timeout(time: 15, unit: 'MINUTES')
        timestamps()
        disableConcurrentBuilds()
    }
    stages {
        stage('Clean state and restart nodes') {
            parallel {
                 stage("Node 2:") {
                    steps {
                        script {
                            cleanStateAndRestart("${NODE2}", "waves-devnet").call()
                        }
                    }
                 }
                 stage("Node 3:") {
                    steps {
                        script {
                            cleanStateAndRestart("${NODE3}", "waves-devnet").call()
                        }
                    }
                 }
                 stage("Node 4:") {
                    steps {
                        script {
                            cleanStateAndRestart("${NODE4}", "waves-devnet").call()
                        }
                    }
                 }
            }
        }
        stage('Clean state and restart matcher') {
            steps {
                sshagent (credentials: ['buildagent-matcher']) {
                    sh "ssh -o StrictHostKeyChecking=no -l buildagent-matcher ${MATCHER} hostname"
                    sh "ssh -q buildagent-matcher@${MATCHER} sudo systemctl stop waves-dex"
                    sh "ssh -q buildagent-matcher@${MATCHER} sudo rm -rf /var/lib/waves-dex/data/*"
                    sh 'ssh -q buildagent-matcher@${MATCHER} sudo sed -i \\"5s/.*/ topic = $(cat /proc/sys/kernel/random/uuid) /\\"  /etc/waves-dex/queue.conf'
                    sh "ssh -q buildagent-matcher@${MATCHER} sudo systemctl start waves-dex"
                }
            }
        }
    }
}
