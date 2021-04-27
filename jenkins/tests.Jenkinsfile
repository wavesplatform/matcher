pipeline {
  agent {
    label 'buildagent-matcher'
  }
  environment {
    SBT_HOME = tool name: 'sbt-1.2.6', type: 'org.jvnet.hudson.plugins.SbtPluginBuilder$SbtInstallation'
    SBT_OPTS = '-Xmx2g -XX:ReservedCodeCacheSize=128m -XX:+CMSClassUnloadingEnabled'
    PATH = "${env.SBT_HOME}/bin:${env.PATH}"
    withoutOrigin = params.BRANCH.replaceAll('origin/', '')
  }
  stages {
    stage('Clean') {
      steps {
        git 'https://github.com/wavesplatform/matcher'
        sh 'git fetch --tags'
        sh 'docker system prune -f || true'
        sh 'docker rmi `docker images --format "{{.Repository}}:{{.Tag}}" | grep "wavesplatform"` || true'
        sh "git fetch origin ${params.BRANCH} && git checkout ${withoutOrigin}"
        sh 'find ~/.sbt/1.0/staging/*/waves -type d -name target | xargs -I{} rm -rf {}'
        sh 'find . -type d -name "target" | xargs -I{} rm -rf {}'
        sh 'sbt "cleanAll"'
      }
    }
    stage('Build Docker') {
      steps {
        sh 'sbt "dex-it/docker"'
      }
    }
    stage ('Push Images') {
      steps {
        sh '''
              IMAGES=$(docker image ls | grep 'wavesplatform/' | awk '{gsub("wavesplatform/", "", $1); print $1 ":" $2}')
              for i in $IMAGES
              do
                  docker tag "wavesplatform/"$i "registry.wvservices.com/waves/dex/"$i
                  docker push "registry.wvservices.com/waves/dex/"$i
              done
        '''
        script {
          currentBuild.displayName = "${withoutOrigin}"
        }
      }
    }
  }
}
