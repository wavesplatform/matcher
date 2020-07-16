#!/bin/bash

mkdir -p /var/lib/waves-dex/log # link to /var/log/waves-dex/ was created in dockerfile

USER_PATH="/var/lib/waves-dex"
CONFIG_PATH="${USER_PATH}/config"
SOURCES_PATH="/usr/share/waves-dex"
LOG_PATH="/var/log/waves-dex"

BASE_CONFIG="${SOURCES_PATH}/conf/dex.conf"
CUSTOM_CONFIG="${CONFIG_PATH}/local.conf"
CUSTOM_LOG_CONFIG="${CONFIG_PATH}/logback.xml"

LOG_FILE="${LOG_PATH}/dex.log"

echo "Matcher Base Settings '${BASE_CONFIG}':" | tee -a ${LOG_FILE}
cat ${BASE_CONFIG} | tee -a ${LOG_FILE}
printf "\n" | tee -a ${LOG_FILE}

if [ ! -f ${CUSTOM_CONFIG} ] ; then
	printf "Custom Matcher Settings '${CUSTOM_CONFIG}' not found\n" | tee -a ${LOG_FILE}
else
	echo "Found Custom Settings '${CUSTOM_CONFIG}', using it:" | tee -a ${LOG_FILE}
	cat ${CUSTOM_CONFIG} | tee -a ${LOG_FILE}
	printf "\n\n" | tee -a ${LOG_FILE}
fi

JAVA_OPTS="${JAVA_OPTS} -Dwaves.dex.root-directory=${USER_PATH}"

echo "Matcher is starting..." |  tee -a ${LOG_FILE}
echo "MATCHER_HEAP_SIZE='${MATCHER_HEAP_SIZE}'" |  tee -a ${LOG_FILE}
printf "JAVA_OPTS='${JAVA_OPTS}'\n\n" |  tee -a ${LOG_FILE}

${SOURCES_PATH}/bin/waves-dex \
  -J-Xmx${MATCHER_HEAP_SIZE} \
  -Dlogback.configurationFile=${CUSTOM_LOG_CONFIG} \
  -Dconfig.override_with_env_vars=true \
  ${JAVA_OPTS} \
  -main com.wavesplatform.dex.Application ${BASE_CONFIG}
