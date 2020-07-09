#!/bin/bash

trap 'kill -TERM $PID' TERM INT

USER_PATH="/var/lib/waves-dex"
SOURCES_PATH="/usr/share/waves-dex"

BASE_CONFIG=${SOURCES_PATH}/conf/dex.conf
CUSTOM_CONFIG="${USER_PATH}/runtime/local.conf"

echo "Base settings '${BASE_CONFIG}':"
cat ${BASE_CONFIG}
echo; echo

if [ ! -f ${CUSTOM_CONFIG} ] ; then
	echo "Custom settings '${CUSTOM_CONFIG}' not found"; echo
else
	echo "Found custom settings '${CUSTOM_CONFIG}':"
	cat ${CUSTOM_CONFIG}; echo; echo
fi

${SOURCES_PATH}/bin/waves-dex \
  -Dlogback.configurationFile=${USER_PATH}/runtime/logback.xml \
  -main com.wavesplatform.dex.Application ${BASE_CONFIG}

PID=$!
echo "PID: ${PID}"
wait ${PID}

trap - TERM INT
wait ${PID}
EXIT_STATUS=$?
echo "Exit status: ${EXIT_STATUS}"
