#!/bin/bash
# Overriding to be able to switch configs

trap 'kill -TERM $PID' TERM INT

echo "Starting process..." >> ${WAVES_NODE_DETAILED_LOG_PATH}
echo Config file: ${WAVES_NODE_CONFIGPATH} >> ${WAVES_NODE_DETAILED_LOG_PATH}
echo Options: ${WAVES_OPTS} >> ${WAVES_NODE_DETAILED_LOG_PATH}

find /opt/waves -type f >> ${WAVES_NODE_DETAILED_LOG_PATH}

java ${WAVES_OPTS} -cp "/usr/share/waves/lib/*:/opt/waves/lib*" com.wavesplatform.Application ${WAVES_NODE_CONFIGPATH} &>> ${WAVES_NODE_DETAILED_LOG_PATH} &

PID=$!
echo "PID: ${PID}" >> ${WAVES_NODE_DETAILED_LOG_PATH}
wait ${PID}

trap - TERM INT
wait ${PID}
EXIT_STATUS=$?
echo "Exit status: ${EXIT_STATUS}" >> ${WAVES_NODE_DETAILED_LOG_PATH}
