#!/bin/bash

trap 'kill -TERM $PID' TERM INT

$WAVES_DEX_CONFIGPATH=${$WAVES_DEX_CONFIGPATH:-/opt/waves-dex/default.conf}
echo Config file: $WAVES_DEX_CONFIGPATH
echo Options: $WAVES_DEX_OPTS
/opt/waves-dex/bin/dex $WAVES_DEX_OPTS -main com.wavesplatform.dex.Application $WAVES_DEX_CONFIGPATH &
PID=$!
wait $PID
trap - TERM INT
wait $PID
EXIT_STATUS=$?
