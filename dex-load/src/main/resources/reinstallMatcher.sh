systemctl stop waves-dex.service || true
dpkg -P waves-dex || true
dpkg -i /home/buildagent-matcher/waves-dex*.deb
systemctl start waves-dex
rm -rf /home/buildagent-matcher/*
