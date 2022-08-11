systemctl stop waves-dex.service || true
dpkg -P waves-dex || true
dpkg -i /home/buildagent-matcher/waves-dex*.deb
systemctl start waves-dex
rm -rf /home/buildagent-matcher/*
iptables -I INPUT -p tcp -m tcp --dport 6886 -j ACCEPT