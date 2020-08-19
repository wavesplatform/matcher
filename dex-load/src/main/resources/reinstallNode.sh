systemctl stop waves-devnet.service || true
rm -rf /var/lib/waves-devnet/data || true
dpkg -P waves-dex-extension-devnet || true
dpkg -P waves-devnet || true
dpkg -i /home/buildagent-matcher/waves-devnet*.deb
systemctl start waves-devnet
rm -rf /home/buildagent-matcher/*
