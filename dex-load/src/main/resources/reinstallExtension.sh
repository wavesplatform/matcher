systemctl stop waves-devnet.service || true
dpkg -P waves-dex-extension-devnet || true
dpkg -i /home/buildagent-matcher/waves-dex-extension-devnet*.deb
systemctl start waves-devnet
rm -rf /home/buildagent-matcher/*
