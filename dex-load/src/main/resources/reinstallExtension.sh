systemctl stop waves-devnet.service || true
dpkg -r waves-dex-extension-devnet || true
dpkg -P waves-dex-extension-devnet || true
dpkg -i /home/buildagent-matcher/waves-dex-extension-devnet*.deb
systemctl start waves-devnet
echo "dex extension installed"
rm -rf /home/buildagent-matcher/*
