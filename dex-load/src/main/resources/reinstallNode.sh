systemctl stop waves-devnet.service || true
rm -rf /var/lib/waves-devnet/data || true
dpkg -r waves-devnet || true
dpkg -P waves-devnet || true
dpkg -i /home/buildagent-matcher/waves-devnet.deb
systemctl start waves-devnet
echo "node installed and started"
rm -rf /home/buildagent-matcher/*