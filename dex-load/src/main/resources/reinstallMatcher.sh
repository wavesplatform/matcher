systemctl stop waves-dex.service || true
rm -rf /var/lib/waves-dex/data || true
dpkg -r waves-dex || true
dpkg -P waves-dex || true
dpkg -i /home/buildagent-matcher/waves-dex*.deb
systemctl start waves-dex
echo "dex installed and started"
rm -rf /home/buildagent-matcher/*