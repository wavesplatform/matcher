sudo su
systemctl stop waves-dex.service
rm -rf /var/lib/waves-dex/data
dpkg -r waves-dex
dpkg -P waves-dex
dpkg -i /home/buildagent-matcher/*dex*.deb
systemctl start waves-dex