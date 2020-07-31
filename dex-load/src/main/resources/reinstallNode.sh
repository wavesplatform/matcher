sudo su
systemctl stop waves-devnet.service
rm -rf /var/lib/waves-devnet/data
dpkg -r waves-devnet
dpkg -P waves-devnet
dpkg -i /home/buildagent-matcher/*.deb
systemctl start waves-devnet