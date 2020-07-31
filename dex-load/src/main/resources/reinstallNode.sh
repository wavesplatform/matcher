sudo su
sudo systemctl stop waves-devnet.service
sudo rm -rf /var/lib/waves-devnet/data
sudo dpkg -r waves-devnet
sudo dpkg -P waves-devnet
sudo dpkg -i /home/buildagent-matcher/*.deb
sudo systemctl start waves-devnet