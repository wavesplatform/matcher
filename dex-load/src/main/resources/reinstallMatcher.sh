sudo systemctl stop waves-dex.service
sudo rm -rf /var/lib/waves-dex/data
sudo dpkg -r waves-dex
sudo dpkg -P waves-dex
sudo dpkg -i /home/buildagent-matcher/*dex*.deb
sudo systemctl start waves-dex