sed -i "6s/.*/ ammofile: $(ls | grep requests)/" /home/yatank/loadtest/dexload.yaml
mv $(ls | grep requests) /home/yatank/loadtest/
rm -rf /home/yatank/loadtest/logs/*
echo "The performance test has been launched, but we made the decision not to print its stdout. Keep patience, it will be finished near $(date -d '+ 9 minutes')"
docker exec -i da469fa869cd yandex-tank -c dexload.yaml > /dev/null
echo "The performance has been finished"
echo "$(cat /home/yatank/loadtest/logs/lunapark/$(sudo ls /home/yatank/loadtest/logs/lunapark/)finish_status.yaml | grep -Po -m 1 https://overload.yandex.net/[0-9]+)"
