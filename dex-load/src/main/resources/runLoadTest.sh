sed -i "6s/.*/ ammofile: $(ls | grep requests)/" /home/yatank/loadtest/dexload.yaml
sed -i "23s/.*/ job_name: $1/" /home/yatank/loadtest/dexload.yaml
mv $(ls | grep requests) /home/yatank/loadtest/
rm -rf /home/yatank/loadtest/logs/*
echo "The performance test has been launched, but we made the decision not to print its stdout. Keep patience, it will be finished near $(date -d '+ 9 minutes')"
if [ ! "$(docker ps -q -f name=dexload)" ]; then
    if [ "$(docker ps -aq -f status=exited -f name=dexload)" ]; then
        docker rm dexload
    fi
    docker run -v /home/yatank/loadtest:/var/loadtest -v /home/ngadiyak/.ssh:/root/.ssh --rm --net host -it --entrypoint /bin/bash -d --name dexload direvius/yandex-tank:latest
fi
docker exec -i dexload yandex-tank -c dexload.yaml > /dev/null
echo "The performance has been finished"
echo "$(cat /home/yatank/loadtest/logs/lunapark/$(sudo ls /home/yatank/loadtest/logs/lunapark/)/finish_status.yaml | grep -Po -m 1 https://overload.yandex.net/[0-9]+)"
