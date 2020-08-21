sed -i "6s/.*/ ammofile: $(ls | grep requests)/" /home/yatank/loadtest/dexload.yaml
mv $(ls | grep requests) /home/yatank/loadtest/
docker exec -i da469fa869cd yandex-tank -c dexload.yaml