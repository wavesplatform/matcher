sed -i "6s/.*/ ammofile: $(ls | grep requests)/" /home/yatank/loadtest/dexload.yaml
mv $(ls | grep requests) /home/yatank/loadtest/
echo "The performance test has been launched, but we made the decision not to print its stdout. Keep patience, it will be finished near $(date -d '+ 9 minutes')"
docker exec -i da469fa869cd yandex-tank -c dexload.yaml
