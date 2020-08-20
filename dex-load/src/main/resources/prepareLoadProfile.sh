echo "success"

mv $(ls | grep requests) /home/yatank/loadtest/
sed -i "6s/.*/ammofile: $(ls | grep requests)/" /home/yatank/loadtest/dexload.yaml
