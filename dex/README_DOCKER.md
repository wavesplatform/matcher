## 7. Matcher Server & Matcher Node in Docker

## About the Images

Matcher's team provides you with 2 docker images:
  1. matcher-node;
  2. matcher-server

These images are focused on fast and convenient deployment of Waves Node with Matcher Extension and Matcher Server.

First one is the Waves Node with pre-installed Matcher Extension (Matcher Node). It is based on the official [wavesnode](https://hub.docker.com/r/wavesplatform/wavesnode) image, so [it's configurations](https://github.com/wavesplatform/Waves/tree/master/docker#configuration-options) can be applied for the matcher-node image as well. If you're going to use volumes, please, pay attention to [data management](https://github.com/wavesplatform/Waves/tree/master/docker#managing-data) section.

Matcher Extension in matcher-node image is already configured. Extension uses port 6887 for communication with Matcher Server via gRPC protocol. You can override Matcher Extension [default settings](https://github.com/wavesplatform/matcher/blob/master/waves-ext/src/main/resources/application.conf) in local.conf, which is located in `/docker/waves/waves-conf/` on your host machine (if you are using volume):

```
# gRPC integration settings for Waves Node
waves.dex.grpc.integration {

    # Extension's host
    host = localhost

    # Extension's port
    port = 6887
}
```

Second one is the official Matcher Server image. In order to start container, you need to specify the following settings in local.conf, which is located in `/docker/waves-dex/runtime/` on your host machine (if you are using volume):

```
waves.dex {

  # Current network. Default is Mainnet (W), should be the same as at Matcher Node
  address-scheme-character = "W"

  # Matcher account storage settings
  account-storage {
   
    type = "not-specified" # "in-mem" or "encrypted-file"
    in-mem.seed-in-base64 = ""
    
    encrypted-file.password = "password-for-file"
  }

  # Matcher Node coordinates
  waves-blockchain-client.grpc.target = "matcher-node:6887"
}
```

## Running Docker images

Successful Node-Matcher interaction requires that they can see each other. Therefore, you should use container linking or a private network.
The order in which containers are started matters! Matcher Node should be started first, then Matcher Server. 

We will use container linking. Although this is the legacy practice, it is a bit simpler (you don't need to set up network before containers running).

Also, Matcher Node and Matcher Server can be connected via network. In this case use network alias for the Matcher Node container and `waves.dex.waves-blockchain-client.grpc.target` setting. 

#### Running Matcher Node
```
docker run -v /docker/waves/waves-data:/var/lib/waves -v /docker/waves/waves-config:/etc/waves -p 6869:6869 -p 6862:6862 -e JAVA_OPTS="-Dwaves.rest-api.enable=yes -Dwaves.rest-api.bind-address=0.0.0.0 -Dwaves.wallet.password=myWalletSuperPassword" -e WAVES_NETWORK=stagenet --name matcher-node -ti wavesplatform/matcher-node
```

Worth to note here that we specified container name by means of `--name matcher-node`. This name will be used as a link to Matcher Node during start of Matcher Server and also in Matcher Server setting `waves.dex.waves-blockchain-client.grpc.target`.

#### Running Matcher Server
```
docker run -v /docker/waves-dex:/var/lib/waves-dex --link matcher-node:matcher-node -p 6886:6886 wavesplatform/matcher-server:latest
```

Here we specified link to Matcher Node container `--link matcher-node:matcher-node` and the mapping between directory on your host machine `/docker/waves-dex/` and inner container directory `/var/lib/waves-dex`. 

Directory `/docker/waves-dex/data` will be used for storing Matcher data. You can put your own Matcher state here.

If you are using encrypted file as a Matcher account storage, please, put `account.dat` file in this directory and do not forget to specify appropriate `waves.dex.account-storage` settings in `local.conf`.

Directory `/docker/waves-dex/runtime/` is used to provide all necessary runtime configurations of the Matcher Server. Note that:
  1. `/docker/waves-dex/runtime/local.conf` should be provided (see its mandatory content above);
  2. you can place logback.xml with your own log settings in `/docker/waves-dex/runtime/` or use our [log management file](https://github.com/wavesplatform/matcher/blob/master/dex/src/package/doc/logback.xml).

### Managing data

As well as at Waves Node, we use dedicated `waves-dex:waves-dex` user with predefined uid/gid `113/116` to launch the container. As such, either change permissions of the created directories or change their owner:

```
sudo chmod -R 777 /docker/waves-dex
```
or
```
sudo chown -R 113:116 /docker/waves-dex  <-- prefered
```