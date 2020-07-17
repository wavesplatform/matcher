## 7. Matcher Server & Matcher Node in Docker

### About the Images

Matcher's team provides you with 2 docker images:
  1. matcher-node
  2. matcher-server

These images are focused on fast and convenient deployment of Waves Node with Matcher Extension and Matcher Server.

First one is the Waves Node with pre-installed Matcher Extension (Matcher Node). It is based on the official [wavesnode](https://hub.docker.com/r/wavesplatform/wavesnode) image, so [it's configurations](https://github.com/wavesplatform/Waves/tree/master/docker#configuration-options) can be applied for the matcher-node image as well. If you're going to use volumes, please, pay attention to [data management](https://github.com/wavesplatform/Waves/tree/master/docker#managing-data) section.

Matcher Extension in matcher-node image is already configured. Extension uses port 6887 for communication with Matcher Server via gRPC protocol. You can override Matcher Extension [default settings](https://github.com/wavesplatform/matcher/blob/master/waves-ext/src/main/resources/application.conf) in `local.conf`, which is located in `/docker/matcher-node/config` on your host machine (if you are using volume):

```
# gRPC integration settings for Waves Node
waves.dex.grpc.integration {

    # Extension's host
    host = localhost

    # Extension's port
    port = 6887
}
```

Second one is the official Matcher Server image. In order to start container, you need to specify the following settings in `local.conf`, which is located in `/docker/matcher-server/config` on your host machine (if you are using volume):

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

### Running Docker Images

Successful Node-Matcher interaction requires that they can see each other. Therefore, you should use container linking or a private network.
The order in which containers are started matters! Matcher Node should be started first, then Matcher Server. 

We will use container linking. Although this is the legacy practice, it is a bit simpler (you don't need to set up network before containers running).

Also, Matcher Node and Matcher Server can be connected via network. In this case use network alias for the Matcher Node container and `waves.dex.waves-blockchain-client.grpc.target` setting. 

#### Running Matcher Node
```
docker run \
-v /docker/matcher-node/data:/var/lib/waves \
-v /docker/matcher-node/config:/etc/waves \
-p 6869:6869 -p 6862:6862 \
-e JAVA_OPTS="-Dwaves.rest-api.enable=yes -Dwaves.rest-api.bind-address=0.0.0.0 -Dwaves.wallet.password=myWalletSuperPassword -Dwaves.dex.grpc.integration.host=0.0.0.0" \
-e WAVES_HEAP_SIZE=4g \
-e WAVES_LOG_LEVEL=DEBUG \
-e WAVES_NETWORK=mainnet \
--name matcher-node \
-ti wavesplatform/matcher-node:latest
```

Worth to note here that we specified container name by means of `--name matcher-node`. This name will be used as a link to Matcher Node during start of Matcher Server and also in Matcher Server setting `waves.dex.waves-blockchain-client.grpc.target`.

#### Running Matcher Server
```
docker run \
-v /docker/matcher-server:/var/lib/waves-dex \
-p 6886:6886 \
-e MATCHER_HEAP_SIZE=4g \
-e JAVA_OPTS="-Dwaves.dex.rest-api.api-key-hash=MyRestApiBase58EncodedKeyHash" \
--link matcher-node:matcher-node \
--name matcher-server \
wavesplatform/matcher-server:latest
```

Here we specified link to Matcher Node container `--link matcher-node:matcher-node` and the mapping between directory on your host machine `/docker/matcher-server` and inner container directory `/var/lib/waves-dex`. 

Directory `/docker/matcher-server/data` will be used for storing Matcher Server data. If you want to move from JAR/DEB installation to the Docker, you can put your own Matcher Server state here.

If you are using encrypted file as a Matcher Server account storage, please, put `account.dat` file in this directory and do not forget to specify `waves.dex.account-storage.type = "encrypted-file"` settings in `local.conf`.

Directory `/docker/matcher-server/config` is used to provide all necessary runtime configurations of the Matcher Server. Note that:
  1. `/docker/matcher-server/config/local.conf` should be provided (see its mandatory content above)
  2. you can place `logback.xml` with your own log settings in `/docker/matcher-server/config` or use our [log management file](https://github.com/wavesplatform/matcher/blob/master/dex/src/package/doc/logback.xml). If you want to store logs on your host machine with our `logback.xml`, change property `logback.file.enabled` to `true`.

### Managing Data

We recommend storing blockchain state and Matcher Node configuration as well as Matcher Server state and configurations on the host side. Therefore, consider using Docker volumes for mapping host directories to directories inside the container.

**Matcher Node Example:**

1. Create directories to store Matcher Node data and configurations:

```
mkdir -p /docker/matcher-node/{data,config}
```

Once Matcher Node container is launched, it will create:

- three subdirectories in `/docker/matcher-node/data`:
```
/docker/matcher-node/data/log    - Matcher Node logs
/docker/matcher-node/data/data   - Matcher Node blockchain state
/docker/matcher-node/data/wallet - Matcher Node wallet data
```
- `/docker/matcher-node/config/waves.conf` - default Matcher Node config.


2. If you already have Matcher Node configuration and/or data - place it in the corresponding directories.


3. *Configure access permissions*. For Matcher Node we use `waves:waves` user with predefined uid/gid `143/143` to launch the container. So, either change permissions of the created directories or change their owner:

```
sudo chmod -R 777 /docker/matcher-node
```
or
```
sudo chown -R 143:143 /docker/matcher-node  <-- prefered
```

4. Add the appropriate arguments `-v` to ```docker run``` command: 
```
docker run \
-v /docker/matcher-node/data:/var/lib/waves \
-v /docker/matcher-node/config:/etc/waves \
-p 6869:6869 -p 6862:6862 \
-e WAVES_WALLET_PASSWORD=myWalletSuperPassword \
--name matcher-node \
-ti wavesplatform/matcher-node:latest
```

**Matcher Server Example:**

1. Create directories to store Matcher Server data and runtime configurations:

```
mkdir -p /docker/matcher-server/config
```

Once Matcher Server container is launched, it will create 2 subdirectories in `/docker/matcher-server`:
 ```
 /docker/matcher-server/log  - Matcher Server logs
 /docker/matcher-server/data - Matcher Server state
```

2. If you already have Matcher Server configuration and/or data - place it in the following directories:
  - state (`data` directory) and `account.dat` should be placed in `/docker/matcher-server` 
  - configuration file `local.conf` and log management file `logback.xml` should be placed in `/docker/matcher-server/config` 

3. *Configure access permissions*. For Matcher Server we use `waves-dex:waves-dex` user with predefined uid/gid `113/116` to launch the container. So, either change permissions of the created directories or change their owner:

```
sudo chmod -R 777 /docker/matcher-server
```
or
```
sudo chown -R 113:116 /docker/matcher-server  <-- prefered
```

4. Add the appropriate arguments `-v` to ```docker run``` command: 
```
docker run \
-v /docker/matcher-server:/var/lib/waves-dex \
-p 6886:6886 \
--link matcher-node:matcher-node \
--name matcher-server \
wavesplatform/matcher-server:latest
```
