# Donu

## Simple Deployment

Stop `donu`

```sh
$ ssh aske.galois.com
aske:~$ sudo systemctl stop donu
```

Copy `donu` to `aske.galois.com:/opt/donu/deploy/donu`:

```sh
$ cabal exec -- scp `which donu` aske.galois.com:/opt/donu/deploy/donu
```

Copy `modelRepo` to `aske.galois.com:/opt/donu/deploy/modelRepo`

```sh
$ scp -r modelRepo aske.galois.com:/opt/donu/deploy
```

Restart `donu`

```sh
$ ssh aske.galois.com
aske:~$ sudo systemctl start donu
```

## `AlgebraicJulia` deployment

The parts of `AlgebraicJulia` we use are exposed by simple server that runs in a detached Docker container. Restarting `donu` in the above manner does not require restarting this container.

To take it down anyway, find the container's ID by executing `sudo docker ps` (it should be the only container running), then execute `sudo docker kill <ID>`.

To restart it, execute:
```sh
sudo docker run --detach --publish 8001:8001 galoisinc/algebraicjulia
```

To view the server's logs, find the container's ID and execute `sudo docker logs <ID>`.

You shouldn't need to build the image that runs this server, but running `scripts/run-algebraic-julia.sh --build` will do so, and will run the server for local testing. Do not use this script to run the server in production.

## Details

nginx config is at `aske.galois.com:/etc/nginx/donu.conf`

certs are at `aske.galois.com:/opt/donu/cert`

systemd config for `donu` is here `aske.galois.com:/lib/systemd/system/donu.service`