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

## Details

nginx config is at `aske.galois.com:/etc/nginx/donu.conf`

certs are at `aske.galois.com:/opt/donu/cert`

systemd config for `donu` is here `aske.galois.com:/lib/systemd/system/donu.service`