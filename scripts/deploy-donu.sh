#!/usr/bin/env bash

# A script to deploy `donu` to our server, aske.galois.com.

set -e

ROOT=`git rev-parse --show-toplevel`
VERSION=`git rev-parse HEAD`

HOST="aske.galois.com"
DEPLOY_USER=${DEPLOY_USER:-$USER}
USERHOST="$DEPLOY_USER@$HOST"

DEPLOY_DIR=/opt/donu/deploy
EXTRACT_DIR=target

IMAGE="galoisinc/donu"

pushd $ROOT > /dev/null

mkdir -p $EXTRACT_DIR

echo "Building donu..."
docker build \
    --build-arg DONU_EXTRACT_DIR=$EXTRACT_DIR \
    --file $ROOT/Dockerfile \
    --tag $IMAGE \
    --target donu-extract \
    .

echo "Extracting donu to $ROOT/$EXTRACT_DIR/donu..."
docker run \
    --rm \
    --mount type=bind,source=`pwd`/$EXTRACT_DIR,target=/$EXTRACT_DIR \
    $IMAGE

echo "Deploying donu to $HOST..."
STOP="systemctl stop donu.service"
SAVE="cp $DEPLOY_DIR/donu $DEPLOY_DIR/donu.prev"
START="systemctl start donu.service"

echo "Stopping donu..."
ssh $USERHOST -t "sudo $SAVE && sudo $STOP"
scp "$EXTRACT_DIR/donu" "$USERHOST:$DEPLOY_DIR/donu"

echo "Restarting donu..."
ssh $USERHOST -t "sudo $START"

NOW=`date`
ssh $USERHOST -t "echo \"$VERSION\" > $DEPLOY_DIR/VERSION"
ssh $USERHOST -t "echo \"$NOW: $VERSION $USER\" >> $DEPLOY_DIR/deploy.log"

popd > /dev/null