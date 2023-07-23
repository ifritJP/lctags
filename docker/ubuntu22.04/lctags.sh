#! /bin/bash

DIR=$(pwd)

docker exec -i lctags_env bash -c "cd ${DIR}; lctags $@"
