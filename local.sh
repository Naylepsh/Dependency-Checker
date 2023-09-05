#!/bin/bash

dbmate up
sbt docker
docker run -it -v $PWD/db.sqlite:/app/db.sqlite --env-file ./.env ganyu/ganyu

