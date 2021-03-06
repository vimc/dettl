#!/usr/bin/env bash
set -e
docker run --rm -d \
    -p 5432:5432 \
    --name dettl-pg \
    -e POSTGRES_DB=dettl_test_db \
    -e POSTGRES_PASSWORD=password \
    postgres:11
