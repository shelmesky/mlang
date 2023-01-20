#!/bin/bash

export PATH=/home/roy/Downloads/x86_64-linux-musl-cross/bin:$PATH
x86_64-linux-musl-gcc -no-pie -static $1
