#!/bin/sh

trap 'exit 101' SIGUSR1
trap 'exit 102' SIGUSR2
sleep 30
