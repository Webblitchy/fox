#!/usr/bin/env bash

cargo build -p corelib
cargo build -p fox

cd ~/kDrive/Documents/Informatique/Programmation/fox/target/debug/
if [[ -n "$2" ]]; then
    ./fox $1 ../../testPrograms/$2 && ./${2%.fox}
elif [[ -n "$1" ]]; then
    ./fox $1
else
    ./fox
fi

cd - &>/dev/null
