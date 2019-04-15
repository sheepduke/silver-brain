#!/bin/bash

function panic() {
    echo "Panic: $1"
    exit 1
}

mkdir build
cd build || panic "Cannot enter to build directory"

if [ -d roswell ]; then
    echo "Roswell source exists. Skip git command."
else
    git clone https://github.com/roswell/roswell.git
fi

cd roswell && ./bootstrap && ./configure --prefix ${HOME}/.local/
make && make install
~/.local/bin/ros
echo "----------------------------------------------------------------------"
echo "Installation finished."
