#!/bin/bash

APP_ROOT=$HOME/.silver-brain/

function panic() {
    echo "Panic: $1"
    exit 1
}

cd backend/ || panic "Cannot enter backend directory."
ros install ./silver-brain.asd || panic "Cannot install system."
ros install ./roswell/silver-brain.ros || panic "Cannot install executable."
cd ..

cd web-ui/ || panic "Cannot enter web-ui directory"
npm install && npm run build || panic "Failed to package Web UI."
mkdir $APP_ROOT
cp -r dist/* $APP_ROOT

echo "----------------------------------------------------------------------"
echo "Installation finished."
