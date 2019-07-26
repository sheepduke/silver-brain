#!/bin/bash

APP_ROOT=$HOME/.silver-brain/

function panic() {
    echo "Panic: $1"
    exit 1
}

cd backend/ || panic "Cannot enter backend directory"
ros build ./silver-brain.ros || panic "Cannot build system"
mv ./roswell/silver-brain ~/.roswell/bin/ || panic "Cannot install executable"
cd ..

# cd web-ui/ || panic "Cannot enter web-ui directory"
# npm install && npm run build || panic "Failed to package Web UI."
# mkdir $APP_ROOT
# cp -r dist/* $APP_ROOT
# cd ..

cp -rv emacs $APP_ROOT || panic "Cannot copy Emacs client"
cd ..

echo "----------------------------------------------------------------------"
echo "Installation finished."
