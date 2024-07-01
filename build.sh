#!/bin/bash

# This build script is a precursor to run the build.py build script

check_python3() {
    check_homebrew
    if command -v python3 &> /dev/null
    then
        echo "Python3 is already installed."
    else
        echo "Python3 is not installed. Installing..."
        install_python3
    fi
}


check_homebrew() {
  if command -v brew &> /dev/null
  then
    echo "Homebrew is already installed."
  else
    echo "Homebrew is not installed."
    install_homebrew
  fi
}

install_homebrew() {
  echo "Installing Homebrew..."
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
}

install_python3() {
    echo "Installing Python3..."
    if command -v brew &> /dev/null
    then
        brew install python
    else
        echo "Homebrew is not installed. Please install homebrew before installing."
        exit 1
    fi
}

run_build() {
    echo "Running build.py with python3..."
    python3 ./build.py
}

# Main script execution
check_python3
run_build


# Check if python exists
#   If exists run build.py
#   If not exists check if homebrew exists
#     If exists install python3
#       Run build.py
#     If not exists install homebrew with curl
#       If failure exit
#     If success install python3
#       Run build.py