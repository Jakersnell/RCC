#!/bin/bash

# Function to check if python3 is installed
check_python3() {
    if command -v python3 &> /dev/null
    then
        echo "Python3 is already installed."
    else
        echo "Python3 is not installed. Installing..."
        install_python3
    fi
}

# Function to install python3
install_python3() {
    if command -v brew &> /dev/null
    then
        brew install python
    else
        echo "Homebrew is not installed. Please install Homebrew first."
        exit 1
    fi
}

# Function to run build.py with python3
run_build() {
    echo "Running build.py with python3..."
    python3 ./build.py
}

# Main script execution
check_python3
run_build
