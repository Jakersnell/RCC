import os
import subprocess
import sys
import tarfile
import urllib.request

LLVM_VERSION = "18.1.7"
LLVM_DOWNLOAD_URL = f"https://github.com/llvm/llvm-project/releases/download/llvmorg-{LLVM_VERSION}/llvm-{LLVM_VERSION}.src.tar.xz"
LLVM_INSTALL_DIR = f"/usr/local/llvm-{LLVM_VERSION}"
CARGO_VERSION = "cargo 1.75.0 (1d8b05cdd 2023-11-20)"


def check_llvm_version():
    try:
        # Check LLVM version
        output = subprocess.check_output(['llvm-config', '--version'], stderr=subprocess.STDOUT)
        version = output.decode().strip()
        if version == LLVM_VERSION:
            print(f"LLVM version {version} is already installed.")
            return True
        else:
            print(f"LLVM version {version} is installed, but version {LLVM_VERSION} is required.")
            return False
    except subprocess.CalledProcessError:
        print("LLVM is not installed.")
        return False
    except FileNotFoundError:
        print("LLVM is not installed.")
        return False


def install_llvm():
    try:
        print(f"Downloading LLVM version {LLVM_VERSION} from {LLVM_DOWNLOAD_URL}...")
        urllib.request.urlretrieve(LLVM_DOWNLOAD_URL, f"llvm-{LLVM_VERSION}.src.tar.xz")

        print(f"Extracting LLVM version {LLVM_VERSION}...")
        with tarfile.open(f"llvm-{LLVM_VERSION}.src.tar.xz", "r:xz") as tar:
            tar.extractall()

        print(f"Installing LLVM version {LLVM_VERSION} to {LLVM_INSTALL_DIR}...")
        os.chdir(f"llvm-{LLVM_VERSION}.src")
        os.makedirs(LLVM_INSTALL_DIR, exist_ok=True)
        subprocess.check_call(["./configure", f"--prefix={LLVM_INSTALL_DIR}"])
        subprocess.check_call(["make"])
        subprocess.check_call(["make", "install"])

        print(f"LLVM version {LLVM_VERSION} installed successfully.")
    except Exception as e:
        print(f"An error occurred while installing LLVM: {e}")
        sys.exit(1)


def check_cargo_version():
    try:
        # Check Cargo version
        output = subprocess.check_output(['cargo', '--version'], stderr=subprocess.STDOUT)
        version = output.decode().strip()
        if version == CARGO_VERSION:
            print(f"Cargo version {version} is already installed.")
            return True
        else:
            print(f"Cargo version {version} is installed, but version {CARGO_VERSION} is required.")
            return False
    except subprocess.CalledProcessError:
        print("Cargo is not installed.")
        return False
    except FileNotFoundError:
        print("Cargo is not installed.")
        return False


def install_cargo():
    try:
        print("Installing Rust and Cargo...")
        # Download and install Rust (which includes Cargo)
        subprocess.check_call(
            ["curl", "--proto", "=https", "--tlsv1.2", "-sSf", "https://sh.rustup.rs", "-o", "rustup-init.sh"])
        subprocess.check_call(["sh", "rustup-init.sh", "-y"])
        os.remove("rustup-init.sh")  # Clean up the installer

        # Set the specific version of Cargo
        subprocess.check_call(["rustup", "default", "stable"])
        subprocess.check_call(["rustup", "update", "stable"])
        subprocess.check_call(["rustup", "override", "set", "1.75.0"])

        print("Cargo (and Rust) installed successfully.")
    except subprocess.CalledProcessError as e:
        print(f"An error occurred while installing Cargo: {e}")
        sys.exit(1)


def run_cargo_build():
    try:
        print("Running Cargo build...")
        subprocess.check_call(["cargo", "build", "--release"])
        print("Cargo build completed successfully.")
    except subprocess.CalledProcessError as e:
        print(f"An error occurred during the Cargo build: {e}")
        sys.exit(1)


def copy_to_usr_bin():
    try:
        print("Copying to usr/bin...")
        subprocess.check_call(["cp", "./target/release/microc", "/usr/bin"])
        print("Successfully copied to '/usr/bin'")
    except subprocess.CalledProcessError as e:
        print(f"An error occured while copying to '/usr/bin': {e}")
        sys.exit(1)


def main():
    try:
        if not check_llvm_version():
            install_llvm()

        if not check_cargo_version():
            install_cargo()

        run_cargo_build()

        copy_to_usr_bin()

    except Exception as e:
        print(f"A fatal error occurred: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
