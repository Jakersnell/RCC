import subprocess
import sys
from shutil import which

LLVM_VERSION = "18"
CARGO_VERSION = "cargo 1.75.0 (1d8b05cdd 2023-11-20)"


# 1. check that llvm-config exists and version is usable
# 2. if llvm exists continue to step 4
# 3. if llvm not exists install llvm@18 from homebrew
# 4. check that rustc & cargo of correct version exists
# 5. if correct cargo version exists skip to step 7
# 6. if cargo version incorrect install correct version
# 7. run cargo build
# 8. copy to usr/bin


def check_llvm():
    if which("llvm-config") is not None:
        print("llvm is installed.")
        return True
    else:
        print("llc is not installed.")
        return False


def install_llvm():
    try:
        print(f"Downloading LLVM version {LLVM_VERSION}...")
        subprocess.check_output(['brew', 'install', f'llvm@{LLVM_VERSION}'])
    except Exception as e:
        print(f"An error occurred while installing LLVM: {e}")
        sys.exit(1)


def check_rust():
    if which("cargo") is not None:
        print(f"Cargo is already installed.")
        return True
    else:
        print(f"Cargo is not installed.")
        return False


def install_rust():
    try:
        print("Installing Cargo...")
        subprocess.check_output(['brew', 'install', 'cargo'])
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
        print(f"An error occurred while copying to '/usr/bin': {e}")
        sys.exit(1)


def main():
    try:
        if not check_llvm():
            install_llvm()

        if not check_rust():
            install_rust()

        run_cargo_build()

        copy_to_usr_bin()

    except Exception as e:
        print(f"A fatal error occurred: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
