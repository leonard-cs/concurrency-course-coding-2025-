# Concurrency Course Code

## Setup (macOS)
Install LLVM 18 using Homebrew:
```bash
brew install llvm@18
```

Add the following lines to your ~/.zshrc (or ~/.bashrc if using Bash):
```bash
export PATH="/opt/homebrew/opt/llvm@18/bin:$PATH"

export LDFLAGS="-L/opt/homebrew/opt/llvm@18/lib"
export CPPFLAGS="-I/opt/homebrew/opt/llvm@18/include"
export CMAKE_PREFIX_PATH="/opt/homebrew/opt/llvm@18"
```
### Project Modifications
In your `check_build` script: change `clang++-18` to `clang++`.\
In your `fix_format` script: change `clang-format-18` to `clang-format`.\
In your `CMakeLists.txt`, remove the following line: 
```bash
add_link_options(-latomic)
```
This option is not required on macOS and may cause linking errors with LLVM 18.