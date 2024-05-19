# 2024年05月19日
emacs经常报这个错误：
```text
Warning (comp): libgccjit.so: error: error invoking gcc driver
```
Google了下，https://github.com/d12frosted/homebrew-emacs-plus/issues/323，让安装一个包：
```sh
brew uninstall gcc libgccjit
brew install gcc libgccjit
```
安装了glibccjit之后，又报了这个错误：
```text
Library not loaded: @rpath/libgcc_s.1.1.dylib
Referenced from: <C93A88B4-A5C5-3C6F-9F9B-CB9D58FF003E> /usr/local/Cellar/libgccjit/14.1.0/lib/gcc/current/libgccjit.0.dylib
Reason: tried: '/usr/local/Cellar/libgccjit/14.1.0/lib/gcc/current/libgcc_s.1.1.dylib' (no such file), '/usr/local/lib/libgcc_s.1.1.dylib' (no such file), '/usr/lib/libgcc_s.1.1.dylib' (no such file, not in dyld cache)
(terminated at launch; ignore backtrace)
```
之后在这个帖子找到了解决办法：
```sh
brew install gcc
install_name_tool -add_rpath "$(brew --prefix gcc)/lib/gcc/current" "$(brew --prefix libgccjit)/lib/gcc/current/libgccjit.0.dylib"
```
