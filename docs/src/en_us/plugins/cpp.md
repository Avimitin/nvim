# c/cpp

C/CPP is configured to use clangd as default LSP server.

## project

The clangd respect your cmake settings.
You will need to provide the compile_commands.json file for clangd to identify
your project correctly.

```console
$ cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES
$ ln -s Debug/compile_commands.json .
```

## Keymap

You can use the keymap provided by LSP.

## Makefile

If you are writing C and using the Makefile, you can use the `:Dispatch` or `:Make`
command to easily build and debug your code.

## Resources

* Clangd official site: <https://clangd.llvm.org/>
* CMake official site: <https://cmake.org/>
