# LSPConfig

This file contains presets for each coding language.

## C/C++

C/C++ are configured to use `ccls` as LSP server. If your project is a cmake based project, add `-Bbuild -DCMAKE_EXPORT_COMPILE_COMMANDS=1` to your configured command line.
If not, try [bear](https://github.com/rizsotto/Bear), but it is not that working though (I try it in OpenSSL project and it is not that satisfying).

The compilation database directory is set to project_root/build. So if you are using cmake, make sure to add `-Bbuild` when configuring the project.
