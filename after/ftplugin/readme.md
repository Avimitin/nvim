# LSPConfig

This file contains presets for each coding language.

## C/C++

C/C++ are configured to use `clangd` as LSP server. If your project is a cmake based project, add `-DCMAKE_EXPORT_COMPILE_COMMANDS=1` to your configured command line.
If not, try [bear](https://github.com/rizsotto/Bear), but it is not that working though (I try it in OpenSSL project and it is not that satisfying).

### Common error

- clangd: -32602: trying to get AST for non-added document

This might related with that clangd is not happy with some compile option, like `-march`.
To fix this, add a `.clangd` file in project root and add the below lines:

```clangd
CompileFlags:
  Remove: [-march=*,-mabi=*,-mcpu=*,-fno-lifetime-dse]
```
