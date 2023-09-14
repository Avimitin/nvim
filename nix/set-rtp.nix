{ writeText }:

name: drvs:
let
  runtimePath = with builtins;
    concatStringsSep "," (map toString drvs);
in
writeText "set-rtp-for-${name}.lua" ''
  vim.opt.rtp:prepend("${runtimePath}")
''
