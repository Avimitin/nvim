{ stdenvNoCC, fetchFromGitHub, neovim }:
let
  rev = "80a16deb5146a3eb4648effccda1ab9f45e43e76";
in
stdenvNoCC.mkDerivation
{
  pname = "nvim-treesitter-lock-file";
  version = rev;

  src = fetchFromGitHub {
    owner = "nvim-treesitter";
    repo = "nvim-treesitter";
    inherit rev;
    hash = "sha256-scrkupf/weQweU0vuTpgNTZ5hddPrLHCCWVSB9Aq9VU=";
  };
  nativeBuildInputs = [ neovim ];

  buildPhase = ''
    cat <<EOF | tee convert.lua
      local lockfile = vim.fn.json_decode(vim.fn.readfile("./lockfile.json"))
      local parsers = require("nvim-treesitter.parsers").get_parser_configs()
      for key, value in pairs(lockfile) do
        lockfile[key] = {
          revision = value.revision,
          url = parsers[key].install_info.url,
        }
      end
      vim.fn.writefile(vim.fn.split(vim.fn.json_encode(lockfile), "\n"), "./parser-info.json")
      vim.cmd("q")
    EOF
    nvim --clean --headless -c "set rtp^=$PWD" -c "luafile ./convert.lua" -c "q"
  '';

  installPhase = ''
    mv ./parser-info.json $out
  '';
}
