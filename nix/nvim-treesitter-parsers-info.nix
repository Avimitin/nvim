{ fetchFromGitHub, stdenv, neovim, expect-version }:
stdenv.mkDerivation {
  pname = "nvim-treesitter-parser-info";
  version = expect-version;
  src = fetchFromGitHub {
    owner = "nvim-treesitter";
    repo = "nvim-treesitter";
    rev = expect-version;
    hash = "sha256-IpeC/GSvm3k/wsg4Tm8+wIIwvmk3fTtj+z7qL2a3cog=";
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
