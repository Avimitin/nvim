{
  stdenvNoCC,
  fetchFromGitHub,
  neovim,
}:

let
  rev = with builtins; (fromJSON (readFile ../lazy-lock.json))."nvim-treesitter".commit;
in
stdenvNoCC.mkDerivation {
  name = "nvim-treesitter-lock-file-${rev}";

  src = fetchFromGitHub {
    owner = "nvim-treesitter";
    repo = "nvim-treesitter";
    inherit rev;
    hash = "sha256-Z3Ga6ZWWro2Dzx/8QW6PqTjbePr/ebVupsCxu61djAE=";
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
