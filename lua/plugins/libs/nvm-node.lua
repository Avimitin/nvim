local packer_util = require("packer.util")

local nvm_alias_for_nvim = "nvim-node"
local stdpath = vim.fn.stdpath

local M = {}

M.get_nvm_node_path = function ()
	local ok, md = pcall(require, "plugin.nvm_node_path_generated")
	if ok then
		return md.node_bin_path
	end
	return nil
end

M.compile_nvm_node_path = function ()
	local output_path = packer_util.join_paths(stdpath("config"), "plugin", "nvm_node_path_generated.lua")
	local nvm_dir = os.getenv("NVM_DIR")
	if not nvm_dir then
		-- emit warning?
		return
	end

	local node_version = nil
	if pcall(function () io.input(packer_util.join_paths(nvm_dir, "alias", nvm_alias_for_nvim)) end) then
		node_version = io.read()
		io.close()
	end

	if not node_version then
		return
	end

	local node_bin_path = packer_util.join_paths(nvm_dir, "versions", "node", node_version, "bin", "node")
	local output_file = io.open(output_path, 'w')
	if not output_file then
		return
	end
	output_file:write(string.format([[
		local M = {}
		M.node_bin_path = %q
		return M
	]], node_bin_path))
	output_file:close()
	return node_bin_path
end


return M
