--
-- [fcitx5 auto toggle]
--
-- In archlinux, fcitx5-remote is already contained in fcitx5 packages.
-- If fcitx5-remote executable is not found on your machine, try install one.
--
-- This autocommand will not always activate fcitx5 in insert mode.
-- My approach is to activate fcitx5 only when fcitx5 is manually triggered last
-- time in insert mode.
--
-- You can disable this feature in `~/.config/lua/custom.lua` file
--

local au = vim.api.nvim_create_autocmd

local function async_close_fcitx5()
  local task = require("plenary.job")
  task
    :new({
      command = "fcitx5-remote",
      args = { "-c" },
      on_exit = function(_, ret)
        if ret ~= 0 then
          vim.notify("fail to close fcitx5")
          return
        end

        vim.g.fcitx5_should_reactivate = true
      end,
    })
    :start()
end

local function async_toggle_fcitx5()
  local task = require("plenary.job")
  task
    :new({
      command = "fcitx5-remote",
      on_exit = function(j, ret)
        if ret ~= 0 then
          vim.notify("Fail to execute fcitx5-remote command")
          return
        end

        local stdout = j:result()
        -- Grievance: why Lua array index start with 1?
        if #stdout > 0 and stdout[1] == "2" then
          async_close_fcitx5()
        end
      end,
    })
    :start()
end

local function async_open_fcitx5()
  local task = require("plenary.job")
  task
    :new({
      command = "fcitx5-remote",
      args = { "-o" },
      on_exit = function(_, ret)
        if ret ~= 0 then
          vim.notify("fail to open fcitx5")
          return
        end

        vim.g.fcitx5_should_reactivate = false
      end,
    })
    :start()
end

au({ "InsertLeave" }, {
  callback = function()
    -- plenary is a wrapper for libuv API, it can help us asynchronously execute commands.
    -- But it is not included in Neovim API, so we have to make sure that it **does** exist.
    local has_plenary, _ = pcall(require, "plenary")

    if has_plenary then
      async_toggle_fcitx5()
      return
    end

    --
    -- use neovim API as a fallback solution
    --
    -- If the fcitx5 is manually activated in insert mode
    local is_active = vim.fn.system("fcitx5-remote"):gsub("%s+", "") == "2"

    if is_active then
      vim.fn.system("fcitx5-remote -c")
      vim.g.fcitx5_should_reactivate = true
    end
  end,
})

au({ "InsertEnter" }, {
  callback = function()
    if not vim.g.fcitx5_should_reactivate then
      return
    end

    -- use plenary as primary solution
    local has_plenary, _ = pcall(require, "plenary")
    if has_plenary then
      async_open_fcitx5()
      return
    end

    -- use neovim API as a fallback solution
    vim.fn.system("fcitx5-remote -o")
    vim.g.fcitx5_should_reactivate = false
  end,
})
