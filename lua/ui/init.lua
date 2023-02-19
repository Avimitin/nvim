local register = require("pack").register

-- Deep dark purple colorscheme
register("rebelot/kanagawa.nvim", {
  cond = vim.cfg.ui.theme == "kanagawa",
  config = function()
    require("ui.kanagawa")
  end,
})

--- List of nerd-font icons
register("kyazdani42/nvim-web-devicons", {
  lazy = true,
})

register("glepnir/galaxyline.nvim", {
  event = "UIEnter",
  config = function()
    require("ui.statusline")
  end,
})

register("akinsho/nvim-bufferline.lua", {
  event = "BufRead",
  config = function()
    require("ui.bufferline")
  end,
})

register("lukas-reineke/indent-blankline.nvim", {
  event = "BufRead",
  config = function()
    require("ui.indent")
  end,
})

register("rcarriga/nvim-notify", {
  event = "UIEnter",
  config = function()
    vim.notify = function(msg, level, opts)
      local function split_length(text, length)
        local lines = {}
        local next_line
        while true do
          if #text == 0 then
            return lines
          end
          next_line, text = text:sub(1, length), text:sub(length)
          lines[#lines + 1] = next_line
        end
      end

      if type(msg) == "string" then
        if msg:len() < 72 then
          return require("notify")(msg, level, opts)
        end
        msg = vim.split(msg, "\n")
      end
      local truncated = {}
      for _, line in ipairs(msg) do
        local new_lines = split_length(line, 72)
        for _, l in ipairs(new_lines) do
          truncated[#truncated + 1] = l
        end
      end
      return require("notify")(truncated, level, opts)
    end
  end,
})
