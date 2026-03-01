return {
  -- Neogit: Magit-like git interface
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    cmd = "Neogit",
    keys = {
      { "<leader>gg", function() require("neogit").open() end, desc = "Neogit (Git Status)" },
      { "<leader>gc", function() require("neogit").open({ "commit" }) end, desc = "Neogit Commit" },
    },
    opts = {
      integrations = {
        diffview = true,
        telescope = true,
      },
    },
  },
  -- Diffview: side-by-side diff viewer
  {
    "sindrets/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewFileHistory" },
    keys = {
      { "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "Diffview Open" },
      { "<leader>gf", "<cmd>DiffviewFileHistory %<cr>", desc = "Diffview File History" },
    },
    opts = {},
  },
  -- Disable snacks.lazygit mappings to avoid conflicts
  {
    "folke/snacks.nvim",
    keys = {
      { "<leader>gg", false },
      { "<leader>gG", false },
    },
  },
}
