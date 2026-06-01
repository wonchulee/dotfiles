local config_path = vim.fn.stdpath("config") .. "/.markdownlint-cli2.yaml"

return {
  {
    "mfussenegger/nvim-lint",
    opts = function(_, opts)
      opts.linters_by_ft = opts.linters_by_ft or {}
      opts.linters_by_ft.markdown = {}
      opts.linters_by_ft["markdown.mdx"] = {}

      opts.linters = opts.linters or {}
      opts.linters["markdownlint-cli2"] = opts.linters["markdownlint-cli2"] or {}
      opts.linters["markdownlint-cli2"].prepend_args = { "--config", config_path }
    end,
  },
  {
    "stevearc/conform.nvim",
    opts = {
      formatters = {
        ["markdownlint-cli2"] = {
          prepend_args = { "--config", config_path },
        },
      },
    },
  },
}
