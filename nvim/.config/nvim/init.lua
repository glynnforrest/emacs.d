vim.g.mapleader = ' '
local map = vim.api.nvim_set_keymap
map('n', '<leader>v', '<cmd>e ~/.config/nvim/init.lua<cr>', {noremap = true})
map('n', '<leader>V', '<cmd>luafile ~/.config/nvim/init.lua<cr>', {noremap = true})

local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
  vim.cmd [[qa]]
end

packer_sync = function()
	require('packer').sync()
end

require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  use {
    'TimUntersberger/neogit',
    requires = 'nvim-lua/plenary.nvim'
  }


  use 'joshdick/onedark.vim'
  use 'nvim-lualine/lualine.nvim'
  use 'terrortylor/nvim-comment'
  use 'lewis6991/gitsigns.nvim'

  use 'neovim/nvim-lspconfig'
  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'folke/neodev.nvim'
  use({"L3MON4D3/LuaSnip", tag = "v1.*"})
end)

vim.opt.tabstop = 4
vim.opt.shiftwidth = 4

require("neodev").setup()
local lspconfig = require('lspconfig')
local capabilities = require("cmp_nvim_lsp").default_capabilities()
local on_attach = function(_, bufnr)
  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
end

-- npm i -g pyright
lspconfig.pyright.setup({
  capabilities = capabilities,
  on_attach = on_attach,
})
-- brew install lua-language-server
lspconfig.sumneko_lua.setup({
  on_attach = on_attach,
})

-- npm i -g intelephense
lspconfig.intelephense.setup({
  on_attach = on_attach,
})

local ls = require('luasnip')
local cmp = require('cmp')

cmp.setup({
  mapping = cmp.mapping.preset.insert({
    ['<C-u>'] = cmp.mapping.scroll_docs(-4),
    ['<C-d>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete({}),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = false,
    }),
    ['<C-j>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item({
	  behavior = cmp.SelectBehavior.Select,
	})
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<C-k>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item({
	  behavior = cmp.SelectBehavior.Select,
	})
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<Tab>'] = cmp.mapping(function(fallback)
      if ls.expand_or_jumpable() then
        ls.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's', }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if ls.jumpable(-1) then
        ls.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's', }),
  }),
  snippet = {
	  expand = function(args)
		  ls.lsp_expand(args.body)
	  end,
  },
  sources = {
    { name = 'nvim_lsp' },
  },
})

require('nvim_comment').setup()
require('gitsigns').setup()

require('lualine').setup({
 options = {
    icons_enabled = false,
    section_separators = '',
    component_separators = '',
  }
})

local neogit = require('neogit')
neogit.setup()

vim.cmd('colorscheme onedark')

vim.opt.termguicolors = true
vim.opt.relativenumber = true

map('n', '<leader>bd', '<cmd>bd<cr>', {noremap = true})
map('n', '<leader>bs', '<cmd>lua require("telescope.builtin").buffers()<cr>', {noremap = true})

map('n', '<leader>ff', '<cmd>lua require("telescope.builtin").find_files()<cr>', {noremap = true})
map('n', '<leader>fs', '<cmd>w<cr>', {noremap = true})
map('n', '<leader>fg', '<cmd>lua require("telescope.builtin").live_grep()<cr>', {noremap = true})
map('n', '<leader>fh', '<cmd>lua require("telescope.builtin").help_tags()<cr>', {noremap = true})

map('n', '<leader>gs', '<cmd>lua require("neogit").open({kind = "vsplit"})<cr>', {noremap = true})

map('n', '<leader>P', '<cmd>lua packer_sync()<cr>', {noremap = true})

map('v', '<', '<gv', {noremap = true, silent = false})
map('v', '>', '>gv', {noremap = true, silent = false})


-- snippets
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node

local function copy(args)
	return args[1]
end

ls.add_snippets("all", {
	-- trigger is `fn`, second argument to snippet-constructor are the nodes to insert into the buffer on expansion.
	s("fn", {
		-- Simple static text.
		t("//Parameters: "),
		-- function, first parameter is the function, second the Placeholders
		-- whose text it gets as input.
		f(copy, 2),
		t({ "", "function " }),
		-- Placeholder/Insert.
		i(1),
		t("("),
		-- Placeholder with initial text.
		i(2, "int foo"),
		-- Linebreak
		t({ ") {", "\t" }),
		-- Last Placeholder, exit Point of the snippet.
		i(0),
		t({ "", "}" }),
	}),
})
