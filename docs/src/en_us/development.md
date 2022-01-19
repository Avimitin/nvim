# Development specifications

## Commit convention

Use format: `<type>[!][module]: <summary>`.

### Available type

- feat: for new feature
- fix: for bug fixes
- triv: for chore
- reft: for refactor
- docs: for document

Use the bang "!" to indicate that this commit contains breaking changes.

## Semantic version

This document add additional details for how to increase semantic version.

### API Compatibility

- Plugins
  - Major: Remove/Replace any plugin
  - Minor: Add new plugin
  - Minor: Modify plugin setting
- Keymaps
  - Major: Remove any keymap
  - Minor: Add/Replace keymap
- Options
  - Minor: Update options
- Colorscheme
  - Major: Remove any colorscheme
  - Minor: Update colorscheme setting
  - Minor: Replace default colorscheme
- utils.lua
  - Major: Remove any functionality
  - Minor: Add/Replace/Modify function
