# Changelog

`pandoc-lua-marshal` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.1.1

Release pending.

* Added a new module `Text.Pandoc.Lua.Marshal.Filter` that handles
  Lua filters.

* Added functions for filtering:

    - Module Text.Pandoc.Lua.Marshal.Block:
        - `walkBlockSplicing`: walk an AST element, applying a
          filter on each Block and splicing the result back into
          the list.
        - `walkBlocks`: walk an AST element, modifying lists of
          Block elements by applying the `Blocks` filter
          function.

    - Module Text.Pandoc.Lua.Marshal.Inline:
        - `walkInlineSplicing`: walk an AST element, applying a
          filter on each Inline and splicing the result back into
          the list.
        - `walkInlines`: walk an AST element, modifying lists of
          Inline elements by applying the `Inlines` filter
          function.

* New internal modules `Text.Pandoc.Lua.Walk` and
  `Text.Pandoc.Lua.SpliceList`. The former handles walking of the
  document tree while modifying elements via filter functions, and
  the latter defines a helper type used to walk a list of elements
  in a way that replaces the element by splicing the function
  result back into the list.

## 0.1.0.1

Released 2021-11-28.

* Added test-simpletable.lua to the list of extra-source-files.

## 0.1.0

Released 2021-11-28.

* Released into the wild. May it live long and prosper.

[1]: https://pvp.haskell.org
[2]: https://github.com/pandoc/pandoc-lua-marshal/releases
