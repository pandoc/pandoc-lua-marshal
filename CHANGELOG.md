# Changelog

`pandoc-lua-marshal` uses [PVP Versioning][].

## 0.1.3

Release pending.

### Lua changes

-   The traversal order of filters can now be selected by setting
    the key `traverse` to either `'topdown'` or `'typewise'`; the
    default remains `'typewise'`.

    Topdown traversals can be cut short by returning `false` as a
    second value from the filter function. No child-element of
    the returned element is processed in that case.

-   All types can be compared. Previously, comparing values of
    different types would lead to errors in a number of cases.

-   Lists now have an `__eq` metamethod. List equality is checked
    by comparing both lists element-wise. Two lists are equal if
    they have the same type and have equal elements.

-   If start indices in `List:find` and `List:find_if` are
    negative the start index is relative to the list length.

-   TableFoot, TableHead, and Row values are marshaled as
    userdata objects.

### Haskell code

-   Text.Pandoc.Lua.Marshal.Filter exports the new type
    `WalkingOrder`. The type `Filter` now contains the the
    traversal specifier as a field.

-   New modules for TableFoot, TableHead, and Row, defining the
    usual marshaling methods and constructor functions for these
    types.

## 0.1.2

Released 2021-12-10.

-   Restored backward compatible retrieval of Rows. Cells can be
    either a userdata value or a table.

## 0.1.1

Released 2021-12-10.

### Behavior of Lua objects

-   Lists of Inline values and lists of Block values are now
    pushed with their own metatables (named "Inlines" and
    "Blocks").

-   The types `Block`, `Blocks`, `Inline`, `Inlines`, and
    `Pandoc` now all have a method `walk` that applies a filter
    to the document subtree.

-   Changed behavior for *Cell* values: these are now pushed as
    userdata; the old table-based structure is still accepted
    when retrieving a Cell from the stack.

### Haskell code

-   Module Text.Pandoc.Lua.Marshal.Cell exports the constructor
    function `mkCell`, the type definition `typeCell` and the
    fuzzy peeker `peekCellFuzzy`.

-   Added a new module `Text.Pandoc.Lua.Marshal.Filter` that
    handles Lua filters.

-   Added functions for filtering:

    -   Module Text.Pandoc.Lua.Marshal.Block:
        -   `walkBlockSplicing`: walk an AST element, applying a
            filter on each Block and splicing the result back
            into the list.
        -   `walkBlocks`: walk an AST element, modifying lists of
            Block elements by applying the `Blocks` filter
            function.
    -   Module Text.Pandoc.Lua.Marshal.Inline:
        -   `walkInlineSplicing`: walk an AST element, applying a
            filter on each Inline and splicing the result back
            into the list.
        -   `walkInlines`: walk an AST element, modifying lists
            of Inline elements by applying the `Inlines` filter
            function.

    -   Module Text.Pandoc.Lua.Marshal.Pandoc:
        -   `applyFully`: fully apply a filter on a Pandoc
            document.

-   New internal modules:

    -   Text.Pandoc.Lua.SpliceList: defines a helper type used to
        walk a list of elements in a way that replaces the
        element by splicing the function result back into the
        list.

        The module is a slight rewrite of pandoc’s
        `SingletonsList`.

    -   Text.Pandoc.Lua.Walk: handles walking of the document
        tree while modifying elements via filter functions. This
        is a re-implementation of large parts of pandoc’s
        T.P.Lua.Filter module.

    -   Text.Pandoc.Lua.Marshal.Shared: provides helper functions
        used in multiple Lua type definitions.

## 0.1.0.1

Released 2021-11-28.

-   Added test-simpletable.lua to the list of extra-source-files.

## 0.1.0

Released 2021-11-28.

-   Released into the wild. May it live long and prosper.

  [PVP Versioning]: https://pvp.haskell.org
