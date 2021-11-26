local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

return {
  group 'Meta' {
    test('inline list is treated as MetaInlines', function ()
      local meta = Pandoc({}, {test = {Emph 'check'}}).meta
      assert.are_same(meta.test, {Emph{Str 'check'}})
    end),
    test('inline element is treated as MetaInlines singleton', function ()
      local meta = Pandoc({}, {test = Emph 'check'}).meta
      assert.are_same(meta.test, {Emph{Str 'check'}})
    end),
    test('block list is treated as MetaBlocks', function ()
      local meta = Pandoc({}, {test = {Plain 'check'}}).meta
      assert.are_same(meta.test, {Plain{Str 'check'}})
    end),
    test('block element is treated as MetaBlocks singleton', function ()
      local meta = Pandoc({}, {test = Plain 'check'}).meta
      assert.are_same(meta.test, {Plain{Str 'check'}})
    end),
  },
}
