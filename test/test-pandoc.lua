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
    test('string is treated as MetaString', function ()
      local meta = Pandoc({}, {test = 'test'}).meta
      assert.are_equal(meta.test, 'test')
    end),
    test('booleans are treated as MetaBool', function ()
      local meta = Pandoc({}, {test = true}).meta
      assert.are_equal(meta.test, true)
    end),
    test('list of strings becomes MetaList of MetaStrings', function ()
      local meta = Pandoc({}, {zahlen = {'eins', 'zwei', 'drei'}}).meta
      assert.are_same(meta.zahlen, {'eins', 'zwei', 'drei'})
    end),
  },
  group 'walk' {
    test('uses `Meta` function', function ()
      local meta = {
        artist = 'Bodi Bill',
        albums = {'Next Time'}
      }
      local doc = Pandoc({}, meta)
      assert.are_equal(
        Pandoc({},
          {artist = 'Bodi Bill', albums = {'Next Time', 'No More Wars'}}
        ),
        doc:walk {
          Meta = function (meta)
            meta.albums:insert('No More Wars')
            return meta
          end
        }
      )
    end),
  }
}
