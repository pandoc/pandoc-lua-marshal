local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

return {
  group 'SimpleTable' {
    test('can access properties', function ()
      local spc = Space()
      local caption = {Str 'Languages', spc, Str 'overview.'}
      local aligns = {AlignDefault, AlignDefault}
      local widths = {0, 0} -- let pandoc determine col widths
      local headers = {{Plain({Str "Language"})},
        {Plain({Str "Typing"})}}
      local rows = {
        {{Plain "Haskell"}, {Plain "static"}},
        {{Plain "Lua"}, {Plain "Dynamic"}},
      }
      local simple_table = SimpleTable(
        caption,
        aligns,
        widths,
        headers,
        rows
      )
      assert.are_same(simple_table.caption, caption)
      assert.are_same(simple_table.aligns, aligns)
      assert.are_same(simple_table.widths, widths)
      assert.are_same(simple_table.headers, headers)
      assert.are_same(simple_table.rows, rows)
    end),
    test('can modify properties', function ()
      local new_table = SimpleTable(
        {'Languages'},
        {AlignDefault, AlignDefault},
        {0.5, 0.5},
        {{Plain({Str "Language"})},
         {Plain({Str "Typing"})}},
        {
          {{Plain "Haskell"}, {Plain "static"}},
          {{Plain "Lua"}, {Plain "Dynamic"}},
        }
      )

      new_table.caption = {Str 'Good', Space(),
                           Str 'languages'}
      new_table.aligns[1] = AlignLeft
      new_table.widths = {0, 0}
      new_table.headers[2] = {Plain{Str 'compiled/interpreted'}}
      new_table.rows[1][2] = {Plain{Str 'both'}}
      new_table.rows[2][2] = {Plain{Str 'interpreted'}}

      local expected_table = SimpleTable(
        {Str 'Good', Space(), Str 'languages'},
        {AlignLeft, AlignDefault},
        {0, 0},
        {{Plain 'Language'}, {Plain 'compiled/interpreted'}},
        {
          {{Plain 'Haskell'}, {Plain 'both'}},
          {{Plain 'Lua'}, {Plain 'interpreted'}}
        }
      )
      assert.are_same(expected_table, new_table)
    end)
  },
}
