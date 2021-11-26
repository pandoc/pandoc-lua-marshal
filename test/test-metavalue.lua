local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

return {
  group 'MetaValue elements' {
    test('MetaList elements behave like lists', function ()
      local metalist = MetaList{}
      assert.are_equal(type(metalist.insert), 'function')
      assert.are_equal(type(metalist.remove), 'function')
    end),
  }
}
