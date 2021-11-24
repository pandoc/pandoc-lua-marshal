--
-- Tests for the pandoc types module
--
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

return {
  group 'sample' {
    test('sample test', function ()
           return nil
    end),
  }
}
