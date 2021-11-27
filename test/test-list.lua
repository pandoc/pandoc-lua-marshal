--
-- Tests for the pandoc types module
--
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

return {
  group 'List' {
    test('is a table', function ()
      assert.are_equal(type(List), 'table')
    end),
    group 'constructor' {
      test('returns a new list if called without args', function ()
        assert.are_same(List(), {})
      end),
    },

    group 'clone' {
      test('changing the clone does not affect original', function ()
        local orig = List:new {23, 42}
        local copy = orig:clone()
        copy[1] = 5
        assert.are_same({23, 42}, orig)
        assert.are_same({5, 42}, copy)
      end),
      test('result is a list', function ()
        local orig = List:new {23, 42}
        assert.are_equal(List, getmetatable(orig:clone()))
      end),
    },

    group 'extend' {
      test('extends list with other list', function ()
        local primes = List:new {2, 3, 5, 7}
        primes:extend {11, 13, 17}
        assert.are_same({2, 3, 5, 7, 11, 13, 17}, primes)
      end)
    },

    group 'filter' {
      test('keep elements for which property is truthy', function ()
        local is_small_prime = function (x)
          return x == 2 or x == 3 or x == 5 or x == 7
        end
        local numbers = List:new {4, 7, 2, 9, 5, 11}
        assert.are_same(List{7, 2, 5}, numbers:filter(is_small_prime))
      end),
      test('predecate function gets current index as second arg', function ()
        local args = List()
        local collect_args = function (...)
          args[#args+1] = {...}
          return false
        end
        (List{0, 1, 1, 2, 3, 5, 8}):filter(collect_args)
        assert.are_same(
          args,
          List{{0, 1}, {1, 2}, {1, 3}, {2, 4}, {3, 5}, {5, 6}, {8, 7}}
        )
      end)
    },

    group 'find' {
      test('returns element and index if found', function ()
        local list = List:new {5, 23, 71}
        local elem, idx = list:find(71)
        assert.are_same(71, elem)
        assert.are_same(3, idx)
      end),
      test('respects start index', function ()
        local list = List:new {19, 23, 29, 71}
        assert.are_equal(23, list:find(23, 1))
        assert.are_equal(23, list:find(23, 2))
        assert.is_nil(list:find(23, 3))
      end),
      test('returns nil if element not found', function ()
        assert.is_nil((List:new {18, 20, 22, 0, 24}):find('0'))
      end),
    },

    group 'find_if' {
      test('returns element and index if found', function ()
        local perm_prime = List:new {2, 3, 5, 7, 11, 13, 17, 31, 37, 71}
        local elem, idx = perm_prime:find_if(function (x) return x >= 10 end)
        assert.are_same(11, elem)
        assert.are_same(5, idx)
      end),
      test('gets current index as second arg', function ()
        assert.are_equal(
          5,
          (List{9, 8, 7, 6, 5, 4, 3, 2, 1}):find_if(
            function (x, i) return x == i end
          )
        )
      end),
      test('returns nil if element not found', function ()
        local is_zero = function (n) return n == 0 end
        assert.is_nil((List:new {18, 20, 22, 24, 27}):find_if(is_zero))
      end),
    },

    group 'includes' {
      test('finds elements in list', function ()
        local lst = List:new {'one', 'two', 'three'}
        assert.is_truthy(lst:includes('one'))
        assert.is_truthy(lst:includes('two'))
        assert.is_truthy(lst:includes('three'))
        assert.is_falsy(lst:includes('four'))
      end)
    },

    group 'insert' {
      test('is a function', function ()
       assert.are_equal(type(List.insert), 'function')
      end),
      test('insert value at end of list.', function ()
        local count_norsk = List {'en', 'to', 'tre'}
        count_norsk:insert('fire')
        assert.are_same({'en', 'to', 'tre', 'fire'}, count_norsk)
      end),
      test('insert value in the middle of list.', function ()
        local count_norsk = List {'fem', 'syv'}
        count_norsk:insert(2, 'seks')
        assert.are_same({'fem', 'seks', 'syv'}, count_norsk)
      end)
    },

    group 'map' {
      test('applies function to elements', function ()
        local primes = List:new {2, 3, 5, 7}
        local squares = primes:map(function (x) return x^2 end)
        assert.are_same({4, 9, 25, 49}, squares)
      end),
      test('leaves original list unchanged', function ()
        local primes = List:new {2, 3, 5, 7}
        local squares = primes:map(function (x) return x^2 end)
        assert.are_same({2, 3, 5, 7}, primes)
      end),
      test('map function gets index as second argument', function ()
        local primes = List:new {2, 3, 5, 7}
        local indices = primes:map(function (x, i) return i end)
        assert.are_same(List{1, 2, 3, 4}, indices)
      end)
    },

    group 'new' {
      test('keeps elements in list', function ()
        local test = {1, 1, 2, 3, 5}
        assert.are_same(List:new(test), test)
      end),
      test('return empty list if no argument is given', function ()
        assert.are_same({}, List:new())
      end),
      test('metatable of result is pandoc.List', function ()
        local test = List:new{5}
        assert.are_equal(List, getmetatable(test))
      end)
    },

    group 'remove' {
      test('is a function', function ()
        assert.are_equal(type(List.remove), 'function')
      end),
      test('remove value at end of list.', function ()
        local understand = List {'jeg', 'forstår', 'ikke'}
        local norsk_not = understand:remove()
        assert.are_same({'jeg', 'forstår'}, understand)
        assert.are_equal('ikke', norsk_not)
      end),
      test('remove value at beginning of list.', function ()
        local count_norsk = List {'en', 'to', 'tre'}
        count_norsk:remove(1)
        assert.are_same({'to', 'tre'}, count_norsk)
      end)
    },

    group 'sort' {
      test('is a function', function ()
        assert.are_equal(type(List.sort), 'function')
      end),
      test('sort numeric list', function ()
        local numbers = List {71, 5, -1, 42, 23, 0, 1}
        numbers:sort()
        assert.are_same({-1, 0, 1, 5, 23, 42, 71}, numbers)
      end),
      test('reverse-sort numeric', function ()
        local numbers = List {71, 5, -1, 42, 23, 0, 1}
        numbers:sort(function (x, y) return x > y end)
        assert.are_same({71, 42, 23, 5, 1, 0, -1}, numbers)
      end)
    },
  }
}
