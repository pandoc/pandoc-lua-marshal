local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

return {
  group "Block" {
    group 'BlockQuote' {
      test('access content via property `content`', function ()
        local elem = BlockQuote{'word'}
        assert.are_same(elem.content, {Plain 'word'})
        assert.are_equal(type(elem.content), 'table')

        elem.content = {
          Para{Str 'one'},
          Para{Str 'two'}
        }
        assert.are_equal(
          BlockQuote{
            Para 'one',
            Para 'two'
          },
          elem
        )
      end),
    },
    group 'BulletList' {
      test('access items via property `content`', function ()
        local para = Para 'one'
        local blist = BulletList{{para}}
        assert.are_same({{para}}, blist.content)
      end),
      test('property `content` uses fuzzy marshalling', function ()
        local old = Plain 'old'
        local new = Plain 'new'
        local blist = BulletList{{old}}
        blist.content = {{new}}
        assert.are_same({{new}}, blist:clone().content)
        blist.content = new
        assert.are_same({{new}}, blist:clone().content)
      end),
    },
    group 'CodeBlock' {
      test('access code via property `text`', function ()
        local cb = CodeBlock('return true')
        assert.are_equal(cb.text, 'return true')
        assert.are_equal(type(cb.text), 'string')

        cb.text = 'return nil'
        assert.are_equal(cb, CodeBlock('return nil'))
      end),
      test('access Attr via property `attr`', function ()
        local cb = CodeBlock('true', {'my-code', {'lua'}})
        assert.are_equal(cb.attr, Attr{'my-code', {'lua'}})
        assert.are_equal(type(cb.attr), 'userdata')

        cb.attr = Attr{'my-other-code', {'java'}}
        assert.are_equal(
          CodeBlock('true', {'my-other-code', {'java'}}),
          cb
        )
      end)
    },
    group 'DefinitionList' {
      test('access items via property `content`', function ()
        local deflist = DefinitionList{
          {'apple', {{Plain 'fruit'}, {Plain 'company'}}},
          {Str 'coffee', 'Best when hot.'}
        }
        assert.are_equal(#deflist.content, 2)
        assert.are_same(deflist.content[1][1], {Str 'apple'})
        assert.are_same(deflist.content[1][2][2],
                         {Plain{Str 'company'}})
        assert.are_same(deflist.content[2][2],
                        {{Plain{
                            Str 'Best', Space(),
                            Str 'when', Space(),
                            Str 'hot.'}}})
      end),
      test('modify items via property `content`', function ()
        local deflist = DefinitionList{
          {'apple', {{{'fruit'}}, {{'company'}}}}
        }
        deflist.content[1][1] = Str 'orange'
        deflist.content[1][2][1] = {Plain 'tasty fruit'}
        local newlist = DefinitionList{
          { {Str 'orange'},
            {{Plain 'tasty fruit'}, {Plain 'company'}}
          }
        }
        assert.are_equal(deflist, newlist)
      end),
    },
    group 'Div' {
      test('access content via property `content`', function ()
        local elem = Div{BlockQuote{Plain 'word'}}
        assert.are_same(elem.content, {BlockQuote{'word'}})
        assert.are_equal(type(elem.content), 'table')

        elem.content = {
          Para{Str 'one'},
          Para{Str 'two'}
        }
        assert.are_equal(
          Div{
            Para 'one',
            Para 'two'
          },
          elem
        )
      end),
      test('access Attr via property `attr`', function ()
        local div = Div('word', {'my-div', {'sample'}})
        assert.are_equal(div.attr, Attr{'my-div', {'sample'}})
        assert.are_equal(type(div.attr), 'userdata')

        div.attr = Attr{'my-other-div', {'example'}}
        assert.are_equal(
          Div('word', {'my-other-div', {'example'}}),
          div
        )
      end)
    },
    group 'Header' {
      test('access inlines via property `content`', function ()
        local header = Header(1, 'test')
        assert.are_same(header.content, {Str 'test'})

        header.content = {'new text'}
        assert.are_equal(header, Header(1, {'new text'}))
      end),
      test('access Attr via property `attr`', function ()
        local header = Header(1, 'test', {'my-test'})
        assert.are_same(header.attr, Attr{'my-test'})

        header.attr = 'second-test'
        assert.are_equal(header, Header(1, 'test', 'second-test'))
      end),
      test('access level via property `level`', function ()
        local header = Header(3, 'test')
        assert.are_same(header.level, 3)

        header.level = 2
        assert.are_equal(header, Header(2, 'test'))
      end),
    },
    group 'LineBlock' {
      test('access lines via property `content`', function ()
        local spc = Space()
        local lineblock = LineBlock{
          {'200', spc, 'Main', spc, 'St.'},
          {'Berkeley', spc, 'CA', spc, '94718'}
        }
        assert.are_equal(#lineblock.content, 2) -- has two lines
        assert.are_same(lineblock.content[2][1], Str 'Berkeley')
      end),
      test('modifying `content` alter the element', function ()
        local spc = Space()
        local lineblock = LineBlock{
          {'200', spc, 'Main', spc, 'St.'},
          {'Berkeley', spc, 'CA', spc, '94718'}
        }
        lineblock.content[1][1] = '404'
        assert.are_same(
          lineblock:clone().content[1],
          {Str '404', spc, Str 'Main', spc, Str 'St.'}
        )

        lineblock.content = {{'line1'}, {'line2'}}
        assert.are_same(
          lineblock:clone(),
          LineBlock{
            {Str 'line1'},
            {Str 'line2'}
          }
        )
      end)
    },
    group 'Null' {
      test('Can be constructed', function ()
        assert.are_equal(Null().t, 'Null')
      end)
    },
    group 'OrderedList' {
      test('access items via property `content`', function ()
        local para = Plain 'one'
        local olist = OrderedList{{para}}
        assert.are_same({{para}}, olist.content)
      end),
      test('forgiving constructor', function ()
        local plain = Plain 'old'
        local olist = OrderedList({plain}, {3, 'Example', 'Period'})
        local listAttribs = ListAttributes(3, 'Example', 'Period')
        assert.are_same(olist.listAttributes, listAttribs)
      end),
      test('has list attribute aliases', function ()
        local olist = OrderedList({}, {4, 'Decimal', 'OneParen'})
        assert.are_equal(olist.start, 4)
        assert.are_equal(olist.style, 'Decimal')
        assert.are_equal(olist.delimiter, 'OneParen')
      end)
                        },
    group 'Para' {
      test('access inline via property `content`', function ()
        local para = Para{'Moin, ', Space(), 'Sylt!'}
        assert.are_same(
          para.content,
          {Str 'Moin, ', Space(), Str 'Sylt!'}
        )
      end),
      test('modifying `content` changes the element', function ()
        local para = Para{'Moin, ', Space(), Str 'Sylt!'}

        para.content[3] = 'Hamburg!'
        assert.are_same(
          para:clone().content,
          {Str 'Moin, ', Space(), Str 'Hamburg!'}
        )

        para.content = 'Huh'
        assert.are_same(
          para:clone().content,
          {Str 'Huh'}
        )
      end),
    },
    group 'RawBlock' {
      test('access raw content via property `text`', function ()
        local raw = RawBlock('markdown', '- one')
        assert.are_equal(type(raw.text), 'string')
        assert.are_equal(raw.text, '- one')

        raw.text = '+ one'
        assert.are_equal(raw, RawBlock('markdown', '+ one'))
      end),
      test('access Format via property `format`', function ()
        local raw = RawBlock('markdown', '* hi')
        assert.are_equal(type(raw.format), 'string')
        assert.are_equal(raw.format, 'markdown')

        raw.format = 'org'
        assert.are_equal(RawBlock('org', '* hi'), raw)
      end)
    },
    group 'Table' {
      test('access Attr via property `attr`', function ()
        local caption = {long = {Plain 'cap'}}
        local tbl = Table(caption, {}, {{}, {}}, {}, {{}, {}},
                                 {'my-tbl', {'a'}})
        assert.are_equal(tbl.attr, Attr{'my-tbl', {'a'}})

        tbl.attr = Attr{'my-other-tbl', {'b'}}
        assert.are_equal(
          Table(caption, {}, {{}, {}}, {}, {{}, {}},
                       {'my-other-tbl', {'b'}}),
          tbl
        )
      end),
      test('access caption via property `caption`', function ()
        local caption = {long = {Plain 'cap'}}
        local tbl = Table(caption, {}, {{}, {}}, {}, {{}, {}})
        assert.are_same(tbl.caption, {long = {Plain 'cap'}})

        tbl.caption.short = 'brief'
        tbl.caption.long  = {Plain 'extended'}

        local new_caption = {
          short = 'brief',
          long = {Plain 'extended'}
        }
        assert.are_equal(
          Table(new_caption, {}, {{}, {}}, {}, {{}, {}}),
          tbl
        )
      end),
      test('access column specifiers via property `colspecs`', function ()
        local colspecs = {{AlignCenter, 1}}
        local tbl = Table({long = {}}, colspecs, {{}, {}}, {}, {{}, {}})
        assert.are_same(tbl.colspecs, colspecs)

        tbl.colspecs[1][1] = AlignRight
        tbl.colspecs[1][2] = nil

        local new_colspecs = {{AlignRight}}
        assert.are_equal(
          Table({long = {}}, new_colspecs, {{}, {}}, {}, {{}, {}}),
          tbl
        )
      end),
      test('access table head via property `head`', function ()
        local head = {Attr{'tbl-head'}, {}}
        local tbl = Table({long = {}}, {}, head, {}, {{}, {}})
        assert.are_same(tbl.head, head)

        tbl.head[1] = Attr{'table-head'}

        local new_head = {'table-head', {}}
        assert.are_equal(
          Table({long = {}}, {}, new_head, {}, {{}, {}}),
          tbl
        )
      end),
      test('access table head via property `head`', function ()
        local foot = {{id = 'tbl-foot'}, {}}
        local tbl = Table({long = {}}, {}, {{}, {}}, {}, foot)
        assert.are_same(tbl.foot, {Attr('tbl-foot'), {}})

        tbl.foot[1] = Attr{'table-foot'}

        local new_foot = {'table-foot', {}}
        assert.are_equal(
          Table({long = {}}, {}, {{}, {}}, {}, new_foot),
          tbl
        )
      end)
    },
  },
  group "Blocks" {
    test('splits a string into words', function ()
      assert.are_same(
        Blocks 'Absolute Giganten',
        {Plain {Str 'Absolute', Space(), Str 'Giganten'}}
      )
    end),
    test('converts single Block into List', function ()
      assert.are_same(
        Blocks(CodeBlock('return true')),
        {CodeBlock('return true')}
      )
    end),
    test('converts elements in a list into Blocks', function ()
      assert.are_same(
        Blocks{'Berlin', 'Berkeley', Plain 'Zürich'},
        {Plain{Str 'Berlin'}, Plain{Str 'Berkeley'}, Plain{Str 'Zürich'}}
      )
    end),
    test('can be mapped over', function ()
      local words = Blocks{Header(1, 'Program'), CodeBlock 'pandoc'}
      assert.are_same(
        words:map(function (x) return x.t end),
        {'Header', 'CodeBlock'}
      )
    end),
  },
}
