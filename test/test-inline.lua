local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

return {
  group "Inline" {
    group 'Cite' {
      test('has property `content`', function ()
        local cite = Cite({Emph 'important'}, {})
        assert.are_same(cite.content, {Emph {Str 'important'}})

        cite.content = 'boring'
        assert.are_equal(cite, Cite({Str 'boring'}, {}))
      end),
      test('has list of citations in property `cite`', function ()
        local citations = {
          Citation('einstein1905', 'NormalCitation')
        }
        local cite = Cite('relativity', citations)
        assert.are_same(cite.citations, citations)

        local new_citations = {
          citations[1],
          Citation('Poincaré1905', 'NormalCitation')
        }
        cite.citations = new_citations
        assert.are_equal(cite, Cite({'relativity'}, new_citations))
      end),
    },
    group 'Code' {
      test('has property `attr`', function ()
        local code = Code('true', {id='true', foo='bar'})
        assert.are_equal(code.attr, Attr('true', {}, {{'foo', 'bar'}}))

        code.attr = {id='t', fubar='quux'}
        assert.are_equal(
          Code('true', Attr('t', {}, {{'fubar', 'quux'}})),
          code
        )
      end),
      test('has property `text`', function ()
        local code = Code('true')
        assert.are_equal(code.text, 'true')

        code.text = '1 + 1'
        assert.are_equal(Code('1 + 1'), code)
      end),
    },
    group 'Emph' {
      test('has property `content`', function ()
        local elem = Emph{'two', Space(), 'words'}
        assert.are_same(
          elem.content,
          {Str 'two', Space(), Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, Emph{'word'})
      end)
    },
    group 'Image' {
      test('has property `caption`', function ()
        local img = Image('example', 'a.png')
        assert.are_same(img.caption, {Str 'example'})

        img.caption = 'A'
        assert.are_equal(img, Image({'A'}, 'a.png'))
      end),
      test('has property `src`', function ()
        local img = Image('example', 'sample.png')
        assert.are_same(img.src, 'sample.png')

        img.src = 'example.svg'
        assert.are_equal(img, Image('example', 'example.svg'))
      end),
      test('has property `title`', function ()
        local img = Image('here', 'img.gif', 'example')
        assert.are_same(img.title, 'example')

        img.title = 'a'
        assert.are_equal(img, Image('here', 'img.gif', 'a'))
      end),
      test('has property `attr`', function ()
        local img = Image('up', 'upwards.png', '', {'up', {'point'}})
        assert.are_same(img.attr, Attr {'up', {'point'}})

        img.attr = Attr {'up', {'point', 'button'}}
        assert.are_equal(
          Image('up', 'upwards.png', nil, {'up', {'point', 'button'}}),
          img
        )
      end)
    },
    group 'Link' {
      test('has property `content`', function ()
        local link = Link('example', 'https://example.org')
        assert.are_same(link.content, {Str 'example'})

        link.content = 'commercial'
        link.target = 'https://example.com'
        assert.are_equal(link, Link('commercial', 'https://example.com'))
      end),
      test('has property `target`', function ()
        local link = Link('example', 'https://example.org')
        assert.are_same(link.content, {Str 'example'})

        link.target = 'https://example.com'
        assert.are_equal(link, Link('example', 'https://example.com'))
      end),
      test('has property `title`', function ()
        local link = Link('here', 'https://example.org', 'example')
        assert.are_same(link.title, 'example')

        link.title = 'a'
        assert.are_equal(link, Link('here', 'https://example.org', 'a'))
      end),
      test('has property `attr`', function ()
        local link = Link('up', '../index.html', '', {'up', {'nav'}})
        assert.are_same(link.attr, Attr {'up', {'nav'}})

        link.attr = Attr {'up', {'nav', 'button'}}
        assert.are_equal(
          Link('up', '../index.html', nil, {'up', {'nav', 'button'}}),
          link
        )
      end)
    },
    group 'Math' {
      test('has property `text`', function ()
        local elem = Math(InlineMath, 'x^2')
        assert.are_same(elem.text, 'x^2')
        elem.text = 'a + b'
        assert.are_equal(elem, Math(InlineMath, 'a + b'))
      end),
      test('has property `mathtype`', function ()
        local elem = Math(InlineMath, 'x^2')
        assert.are_same(elem.mathtype, 'InlineMath')
        elem.mathtype = DisplayMath
        assert.are_equal(elem, Math(DisplayMath, 'x^2'))
      end),
    },
    group 'Note' {
      -- FIXME: waiting for Block support
      -- test('has property `content`', function ()
      --   local elem = Note{Para {'two', Space(), 'words'}}
      --   assert.are_same(
      --     elem.content,
      --     {Para {Str 'two', Space(), Str 'words'}}
      --   )
      --   elem.content = Plain 'word'
      --   assert.are_equal(elem, Note{'word'})
      -- end)
    },
    group 'Quoted' {
      test('has property `content`', function ()
        local elem = Quoted('SingleQuote', Emph{'emph'})
        assert.are_same(
          elem.content,
          {Emph{Str 'emph'}}
        )
        elem.content = {'word'}
        assert.are_equal(elem, Quoted(SingleQuote, {'word'}))
      end),
      test('has property `quotetype`', function ()
        local elem = Quoted('SingleQuote', 'a')
        assert.are_same(elem.quotetype, SingleQuote)
        elem.quotetype = 'DoubleQuote'
        assert.are_equal(elem, Quoted(DoubleQuote, {'a'}))
      end)
    },
    group 'SmallCaps' {
      test('has property `content`', function ()
        local elem = SmallCaps{'two', Space(), 'words'}
        assert.are_same(
          elem.content,
          {Str 'two', Space(), Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, SmallCaps{'word'})
      end)
    },
    group 'SoftBreak' {
      test('can be constructed', function ()
        local sb = SoftBreak()
        assert.are_equal(sb.t, 'SoftBreak')
      end)
    },
    group 'Span' {
      test('has property `attr`', function ()
        local elem = Span('one', {'', {'number'}})
        assert.are_same(
          elem.attr,
          Attr('', {'number'})
        )
        elem.attr = {'', {}, {{'a', 'b'}}}
        assert.are_equal(elem, Span({'one'}, {a='b'}))
      end),
      test('has property `content`', function ()
        local elem = Span{'two', Space(), 'words'}
        assert.are_same(
          elem.content,
          {Str 'two', Space(), Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, Span{'word'})
      end)
    },
    group 'Str' {
      test('has property `text`', function ()
        local elem = Str 'nein'
        assert.are_same(elem.text, 'nein')
        elem.text = 'doch'
        assert.are_equal(elem, Str 'doch')
      end)
    },
    group 'Strikeout' {
      test('has property `content`', function ()
        local elem = Strikeout{'two', Space(), 'words'}
        assert.are_same(
          elem.content,
          {Str 'two', Space(), Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, Strikeout{'word'})
      end)
    },
    group 'Strong' {
      test('has property `content`', function ()
        local elem = Strong{'two', Space(), 'words'}
        assert.are_same(
          elem.content,
          {Str 'two', Space(), Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, Strong{'word'})
      end)
    },
    group 'Subscript' {
      test('has property `content`', function ()
        local elem = Subscript{'two', Space(), 'words'}
        assert.are_same(
          elem.content,
          {Str 'two', Space(), Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, Subscript{'word'})
      end)
    },
    group 'Superscript' {
      test('has property `content`', function ()
        local elem = Superscript{'two', Space(), 'words'}
        assert.are_same(
          elem.content,
          {Str 'two', Space(), Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, Superscript{'word'})
      end)
    },
    group 'Underline' {
      test('has property `content`', function ()
        local elem = Underline{'two', Space(), 'words'}
        assert.are_same(
          elem.content,
          {Str 'two', Space(), Str 'words'}
        )
        elem.content = {'word'}
        assert.are_equal(elem, Underline{'word'})
      end)
    },
  }
}
