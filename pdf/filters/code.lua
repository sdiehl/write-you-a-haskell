-- Keep language tags and mirror mdBook's hidden-line convention for Rust.
function CodeBlock(block)
  if block.classes:includes('fun') then
    block.classes = {'fun'}
  elseif block.classes:includes('rust') then
    local lines = {}
    for line in (block.text .. '\n'):gmatch('(.-)\n') do
      if line:match('^%s*##') then
        table.insert(lines, (line:gsub('^(%s*)##', '%1#', 1)))
      elseif not line:match('^%s*# ') and not line:match('^%s*#$') then
        table.insert(lines, line)
      end
    end
    block.text = table.concat(lines, '\n')
  end
  return block
end

-- Roadmap tables use stable column proportions and compact, ragged-right text.
function Table(t)
  if #t.colspecs ~= 3 or #t.head.rows == 0 then return nil end
  local cells = t.head.rows[1].cells
  if pandoc.utils.stringify(cells[1].contents) ~= 'No.' then return nil end
  t.colspecs = {{pandoc.AlignRight, 0.09}, {pandoc.AlignLeft, 0.28}, {pandoc.AlignLeft, 0.63}}
  local body = pandoc.write(pandoc.Pandoc({t}), 'typst')
  return pandoc.RawBlock('typst', '#block(breakable: true)[\n#show figure: set block(breakable: true)\n#set text(size: 9.2pt, hyphenate: false)\n#set par(justify: false, leading: 0.5em)\n' .. body .. '\n]')
end
