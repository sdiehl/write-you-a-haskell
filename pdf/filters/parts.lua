function Div(div)
  if div.classes:includes('chapter-source') then
    div.content:insert(pandoc.RawBlock('typst', '#metadata("chapter-end")'))
    return div.content
  end
  if div.classes:includes('part') then
    local body = pandoc.write(pandoc.Pandoc(div.content), 'typst')
    return pandoc.RawBlock('typst', '#part[' .. body .. ']')
  end
end
function Header(h)
  if h.level ~= 1 then return nil end
  local text = pandoc.utils.stringify(h.content)
  local n, title = text:match('^Chapter (%d+): (.*)$')
  if not n then
    local letter, appendix_title = text:match('^Appendix ([A-Z]): (.*)$')
    if not letter then return nil end
    local body = pandoc.write(pandoc.Pandoc({pandoc.Plain({pandoc.Str(appendix_title)})}), 'typst')
    return pandoc.RawBlock('typst', '#appendix(' .. (string.byte(letter) - string.byte('A') + 1) .. ', [' .. body .. '], "' .. h.identifier .. '")')
  end
  local body = pandoc.write(pandoc.Pandoc({pandoc.Plain({pandoc.Str(title)})}), 'typst')
  return pandoc.RawBlock('typst', '#chapter(' .. n .. ', [' .. body .. '], "' .. h.identifier .. '")')
end
