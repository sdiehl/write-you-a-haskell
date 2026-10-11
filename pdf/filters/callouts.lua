function BlockQuote(quote)
  local first = quote.content[1]
  if not first or not first.content then return nil end
  local label = first.content[1]
  if not label or label.t ~= 'Strong' then return nil end
  local kind = pandoc.utils.stringify(label):match('^(Note)%.')
    or pandoc.utils.stringify(label):match('^(Aside)%.')
    or pandoc.utils.stringify(label):match('^(Exercise)%.')
  if not kind then return nil end
  first.content:remove(1)
  local body = pandoc.write(pandoc.Pandoc(quote.content), 'typst')
  return pandoc.RawBlock('typst', '#callout("' .. kind .. '")[\n' .. body .. '\n]')
end
