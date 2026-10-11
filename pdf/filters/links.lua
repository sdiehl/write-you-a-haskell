-- Namespace headings because every chapter has a "Motivation" section.
local function slug(s)
  return s:lower():gsub('[^%w%s_-]', ''):gsub('%s+', '-')
end
function Div(div)
  local chapter = div.attributes['data-chapter']
  if not chapter then return nil end
  return div:walk({
    Header = function(h)
      local id = h.identifier ~= '' and h.identifier or slug(pandoc.utils.stringify(h.content))
      h.identifier = chapter .. '--' .. id
      if h.level == 1 then h.identifier = chapter end
      return h
    end,
    Link = function(link)
      local target, anchor = link.target:match('^([^#]+)%.md#?(.*)$')
      if target then
        if anchor == '' and target:match('^ch%d%d%-')
          and pandoc.utils.stringify(link.content):match('^Chapter %d+$') then
          return pandoc.RawInline('typst', '@' .. target)
        end
        link.target = '#' .. target .. (anchor ~= '' and '--' .. anchor or '')
        return link
      end
      if link.target:sub(1, 1) == '#' then
        link.target = '#' .. chapter .. '--' .. link.target:sub(2)
        return link
      end
    end,
    Image = function(img)
      if img.src:sub(1, 4) == 'img/' then img.src = '../../book/src/' .. img.src end
      return img
    end
  })
end
