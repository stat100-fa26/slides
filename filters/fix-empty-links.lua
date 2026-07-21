-- fix-empty-links.lua
function Link(el)
  -- el.target is the href value for HTML output
  if el.target == "" then
    -- Remove the link but keep the content (if any)
    return el.content
  end
  return el
end