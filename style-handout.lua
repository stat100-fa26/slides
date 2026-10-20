-- Handout filter for STAT 100.
--
-- One source file makes two handouts:
--   key: false  -> student version: answers are replaced by blank space
--   key: true   -> answer key: answers are shown in crimson
--
-- Usage in the .qmd:
--   Inline blank:  [sample space]{.blank}          (optional: width="4cm")
--   Block answer:  ::: {.answer lines="3"}
--                  ...answer...
--                  :::
-- In the student version a block answer becomes empty writing space
-- (about `lines` lines tall); in the key it is shown in a crimson box.

local key = false

local function is_latex()
  return FORMAT:match("latex") or FORMAT:match("pdf")
end

local function get_meta(meta)
  if meta.key ~= nil then
    local v = pandoc.utils.stringify(meta.key)
    key = (v == "true" or v == "True" or v == "yes")
  end
end

local function blank_span(el)
  if not el.classes:includes("blank") then return nil end
  if key then
    if is_latex() then
      local out = { pandoc.RawInline("latex", "\\textcolor{harvardcrimson}{\\textbf{") }
      for _, x in ipairs(el.content) do table.insert(out, x) end
      table.insert(out, pandoc.RawInline("latex", "}}"))
      return out
    else
      el.classes = { "ans-inline" }
      return el
    end
  else
    local n = utf8.len(pandoc.utils.stringify(el)) or 8
    local w = el.attributes["width"]
    if is_latex() then
      w = w or string.format("%.1fcm", math.max(3.5, n * 0.3))
      return pandoc.RawInline("latex", "\\rule[-4pt]{" .. w .. "}{0.5pt}")
    else
      w = w or string.format("%.1fem", math.max(6, n * 0.6))
      return pandoc.RawInline("html",
        '<span class="blank-line" style="width:' .. w .. '">' ..
        '<span class="sr-only">blank to fill in</span></span>')
    end
  end
end

local function answer_div(el)
  if not el.classes:includes("answer") then return nil end
  local lines = tonumber(el.attributes["lines"] or "3")
  if key then
    if is_latex() then
      local out = { pandoc.RawBlock("latex", "\\begingroup\\color{harvardcrimson}") }
      for _, x in ipairs(el.content) do table.insert(out, x) end
      table.insert(out, pandoc.RawBlock("latex", "\\par\\endgroup"))
      return out
    else
      el.classes = { "ans-block" }
      return el
    end
  else
    if is_latex() then
      return pandoc.RawBlock("latex", string.format("\\vspace{%.1fcm}", lines * 1.1))
    else
      return pandoc.RawBlock("html",
        string.format('<div class="write-space" style="height:%.1fem"></div>', lines * 2.4))
    end
  end
end

return {
  { Meta = get_meta },
  { Span = blank_span, Div = answer_div }
}
