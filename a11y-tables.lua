-- Accessible tables for STAT 100 slides and handouts.
--
-- Markdown pipe tables only produce column headers. This filter also
-- marks the first column of every table as row headers, so screen
-- readers announce both the row label and the column label for each
-- cell (e.g., "Did not take one, Does not believe, 115").
--
-- HTML output:
--   * header-row cells get  <th scope="col">
--   * first-column cells get <th scope="row">
-- PDF output is unchanged visually.
--
-- Opt out for a table whose first column is not a label column by
-- giving it the class .no-row-headers in its caption:
--   : My caption {.no-row-headers}
--
-- For a horizontal table (values run across, e.g. x | 0 | 1 | 2), add
-- .horizontal so the top-left cell labels its row, not a column:
--   : Distribution of X {.horizontal}

local function set_scope(row, scope_first, scope_rest)
  for i, cell in ipairs(row.cells) do
    local scope = (i == 1) and scope_first or scope_rest
    if scope then cell.attributes["scope"] = scope end
  end
end

function Table(tbl)
  local row_heads = not tbl.classes:includes("no-row-headers")
  local html = FORMAT:match("html") or FORMAT:match("revealjs")

  for _, body in ipairs(tbl.bodies) do
    if row_heads then body.row_head_columns = 1 end
    if html and row_heads then
      for _, row in ipairs(body.body) do set_scope(row, "row", nil) end
    end
  end

  if html then
    local first = tbl.classes:includes("horizontal") and "row" or "col"
    for _, row in ipairs(tbl.head.rows) do set_scope(row, first, "col") end
  end
  return tbl
end
