module ("L_Netatmo2", package.seeall)

------
--
-- Netatmo2 - supplementary modules
--

------
--
-- Command Line Interface parser - for HTTP request handlers
--
-- Given GROUPS of command NAMES and their command line ALIASES and VALUES,
-- parse the input to generate a RESULT table with GROUPS tables containing validated NAME / VALUE pairs.
-- If there is an error return nil and a useful diagnostic STATUS message.
-- An auto-generated HELP message is available, along with an EXAMPLE line if given to the parse instance.
--

function cli()

local function parser (example)
	
	local version = "2013.12.19  @akbooer"

	local allowOther = true		-- set to true to suppress error for unknown parameters (which end up in "other" category)
	local index  = {}			-- index of parameters, keyed by full name
	local syntax = {}			-- syntax structure
	local lookup = {}			-- maps parameter aliases to full names
	
	local function parameter (group, name, aliases, value, help)
		local function aliasSet (names, set)					-- create table of truncated aliases which all point to their full name
			local set = set or {}								-- concatenate with given table. if present
			for _,name in ipairs (names) do
				for i = 1,#name do 								-- one or more letters
					local subset = name:sub(1,i)
					set[subset] = set[subset] or name 			-- don't overwrite previous definition
				end
			end
			return set
		end
		local set
		if type (aliases) == "string" then aliases = {aliases} end
		if type (value) == "table" then set = aliasSet (value) end
		local param = {group = group, name = name, cmds = aliases, vals = value, set = set, help = help}
		syntax[group] = syntax[group] or {}							-- may be a new group
		syntax[group][name] = param
		lookup = aliasSet (aliases, lookup)							-- add these parameter name aliases to all the rest
		for _,alias in ipairs (aliases) do index[alias] = param end
	end	
		
	local function help ()
		local intro = "PARAMETERS: names and values can be abbreviated"
		local t = {intro}
		local function p(x) t[#t+1]=x end
		for class, params in pairs (syntax) do
			p (("\n  [%s]"): format (class))
			for _,b in pairs (params) do
				local val = b.vals
				if type (val) == "table" then val = table.concat (val, '/') end
				p (("%20s = %s [%s]"): format (table.concat (b.cmds, '/'), b.help, val))
			end
		end
		p ''; 
		if example then p ("EXAMPLE: ".. example) end
		return table.concat (t, '\n')
	end
	
	local function parse (lul_parameters)
		local result = {}
		local status = "ok"
		local empty = true
		local function fail (text) result = nil; status = text end
		for group in pairs (syntax) do result[group] = {} end					-- create the output groups
		for param, value in pairs(lul_parameters) do
			empty = false
			local fullName = lookup[param]
			local finalValue = value
			if fullName then 
				local cmd = index[fullName]
				if cmd.set then finalValue = cmd.set[value] end		
				if cmd.vals == "number" then finalValue = tonumber (value) end	-- convert numeric value 
				if not finalValue then											-- value must be in allowed list
					fail (("invalid value '%s' for parameter '%s'"): format (value, fullName) )
					break 
				end
				result[cmd.group][cmd.name] = finalValue
			elseif allowOther then									-- collect 'other' pararameters, or...
				result.other = result.other or {}
				result.other[param] = value
			else													-- ...flag as error
				fail ("unknown parameter: "..param)
				break
			end
		end
		if empty then fail (help()) end								-- return help text if no parameters
		return result, status
	end
	return {parameter = parameter, help = help, parse = parse, version = version, allowOther = allowOther}
end

return {parser = parser}
end

----------
--
-- This Lua package is an API to a subset of the google.visualization javascript library.
-- see: https://google-developers.appspot.com/chart/interactive/docs/index
-- 

function gviz ()

version = "2014.04.30  @akbooer"

local key
local quote, equote, nowt = "'", '', 'null' 
local old = "[\"'\\\b\f\n\r\t]"
local new = { ['"']  = '\\"', ["'"]="\\'", ['\b']="\\b", ['\f']="\\f", ['\n']="\\n", ['\r']="\\r", ['\t']="\\t"}

local string_char = string.char

local function null   (x) return nowt end 
local function user   (x) return x () end 
local function boolean  (x) return tostring (x) end
local function number (x) return tostring (x) or nowt end
local function string   (x, sep) sep = sep or quote; x = tostring(x); return table.concat {sep, x: gsub (old, new), sep} end

-- toJScr() convert Lua data structures to JavaScript
local function toJScr (Lua)
  local lua_type    
  local function value (x) return lua_type [type (x)] (x) end
  local function array (x, X) for i = 1, #x do X[i] = value (x[i]) end; return '['..table.concat(X,',')..']' end
  local function object (x, X) for i,j in pairs (x) do X[#X+1] = string(i, equote)..':'..value (j) end; return '{'..table.concat(X,',')..'}'; end
  local function object_or_array (x) if #x > 0 then return array (x, {}) else return object (x, {}) end; end
  lua_type = {table = object_or_array, string = string, number = number, boolean = boolean, ["nil"] = null, ["function"] = user}  
  return value (Lua)
end

-- DataTable (), fundamental data type for charts
local function DataTable (data)
  local cols, rows = {}, {}

  local function formatDate    (x) return table.concat {"new Date (", x*1e3, ")"} end
  local function formatTime    (x) local t = os.date ("*t", x); return table.concat {"[", t.hour, ",", t.min, ",", t.sec, "]"} end

  local format = {boolean = boolean, string = string, number = number, 
          date = formatDate, datetime = formatDate, timeofday = formatTime}

  local function getNumberOfColumns () return #cols end
  local function getNumberOfRows () return #rows end
  local function addRow (row) rows[#rows+1] = row end -- should clone?
  local function addRows (rows) for _,row in ipairs (rows) do addRow (row) end; end
  local function addColumn (tableOrType, label, id) 
    local info = {}
    if type (tableOrType) ~= "table" 
      then info = {type = tableOrType, label = label, id = id}  -- make a table, or...
      else for i,j in pairs (tableOrType) do info[i] = j end    -- ...make a copy
    end
    if format[info.type] 
      then cols[#cols+1] = info 
      else error (("unsupported column type '%s' in DataTable"): format (info.type or '?'), 2) end
  end
  local function setValue (row, col, value) rows[row][col] = value end

  local function sort (col) -- unlike JavaScript, we start column number at 1 in Lua
    local desc = false
    local function ascending  (a,b) return a[col] < b[col] end  -- TODO: cope with tables (formats and properties)
    local function descending (a,b) return a[col] > b[col] end
    if type (col) == "table" then
      desc = col.desc or desc
      col = col.column
    end
    if desc 
      then table.sort (rows, descending)
      else table.sort (rows, ascending)
    end
  end
  
  
  local function toJavaScript (buffer)
    local b = buffer or {}
    local function p (x) b[#b+1] = x end
    local formatter = {}
    for i,col in ipairs (cols) do formatter[i] = format[col.type] end
    p "\n{cols: "; p (toJScr (cols))
    p ",\nrows: [\n"
    for n,row in ipairs (rows) do
      if n > 1 then p ',\n' end
      p "{c:["
      for i,f in ipairs (formatter) do 
        if i > 1 then p ',' end
        p '{v: '
        local v = row[i] or nowt
        if type(v) == "table" then
          p (f(v.v))
          p ', f: '
          p (string(v.f))
        elseif v == nowt then 
          p (nowt) 
        else
          p (f(v))
        end 
        p '}'
      end
      p "]}"
    end
    p "]\n}"
    if not buffer then return table.concat (b) end
  end

  return  {toJScr = toJavaScript, addColumn = addColumn, addRow = addRow, addRows = addRows,  
          getNumberOfColumns = getNumberOfColumns, getNumberOfRows = getNumberOfRows, 
          setValue = setValue, sort = sort}
end

-- JavaScript() concatentate string buffers and macros into valid script
local function JavaScript(S)
  local b= {}
  for _, x in ipairs (S) do
     if type (x) == "function" then x(b) else b[#b+1] = x end
     b[#b+1] = '\n' 
  end
  return table.concat (b)
 end  

-- ChartWrapper ()
local function ChartWrapper (this)
  this = this or {}
  local function draw (extras, head, body)  
    local t = os.clock ()       
    local id   = this.containerId  or "gVizDiv"
    local opts = {options = this.options or {}, chartType = this.chartType, containerId = id}
    body = body or table.concat {'<div id="', id, '"></div>'}
    head = head or ''
    extras = extras or ''
    local html = JavaScript {[[
    <!DOCTYPE html>
    <html>
      <head>
        <meta charset="utf-8" />
        <script type="text/javascript" src="https://www.google.com/jsapi"></script>
        <script type="text/javascript">
          google.load('visualization','1');
          google.setOnLoadCallback(gViz);
          function gViz() {
              var w = new google.visualization.ChartWrapper(]], toJScr (opts), [[);
              var data = new google.visualization.DataTable(]], this.dataTable.toJScr, [[);
              w.setDataTable(data);
              w.draw();]],
              extras, [[
            }
        </script>
       ]], head, [[
      </head>
      <body>]],
        body,
      [[</body>
    </html>
    ]]}
    t = (os.clock() - t) * 1e3
    if luup then luup.log (
      ("visualization: %s(%dx%d) %dkB in %dmS"): format (this.chartType,  
              this.dataTable.getNumberOfRows(), this.dataTable.getNumberOfColumns(), 
              math.floor(#html/1e3 + 0.5), math.floor(t+0.5) )) end
    return html
  end 

  return {
    draw = draw,
    setOptions    = function (x) this.options = x   end,
    setChartType  = function (x) this.chartType = x   end,
    setContainerId  = function (x) this.containerId = x end,
    setDataTable  = function (x) this.dataTable = x   end,
    }
end

-- Chart (), generic Chart object
local function Chart (chartType)
  local this = ChartWrapper {chartType = chartType}
  local function draw (dataTable, options, extras, head, body)  
    this.setDataTable (dataTable)
    this.setOptions (options)
    return this.draw (extras, head, body)
  end 
  return {draw = draw}
end

-- Methods

return {
      Chart        = Chart,
      DataTable    = DataTable,
      ChartWrapper = ChartWrapper,
      setKey       = function (x) key = x end,
      Table        = function () return Chart "Table"         end,
      Gauge        = function () return Chart "Gauge"         end,
      TreeMap      = function () return Chart "TreeMap"       end,
      BarChart     = function () return Chart "BarChart"      end,
      LineChart    = function () return Chart "LineChart"     end,
      ColumnChart  = function () return Chart "ColumnChart"   end,
      AreaChart    = function () return Chart "AreaChart"     end,
      PieChart     = function () return Chart "PieChart"      end,
      ScatterChart = function () return Chart "ScatterChart"  end,
      OrgChart     = function () return Chart "OrgChart"      end,
    }

end

-----

function json ()
-- JSON encode/decode with full functionality including unicode escapes to UTF-8 encoding.
-- now does pretty-printing of encoded JSON strings.
-- (c) 2013,2014,2015  AK Booer

  local version    = "2015.11.29 @akbooer"   
  
-- 2015.04.10   allow comma before closing '}' or ']'
-- 2015.11.29   improve formatting of nested objects, cache encoded strings

  local default   = 
    {
      huge = "8.88e888",          -- representation for JSON infinity (looks like infinity symbols on their side)
      max_array_length = 1000,    -- not a coding restriction, per se, just a sanity check against, eg {[1e6] = 1}
                                  -- since arrays are enumerated from starting index 1 with all the intervening 'nil' values
    }
    
    
  -- encode (), Lua to JSON
  local function encode (Lua)

        
    local function json_error (text, severity)    -- raise error
      severity = severity or 'error'
      error ( ('JSON encode %s : %s '): format (severity, text) , 0 ) 
    end
    
    local value               -- forward function reference
    local depth = 1           -- for pretty printing
    local encoding = {}       -- set of tables currently being encoded (to avoid infinite self reference loop)
    
    local function null    (x)   return "null"    end       --  nil
    local function boolean (x)   return tostring(x) end       --  true or false
    
    local function number  (x)
      if x ~=  x      then return     "null"  end       --  NaN
      if x >=  math.huge  then return      default.huge   end   -- +infinity
      if x <= -math.huge  then return '-'..default.huge   end   -- -infinity
      return tostring (x) 
    end

    local replace = {
         ['"']  = '\\"',    -- double quote
         ['/']  = '\\/',    -- solidus
         ['\\'] = '\\\\',   -- reverse solidus
         ['\b'] = "\\b",    -- backspace  
         ['\f'] = "\\f",    -- formfeed
         ['\n'] = "\\n",    -- newline
         ['\r'] = "\\r",    -- carriage return
         ['\t'] = "\\t",    -- horizontal tab
      }

    local function new (old)
      return replace [old] or ("\\u%04x"): format (old: byte () ) 
    end
    
    local str_cache  = {}                         -- cache storage for encoded strings
    local ctrl_chars = "%z\001-\031"              -- whole range of control characters
    local old = '[' .. '"' .. '/' .. '\\' .. ctrl_chars .. ']'
        
    local function string (x)
      if not str_cache[x] then
        str_cache[x] =  '"' .. x:gsub (old, new) .. '"'        -- deal with escapes, etc. 
      end
      return str_cache[x]
    end
    
    local function array (x, index)
      local items = {}
      table.sort (index)                -- to find min and max, numeric indices guaranteed 
      local min = index[1] or 1         -- index may be zero length
      local max = index[#index] or 0    -- max less than min for empty matrix
      if min < 1                        then json_error 'array start index is less than 1' end
      if max > default.max_array_length then json_error 'array final index is too large'   end 
      for i = 1, max do
        items[i] = value (x[i])         -- may contain nulls
      end
      return '[' .. table.concat (items, ',') .. ']'
    end
     
    local function object (x, index)  
      local function nl (d) return '\n'..('  '):rep (d), '\n'..('  '):rep (d-1) end
      local items, nl1, nl2 = {}, '', ''
      table.sort (index)                -- nice ordering, string indices guaranteed 
      if #index > 1 then nl1, nl2 = nl(depth) end
      depth = depth + 1
      for i,j in ipairs (index) do
        items[i] = string(j) ..':'.. value (x[j])
      end
      depth = depth - 1
      return table.concat {'{', nl1, table.concat (items, ','..nl1), nl2, '}'}
    end
  
    local function object_or_array (x)
      local index, result = {}
      local only_numbers, only_strings = true, true
      if encoding[x] then json_error "table structure has self-reference" end
      encoding[x] = true
      for i in pairs (x) do
        index[#index+1] = i
        local  kind = type (i)
        if     kind == "string" then only_numbers = false
        elseif kind == "number" then only_strings = false
        else   json_error ("invalid table index type '" .. kind ..'"') end
      end
      if only_numbers then result = array (x, index) 
      elseif only_strings then result = object (x, index) 
      else json_error "table has mixed numeric and string indices" end
      encoding [x] = nil          -- finished encoding this structure
      return result
    end

    local lua_type  = {           -- dispatch table for different types
          table   = object_or_array,
          string  = string,
          number  = number,
          boolean = boolean,
          ["nil"] = null,
        }

    local function err(x) json_error ("can't encode type '".. type(x) .."'" ) end
    
    function value (x)            -- already declared local
      return (lua_type [type(x)] or err) (x)      
    end
    
    -- encode()
    
    local ok, message, json = pcall (function () return nil, value(Lua) end)
    return json, message
    
  end -- encode ()


  -- decode (json), decodes a json string, returning (value) or (value, warning_message) or (nil, error_message) 
  local function decode (json)
  
    local openbrace       = '^%s*(%{)%s*'   -- note that all the search strings are anchored to the start
    local openbracket     = '^%s*(%[)%s*'
    local openquote       = '^%s*%"'
    local endquote_or_backslash = '^([^"\\]-)(["\\])'
    local trailing_spaces   = '^"%s*'

    local numeric_string    = '^%s*([%-]?%d+[%.]?%d*[Ee]?[%+%-]?%d*)%s*'
    local literal_string    = '^%s*(%a+)%s*'
    local colon_separator   = '^:%s*'
    local UTF_code          = '^u(%x%x%x%x)'
  
    local endbrace            = '^%s*(%})%s*'
    local endbracket          = '^%s*(%])%s*'
    local endbrace_or_comma   = '^%s*([%},])%s*'
    local endbracket_or_comma = '^%s*([%],])%s*'
  
    local value         -- forward definition for recursive function
    
    local function json_message (text, position, severity)    -- format error or warning
      severity = severity or 'error'
      return ("JSON decode %s @[%d of %d]: %s at or near '%s'"): 
            format (severity, position, #json, text, json:sub(position,position + 20) )
    end
    
    -- note that inline 'if ... then ... end' calls to this are significantly faster than an 'assert' function call
    local function json_error (...) error ( json_message (...), 0 ) end   -- raise error
      
    
    local valid_literal = {["true"] = {true}, ["false"]= {false}, ["null"] = {nil} }    -- anything else invalid

    local function literal (i)
      local a,b,c = json:find (literal_string, i) 
      if a and valid_literal[c] then return {x = valid_literal[c][1], b = b} end
    end

    local function number (i)
      local a,b,c = json:find (numeric_string, i) 
      if a then return {x = tonumber (c), b = b} end
    end
    
    local function utf8 (codepoint)         -- encode as UTF-8 Basic Multilingual Plane codepoint
      local function encode (x, bits, high)
        local y = math.floor (x / 0x40)
        x = x - y * 0x40
        if y == 0 then return x + high end
        return x + 0x80, encode (y, bits/2, high/2 + 0x80)
      end
      if codepoint < 0x80 then return string.char (codepoint) end 
      return string.reverse (string.char (encode (codepoint, 0x40, 0x80))) 
    end
  
    local replace = {
        ['b']  = "\b",    -- backspace  
        ['f']  = "\f",    -- formfeed
        ['n']  = "\n",    -- newline
        ['r']  = "\r",    -- carriage return
        ['t']  = "\t",    -- horizontal tab
        -- everything else replaced by itself (aside from "\uxxxx", which is handled separately below) 
      }
    
    local function escape_replacement (b)
      local a,c
      c = json:sub (b,b)              -- pick up escaped character
      if c == 'u' then                  -- special UTF hex code
        local i = b
        a,b,c = json:find (UTF_code, b) 
        if not a then json_error ("escape \\u not followed by four hex digits", i) end 
        c = utf8 (tonumber (c, 16))        -- convert to UTF-8 Basic Multilingual Plane codepoint
      end
      return replace[c] or c, b
    end
    
    local function string (i)     
      local a,b,c,t = json:find (openquote, i)
      if not a then return end
      local str = {}
      repeat
        local i = b+1
        a,b,c,t = json:find (endquote_or_backslash, i)
        if not a then json_error ("unterminated string", i) end
        str[#str+1] = c                           -- save the string segment
        if t == '\\' then str[#str+1], b = escape_replacement (b+1) end   -- deal with escapes
      until t == '"' 
      a,b = json: find (trailing_spaces, b) 
      return {x = table.concat(str), b = b} 
    end
    
    local function array (i)
      local a,b,c,b2 = json:find (openbracket, i) 
      if not a then return end
      local n = 0                     -- can't use #x because of possible nil values in x
      local x = {}
      while c ~= ']' do
        a,b2,c = json:find (endbracket, b+1)
        if a then b=b2 break end      -- maybe there was nothing after that last comma
        local val = value (b+1)
        if not val then json_error ("array value invalid", b+1) end
        n = n+1 
        x[n] = val.x
        a,b,c = json:find (endbracket_or_comma, val.b+1)
        if not a then json_error ("array value terminator not ',' or ']'", val.b+1) end
      end
      return {x = x, b = b}
    end
    
    local function object (i)
      local a,b,c,b2 = json:find (openbrace, i) 
      if not a then return end 
      local x = {}
      while c ~= '}' do
        a,b2,c = json:find (endbrace, b+1)
        if a then b=b2 break end      -- maybe there was nothing after that last comma
        local name = string (b+1)
        if not name then json_error ("object name invalid", b+1) end
        a,b = json:find (colon_separator, name.b+1)
        if not a then json_error ("object name not followed by ':'", name.b+1) end
        local val = value (b+1)
        if not val then json_error ("object value invalid", b+1) end
        x[name.x] = val.x
        a,b,c = json:find (endbrace_or_comma, val.b+1)
        if not c then json_error ("object value terminator not ',' or '}'", val.b+1) end
      end 
      return {x = x, b = b}
    end

    function value (i)      -- already declared local 
      return string (i) or object (i) or array (i) or number (i) or literal (i) -- start at given character, test in order of probability
          or json_error ("invalid JSON syntax", i)
    end

    local function parse_json ()
      if type (json) ~= "string" then json = type(json); json_error ("JSON input parameter is not a string", 1) end
      local warning
      local result = value (1)        -- start at first character
      if result.b ~= #json  then 
        warning = json_message ("not all of json string parsed", result.b, "warning")  
      end
      return warning, result.x    
    end
    
    -- decode ()

    local ok, message, Lua = pcall (parse_json, 1)
    return Lua, message

  end  -- decode ()
  
return {
    decode = decode, 
    default = default,
    encode = encode, 
    version = version, 
  
    _VERSION = version,   
}

end

-----

