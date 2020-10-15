--module ("L_Netatmo", package.seeall)   -- for debug only

ABOUT = {
  NAME          = "Netatmo",
  VERSION       = "2020.10.15",
  DESCRIPTION   = "Netatmo plugin - Virtual sensors for all your Netatmo Weather Station devices and modules",
  AUTHOR        = "@akbooer",
  COPYRIGHT     = "(c) 2013-2020 AKBooer",
  DOCUMENTATION = "https://github.com/akbooer/Netatmo/tree/master/",
  LICENSE       = [[
  Copyright 2013-2020 AK Booer

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
]]
}


------------------------------------------------------------------------
--
-- NETATMO Vera plugin
--
-- Virtual sensors for all your Netatmo Weather Station devices and modules (
-- temperature, humidity, pressure, CO2, noise & rainfall.)  
-- @akbooer 2013-2017
-- 
-- inspired by the excellent web tutorial by Sébastien Joly "Collecter les données d'une station Netatmo":
-- http://www.domotique-info.fr/2013/05/collecter-les-donnees-dune-station-netatmo-depuis-une-vera-tuto/
--
-- June 2014 - refactored to use updated API
-- special thanks to @Kullematz for fast-turnaround beta testing.
-- and to @reneboer for fixing the UI7 "Can't Detect Device" error, http://forum.micasaverde.com/index.php/topic,16276.msg203653.html#msg203653
-- AND for fixing the UI7 .json files http://forum.micasaverde.com/index.php/topic,16276.msg203683.html#msg203683

------------------------------------------------------------------------
--
-- 20-Aug-2013, Beta 0.1:	http://forum.micasaverde.com/index.php/topic,13489.msg123451.html#msg123451
-- 27-Aug-2013, Beta 0.2: 	http://forum.micasaverde.com/index.php/topic,16276.msg124383.html#msg124383
-- 30-Aug-2013, Beta 0.3: 	http://forum.micasaverde.com/index.php/topic,16276.msg124705.html#msg124705
-- 03-Sep-2013, Release 1:	http://forum.micasaverde.com/index.php/topic,16276.msg125175.html#msg125175
-- 17-Sep-2013, Patch 1:	  http://forum.micasaverde.com/index.php/topic,16276.msg127541.html#msg127541
-- 21-Sep-2013, Beta 1.1:	http://forum.micasaverde.com/index.php/topic,16276.msg127886.html#msg127886
-- 23-Sep-2013, Beta 1.2:	http://forum.micasaverde.com/index.php/topic,16276.msg128194.html#msg128194
-- 05-Nov-2013, Release 2:	http://forum.micasaverde.com/index.php/topic,16276.msg135899.html#msg135899
-- 18-Dec-2013, Beta 2.1:  http://forum.micasaverde.com/index.php/topic,16276.msg142231.html#msg142231
-- 24-Apr-2014, Beta 2.2:  http://forum.micasaverde.com/index.php/topic,16276.msg173122.html#msg173122
-- 09-Jun-2014, New API:   http://forum.micasaverde.com/index.php/topic,16276.msg179917.html#msg179917
-- 12-Nov-2014, Release 3: http://forum.micasaverde.com/index.php/topic,16276.msg203099.html#msg203099
-- Beta 3.1:   UI7 fixes
-- 01-Feb-2015, add hack to remove "<br>" from device variable value
-- 02-Feb-2015, remove history measurement cache and plotting capability (best done elsewhere)
-- 27-Jul-2015, add ALTUI compatibility with formatted display line
-- Beta 3.2:   New API (another one)
-- 04-Dec-2015, add support for new wind module
-- 14-Jan-2016, further use of new API and single device type NetatmoMetric for all non-standard metrics
-- 20-Jan-2016, Release 4: candidate
-- 26-Jan-2016, fix wind child units problem, thanks @korttoma (and for the use of your wind gauge)
--              see: http://forum.micasaverde.com/index.php/topic,35162.msg266522.html#msg266522
--

-- 2017.01.16   fix icon name typo in setMetricIcon, thanks @reneboer
-- 2017.02.14   fix nil value error in format

-- 2018.02.27   add ABOUT global with VERSION for openLuup Plugins page

-- 2019.01.30   move HTTP handler startup to earlier in init code (debug info available)
-- 2019.12.16   fix error in D_NetatmoMetric.xml, update .json file to point to CDN for online icons

-- 2020.01.09   update D_Netatmo.json to use CDN
-- 2020.05.11   quick fix to create 'missing' devices (not found in current modules)

-- 2020.10.15   fix possibly missing "station_name" / "home_name" (thanks @Krisztian_Szabo)


local https 	= require "ssl.https"
local library	= require "L_Netatmo2"

local cli   	= library.cli()
local gviz  	= library.gviz()
local json  	= library.json() 

local Netatmo		      -- Netatmo API object with access methods
local Timestamp		    -- last update
local stationInfo			-- table with latest Netatmo device configuration

-- ServiceId strings for the different sensors

local NetatmoSID = "urn:akbooer-com:serviceId:Netatmo1"
local tempSID    = "urn:upnp-org:serviceId:TemperatureSensor1"
local humidSID   = "urn:micasaverde-com:serviceId:HumiditySensor1"
local genericSID = "urn:micasaverde-com:serviceId:GenericSensor1"
local altuiSID   = "urn:upnp-org:serviceId:altui1" -- Variables = 'DisplayLine1' and 'DisplayLine2'
local batterySID =  "urn:micasaverde-com:serviceId:HaDevice1" -- 'BatteryLevel'

-- Device files for the different sensors

local tempXML		  = "D_TemperatureSensor1.xml"
local humidXML		= "D_HumiditySensor1.xml"
local genericXML	= "D_NetatmoMetric.xml"

--   shorthand for the measurements
local T,H,C,P,N,R,W     = "Temperature", "Humidity", "CO2", "Pressure", "Noise", "Rain", "WindStrength" 

local THCPNRW	= {T=T, H=H, C=C, P=P, N=N, R=R, W=W}				-- lookup table for user-defined sensor types

local metric_types =     -- these can all be written to child devices
{
  [T] = {T, "MinTemp", "MaxTemp", "DewPoint"},
  [H] = {H},
  [C] = {C},
  [P] = {P, "AbsolutePressure"},
  [N] = {N},
  [R] = {R, "SumRain1", "SumRain24"},
  [W] = {W, "GustStrength", "MaxWindStr"},
}

local type_of_metric = {}     -- reverse lookup table for metric type
for t, types in pairs(metric_types) do
  for _,name in ipairs (types) do
    type_of_metric [name] = t
  end
end

-- Luup variable names for Child devices
local unitsVariable		   = "Units"					      -- measurement units
local calibrationOffset  = "CalibrationOffset"		-- calibration parameter
local iconsVariable		   = "IconSet"					    -- which icons to use
local dateDisplay        = "DateDisplay"          -- the formatted measurement date/time
local dateFormat         = "DateFormat"           -- the os.date style format for the above

-- for writing to Luup variables, need serviceId and variable name for each sensor type
-- for creating child devices also need device xml filename
local LuupInfo = setmetatable (
  {	
    [T] = { deviceXML = tempXML,  service = tempSID,  variable = "CurrentTemperature"},
    [H] = { deviceXML = humidXML, service = humidSID, variable = "CurrentLevel"}
  },
  {__index = function ()  -- default for everything else
      return { deviceXML = genericXML, service = genericSID, variable = "CurrentLevel"} 
    end
  }
)

local context = {}        -- for debugging, etc.
local ChildDevice = {}    -- lookup table: ChildDevice [ChildId] = ChildObject
local metric              -- metric handling utilities

---	
-- 'global' program variables assigned in init()

local NetatmoID						  -- Luup device ID
local tokenRefresh					-- interval (in minutes) in which to refresh access tokens
local measurementPoll				-- polling frequency (in minutes) for measurement update
local timeFormat					  -- os.date() compatible format string for timestamp

------------------------------------------------------------------------
--
-- Luup utility routines
-- 

local function log (message)
  luup.log ('Netatmo: '.. (message or '???') )
end

local function get (name, service, device) 
  local x = luup.variable_get (service or NetatmoSID, name, device or NetatmoID)
  return x
end

local function set (name, value, service, device)
  service = service or NetatmoSID
  device = device or NetatmoID
  local old = get (name, service, device)
  if tostring(value) ~= old then 
    luup.variable_set (service, name, value, device)
  end
end

-- get and check UI variables
local function uiVar (name, default, lower, upper)
  local value = get (name) 
  local oldvalue = value
  if value and (value ~= "") then           -- bounds check if required
    if lower and (tonumber (value) < lower) then value = lower end
    if upper and (tonumber (value) > upper) then value = upper end
  else
    value = default
  end
  value = tostring (value)
  if value ~= oldvalue then set (name, value) end   -- default or limits may have modified value
  return value
end

--get device Variables, creating with default value if non-existent
local function devVar (name, default, deviceNo)
  local value = get (name, NetatmoSID, deviceNo) 
  if not value then
    value = default                   -- use default value
    set (name, default, NetatmoSID, deviceNo)     -- create missing variable with default value 
  end
  return value
end

-- parent device icons
local function setNetatmoIcon (value)
  local index = {clear = 0, blue = 1, green = 2, yellow = 3, red = 4}
  if index[value] then set (iconsVariable, index[value]) end
end

-- child device icons THCPNRW
-- corresponding to files NetatmoMetric_0.png, ... NetatmoMetric_250.png
-- to make it work on UI5 and UI7
local function setMetricIcon (value, device)
  local icons = {T, H, C, P, N, R, W, "CO2_low", "CO2_med", "CO2_high"}
  local index = {generic = 0}
  for i,n in ipairs (icons) do index[n] = i end
  set (iconsVariable, index[value] or 0, nil, device)
end

------------------------------------------------------------------------
--
-- Dew point calculation using Magnus formula: http://en.wikipedia.org/wiki/Dew_point
--
-- however, @watou uses: local dewpoint = (rh/100)^(1/8) * (112 + (0.9 * t)) - 112 + (0.1 * t)
-- see: https://github.com/watou/lua-weathermetrics/blob/master/weathermetrics.lua
-- ...there is about 0.1 degree difference, at worst, over any reasonable range.
-- 
local function dewPoint (T, RH)
  -- a,b,c taken from a 1980 paper by David Bolton in the Monthly Weather Review
--  local a = 6.112     -- a is not used in this approximation
  local b,c = 17.67, 243.5
  RH = math.max (RH or 0, 1e-3)
  local gamma = math.log (RH/100) + b * T / (c + T) 
  return c * gamma / (b - gamma)
end


------------------------------------------------------------------------
--
-- METRICS handling
--

-- admin data includes amongst other things: 
-- unit (temperature, rain): 0 -> metric system, 1 -> imperial system 
-- pressureunit: 0 -> mbar, 1 -> inHg, 2 -> mmHg 
-- windunit: 0 -> kph, 1 -> mph, 2 -> m/s, 3 -> beaufort, 4 -> knot
-- 
local function Metrics (admin)

  local function UnitsTable (unit, decimalPlaces, multiplier, offset)  -- table constructor
    return {unit = unit, dp = decimalPlaces or 0, m = multiplier or 1, c = offset or 0}
  end

  local unitsLookup = {	-- these indices correspond to the Netatmo configuration settings values
    [T] =	{
      [0] = UnitsTable ('°C', 1),
      [1] = UnitsTable ('°F', 1, 9/5, 32)
    },
    [P] = {
      [0] = UnitsTable ('mbar', 1),
      [1] = UnitsTable ('inHg', 2, 0.0295333727),
      [2] = UnitsTable ('mmHg', 1, 0.7500616830)
    },
    [H] = { [0] = UnitsTable ('%') },
    [C] = { [0] = UnitsTable ('ppm') },
    [N] = { [0] = UnitsTable ('dB') },
    [R] = {   
      [0] = UnitsTable ('mm', 1), 
      [1] = UnitsTable ('in', 2, 1 / 25.4), 
    },
    [W] = {  -- windunit: 0 -> kph, 1 -> mph, 2 -> m/s, 3 -> beaufort, 4 -> knot 
      [0] = UnitsTable ('kph', 0), 
      [1] = UnitsTable ('mph', 0, 0.621371), 
      [2] = UnitsTable ('m/s', 1, 1000 / 60 /60), 
      [3] = UnitsTable ('beaufort', 0, 1), -- TODO: beaufort calculation is non-linear: fractional power
      -- v = 0.836 B^3/2 m/s, see https://en.wikipedia.org/wiki/Beaufort_scale 
      [4] = UnitsTable ('knot', 1, 0.539957),
    },
  }

  -- get measurement units from Netatmo settings
  -- only Temperature, Pressure, and Rain and Wind configurable at this time 
  -- (other sensors have 'universal' units of %, ppm, dB)
  local units = {}
  if admin then							-- set T, P, R, and P units to specified units

    local temp = admin.unit
    log ('admin.unit = ' .. (temp or '?') )
    units[R]    = unitsLookup [R] [temp or 0]
    units[T]    = unitsLookup [T] [temp or 0]

    local pres = admin.pressureunit
    log ('admin.pressureunit = ' .. (pres or '?') )
    units[P]    = unitsLookup [P] [pres or 0]

    local wind = admin.windunit
    log ('admin.windunit = ' .. (wind or '?') )
    units[W]    = unitsLookup [W] [wind or 0]

  else
    log 'admin data not found'
  end

  for sensor, info in pairs (unitsLookup) do
    units[sensor] = units[sensor] or info[0]					-- set all undefined units to default values
  end

  -- given a metric name, convert to required units, possibly adding offset
  local function format (sensor, value, offset)
    local localUnit
    local conversion = units[type_of_metric[sensor] or '']
    if conversion then
      localUnit  = conversion.unit 	    -- unit converted to local preference
      value = (value or 0) * conversion.m + conversion.c + (offset or 0)	-- apply units conversion + 2017.02.14
      value = ("%0."..conversion.dp.."f"): format (value)		      -- specify precision for readings
    end
    return value, localUnit 
  end

  local function unit (m)
    local raw_unit, new_unit = ''
    local t = type_of_metric[m]
    if t then
      raw_unit = unitsLookup [t][0].unit or ''
      new_unit = units[t].unit or ''
    end
    return new_unit or raw_unit, raw_unit
  end

  return {
    format  = format,
    unit    = unit,
  }

end


------------------------------------------------------------------------
--
-- Netatmo object with basic low-level Netatmo API calls
-- see documentation at: http://dev.netatmo.com/

-- netatmoAPI (client_id, client_secret), netatmo API object constructor with application registration info parameters
-- returns methods to authenticate, refresh tokens, get user options, devices and measurements
local function netatmoAPI (client_id, client_secret)

  local access_token, refresh_token 	-- updated periodically after authorisation

  -- HTTPS_request(),  HTTPS GET/POST with Lua table body definition and JSON return
  -- see http://notebook.kulchenko.com/programming/https-ssl-calls-with-lua-and-luasec
  local function HTTPS_request (url, params)
    local req, Json 
    if params then 	-- it's a POST (otherwise a GET)
      req = {}
      for name,value in pairs (params) do 
        req[#req+1] = table.concat {name, '=', value}
      end	-- build the parameter string
      req = table.concat (req,'&')	
    end
    local reply,code = https.request (url, req)		-- body, code, headers, status
    if code ~= 200 then
      log ('HTTPS error = ' .. (code or 'nil'))
      Json = {}
    else
      Json = json.decode (reply)
    end
    return Json
  end

  local function authenticate (username, password, scope)
    scope = scope or "read_station"
    local reply = HTTPS_request ("https://api.netatmo.net/oauth2/token",	
      {
        grant_type    = "password",
        client_id     = client_id,
        client_secret = client_secret,
        username      = username, 
        password      = password,
        scope         = scope,
        } )
    access_token, refresh_token = reply.access_token, reply.refresh_token
    return access_token ~= nil
  end

  local function refresh_tokens ()
    local reply = HTTPS_request ("https://api.netatmo.net/oauth2/token",	
      {
        grant_type    = "refresh_token",
        client_id     = client_id,
        client_secret = client_secret,
        refresh_token = refresh_token
        } )
    if reply.refresh_token then     -- only rotate if valid, else retry next time
      access_token, refresh_token = reply.access_token, reply.refresh_token
    end
    return reply.refresh_token ~= nil
  end

  local function get_stationsdata ()
    local reply = HTTPS_request ("https://api.netatmo.net/api/getstationsdata", 
      {
        access_token = access_token
        } )
    return  reply.body	-- ALL device info!
  end

  -- get_measurements ()
  local function get_measurements (typelist, device, module, scale)
    local reply = HTTPS_request ("https://api.netatmo.net/api/getmeasure",
      {
        access_token  = access_token,
        device_id     = device, 
        module_id     = module,
        type          = typelist,
        scale         = scale or "max",
        date_end      = "last"
        } )
    if reply.body and not reply.body[1].error then 
      return reply.body[1].value[1]	-- NB. this is a {list} of values matching the requested typelist
    end
  end

  return {  --  methods
    authenticate 		  = authenticate, 
    refresh_tokens 		= refresh_tokens, 
    get_measurements 	= get_measurements, --  NOT NEEDED (in this plugin) WITH NEW API getstationsdata
    get_stationsdata = get_stationsdata,
  }

end  -- netatmoAPI module

------------------------------------------------------------------------
--
-- High-level routines
--
-- key data structure for these routines is:
-- stations[stationName][moduleName] = {deviceID, moduleId, measurements}	(including base device in modules)

-- build the measurements list from the device/module dashboard_data
local function build_measurements (m)
  local x = {Battery= m.battery_percent}        -- throw in the battery level for good measure
  for name,value in pairs (m.dashboard_data or {}) do   -- 2019.01.30  TODO: avoid nil pointer... but WHY?
    -- remove underscores and change to CamelCase
    local Name = name: gsub ("_(%w)", string.upper): gsub ("^%w", string.upper) 
    if type (value) ~= "table" then x[Name] = value end   -- ignore table structures
  end 
  -- add dewpoint calculation for modules with temperature and humidity
  if x[T] and x[H] then 
    local dp = dewPoint (x[T],x[H])
    x["DewPoint"] = dp - dp % 0.1 
  end
  return x
end

-- build the stations table
local function station_data (info)
  local stations = {}
  local stationName = {}		-- lookup table for _id --> name translation
  for i,d in ipairs (info.devices) do	-- go through the devices
    local station_name = d.station_name or d.home_name or ("STATION_" .. i)
    d.station_name = station_name       -- 2020.10.15  fix missing station name
    stationName[d._id] = station_name
    log ("station name: " .. (d.station_name or '?'))
    stations[station_name] = {[d.module_name] = 
      {deviceId = d._id, measurements = build_measurements (d)} }  		-- base device has no module _id
    log ("module name: " .. (d.module_name or '?'))

    for _,m in ipairs (d.modules) do -- go through the modules assigning to correct station
      log ("module name: " .. (m.module_name or '?'))
      stations [d.station_name][m.module_name] = 
      {deviceId = m.main_device, moduleId = m._id, measurements = build_measurements (m) }
    end  
  end
  return stations
end


------------------------------------------------------------------------
--
-- HTTP request handler routines: reports and plots
--

local function mapSensors (fct)		-- calls fct with station / module / sensor / value information
  for station, s in pairs (stationInfo) do
    for module, m in pairs (s) do
      for sensor, value in pairs (m.measurements) do
        if type_of_metric[sensor] then             -- only save interesting variables
          fct (station, module, sensor, value)
        end
      end
    end
  end
end

local function formattedSensor (sensor)
  local subscript = {CO2 = "CO<sub>2</sub>"}
  return subscript[sensor] or sensor
end


--org chart
local function orgChart (p)		-- a different visualization of the station / module / sensor structure
  local d = gviz.DataTable ()
  local root = "Vera / Netatmo<br><br>Last Update "..Timestamp
  d.addColumn ("string", "Item")
  d.addColumn ("string", "Parent")
  d.addColumn ("string", "ToolTip")
  for station, s in pairs (stationInfo) do
    d.addRow {station, root, ''}
    for module, m in pairs (s) do
      d.addRow {module, station, ''}
      local parent = module
      for _,sensor in ipairs {T,H,C,P,N,R, 
          "SumRain1","SumRain24", W, "WindAngle", "GustStrength", "MaxWindStr"} do		-- enforce specific ordering
        local value = (m.measurements or {})[sensor]
        if value then
          local moduleSensor = module..sensor
          local name = formattedSensor (sensor)
          local v,u  = metric.format (sensor, value)
          local this = {v = moduleSensor, f = table.concat {name,"<br>",v," ",u} }
          d.addRow {this, parent, ''}
          parent = moduleSensor
        end
      end
    end
  end
  local options = {allowHtml = true, width = p.options.width}
  local chart = gviz.Chart "OrgChart"
  return chart.draw (d, options)
end

-- metric list 
local function list_Devices (p)
  local d = gviz.DataTable ()

  local function buildRow (station, module, sensor, value)
    local _, raw_unit = metric.unit (sensor)
    d.addRow {station, module, formattedSensor(sensor), value, raw_unit, metric.format (sensor, value)}
  end
  d.addColumn ("string", "Station")
  d.addColumn ("string", "Module")
  d.addColumn ("string", "Sensor")
  d.addColumn ("number", "Raw Value")
  d.addColumn ("string", "Raw Units")
  d.addColumn ("number", "Local Value")
  d.addColumn ("string", "Local Units")
  mapSensors (buildRow)
  local options = {allowHtml = true, height= p.options.height or 700, width = p.options.width}
  local chart = gviz.Table ()
  return chart.draw (d, options)
end


-- diagnostic dump of CONFIGURATION
local function diagnostics ()
  local idx = {}
--  local function other (x) return "--- not number, string, or table --- "..type(x) end
  local function tbl (x)  return json.encode (x) end
  local convert = {number = tostring, string = tostring, table = tbl}
  local info = {"NETATMO CONFIGURATION PAGE at " .. os.date "%c\n"}
  for a in pairs (context) do idx[#idx+1] = a end
  table.sort (idx)
  local b,c
  for _,a in ipairs (idx) do
    b = context[a]
    c = (convert[type(b)] or type) (b)
    info[#info+1] = table.concat {a, " = ", c or "NIL ???"} 
  end
  return table.concat (info, '\n\n')
end

-- HTTP request handler
_G.HTTP_Netatmo = function (_, lul_parameters)
  local function exec ()
    local p, status = cli.parse (lul_parameters)
    local html = status
    if p then
      local reportType = p.actions.report
      if reportType 
      then html = ({list = list_Devices, organization = orgChart, diagnostics = diagnostics}) [reportType] (p) 
      end
    end
    return html
  end
  local _, result = pcall (exec)   -- catch any errors
  return result
end

------------------------------------------------------------------------
--
-- Vera Luup routines

local function update_child (child, sensor, metrics)

  local function setChildVariable (name)
    local value = metric.format (name, metrics[name])
    set (name, value, genericSID, child.deviceNo)   -- use correct variable name or secondary pseudo-name
    return value
  end

  -- each child gets the primary metric from its module, with appropriate units conversion, but NO offset

  setChildVariable (sensor)
  local updated = os.time()
  set ("LastUpdated", updated, NetatmoSID, child.deviceNo)

  -- for the primary measurement of this sensor, calibrate WITH possible user-supplied offset,

  local metric_type = type_of_metric[sensor]
  local Luup = LuupInfo[metric_type] 		-- {deviceXML = dev, service = srv, variable = var}
  local val, unit = metric.format (sensor, metrics[sensor], child.offset)
  set (Luup.variable, val, Luup.service, child.deviceNo)   -- use correct serviceId and name

  -- write a formatted line to the AltUI display variable

  local line = table.concat {val, ' ', unit}
  set ("DisplayLine1", line, altuiSID, child.deviceNo)    -- ALTUI compatibility!!
  set (Luup.variable, val, Luup.service, child.deviceNo)

  -- battery level: convert into something that the UI understands

  local level = metrics["Battery"]
  if level then
    set ("BatteryLevel", level, batterySID, child.deviceNo)
  end

  do -- TEMPERATURE
    if (sensor == T) or (sensor == "MaxTemp") or (sensor == "MinTemp") then    -- add the max/min values
      local format = get (dateFormat, NetatmoSID, child.deviceNo)
      local dateMax = metrics ["DateMaxTemp"]
      local dateMin = metrics ["DateMinTemp"]
      if sensor == "MaxTemp" then 
        if format ~= '' then set (dateDisplay, os.date (format, dateMax), NetatmoSID, child.deviceNo) end
      else
        setChildVariable "MinTemp" 
        setChildVariable "DateMinTemp"
        set ("DateMinTemp", dateMin, Luup.service, child.deviceNo) 
      end
      if sensor == "MinTemp" then
        if format ~= '' then set (dateDisplay, os.date (format, dateMin), NetatmoSID, child.deviceNo) end
      else
        setChildVariable "MaxTemp"
        setChildVariable "DateMaxTemp"
        set ("DateMaxTemp", dateMax, Luup.service, child.deviceNo) 
      end
    end
  end

  do -- HUMIDITY
    if sensor == H then
      -- nothing extra
    end
  end

  do -- RAIN
    if sensor == R then   -- add hourly and daily cumulative values
      setChildVariable "SumRain1"
      local r24 = setChildVariable "SumRain24"
      set ("DisplayLine2", "24hrs: " .. r24, altuiSID, child.deviceNo)    -- ALTUI compatibility!!
    end					
  end					

  do -- PRESSURE
    if sensor == P then   -- add sea-level pressure
      setChildVariable "AbsolutePressure"
      set ("DisplayLine2", "..." .. (metrics["PressureTrend"] or '?'), altuiSID, child.deviceNo) -- ALTUI compatibility!!
    end
  end

  do -- CO2
    if sensor == C then 	-- change icon with measurement thresholds
      local value = metrics[sensor]
      local icon
      if 		  value < 1000 then icon = "CO2_low"		-- green
      elseif  value < 2000 then icon = "CO2_med"		-- yellow
      else 	    		            icon = "CO2_high"	-- red
      end
      setMetricIcon (icon, child.deviceNo)
    end
  end

  do -- WIND
    if sensor == W then   
      setChildVariable "MaxWindStr"
      setChildVariable "DateMaxWindStr"

      setChildVariable "GustStrength"
      setChildVariable "GustAngle"

      local v = setChildVariable "WindAngle"
      setChildVariable "MaxWindAngle"
      set ("DisplayLine2", (v or '?') .. '°', altuiSID, child.deviceNo)    -- ALTUI compatibility!!
    end
  end

end

-- construct and deconstruct child device IDs
-- they are of the form: [module MAC address]-[sensor type]

local function childID ()
  return {
    name = function (mac, type) return mac .. '-' .. type	end,
    mac  = function (ID) 		return ID:match "(.+)-%w+"	end,
    type = function (ID) 		return ID:match ".+-(%w+)"	end
  }
end

-- save all the measurements as Luup variables on this master plugin device
-- and any child devices for specific individual measurements (with units and calibration adjustments)
local function updateLuupVariables (stations)
  local batteryWarning = 10       -- percentage threshold level to generate warning
  local ID = childID ()
  for _, s in pairs (stations) do
    for module, m in pairs (s) do
      for sensor, value in pairs (m.measurements) do 

        -- write metric (unchanged) to parent device
        set (module..sensor, value, genericSID)  -- write to master device variables (as GenericSensor1)

        -- write to child device, if present
        local child = ChildDevice[ID.name (m.moduleId or m.deviceId, sensor) ]	-- use device ID if no module
        if child then															-- write to child device variables with units and offset
          update_child (child, sensor, m.measurements)
        end

        -- Battery levels
        if sensor == "Battery" and value < batteryWarning then 
          setNetatmoIcon "blue"                  -- change device icon to blue
        end

      end
    end
  end

  -- parent timestamp udate
  Timestamp = os.date (timeFormat)
  set ('Timestamp', Timestamp )		             -- say when this happened
  set ('DisplayLine1', Timestamp, altuiSID)     -- make it work in ALTUI too !
end

-- rotate the access keys
_G.refreshNetatmo = function ()
  local delay = tokenRefresh * 60       -- normal periodic refresh of access tokens
  local ok = Netatmo.refresh_tokens () 
  if ok then 
    setNetatmoIcon "green"
    log 'Access tokens rotated' 
  else 
    delay = 600                    -- retry in 10 minutes
    setNetatmoIcon "yellow"
    log 'Access token rotation FAILURE, retrying in 10 minutes' 
  end
  luup.call_delay ('refreshNetatmo', delay, "")   -- reschedule
  return
end

-- get new measurements and update Luup variable on this device and child devices
_G.pollNetatmo = function()
  luup.call_delay ('pollNetatmo', measurementPoll * 60, "")	       -- periodic poll of measurements
  local info, status = Netatmo.get_stationsdata ()
  if info then
    setNetatmoIcon "green"               -- although it may be blue because of low batteries...
    stationInfo = station_data (info)		-- create useful table with device configuration
    updateLuupVariables (stationInfo)    -- ...this will change it back to blue if needed
  else
    log (status)
    setNetatmoIcon "yellow"
  end
  local AppMemoryUsed =  math.floor(collectgarbage "count")         -- app's own memory usage in kB
  set ("AppMemoryUsed", AppMemoryUsed) 
  log "poll complete"
  collectgarbage()                                                  -- tidy up a bit
end


------------------------------------------------------------------------
--
-- Initialisation 
--

-- find current family, device No, indexed by altid
local function existing_children ()
  local parent = NetatmoID
  local family = {}
  for i,d in pairs(luup.devices) do
    if d.device_num_parent == parent then
      family[d.id] = i
    end
  end
  return family
end

-- set up child devices with appropriate device files
local function create_children (stations, childSensors)
  local ID = childID ()											-- access child ID name utilities
  local makeChild = {}											-- table of sensors for which to create child devices
  for c in childSensors:gmatch "%w" do			-- looking for individual (uppercase) letters...
    local sensor = THCPNRW[c] 
    if sensor then makeChild[sensor] = true end
  end

  local family = existing_children ()
  
  local child_devices = luup.chdev.start(NetatmoID);				-- create child devices...
  for _, s in pairs (stations) do
    for module, m in pairs (s) do
      local adopt  = {}               -- list for further adoption as children of an extended family

      local extras = devVar (module.."Children", '', NetatmoID)      -- list of adopted children
      for name in extras: gmatch "%w+" do
        if type_of_metric[name] then adopt[name] = true end       -- ...only if we know the type of metric
      end

      for sensor in pairs (m.measurements) do   
        local child = ID.name (m.moduleId or m.deviceId, sensor)
        family[child] = nil               -- remove from current list
        if makeChild[sensor] or adopt[sensor] then			 -- only create or adopt children for required sensor types
          luup.chdev.append(
            NetatmoID,  								   -- parent (this device)
            child_devices, 								 -- pointer from above "start" call
            child,										     -- child ID
            module..' - '..sensor,				 -- child device description 
            "", 										       -- serviceId defined in device file
            LuupInfo[type_of_metric[sensor]].deviceXML,   -- device file
            "",											       -- no implementation file required
            "",											       -- no parameters to set 
            false)										     -- not embedded child devices (can go in any room)
        end 
      end
    end
  end 
  
  -- 2020.05.11 create 'missing' devices
  for child, devNo in pairs(family) do
    local missing = "device '[%d]%s' not found in current list of modules"
    local d = luup.devices[devNo]
    local dtype = d.device_type
    dtype = (dtype:match "Temperature" and T) or (dtype:match "Humidity" and T) or 'X'
    log (missing: format (devNo, d.description))
    luup.chdev.append(
      NetatmoID,  								   -- parent (this device)
      child_devices, 								 -- pointer from above "start" call
      child,										     -- child ID
      d.description,				         -- child device description 
      "", 										       -- serviceId defined in device file
      LuupInfo[dtype].deviceXML,    -- device file
      "",											       -- no implementation file required
      "",											       -- no parameters to set 
      false)										     -- not embedded child devices (can go in any room)
  end

  luup.chdev.sync(NetatmoID, child_devices)	-- any changes in configuration will cause a restart at this point

  for deviceNo, d in pairs (luup.devices) do							-- pick up the child device numbers from their IDs
    if d.device_num_parent == NetatmoID then
      local sensor = ID.type (d.id)
      local unit = metric.unit (sensor) or 'unknown'
      log ('Child = '..deviceNo.. ' '..d.id..' ('..unit.. ')' )

      -- calibration offset
      local offset = devVar (calibrationOffset, "0", deviceNo) 	-- default is no offset
      -- units
      set (unitsVariable, unit, NetatmoSID, deviceNo)      
      -- icon
      setMetricIcon (sensor, deviceNo)

      ChildDevice [d.id] = {deviceNo = deviceNo, offset = tonumber (offset) or 0}
    end
  end
  return ChildDevice
end

--@reneboer, http://forum.micasaverde.com/index.php/topic,16276.msg203653.html#msg203653
-- On UI5 it have a true or false as parameter, the 0,1,2 should be UI7 specific.  
local function set_failure (status)
  if (luup.version_major < 7) then status = status ~= 0 end        -- fix UI5 status type
  luup.set_failure(status)
end

-- init () called on startup
function init  (lul_device)	
  NetatmoID = lul_device			-- save the global device ID
  set ('Version', ABOUT.VERSION)		-- save code version number in UI variable

  -- Get user-defined info, creating the variables in the UI with defaults if required

  local client_id		   = uiVar ("ClientID", 		"Register at http://dev.netatmo.com/")
  local client_secret	 = uiVar ("ClientSecret",	"Register at http://dev.netatmo.com/")
  local username       = uiVar ("Username", 		"???")				-- need better way to secure credentials
  local password		   = uiVar ("Password", 		"???")
--  local syslogInfo     = uiVar ("Syslog",       "")         -- send to syslog if IP address and Port 'XXX.XX.XX.XXX:YYY'

  tokenRefresh 		     = uiVar ("TokenRefresh",    120, 10, 180)		-- get update intervals
  measurementPoll 	   = uiVar ("MeasurementPoll",  10,  10,  60)		-- 10 minute default for polling

  timeFormat			= uiVar ("TimeFormat",		"%a %H:%M")	
  local childSensors		= uiVar ("ChildSensors",	"THCPNRW")			-- create children for these sensor types

  cli = cli.parser "&report=org"

  cli.parameter ("actions", "report", {"report", "show", "page"}, {"list","organization","diagnostics"}, "show device status, configuration, or diagnostics")
--	cli.parameter ("actions", "plot", "plot", "string", "plot specified sensor")
--
  cli.parameter ("options",    "width",     "width",   "number",    "HTML output width")


  -- create Netatmo object
  log "Netatmo initialisation..."
  setNetatmoIcon "red"       -- turn icon red until authorization successful
  Netatmo = netatmoAPI (client_id, client_secret)

  -- username / password authentication
  local ok = Netatmo.authenticate (username, password)
  if not ok then 
    log "Authorisation failed"
    set_failure (2) 
    return false, "Authorisation failed", "Netatmo" 
  end

  -- get device configuration
  local info, status = Netatmo.get_stationsdata ()
  if not info then 
    log "Failed to get stations data"
    log (status)                  -- more info about failure
    set_failure (1)
    return false, "Failed to get stations data", "Netatmo" 
  end

  luup.register_handler ("HTTP_Netatmo", "Netatmo") -- HTTP request handler (2019.01.30  moved earlier in code)

  metric = Metrics (info.user.administrative)     -- set up metric unit conversions, etc.
-- TODO: FOR TESTING ONLY: override units
-- unit (temperature, rain): 0 -> metric system, 1 -> imperial system 
-- pressureunit: 0 -> mbar, 1 -> inHg, 2 -> mmHg 
-- windunit: 0 -> kph, 1 -> mph, 2 -> m/s, 3 -> beaufort, 4 -> knot
--  metric = Metrics {unit = 1, pressureunit = 1, windunit = 2}
--
  stationInfo = station_data (info)				   -- create useful table with device configuration
  context = {NetatmoConfig = info, StationInfo = stationInfo}
  context.VERSION = ABOUT.VERSION

  log 'creating child devices...'
  create_children (stationInfo, childSensors)     -- child device structure for Netatmo modules and sensors
  log '...child devices created'

  luup.call_delay ('refreshNetatmo', 10, "")			 -- periodic refresh of access tokens
  luup.call_delay ('pollNetatmo', 20, "")				   -- periodic poll of measurements

  set_failure (0)
  setNetatmoIcon "clear"       -- clear icon after successful initialization
  return true, "OK", "Netatmo"
end

-----
