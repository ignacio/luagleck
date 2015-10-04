local type, tostring, select = type, tostring, select
local string_format = string.format
local table_concat = table.concat
local setmetatable = setmetatable
local unpack = unpack or table.unpack

local function LogArgumentsFormatter (...)
	local args = {...}
	for i = 1, select("#", ...) do
		local arg = args[i]
		local arg_type = type(arg)
		if arg_type ~= "string" and arg_type ~= "number" then
			args[i] = tostring(arg)
		end
	end
	return unpack(args)
end

local function ArgumentsToStrings (t, ...)
	for i = 1, select("#", ...)  do
		local arg = select(i, ...)
		local arg_type = type(arg)
		if arg_type ~= "string" and arg_type ~= "number" then
			t[#t + 1] = tostring(arg)
		else
			t[#t + 1] = arg
		end
	end
	return t
end

local function BuildMessage (fmt, ...)
	local msg
	if type(fmt) ~= "string" then
		msg = { tostring(fmt) }
		ArgumentsToStrings(msg, ...)
		msg = table_concat(msg, "\t")
	else
		if fmt:find("%%") then
			msg = string_format(fmt, LogArgumentsFormatter(...))
		else
			msg = { fmt }
			ArgumentsToStrings(msg, ...)
			msg = table_concat(msg, "\t")
		end
	end
	return msg
end

local function MakeLogger ()
	local t = {}
	t.debug = function(fmt, ...)
		local msg = BuildMessage(fmt, ...)
		print("[DEBUG] " .. msg)
		return msg
	end

	t.info = function(fmt, ...)
		local msg = BuildMessage(fmt, ...)
		print("[INFO ] " .. msg)
		return msg
	end
	
	t.warning = function(fmt, ...)
		local msg = BuildMessage(fmt, ...)
		print("[WARN ] " .. msg)
		return msg
	end
	
	t.error = function(fmt, ...)
		local msg = BuildMessage(fmt, ...)
		print("[ERROR] " .. msg)
		return msg
	end
	
	t.fatal = function(fmt, ...)
		local msg = BuildMessage(fmt, ...)
		print("[FATAL] " .. msg)
		return msg
	end
	
	t.profile = function(fmt, ...)
		local msg = BuildMessage(fmt, ...)
		print("[PROFI] " .. msg)
		return msg
	end
	
	t.__tostring = function()
		return tostring(logger)
	end

	setmetatable(t, t)
	return t
end

return MakeLogger()
