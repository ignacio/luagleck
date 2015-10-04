local logger = require "logger"

local module = {}

---
-- Given a file name, try to guess the format and load it
--
function module.load (machine, file_name)
	print(file_name)
	-- We're lazy and rely on the file extension
	local ext = file_name:match("%.(%w%w%w)$")
	if not ext then
		logger.info("Can't recognize file '%s'", file_name)
		return false
	end

	local f, err = io.open(file_name, "rb")
	if not f then
		logger.error("%s", err)
		return false
	end
	local data = f:read("*a")
	f:close()
	
	local ok, handler = pcall(require, "file_format."..ext:lower())
	if not ok then
		logger.info("Can't load '%s' files", ext)
		return false
	end

	return handler.load(machine, data)
end

return module
