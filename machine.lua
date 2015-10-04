---
-- A machine represents a given model. A combination of a cpu, memory, a given ROM,.
-- Each model has certain properties, like its timings, the frequency of the cpu,
-- how it deals with contended memory, etc.

local display = require "display"

local machine = {}


function machine.initialize (model_type)
	if model_type == "spectrum_48k" then
		local model = require("machine.spectrum_48")
		local m = model.initialize()
		display.set_machine(m)
		return m
	end
end


return machine
