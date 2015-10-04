local logger = require "logger"

local RETN = require ("opcodes.ed")[0x45] -- grab the RETN function

local sna = {}

local function high_low (value)
	return (value & 0xff00) >> 8, value & 0x00ff
end

-- TODO: Move get_locals and parse to a another module, so we can reuse them.
local function get_locals (level)
	level = (level + 1) or 2

	local t, i = {}, 1
	local name = debug.getlocal(level, i)
	while name and name:sub(1,1) ~= "(" do
		t[name] = i
		i = i + 1
		name = debug.getlocal(level, i)
	end
	return t
end

local function parse (pattern, data)
	local locs, options = {}, {}
	
	for option, loc in pattern:gmatch("([%p%w]-)%s*%->%s*([%w%p]-)\n") do
		table.insert(options, option)
		table.insert(locs, loc)
	end
	local parent_locals = get_locals(2) -- Get the locals of the caller
	local matches = { string.unpack(table.concat(options), data) }
	matches[#matches] = nil	-- Remove unneeded last value from unpack (first unread byte)
	for i, match in ipairs(matches) do
		local l_name = locs[i]
		debug.setlocal(2, parent_locals[l_name], match)
	end
end

local function load48 (machine, data)
	machine.cpu:reset()
	
	local I, HLp, DEp, BCp, AFp, HL, DE, BC, IY, IX, IFF2, R, F, A, SP, int_mode, FE, bank1, bank2, bank3
	parse([[
		<B -> I
		I2 -> HLp
		I2 -> DEp
		I2 -> BCp
		I2 -> AFp
		I2 -> HL
		I2 -> DE
		I2 -> BC
		I2 -> IY
		I2 -> IX
		B  -> IFF2
		B  -> R
		B  -> F
		B  -> A
		I2 -> SP
		B -> int_mode
		B -> FE
		c16384 -> bank1
		c16384 -> bank2
		c16384 -> bank3
	]],
	data)

	local cpu = machine.cpu
	local memory = machine.memory
	local ports = machine.ports

	memory.set_bank(1, bank1)
	memory.set_bank(2, bank2)
	memory.set_bank(3, bank3)
	
	cpu.A = A
	cpu.F = F
	cpu.H, cpu.L = high_low(HL)
	
	cpu.B, cpu.C = high_low(BC)
	cpu.D, cpu.E = high_low(DE)
	cpu.IX = IX
	cpu.IY = IY

	if IFF2 & 0x04 ~= 0 then
		cpu.iff1, cpu.iff2 = 1, 1
	else
		cpu.iff1, cpu.iff2 = 0, 0
	end
	cpu.I = I
	cpu.int_mode = int_mode
	cpu.SP = SP
	cpu.R = R
	cpu.tstates = 0
	cpu.halted = false
	ports.port_fe = FE

	cpu.Ap, cpu.Fp = high_low(AFp)
	cpu.Hp, cpu.Lp = high_low(HLp)
	cpu.Bp, cpu.Cp = high_low(BCp)
	cpu.Dp, cpu.Ep = high_low(DEp)
	
	-- I should redraw the screen first
	RETN(cpu)

	return true
end

function sna.load (machine, data)
	
	if #data == 49179 then
		-- 48K snapshot
		return load48(machine, data)

	elseif #data == 131103 then
		error("128K snapshots are not supported")
	end
end

return sna
