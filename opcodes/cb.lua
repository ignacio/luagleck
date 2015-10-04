--
-- CBxx opcodes
--

local logger = require "logger"
local common_ops = require "opcodes.common"

local BIT = common_ops.BIT
local SET = common_ops.SET
local RES = common_ops.RES

local RL = common_ops.RL
local RLC = common_ops.RLC
local RR = common_ops.RR
local RRC = common_ops.RRC
local SLA = common_ops.SLA
local SLL = common_ops.SLL
local SRA = common_ops.SRA
local SRL = common_ops.SRL

-- locals for faster access (maybe remove this later)
local contend, read_mem_byte, write_mem_byte


local FLAG_S      = 128
local FLAG_Z      = 64
local FLAG_B5     = 32
local FLAG_H      = 16
local FLAG_B3     = 8
local FLAG_PV     = 4
local FLAG_N      = 2
local FLAG_C      = 1

local FLAG_N_OFF  =	253
local FLAG_PV_OFF = 251
local FLAG_Z_OFF  =	191
local FLAG_H_OFF  =	239
local FLAG_S_OFF  =	127
local FLAG_C_OFF  =	254





---
-- Templates for opcodes creation: RL, RR, SLA, SRA, etc
local regular_pattern = [[
local %s, logger = ...
return function (cpu)
	--logger.debug("%s %s")
	cpu.%s = %s(cpu, cpu.%s)
	cpu.PC = (cpu.PC + 1) & 0xffff
end
]]

-- Variant of the opcode where (HL) is used
local hl_pattern = [[
local %s, logger, read_mem_byte, contend, write_mem_byte = ...
return function (cpu)
	--logger.debug("%s (HL)")
	local HL = cpu.L | cpu.H << 8
	local byte = read_mem_byte(cpu, HL)
	contend(cpu, HL, 1)
	byte = %s(cpu, byte)
	write_mem_byte(cpu, HL, byte)
	cpu.PC = (cpu.PC + 1) & 0xffff
end
]]


local function generate_opcodes (opcodes, base, operation_name, operation)
	local variants = { "B", "C", "D", "E", "H", "L", "(HL)", "A" }
	for i, variant in ipairs(variants) do
		if variant == "(HL)" then
			local source = hl_pattern:format(operation_name, operation_name, operation_name)
			opcodes[base + i - 1] = load(source, source, "t", {})(operation, logger, read_mem_byte, contend, write_mem_byte)
		else
			local source = regular_pattern:format(operation_name, operation_name, variant, variant, operation_name, variant)
			opcodes[base + i - 1] = load(source, source, "t", {})(operation, logger)
		end
	end
end

local function generate_bit_opcodes (opcodes, base, regular_pattern, hl_pattern)
	local i = 0
	local variants = { "B", "C", "D", "E", "H", "L", "(HL)", "A" }
	for bit = 0, 7 do
		for _, variant in ipairs(variants) do
			if variant == "(HL)" then
				local source = hl_pattern:format(bit, variant, bit, variant)
				opcodes[base + i] = load(source, source, "t", {})(BIT, logger, read_mem_byte, contend, write_mem_byte)
			else
				local source = regular_pattern:format(bit, variant, bit, variant)
				opcodes[base + i] = load(source, source, "t", {})(BIT, logger)
			end
			i = i + 1
		end
	end
end


-- operation is either RES or SET
local function generate_res_set_opcodes (opcodes, base, regular_pattern, hl_pattern, operation)
	local i = 0
	local variants = { "B", "C", "D", "E", "H", "L", "(HL)", "A" }
	for bit = 0, 7 do
		for _, variant in ipairs(variants) do
			if variant == "(HL)" then
				local source = hl_pattern:format(bit, bit)
				opcodes[base + i] = load(source, source, "t", {})(operation, logger, read_mem_byte, contend, write_mem_byte)
			else
				local source = regular_pattern:format(bit, variant, variant, bit, variant)
				opcodes[base + i] = load(source, source, "t", {})(operation, logger)
			end
			i = i + 1
		end
	end
end



local opcodes = {}

function opcodes.bind_io (binds)
	read_mem_byte = assert(binds.memory.read_mem_byte)
	write_mem_byte = assert(binds.memory.write_mem_byte)
	contend = assert(binds.memory.contend)

	-- Generate opcodes for RLC: RLC B, RLC C, RLC D, RLC E, RLC H, RLC L, RLC (HL), RLC A
	generate_opcodes(opcodes, 0x00, "RLC", RLC)

	-- Generate opcodes for RRC: RRC B, RRC C, RRC D, RRC E, RRC H, RRC L, RRC (HL), RRC A
	generate_opcodes(opcodes, 0x08, "RRC", RRC)

	-- Generate opcodes for RL: RL B, RL C, RL D, RL E, RL H, RL L, RL (HL), RL A
	generate_opcodes(opcodes, 0x10, "RL", RL)

	-- Generate opcodes for RR: RR B, RR C, RR D, RR E, RR H, RR L, RR (HL), RR A
	generate_opcodes(opcodes, 0x18, "RR", RR)

	-- Generate opcodes for SLA: SLA B, SLA C, SLA D, SLA E, SLA H, SLA L, SLA (HL), SLA A
	generate_opcodes(opcodes, 0x20, "SLA", SLA)

	-- Generate opcodes for SRA: SRA B, SRA C, SRA D, SRA E, SRA H, SRA L, SRA (HL), SRA A
	generate_opcodes(opcodes, 0x28, "SRA", SRA)

	-- Generate opcodes for SLL: SLL B, SLL C, SLL D, SLL E, SLL H, SLL L, SLL (HL), SLL A
	generate_opcodes(opcodes, 0x30, "SLL", SLL)

	-- Generate opcodes for SRL: SRL B, SRL C, SRL D, SRL E, SRL H, SRL L, SRL (HL), SRL A
	generate_opcodes(opcodes, 0x38, "SRL", SRL)

	-- Generate opcodes for BIT operations: BIT 0,B ... BIT 1,B ... ... BIT 7,A
	generate_bit_opcodes(opcodes, 0x40, [[
local BIT, logger = ...
return function (cpu)
	--logger.debug("BIT %s,%s")
	BIT(cpu, %s, cpu.%s)
	cpu.PC = (cpu.PC + 1) & 0xffff
end
]],
[[
local BIT, logger, read_mem_byte, contend, write_mem_byte = ...
return function (cpu)
	--logger.debug("BIT %s,(HL)", cpu.PC)
	local HL = cpu.L | cpu.H << 8
	local byte = read_mem_byte(cpu, HL)
	--logger.debug("(HL)=%s", byte)
	BIT(cpu, %s, byte)
	contend(cpu, HL, 1)
	cpu.PC = (cpu.PC + 1) & 0xffff
end
]])


	-- Generate opcodes for RES operations: RES 0,B ... RES 1,B ... ... RES 7,A
	generate_res_set_opcodes(opcodes, 0x80, [[
local RES, logger = ...
return function (cpu)
	--logger.debug("RES %s,%s")
	cpu.%s = RES(cpu, %s, cpu.%s)
	cpu.PC = (cpu.PC + 1) & 0xffff
end
]],
[[
local RES, logger, read_mem_byte, contend, write_mem_byte = ...
return function (cpu)
	--logger.debug("RES %s,(HL)")
	local HL = cpu.L | cpu.H << 8
	local byte = read_mem_byte(cpu, HL)
	byte = RES(cpu, %s, byte)
	contend(cpu, HL, 1)
	write_mem_byte(cpu, HL, byte)
	cpu.PC = (cpu.PC + 1) & 0xffff
end
]], RES)


	-- Generate opcodes for SET operations: SET 0,B ... SET 1,B ... ... SET 7,A
	generate_res_set_opcodes(opcodes, 0xc0, [[
local SET, logger = ...
return function (cpu)
	--logger.debug("SET %s,%s")
	cpu.%s = SET(cpu, %s, cpu.%s)
	cpu.PC = (cpu.PC + 1) & 0xffff
end
]],
[[
local SET, logger, read_mem_byte, contend, write_mem_byte = ...
return function (cpu)
	--logger.debug("SET %s,(HL)")
	local HL = cpu.L | cpu.H << 8
	local byte = read_mem_byte(cpu, HL)
	byte = SET(cpu, %s, byte)
	contend(cpu, HL, 1)
	write_mem_byte(cpu, HL, byte)
	cpu.PC = (cpu.PC + 1) & 0xffff
end
]], SET)

end

return opcodes
