--
-- DDCBxx opcodes
--

local logger = require "logger"
local common_ops = require "opcodes.common"

local BIT = common_ops.BIT
local BIT_INDEX = common_ops.BIT_INDEX
local RLC = common_ops.RLC
local RRC = common_ops.RRC
local RL = common_ops.RL
local RR = common_ops.RR
local SLA = common_ops.SLA
local SLL = common_ops.SLL
local SRA = common_ops.SRA
local SRL = common_ops.SRL

---
-- Will be bound to the actual functions to be called later in bind_io
--
local contend_read_no_mreq
local read_mem_byte
local write_mem_byte

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


local opcodes = {}

function opcodes.bind_io (binds)
	contend_read_no_mreq = assert(binds.memory.contend_read_no_mreq)
	read_mem_byte = assert(binds.memory.read_mem_byte)
	write_mem_byte = assert(binds.memory.write_mem_byte)
end

-- "foo" is used to skip, for instance, RLC IX
-- LD B,RLC (REGISTER+dd)
local instructions = { RLC, RRC, RL, RR, SLA, SRA, SLL, SRL }
local i = 0
for _, instruction in ipairs(instructions) do
	local regs = {"B", "C", "D", "E", "H", "L", "foo", "A"}
	for _, reg in ipairs(regs) do
		opcodes[i] = function (cpu, offset)
			local address = cpu.IX + offset
			local byte = read_mem_byte(cpu, address)
			contend_read_no_mreq(cpu, address, 1)
			cpu[reg] = instruction(cpu, byte)
			write_mem_byte(cpu, address, cpu[reg])
		end
		i = i + 1
	end
end

-- RLC IX
opcodes[0x06] = function (cpu, offset)
	local address = cpu.IX + offset
	local byte = read_mem_byte(cpu, address)
	contend_read_no_mreq(cpu, address, 1)
	byte = RLC(cpu, byte)
	write_mem_byte(cpu, address, byte)
end

local base = 0x40
for bit=0, 7 do
	for i=0,7 do--for i=0x40, 0x7f, 8 do
		opcodes[base + i] = function (cpu, offset)
			--logger.debug("BIT 0,IX+nn")
			local address = cpu.IX + offset
			local byte = read_mem_byte(cpu, address)
			contend_read_no_mreq(cpu, address, 1)
			BIT_INDEX(cpu, bit, byte, address)
		end
	end
	base = base + 8
end

-- LD B,RES 0,(IX+dd), etc
local base = 0x80
for b=0, 7 do
	local regs = {"B", "C", "D", "E", "H", "L", "foo", "A"}
	for i, reg in ipairs(regs) do
		local bit = 1 << b -- 2^bit
		bit = bit ~ 0xff -- invert
		if reg ~= "foo" then
			opcodes[base + i - 1] = function (cpu, offset)
				local address = cpu.IX + offset
				local byte = read_mem_byte(cpu, address)
				contend_read_no_mreq(cpu, address, 1)
				byte = byte & bit -- turn it off
				cpu[reg] = byte
				write_mem_byte(cpu, address, byte)
			end
		else
			opcodes[base + i - 1] = function (cpu, offset)
				local address = cpu.IX + offset
				local byte = read_mem_byte(cpu, address)
				contend_read_no_mreq(cpu, address, 1)
				byte = byte & bit -- turn it off
				write_mem_byte(cpu, address, byte)
			end
		end
	end
	base = base + 8
end


-- LD B,SET 0,(IX+dd), etc
local base = 0xc0
for b=0, 7 do
	local regs = {"B", "C", "D", "E", "H", "L", "foo", "A"}
	for i, reg in ipairs(regs) do
		local bit = 1 << b -- 2^bit
		if reg ~= "foo" then
			opcodes[base + i - 1] = function (cpu, offset)
				local address = cpu.IX + offset
				local byte = read_mem_byte(cpu, address)
				contend_read_no_mreq(cpu, address, 1)
				byte = byte | bit
				cpu[reg] = byte
				write_mem_byte(cpu, address, byte)
			end
		else
			opcodes[base + i - 1] = function (cpu, offset)
				local address = cpu.IX + offset
				local byte = read_mem_byte(cpu, address)
				contend_read_no_mreq(cpu, address, 1)
				byte = byte | bit
				write_mem_byte(cpu, address, byte)
			end
		end
	end
	base = base + 8
end




return opcodes
