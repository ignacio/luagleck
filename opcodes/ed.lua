--
-- EDxx opcodes
--

local logger = require "logger"
local common_ops = require "opcodes.common"

---
-- Will be bound to the actual functions to be called later in bind_io
--
local contend_write_no_mreq
local contend_read_no_mreq
local read_mem_word
local write_mem_word
local write_mem_byte
local read_mem_byte
local write_port
local read_port

local SUB = common_ops.SUB

local overflow_add_table = common_ops.tables.overflow_add_table
local overflow_sub_table = common_ops.tables.overflow_sub_table
local halfcarry_add_table = common_ops.tables.halfcarry_add_table
local halfcarry_sub_table = common_ops.tables.halfcarry_sub_table
local sz53p_table = common_ops.tables.sz53p_table
local sz53_table = common_ops.tables.sz53_table
local parity_table = common_ops.tables.parity_table


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


-- increments a 16 bit register
local function increment_16 (high, low)
	local temp = ((low | (high << 8)) + 1) & 0xffff
	return temp >> 8, temp & 0xff
end

-- decrements a 16 bit register
local function decrement_16 (high, low)
	local temp = ((low | (high << 8)) - 1) & 0xffff
	return temp >> 8, temp & 0xff
end



local function SBC_16 (cpu, value)
	local HL = cpu.L | cpu.H << 8
	local sub16temp = HL - value - (cpu.F & FLAG_C)
	local lookup =  ( (        HL & 0x8800 ) >> 11 ) |
					( (     value & 0x8800 ) >> 10 ) |
					( ( sub16temp & 0x8800 ) >>  9 )
	cpu.H = (sub16temp >> 8) & 0xff
	cpu.L = sub16temp & 0xff
	HL = cpu.L | cpu.H << 8
	cpu.F = ( (sub16temp & 0x10000 ~= 0) and FLAG_C or 0 ) |
			FLAG_N |
			overflow_sub_table[lookup >> 4] |
			( cpu.H & ( FLAG_B3 | FLAG_B5 | FLAG_S ) ) |
			halfcarry_sub_table[lookup & 0x07] |
			( HL ~= 0 and 0 or FLAG_Z)
end

local function ADC_16 (cpu, value)
	local HL = cpu.L | cpu.H << 8
	local add16temp = HL + value + (cpu.F & FLAG_C)
	local lookup =  ( (        HL & 0x8800 ) >> 11 ) |
					( (     value & 0x8800 ) >> 10 ) |
					( ( add16temp & 0x8800 ) >>  9 )
	cpu.H = (add16temp >> 8) & 0xff
	cpu.L = add16temp & 0xff
	HL = cpu.L | cpu.H << 8
	cpu.F = ( (add16temp & 0x10000 ~= 0) and FLAG_C or 0 ) |
			overflow_add_table[lookup >> 4] |
			( cpu.H & ( FLAG_B3 | FLAG_B5 | FLAG_S ) ) |
			halfcarry_add_table[lookup & 0x07] |
			( HL ~= 0 and 0 or FLAG_Z)
end


local function NEG (cpu)
	local byte = cpu.A
	cpu.A = 0
	SUB(cpu, byte)
end



local opcodes = {}

function opcodes.bind_io (binds)
	contend_read_no_mreq = assert(binds.memory.contend_read_no_mreq)
	contend_write_no_mreq = assert(binds.memory.contend_write_no_mreq)
	read_mem_byte = assert(binds.memory.read_mem_byte)
	read_mem_word = assert(binds.memory.read_mem_word)
	write_mem_byte = assert(binds.memory.write_mem_byte)
	write_mem_word = assert(binds.memory.write_mem_word)

	write_port = assert(binds.ports.write_port)
	read_port = assert(binds.ports.read_port)
end


-- 0x40 IN B,(C)
opcodes[0x40] = function (cpu)
	--logger.debug("IN B,C")
	local BC = cpu.C | cpu.B << 8
	cpu.B = assert(read_port(cpu, BC))
	cpu.F = ( cpu.F & FLAG_C) | sz53p_table[cpu.B]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- 0x41 OUT (C),B
opcodes[0x41] = function (cpu)
	--logger.debug("OUT (C),B")
	write_port(cpu, cpu.C | cpu.B << 8, cpu.B)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- 0x42 SBC HL,BC
opcodes[0x42] = function (cpu)
	--logger.debug("SBC HL,BC")
	local IR = cpu.R | cpu.I << 8
	for i=1,7 do
		contend_read_no_mreq(cpu, IR, 1)
	end
	SBC_16(cpu, cpu.C | cpu.B << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- 0x43 LD (nnnn),BC
opcodes[0x43] = function (cpu)
	local address = read_mem_word(cpu, (cpu.PC + 1) & 0xffff)
	--logger.debug("LD ($%04x),BC", address)
	write_mem_word(cpu, address, cpu.C | cpu.B << 8)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- 0x44 NEG
opcodes[0x44] = function (cpu)
	--logger.debug("NEG")
	NEG(cpu)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- 0x45 RETN
opcodes[0x45] = function (cpu)
	--logger.debug("RETN")
	cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
	cpu.SP = (cpu.SP + 2) & 0xffff
	cpu.iff1 = cpu.iff2 -- activa de nuevo las NMI
end

opcodes[0x46] = function (cpu)
	--logger.debug("IM 0")
	cpu.int_mode = 0
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- 0x47 LD I,A
opcodes[0x47] = function (cpu)
	--logger.debug("LD I,A")
	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1)
	cpu.I = cpu.A
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x48] = function (cpu)
	--logger.debug("IN C,C")
	local BC = cpu.C | cpu.B << 8
	cpu.C = assert(read_port(cpu, BC))
	cpu.F = ( cpu.F & FLAG_C) | sz53p_table[cpu.C]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x49] = function (cpu)
	--logger.debug("OUT (C),C")
	write_port(cpu, cpu.C | cpu.B << 8, cpu.C)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x4a] = function (cpu)
	--logger.debug("ADC HL,BC")
	local IR = cpu.R | cpu.I << 8
	for i=1,7 do
		contend_read_no_mreq(cpu, IR, 1)
	end
	ADC_16(cpu, cpu.C | cpu.B << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD BC,(nnnn)
opcodes[0x4b] = function (cpu)
	--logger.debug("LD BC,(nnnn)")
	local address = read_mem_word(cpu, (cpu.PC + 1) & 0xffff)
	cpu.C = read_mem_byte(cpu, address)
	cpu.B = read_mem_byte(cpu, (address + 1) & 0xffff)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- 0x4c NEG
opcodes[0x4c] = function (cpu)
	--logger.debug("NEG")
	NEG(cpu)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x4d] = function (cpu)
	--logger.debug("RETI") -- igual que retn
	cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
	cpu.SP = (cpu.SP + 2) & 0xffff
	cpu.iff1 = cpu.iff2 -- activa de nuevo las NMI
end

-- IM 0
opcodes[0x4e] = function (cpu)
	--logger.debug("IM 0")
	cpu.int_mode = 0
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x4f] = function (cpu)
	--logger.debug("LD R,A")
	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1)
	cpu.R = cpu.A
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x50] = function (cpu)
	--logger.debug("IN D,C")
	local BC = cpu.C | cpu.B << 8
	cpu.D = assert(read_port(cpu, BC))
	cpu.F = (cpu.F & FLAG_C) | sz53p_table[cpu.D]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x51] = function (cpu)
	--logger.debug("OUT (C),D")
	write_port(cpu, cpu.C | cpu.B << 8, cpu.D)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- SBC HL,DE
opcodes[0x52] = function (cpu)
	--logger.debug("SBC HL,DE")
	local IR = cpu.R | cpu.I << 8
	for i=1,7 do
		contend_read_no_mreq(cpu, IR, 1)
	end
	SBC_16(cpu, cpu.E | cpu.D << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD (nnnn),DE
opcodes[0x53] = function (cpu)
	local address = read_mem_word(cpu, (cpu.PC + 1) & 0xffff)
	--logger.debug("LD ($%04x),DE", address)
	write_mem_word(cpu, address, cpu.E | cpu.D << 8)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- 0x54 NEG
opcodes[0x54] = function (cpu)
	--logger.debug("NEG")
	NEG(cpu)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- 0x45 RETN
opcodes[0x55] = function (cpu)
	--logger.debug("RETN")
	cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
	cpu.SP = (cpu.SP + 2) & 0xffff
	cpu.iff1 = cpu.iff2 -- activa de nuevo las NMI
end

-- IM 1
opcodes[0x56] = function (cpu)
	--logger.debug("IM 1")
	cpu.int_mode = 1
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x57] = function (cpu)
	--logger.debug("LD A,I")
	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1)
	cpu.A = cpu.I
	cpu.F = ( cpu.F & FLAG_C ) | sz53_table[cpu.A] | ( cpu.iff2 ~= 0 and FLAG_PV or 0 )
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x58] = function (cpu)
	--logger.debug("IN E,C")
	local BC = cpu.C | cpu.B << 8
	cpu.E = assert(read_port(cpu, BC))
	cpu.F = ( cpu.F & FLAG_C) | sz53p_table[cpu.E]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x59] = function (cpu)
	--logger.debug("OUT (C),E")
	write_port(cpu, cpu.C | cpu.B << 8, cpu.E)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x5a] = function (cpu)
	--logger.debug("ADC HL,DE")
	local IR = cpu.R | cpu.I << 8
	for i=1,7 do
		contend_read_no_mreq(cpu, IR, 1)
	end
	ADC_16(cpu, cpu.E | cpu.D << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD DE,(nnnn)
opcodes[0x5b] = function (cpu)
	--logger.debug("LD DE,(nnnn)")
	local address = read_mem_word(cpu, (cpu.PC + 1) & 0xffff)
	cpu.E = read_mem_byte(cpu, address)
	cpu.D = read_mem_byte(cpu, (address + 1) & 0xffff)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- 0x5c NEG
opcodes[0x5c] = function (cpu)
	--logger.debug("NEG")
	NEG(cpu)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- 0x5d RETN
opcodes[0x5d] = function (cpu)
	--logger.debug("RETN")
	cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
	cpu.SP = (cpu.SP + 2) & 0xffff
	cpu.iff1 = cpu.iff2 -- activa de nuevo las NMI
end

opcodes[0x5e] = function (cpu)
	--logger.debug("IM 2")
	cpu.int_mode = 2
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD A,R
opcodes[0x5f] = function (cpu)
	--logger.debug("LD A,R")
	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1)
	cpu.A = cpu.R
	cpu.F = ( cpu.F & FLAG_C ) | sz53_table[cpu.A] | ( cpu.iff2 ~= 0 and FLAG_PV or 0 )
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x60] = function (cpu)
	--logger.debug("IN D,C")
	local BC = cpu.C | cpu.B << 8
	cpu.H = assert(read_port(cpu, BC))
	cpu.F = (cpu.F & FLAG_C) | sz53p_table[cpu.H]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x61] = function (cpu)
	--logger.debug("OUT (C),H")
	write_port(cpu, cpu.C | cpu.B << 8, cpu.H)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x62] = function (cpu)
	--logger.debug("SBC HL,HL")
	local IR = cpu.R | cpu.I << 8
	for i=1,7 do
		contend_read_no_mreq(cpu, IR, 1)
	end
	SBC_16(cpu, cpu.L | cpu.H << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD (nnnn),HL
opcodes[0x63] = function (cpu)
	local address = read_mem_word(cpu, (cpu.PC + 1) & 0xffff)
	--logger.debug("LD ($%04x),HL", address)
	write_mem_word(cpu, address, cpu.L | cpu.H << 8)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- 0x64 NEG
opcodes[0x64] = function (cpu)
	--logger.debug("NEG")
	NEG(cpu)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- 0x65 RETN
opcodes[0x65] = function (cpu)
	--logger.debug("RETN")
	cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
	cpu.SP = (cpu.SP + 2) & 0xffff
	cpu.iff1 = cpu.iff2 -- activa de nuevo las NMI
end

opcodes[0x66] = function (cpu)
	--logger.debug("IM 0")
	cpu.int_mode = 0
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x67] = function (cpu)
	--logger.debug("RRD")
	local HL = cpu.L | cpu.H << 8
	local byte = read_mem_byte(cpu, HL)
	for i=1,4 do
		contend_read_no_mreq(cpu, HL, 1 )
	end
	write_mem_byte(cpu, HL, ( ( cpu.A << 4 ) | ( byte >> 4 ) ) & 0xff )
	cpu.A = ( cpu.A & 0xf0 ) | ( byte & 0x0f )
	cpu.F = ( cpu.F & FLAG_C ) | sz53p_table[cpu.A]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x68] = function (cpu)
	--logger.debug("IN D,C")
	local BC = cpu.C | cpu.B << 8
	cpu.L = assert(read_port(cpu, BC))
	cpu.F = ( cpu.F & FLAG_C) | sz53p_table[cpu.L]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x69] = function (cpu)
	--logger.debug("OUT (C),L")
	write_port(cpu, cpu.C | cpu.B << 8, cpu.L)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x6a] = function (cpu)
	--logger.debug("ADC HL,HL")
	local IR = cpu.R | cpu.I << 8
	for i=1,7 do
		contend_read_no_mreq(cpu, IR, 1)
	end
	ADC_16(cpu, cpu.L | cpu.H << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x6b] = function (cpu)
	--logger.debug("LD HL,(nnnn)")
	local address = read_mem_word(cpu, (cpu.PC + 1) & 0xffff)
	cpu.L = read_mem_byte(cpu, address)
	cpu.H = read_mem_byte(cpu, (address + 1) & 0xffff)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- 0x6c NEG
opcodes[0x6c] = function (cpu)
	--logger.debug("NEG")
	NEG(cpu)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- 0x6d RETN
opcodes[0x6d] = function (cpu)
	--logger.debug("RETN")
	cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
	cpu.SP = (cpu.SP + 2) & 0xffff
	cpu.iff1 = cpu.iff2 -- activa de nuevo las NMI
end

opcodes[0x6e] = function (cpu)
	--logger.debug("IM 0")
	cpu.int_mode = 0
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x6f] = function (cpu)
	--logger.debug("RLD")
	local HL = cpu.L | cpu.H << 8
	local byte = read_mem_byte(cpu, HL)
	for i=1,4 do
		contend_read_no_mreq(cpu, HL, 1 )
	end
	write_mem_byte(cpu, HL, ( (byte << 4 ) | ( cpu.A & 0x0f ) ) & 0xff )
	cpu.A = ( cpu.A & 0xf0 ) | ( byte >> 4 )
	cpu.F = ( cpu.F & FLAG_C ) | sz53p_table[cpu.A]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x70] = function (cpu)
	--logger.debug("IN F,C")
	local BC = cpu.C | cpu.B << 8
	assert(read_port(cpu, BC))
	cpu.F = ( cpu.F & FLAG_C) | sz53p_table[cpu.B]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x71] = function (cpu)
	--logger.debug("OUT C,0")
	write_port(cpu, cpu.C | cpu.B << 8, 0)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x72] = function (cpu)
	--logger.debug("SBC HL, SP")
	local IR = cpu.R | cpu.I << 8
	for i=1,7 do
		contend_read_no_mreq(cpu, IR, 1)
	end
	SBC_16(cpu, cpu.SP)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD (nnnn),SP
opcodes[0x73] = function (cpu)
	local address = read_mem_word(cpu, (cpu.PC + 1) & 0xffff)
	--logger.debug("LD ($%04x),SP", address)
	write_mem_word(cpu, address, cpu.SP)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- 0x74 NEG
opcodes[0x74] = function (cpu)
	--logger.debug("NEG")
	NEG(cpu)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- 0x75 RETN
opcodes[0x75] = function (cpu)
	--logger.debug("RETN")
	cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
	cpu.SP = (cpu.SP + 2) & 0xffff
	cpu.iff1 = cpu.iff2 -- activa de nuevo las NMI
end

-- IM 1
opcodes[0x76] = function (cpu)
	--logger.debug("IM 1")
	cpu.int_mode = 1
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- IN A,C
opcodes[0x78] = function (cpu)
	--logger.debug("IN A,C")
	local BC = cpu.C | cpu.B << 8
	cpu.A = assert(read_port(cpu, BC))
	cpu.F = ( cpu.F & FLAG_C) | sz53p_table[cpu.A]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x79] = function (cpu)
	--logger.debug("OUT (C),A")
	write_port(cpu, cpu.C | cpu.B << 8, cpu.A)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x7a] = function (cpu)
	--logger.debug("ADC HL,SP")
	local IR = cpu.R | cpu.I << 8
	for i=1,7 do
		contend_read_no_mreq(cpu, IR, 1)
	end
	ADC_16(cpu, cpu.SP)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0x7b] = function (cpu)
	--logger.debug("LD SP,(nnnn)")
	local address = read_mem_word(cpu, (cpu.PC + 1) & 0xffff)
	cpu.SP = read_mem_byte(cpu, address) | read_mem_byte(cpu, (address + 1) & 0xffff) << 8
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- 0x7c NEG
opcodes[0x7c] = function (cpu)
	--logger.debug("NEG")
	NEG(cpu)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- 0x7d RETN
opcodes[0x7d] = function (cpu)
	--logger.debug("RETN")
	cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
	cpu.SP = (cpu.SP + 2) & 0xffff
	cpu.iff1 = cpu.iff2 -- activa de nuevo las NMI
end

opcodes[0x7e] = function (cpu)
	--logger.debug("IM 2")
	cpu.int_mode = 2
	cpu.PC = (cpu.PC + 1) & 0xffff
end

opcodes[0xa0] = function (cpu)
	--logger.debug("LDI")

	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	local DE = cpu.E | cpu.D << 8
	write_mem_byte(cpu, DE, byte)
	contend_write_no_mreq(cpu, DE, 1)
	contend_write_no_mreq(cpu, DE, 1)
	cpu.B, cpu.C = decrement_16(cpu.B, cpu.C)
	local BC = cpu.C | cpu.B << 8
	byte = (byte + cpu.A) & 0xff

	cpu.F = ( cpu.F & ( FLAG_C | FLAG_Z | FLAG_S ) ) | ( BC ~= 0 and FLAG_PV or 0 ) |
			( byte & FLAG_B3 ) | ( (byte & 0x02 ~= 0) and FLAG_B5 or 0 )

	cpu.H, cpu.L = increment_16(cpu.H, cpu.L)
	cpu.D, cpu.E = increment_16(cpu.D, cpu.E)

	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CPI
opcodes[0xa1] = function (cpu)
	--logger.debug("CPI")
	local HL = cpu.L | cpu.H << 8
	local value = read_mem_byte(cpu, HL)
	local bytetemp = cpu.A - value
	local lookup =  ( (    cpu.A & 0x08 ) >> 3 ) |
					( (    value & 0x08 ) >> 2 ) |
					( ( bytetemp & 0x08 ) >> 1 )
	for i=1,5 do
		contend_read_no_mreq(cpu, HL, 1)
	end
	cpu.H, cpu.L = increment_16(cpu.H, cpu.L)
	cpu.B, cpu.C = decrement_16(cpu.B, cpu.C)
	
	local BC = cpu.C | cpu.B << 8
	cpu.F = ( cpu.F & FLAG_C ) |
			( BC ~= 0 and ( FLAG_PV | FLAG_N ) or FLAG_N ) |
			halfcarry_sub_table[lookup] |
			( bytetemp ~= 0 and 0 or FLAG_Z ) |
			( bytetemp & FLAG_S )
	
	if cpu.F & FLAG_H ~= 0 then
		bytetemp = (bytetemp - 1) & 0xff
	end
	cpu.F = cpu.F |
			( bytetemp & FLAG_B3 ) |
			( bytetemp & 0x02 ~= 0 and FLAG_B5 or 0 )

	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- INI
opcodes[0xa2] = function (cpu)
	--logger.debug("INI")
	
	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1 )
	local initemp = read_port(cpu, cpu.C | cpu.B << 8)
	write_mem_byte(cpu, cpu.L | cpu.H << 8, initemp)

	cpu.B = (cpu.B - 1) & 0xff
	cpu.H, cpu.L = increment_16(cpu.H, cpu.L)
	local initemp2 = (initemp + cpu.C + 1) & 0xff
	cpu.F = ( initemp & 0x80 ~= 0 and FLAG_N or 0 ) |
			( initemp2 < initemp and (FLAG_H | FLAG_C) or 0 ) |
			( parity_table[ initemp2 & 0x07 ~ cpu.B ] ~= 0 and FLAG_PV or 0 ) |
			sz53_table[cpu.B]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- OUTI
opcodes[0xa3] = function (cpu)
	--logger.debug("OUTI")
	
	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1 )
	local temp = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	cpu.B = (cpu.B - 1) & 0xff
	write_port(cpu, cpu.C | cpu.B << 8, temp)

	cpu.H, cpu.L = increment_16(cpu.H, cpu.L)
	local temp2 = (temp + cpu.L) & 0xff
	cpu.F = ( temp & 0x80 ~= 0 and FLAG_N or 0 ) |
			( temp2 < temp and (FLAG_H | FLAG_C) or 0 ) |
			( parity_table[ temp2 & 0x07 ~ cpu.B ] ~= 0 and FLAG_PV or 0 ) |
			sz53_table[cpu.B]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LDD
opcodes[0xa8] = function (cpu)
	--logger.debug("LDD")
	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	local DE = cpu.E | cpu.D << 8
	write_mem_byte(cpu, DE, byte)
	contend_write_no_mreq(cpu, DE, 1)
	contend_write_no_mreq(cpu, DE, 1)
	cpu.B, cpu.C = decrement_16(cpu.B, cpu.C)
	local BC = cpu.C | cpu.B << 8
	byte = (byte + cpu.A) & 0xff

	cpu.F = ( cpu.F & ( FLAG_C | FLAG_Z | FLAG_S ) ) | ( BC ~= 0 and FLAG_PV or 0 ) |
			( byte & FLAG_B3 ) | ( (byte & 0x02 ~= 0) and FLAG_B5 or 0 )

	cpu.H, cpu.L = decrement_16(cpu.H, cpu.L)
	cpu.D, cpu.E = decrement_16(cpu.D, cpu.E)

	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CPD
opcodes[0xa9] = function (cpu)
	--logger.debug("CPD")
	local HL = cpu.L | cpu.H << 8
	local value = read_mem_byte(cpu, HL)
	local bytetemp = cpu.A - value
	local lookup =  ( (    cpu.A & 0x08 ) >> 3 ) |
					( (    value & 0x08 ) >> 2 ) |
					( ( bytetemp & 0x08 ) >> 1 )
	for i=1,5 do
		contend_read_no_mreq(cpu, HL, 1)
	end
	cpu.H, cpu.L = decrement_16(cpu.H, cpu.L)
	cpu.B, cpu.C = decrement_16(cpu.B, cpu.C)
	
	local BC = cpu.C | cpu.B << 8
	cpu.F = ( cpu.F & FLAG_C ) |
			( BC ~= 0 and ( FLAG_PV | FLAG_N ) or FLAG_N ) |
			halfcarry_sub_table[lookup] |
			( bytetemp ~= 0 and 0 or FLAG_Z ) |
			( bytetemp & FLAG_S )
	
	if cpu.F & FLAG_H ~= 0 then
		bytetemp = (bytetemp - 1) & 0xff
	end
	cpu.F = cpu.F |
			( bytetemp & FLAG_B3 ) |
			( bytetemp & 0x02 ~= 0 and FLAG_B5 or 0 )

	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- IND
opcodes[0xaa] = function (cpu)
	--logger.debug("IND")
	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1 )
	local initemp = read_port(cpu, cpu.C | cpu.B << 8)
	write_mem_byte(cpu, cpu.L | cpu.H << 8, initemp)

	cpu.B = (cpu.B - 1) & 0xff
	cpu.H, cpu.L = decrement_16(cpu.H, cpu.L)
	local initemp2 = (initemp + cpu.C - 1) & 0xff
	cpu.F = ( initemp & 0x80 ~= 0 and FLAG_N or 0 ) |
			( initemp2 < initemp and (FLAG_H | FLAG_C) or 0 ) |
			( parity_table[ initemp2 & 0x07 ~ cpu.B ] ~= 0 and FLAG_PV or 0 ) |
			sz53_table[cpu.B]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- OUTD
opcodes[0xab] = function (cpu)
	--logger.debug("OUTD")
	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1 )
	local temp = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	cpu.B = (cpu.B - 1) & 0xff
	write_port(cpu, cpu.C | cpu.B << 8, temp)

	cpu.H, cpu.L = decrement_16(cpu.H, cpu.L)
	local temp2 = (temp + cpu.L) & 0xff
	cpu.F = ( temp & 0x80 ~= 0 and FLAG_N or 0 ) |
			( temp2 < temp and (FLAG_H | FLAG_C) or 0 ) |
			( parity_table[ temp2 & 0x07 ~ cpu.B ] ~= 0 and FLAG_PV or 0 ) |
			sz53_table[cpu.B]
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LDIR
opcodes[0xb0] = function (cpu)
	--logger.debug("LDIR")
	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	local DE = cpu.E | cpu.D << 8
	write_mem_byte(cpu, DE, byte)
	contend_write_no_mreq(cpu, DE, 1)
	contend_write_no_mreq(cpu, DE, 1)
	cpu.B, cpu.C = decrement_16(cpu.B, cpu.C)
	local BC = cpu.C | cpu.B << 8
	byte = (byte + cpu.A) & 0xff

	cpu.F = ( cpu.F & ( FLAG_C | FLAG_Z | FLAG_S ) ) | ( BC ~= 0 and FLAG_PV or 0 ) |
			( byte & FLAG_B3 ) | ( (byte & 0x02 ~= 0) and FLAG_B5 or 0 )
	if BC ~= 0 then
		for i = 1, 5 do
			contend_write_no_mreq(cpu, DE, 1)
		end
		cpu.PC = (cpu.PC - 2) & 0xffff
	end
	cpu.H, cpu.L = increment_16(cpu.H, cpu.L)
	cpu.D, cpu.E = increment_16(cpu.D, cpu.E)

	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CPIR
opcodes[0xb1] = function (cpu)
	--logger.debug("CPIR")
	local HL = cpu.L | cpu.H << 8
	local value = read_mem_byte(cpu, HL)
	local bytetemp = cpu.A - value
	local lookup =  ( (    cpu.A & 0x08 ) >> 3 ) |
					( (    value & 0x08 ) >> 2 ) |
					( ( bytetemp & 0x08 ) >> 1 )
	for i=1,5 do
		contend_read_no_mreq(cpu, HL, 1)
	end
	cpu.B, cpu.C = decrement_16(cpu.B, cpu.C)

	local BC = cpu.C | cpu.B << 8
	cpu.F = ( cpu.F & FLAG_C ) |
			( BC ~= 0 and ( FLAG_PV | FLAG_N ) or FLAG_N ) |
			halfcarry_sub_table[lookup] |
			( bytetemp ~= 0 and 0 or FLAG_Z ) |
			( bytetemp & FLAG_S )
	
	if cpu.F & FLAG_H ~= 0 then
		bytetemp = (bytetemp - 1) & 0xff
	end
	cpu.F = cpu.F |
			( bytetemp & FLAG_B3 ) |
			( bytetemp & 0x02 ~= 0 and FLAG_B5 or 0 )
	if cpu.F & ( FLAG_PV | FLAG_Z ) == FLAG_PV then
		local HL = cpu.L | cpu.H << 8
		for i = 1, 5 do
			contend_write_no_mreq(cpu, HL, 1)
		end
		cpu.PC = (cpu.PC - 2) & 0xffff
	end
	cpu.H, cpu.L = increment_16(cpu.H, cpu.L)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- INIR
opcodes[0xb2] = function (cpu)
	--logger.debug("INIR")
	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1 )
	local initemp = read_port(cpu, cpu.C | cpu.B << 8)
	write_mem_byte(cpu, cpu.L | cpu.H << 8, initemp)

	cpu.B = (cpu.B - 1) & 0xff
	local initemp2 = (initemp + cpu.C + 1) & 0xff
	cpu.F = ( initemp & 0x80 ~= 0 and FLAG_N or 0 ) |
			( initemp2 < initemp and (FLAG_H | FLAG_C) or 0 ) |
			( parity_table[ initemp2 & 0x07 ~ cpu.B ] ~= 0 and FLAG_PV or 0 ) |
			sz53_table[cpu.B]

	if cpu.B ~= 0 then
		local HL = cpu.L | cpu.H << 8
		for i = 1, 5 do
			contend_write_no_mreq(cpu, HL, 1)
		end
		cpu.PC = (cpu.PC - 2) & 0xffff
	end

	cpu.H, cpu.L = increment_16(cpu.H, cpu.L)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- OTIR
opcodes[0xb3] = function (cpu)
	--logger.debug("OTIR")

	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1 )
	local temp = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	cpu.B = (cpu.B - 1) & 0xff
	write_port(cpu, cpu.C | cpu.B << 8, temp)

	cpu.H, cpu.L = increment_16(cpu.H, cpu.L)
	local temp2 = (temp + cpu.L) & 0xff
	cpu.F = ( temp & 0x80 ~= 0 and FLAG_N or 0 ) |
			( temp2 < temp and (FLAG_H | FLAG_C) or 0 ) |
			( parity_table[ temp2 & 0x07 ~ cpu.B ] ~= 0 and FLAG_PV or 0 ) |
			sz53_table[cpu.B]

	if cpu.B ~= 0 then
		local BC = cpu.C | cpu.B << 8
		for i = 1, 5 do
			contend_write_no_mreq(cpu, BC, 1)
		end
		cpu.PC = (cpu.PC - 2) & 0xffff
	end
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LDDR
opcodes[0xb8] = function (cpu)
	--logger.debug("LDDR")
	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	local DE = cpu.E | cpu.D << 8
	write_mem_byte(cpu, DE, byte)
	contend_write_no_mreq(cpu, DE, 1)
	contend_write_no_mreq(cpu, DE, 1)
	cpu.B, cpu.C = decrement_16(cpu.B, cpu.C)
	local BC = cpu.C | cpu.B << 8
	byte = (byte + cpu.A) & 0xff
	
	cpu.F = ( cpu.F & ( FLAG_C | FLAG_Z | FLAG_S ) ) | ( BC ~= 0 and FLAG_PV or 0 ) |
			( byte & FLAG_B3 ) | ( (byte & 0x02 ~= 0) and FLAG_B5 or 0 )
	if BC ~= 0 then
		for i = 1, 5 do
			contend_write_no_mreq(cpu, DE, 1)
		end
		cpu.PC = (cpu.PC - 2) & 0xffff
	end
	cpu.H, cpu.L = decrement_16(cpu.H, cpu.L)
	cpu.D, cpu.E = decrement_16(cpu.D, cpu.E)

	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CPDR
opcodes[0xb9] = function (cpu)
	--logger.debug("CPDR")
	local HL = cpu.L | cpu.H << 8
	local value = read_mem_byte(cpu, HL)
	local bytetemp = cpu.A - value
	local lookup =  ( (    cpu.A & 0x08 ) >> 3 ) |
					( (    value & 0x08 ) >> 2 ) |
					( ( bytetemp & 0x08 ) >> 1 )
	for i=1,5 do
		contend_read_no_mreq(cpu, HL, 1)
	end
	cpu.B, cpu.C = decrement_16(cpu.B, cpu.C)

	local BC = cpu.C | cpu.B << 8
	cpu.F = ( cpu.F & FLAG_C ) |
			( BC ~= 0 and ( FLAG_PV | FLAG_N ) or FLAG_N ) |
			halfcarry_sub_table[lookup] |
			( bytetemp ~= 0 and 0 or FLAG_Z ) |
			( bytetemp & FLAG_S )
	
	if cpu.F & FLAG_H ~= 0 then
		bytetemp = (bytetemp - 1) & 0xff
	end
	cpu.F = cpu.F |
			( bytetemp & FLAG_B3 ) |
			( bytetemp & 0x02 ~= 0 and FLAG_B5 or 0 )
	if cpu.F & ( FLAG_PV | FLAG_Z ) == FLAG_PV then
		local HL = cpu.L | cpu.H << 8
		for i = 1, 5 do
			contend_write_no_mreq(cpu, HL, 1)
		end
		cpu.PC = (cpu.PC - 2) & 0xffff
	end
	cpu.H, cpu.L = decrement_16(cpu.H, cpu.L)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- INDR
opcodes[0xba] = function (cpu)
	--logger.debug("INDR")
	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1 )
	local initemp = read_port(cpu, cpu.C | cpu.B << 8)
	write_mem_byte(cpu, cpu.L | cpu.H << 8, initemp)

	cpu.B = (cpu.B - 1) & 0xff
	local initemp2 = (initemp + cpu.C + 1) & 0xff
	cpu.F = ( initemp & 0x80 ~= 0 and FLAG_N or 0 ) |
			( initemp2 < initemp and (FLAG_H | FLAG_C) or 0 ) |
			( parity_table[ initemp2 & 0x07 ~ cpu.B ] ~= 0 and FLAG_PV or 0 ) |
			sz53_table[cpu.B]

	if cpu.B ~= 0 then
		local HL = cpu.L | cpu.H << 8
		for i = 1, 5 do
			contend_write_no_mreq(cpu, HL, 1)
		end
		cpu.PC = (cpu.PC - 2) & 0xffff
	end

	cpu.H, cpu.L = decrement_16(cpu.H, cpu.L)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- OTDR
opcodes[0xbb] = function (cpu)
	--logger.debug("OTDR")
	contend_read_no_mreq(cpu, cpu.R | cpu.I << 8, 1 )
	local temp = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	cpu.B = (cpu.B - 1) & 0xff
	write_port(cpu, cpu.C | cpu.B << 8, temp)

	cpu.H, cpu.L = decrement_16(cpu.H, cpu.L)
	local temp2 = (temp + cpu.L) & 0xff
	cpu.F = ( temp & 0x80 ~= 0 and FLAG_N or 0 ) |
			( temp2 < temp and (FLAG_H | FLAG_C) or 0 ) |
			( parity_table[ temp2 & 0x07 ~ cpu.B ] ~= 0 and FLAG_PV or 0 ) |
			sz53_table[cpu.B]

	if cpu.B ~= 0 then
		local BC = cpu.C | cpu.B << 8
		for i = 1, 5 do
			contend_write_no_mreq(cpu, BC, 1)
		end
		cpu.PC = (cpu.PC - 2) & 0xffff
	end
	cpu.PC = (cpu.PC + 1) & 0xffff
end

return opcodes
