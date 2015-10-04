--
-- FDxx opcodes
--

local logger = require "logger"
local common_ops = require "opcodes.common"


local function sign_extend (v)
	return v < 128 and v or v - 256
end

local function high_low (value)
	return (value & 0xff00) >> 8, value & 0x00ff
end

local ADD_16 = common_ops.ADD_16
local ADD_8  = common_ops.ADD_8
local AND    = common_ops.AND
local XOR    = common_ops.XOR
local OR     = common_ops.OR
local CP     = common_ops.CP
local SBC    = common_ops.SBC
local SUB    = common_ops.SUB
local ADC_8  = common_ops.ADC_8
local INC    = common_ops.INC
local DEC    = common_ops.DEC

local opcodes_ddcb = require "opcodes.ddcb"

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

---
-- Will be bound to the actual functions to be called later in bind_io
--
local contend_read_no_mreq
local contend_write_no_mreq
local read_mem_byte
local read_mem_byte_internal
local read_mem_word
local write_mem_byte
local write_mem_word

function opcodes.bind_io (binds)
	contend_read_no_mreq = assert(binds.memory.contend_read_no_mreq)
	contend_write_no_mreq = assert(binds.memory.contend_write_no_mreq)
	read_mem_byte = assert(binds.memory.read_mem_byte)
	read_mem_byte_internal = assert(binds.memory.read_mem_byte_internal)
	read_mem_word = assert(binds.memory.read_mem_word)
	write_mem_byte = assert(binds.memory.write_mem_byte)
	write_mem_word = assert(binds.memory.write_mem_word)

	opcodes_ddcb.bind_io(binds)
end


-- TODO ESTO y FCCB.lua se duplican para IX e IX

-- ADD IX,BC
opcodes[0x09] = function (cpu)
	--logger.debug("ADD IX,BC")
	cpu.IX = ADD_16(cpu, cpu.IX, cpu.C | cpu.B << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- ADD IX,DE
opcodes[0x19] = function (cpu)
	--logger.debug("ADD IX,DE")
	cpu.IX = ADD_16(cpu, cpu.IX, cpu.E | cpu.D << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IX,nnnn
opcodes[0x21] = function (cpu)
	local word = read_mem_word(cpu, (cpu.PC + 1) & 0xffff)
	--logger.debug("LD IX,$%04x", word)
	cpu.IX = word
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- LD (nnnn),IX
opcodes[0x22] = function (cpu)
	local address = read_mem_word(cpu, (cpu.PC + 1) & 0xffff)
	--logger.debug("LD ($%04x),IX", address)
	write_mem_word(cpu, address, cpu.IX)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- INC IX
opcodes[0x23] = function (cpu)
	--logger.debug("INC IX")
	cpu.IX = (cpu.IX + 1) & 0xffff
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- INC IXh
opcodes[0x24] = function (cpu)
	--logger.debug("INC IXh")
	-- TODO: faltan los tstates
	local IXh, IXl = high_low(cpu.IX)
	IXh = INC(cpu, IXh)
	cpu.IX = IXl | IXh << 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- DEC
opcodes[0x25] = function (cpu)
	--logger.debug("DEC IXh")
	local IXh, IXl = high_low(cpu.IX)
	IXh = DEC(cpu, IXh)
	cpu.IX = IXl | IXh << 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXh,nn
opcodes[0x26] = function (cpu)
	--logger.debug("LD IXh,nn")
	local byte = read_mem_byte(cpu, (cpu.PC + 1) & 0xffff)
	cpu.IX = (byte << 8) | (cpu.IX & 0xff)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- ADD IX,IX
opcodes[0x29] = function (cpu)
	--logger.debug("ADD IX,IX")
	cpu.IX = ADD_16(cpu, cpu.IX, cpu.IX)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IX,(nnnn)
opcodes[0x2a] = function (cpu)
	local address = read_mem_word(cpu, (cpu.PC + 1) & 0xffff)
	--logger.debug("LD IX,($%04x)", address)
	cpu.IX = read_mem_word(cpu, address)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- DEC IX
opcodes[0x2b] = function (cpu)
	--logger.debug("DEC IX")
	cpu.IX = (cpu.IX - 1) & 0xffff
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- INC IXl
opcodes[0x2c] = function (cpu)
	--logger.debug("INC IXl")
	-- TODO: faltan los tstates
	local IXh, IXl = high_low(cpu.IX)
	IXl = INC(cpu, IXl)
	cpu.IX = IXl | IXh << 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- DEC IXl
opcodes[0x2d] = function (cpu)
	--logger.debug("DEC IXl")
	local IXh, IXl = high_low(cpu.IX)
	IXl = DEC(cpu, IXl)
	cpu.IX = IXl | IXh << 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXl,nn
opcodes[0x2e] = function (cpu)
	--logger.debug("LD IXl,nn")
	local byte = read_mem_byte(cpu, (cpu.PC + 1) & 0xffff)
	cpu.IX = (cpu.IX & 0xff00) | byte
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- INC (IX+nn)
opcodes[0x34] = function (cpu)
	--logger.debug("INC (IX+nn)")
	local offset = sign_extend(read_mem_byte(cpu, (cpu.PC + 1) & 0xffff))
	for i=1,5 do
		contend_read_no_mreq(cpu, (cpu.PC + 1) & 0xffff, 1)
	end
	local byte = read_mem_byte(cpu, cpu.IX + offset)
	contend_read_no_mreq(cpu, cpu.IX + offset, 1)
	byte = INC(cpu, byte)
	write_mem_byte(cpu, cpu.IX + offset, byte)

	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- DEC (IX+nn)
opcodes[0x35] = function (cpu)
	--logger.debug("DEC (IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, (cpu.PC + 1) & 0xffff) )

	for i=1,5 do
		contend_read_no_mreq(cpu, (cpu.PC + 1) & 0xffff, 1)
	end
	local byte = read_mem_byte(cpu, cpu.IX + offset)
	contend_read_no_mreq(cpu, cpu.IX + offset, 1)
	byte = DEC(cpu, byte)
	write_mem_byte(cpu, cpu.IX + offset, byte)

	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IX+dd),nn
opcodes[0x36] = function (cpu)
	--logger.debug("LD (IX+dd),nn")
	local offset = sign_extend( read_mem_byte(cpu, (cpu.PC + 1) & 0xffff) )
	local byte = read_mem_byte(cpu, (cpu.PC + 2) & 0xffff)
	contend_read_no_mreq(cpu, (cpu.PC + 2) & 0xffff, 1)
	contend_read_no_mreq(cpu, (cpu.PC + 2) & 0xffff, 1)
	write_mem_byte(cpu, cpu.IX + offset, byte)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- ADD IX,SP
opcodes[0x39] = function (cpu)
	--logger.debug("ADD IX,SP")
	cpu.IX = ADD_16(cpu, cpu.IX, cpu.SP)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD B,IXh
opcodes[0x44] = function (cpu)
	--logger.debug("LD B,IXh")
	cpu.B = (cpu.IX & 0xff00) >> 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD B,IXl
opcodes[0x45] = function (cpu)
	--logger.debug("LD B,IXl")
	cpu.B = cpu.IX & 0x00ff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD B,(IX+nn)
opcodes[0x46] = function (cpu)
	--logger.debug("LD B,(IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, (cpu.PC + 1) & 0xffff) )
	for i=1,5 do
		contend_read_no_mreq(cpu, (cpu.PC + 1) & 0xffff, 1)
	end
	cpu.B = read_mem_byte(cpu, cpu.IX + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD C,IXh
opcodes[0x4c] = function (cpu)
	--logger.debug("LD C,IXh")
	cpu.C = (cpu.IX & 0xff00) >> 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD C,IXl
opcodes[0x4d] = function (cpu)
	--logger.debug("LD C,IXl")
	cpu.C = cpu.IX & 0x00ff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD C,(IX+nn)
opcodes[0x4e] = function (cpu)
	--logger.debug("LD C,(IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, (cpu.PC + 1) & 0xffff) )
	for i=1,5 do
		contend_read_no_mreq(cpu, (cpu.PC + 1) & 0xffff, 1)
	end
	cpu.C = read_mem_byte(cpu, cpu.IX + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD D,IXh
opcodes[0x54] = function (cpu)
	--logger.debug("LD D,IXh")
	cpu.D = (cpu.IX & 0xff00) >> 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD D,IXl
opcodes[0x55] = function (cpu)
	--logger.debug("LD D,IXl")
	cpu.D = cpu.IX & 0x00ff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD D,(IX+nn)
opcodes[0x56] = function (cpu)
	--logger.debug("LD D,(IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, (cpu.PC + 1) & 0xffff) )
	for i=1,5 do
		contend_read_no_mreq(cpu, (cpu.PC + 1) & 0xffff, 1)
	end
	cpu.D = read_mem_byte(cpu, cpu.IX + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD E,IXh
opcodes[0x5c] = function (cpu)
	--logger.debug("LD E,IXh")
	cpu.E = (cpu.IX & 0xff00) >> 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD E,IXl
opcodes[0x5d] = function (cpu)
	--logger.debug("LD E,IXl")
	cpu.E = cpu.IX & 0x00ff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD E,(IX+nn)
opcodes[0x5e] = function (cpu)
	--logger.debug("LD E,(IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	cpu.E = read_mem_byte(cpu, cpu.IX + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD IXh,B
opcodes[0x60] = function (cpu)
	--logger.debug("LD IXh,B")
	cpu.IX = (cpu.B << 8) | (cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXh,C
opcodes[0x61] = function (cpu)
	--logger.debug("LD IXh,C")
	cpu.IX = (cpu.C << 8) | (cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXh,D
opcodes[0x62] = function (cpu)
	--logger.debug("LD IXh,D")
	cpu.IX = (cpu.D << 8) | (cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXh,E
opcodes[0x63] = function (cpu)
	--logger.debug("LD IXh,E")
	cpu.IX = (cpu.E << 8) | (cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXh,IXh
opcodes[0x64] = function (cpu)
	--logger.debug("LD IXh,IXh")
	-- LD IXh,IXh
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXh,IXl
opcodes[0x65] = function (cpu)
	--logger.debug("LD IXh,IXl")
	local IXl = cpu.IX & 0x00ff
	cpu.IX = IXl | (IXl << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD H,(IX+nn)
opcodes[0x66] = function (cpu)
	--logger.debug("LD H,(IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	cpu.H = read_mem_byte(cpu, cpu.IX + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD IXh,A
opcodes[0x67] = function (cpu)
	--logger.debug("LD IXh,A")
	cpu.IX = (cpu.A << 8) | (cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXl,B
opcodes[0x68] = function (cpu)
	--logger.debug("LD IXl,B")
	cpu.IX = (cpu.IX & 0xff00) | cpu.B
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXl,C
opcodes[0x69] = function (cpu)
	--logger.debug("LD IXl,C")
	cpu.IX = (cpu.IX & 0xff00) | cpu.C
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXl,D
opcodes[0x6a] = function (cpu)
	--logger.debug("LD IXl,D")
	cpu.IX = (cpu.IX & 0xff00) | cpu.D
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXl,E
opcodes[0x6b] = function (cpu)
	--logger.debug("LD IXl,E")
	cpu.IX = (cpu.IX & 0xff00) | cpu.E
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXl,IXh
opcodes[0x6c] = function (cpu)
	--logger.debug("LD IXl,IXh")
	local high, low = high_low(cpu.IX)
	cpu.IX = high | (high << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IXl,IXl
opcodes[0x6d] = function (cpu)
	--logger.debug("LD IXl,IXl")
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD L,IX+nn
opcodes[0x6e] = function (cpu)
	--logger.debug("LD L,(IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	cpu.L = read_mem_byte(cpu, cpu.IX + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD IXl,A
opcodes[0x6f] = function (cpu)
	--logger.debug("LD IXl,A")
	cpu.IX = (cpu.IX & 0xff00) | cpu.A
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD (IX+nn),B
opcodes[0x70] = function (cpu)
	--logger.debug("LD (IX+nn),B")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IX + offset, cpu.B)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IX+nn),C
opcodes[0x71] = function (cpu)
	--logger.debug("LD (IX+nn),C")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IX + offset, cpu.C)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IX+nn),D
opcodes[0x72] = function (cpu)
	--logger.debug("LD (IX+nn),D")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IX + offset, cpu.D)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IX+nn),E
opcodes[0x73] = function (cpu)
	--logger.debug("LD (IX+nn),E")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IX + offset, cpu.E)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IX+nn),H
opcodes[0x74] = function (cpu)
	--logger.debug("LD (IX+nn),H")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IX + offset, cpu.H)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IX+nn),L
opcodes[0x75] = function (cpu)
	--logger.debug("LD (IX+nn),L")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IX + offset, cpu.L)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IX+nn),A
opcodes[0x77] = function (cpu)
	--logger.debug("LD (IX+nn),A")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IX + offset, cpu.A)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD A,IXh
opcodes[0x7c] = function (cpu)
	--logger.debug("LD A,IXh")
	cpu.A = (cpu.IX & 0xff00) >> 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD A,IXl
opcodes[0x7d] = function (cpu)
	--logger.debug("LD A,IXl")
	cpu.A = cpu.IX & 0x00ff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD A,(IX+nn)
opcodes[0x7e] = function (cpu)
	--logger.debug("LD A,(IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	cpu.A = read_mem_byte(cpu, cpu.IX + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- ADD A,IXh
opcodes[0x84] = function (cpu)
	--logger.debug("ADD A,IXh")
	ADD_8(cpu, (cpu.IX & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- ADD A,IXl
opcodes[0x85] = function (cpu)
	--logger.debug("ADD A,IXl")
	ADD_8(cpu, cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- ADD A,(IX+nn)
opcodes[0x86] = function (cpu)
	--logger.debug("ADD A,(IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	local byte = read_mem_byte(cpu, cpu.IX + offset)
	ADD_8(cpu, byte)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- ADC A,IXh
opcodes[0x8c] = function (cpu)
	--logger.debug("ADC A,IXh")
	ADC_8(cpu, (cpu.IX & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- ADC A,IXl
opcodes[0x8d] = function (cpu)
	--logger.debug("ADC A,IXl")
	ADC_8(cpu, cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- ADC A,(IX+nn)
opcodes[0x8e] = function (cpu)
	--logger.debug("ADC A,(IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	ADC_8(cpu, read_mem_byte(cpu, cpu.IX + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- SUB IXh
opcodes[0x94] = function (cpu)
	--logger.debug("SUB IXh")
	SUB(cpu, (cpu.IX & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- SUB IXl
opcodes[0x95] = function (cpu)
	--logger.debug("SUB IXl")
	SUB(cpu, cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- SUB A,(IX+nn)
opcodes[0x96] = function (cpu)
	--logger.debug("SUB A,(IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	SUB(cpu, read_mem_byte(cpu, cpu.IX + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- SBC A,IXh
opcodes[0x9c] = function (cpu)
	--logger.debug("SBC A,IXh")
	SBC(cpu, (cpu.IX & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- SBC A,IXl
opcodes[0x9d] = function (cpu)
	--logger.debug("SBC A,IXl")
	SBC(cpu, cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- SBC A,(IX+nn)
opcodes[0x9e] = function (cpu)
	--logger.debug("SBC A,(IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	SBC(cpu, read_mem_byte(cpu, cpu.IX + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- AND IXh
opcodes[0xa4] = function (cpu)
	--logger.debug("AND IXh")
	AND(cpu, (cpu.IX & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- AND IXl
opcodes[0xa5] = function (cpu)
	--logger.debug("AND IXl")
	AND(cpu, cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- AND (IX+nn)
opcodes[0xa6] = function (cpu)
	--logger.debug("AND (IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	AND(cpu, read_mem_byte(cpu, cpu.IX + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- XOR IXh
opcodes[0xac] = function (cpu)
	--logger.debug("XOR IXh")
	XOR(cpu, (cpu.IX & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- XOR IXl
opcodes[0xad] = function (cpu)
	--logger.debug("XOR IXl")
	XOR(cpu, cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- XOR (IX+nn)
opcodes[0xae] = function (cpu)
	--logger.debug("XOR (IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	XOR(cpu, read_mem_byte(cpu, cpu.IX + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- OR IXh
opcodes[0xb4] = function (cpu)
	--logger.debug("OR IXh")
	OR(cpu, (cpu.IX & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- OR IXl
opcodes[0xb5] = function (cpu)
	--logger.debug("OR IXl")
	OR(cpu, cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- OR (IX+nn)
opcodes[0xb6] = function (cpu)
	--logger.debug("OR (IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	OR(cpu, read_mem_byte(cpu, cpu.IX + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- CP IXh
opcodes[0xbc] = function (cpu)
	--logger.debug("CP IXh")
	CP(cpu, (cpu.IX & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CP IXl
opcodes[0xbd] = function (cpu)
	--logger.debug("CP IXl")
	CP(cpu, cpu.IX & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CP (IX+nn)
opcodes[0xbe] = function (cpu)
	--logger.debug("CP (IX+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	CP(cpu, read_mem_byte(cpu, cpu.IX + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- 0xCB DDCB opcodes
opcodes[0xcb] = function (cpu)
	--logger.warning("shifting DDCB")
	contend_read_no_mreq(cpu, cpu.PC + 1, 3)
	local offset = sign_extend( read_mem_byte_internal(cpu, cpu.PC + 1) )
	contend_read_no_mreq(cpu, cpu.PC + 2, 3)
	local opcode = read_mem_byte_internal(cpu, cpu.PC + 2)
	contend_read_no_mreq(cpu, cpu.PC + 2, 1)
	contend_read_no_mreq(cpu, cpu.PC + 2, 1)
	local f = opcodes_ddcb[opcode]
	if not f then
		--logger.fatal("Opcode DDCB 0x%0x (%d) not found", opcode, opcode)
		error("")
		return
	end
	f(cpu, offset)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- POP IX
opcodes[0xe1] = function (cpu)
	--logger.debug("POP IX")
	local low  = read_mem_byte(cpu, cpu.SP)
	local high = read_mem_byte(cpu, cpu.SP + 1)
	cpu.IX = low | (high << 8)
	cpu.SP = (cpu.SP + 2) & 0xffff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- EX (SP),IX
opcodes[0xe3] = function (cpu)
	--logger.debug("EX (SP),IX")
	local temp = cpu.IX
	cpu.IX = read_mem_word(cpu, cpu.SP)
	contend_read_no_mreq(cpu, cpu.SP + 1, 1)
	write_mem_byte(cpu, cpu.SP + 1, (temp & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, temp & 0xff)
	contend_write_no_mreq(cpu, cpu.SP, 1)
	contend_write_no_mreq(cpu, cpu.SP, 1)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- PUSH IX
opcodes[0xe5] = function (cpu)
	--logger.debug("PUSH IX")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)

	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, cpu.SP + 1, (cpu.IX & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, cpu.IX & 0xff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- JP IX
opcodes[0xe9] = function (cpu)
	--logger.debug("JP IX")
	cpu.PC = cpu.IX
end

-- LD SP,IX
opcodes[0xf9] = function (cpu)
	--logger.debug("LD SP,IX")
	cpu.SP = cpu.IX
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.PC = (cpu.PC + 1) & 0xffff
end


return opcodes
