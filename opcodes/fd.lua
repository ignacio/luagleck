--
-- FDxx opcodes
--

local logger = require "logger"
local common_ops = require "opcodes.common"

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

local opcodes_fdcb = require "opcodes.fdcb"

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
	contend_write_no_mreq = assert(binds.memory.contend_write_no_mreq)
	read_mem_byte = assert(binds.memory.read_mem_byte)
	read_mem_byte_internal = assert(binds.memory.read_mem_byte_internal)
	read_mem_word = assert(binds.memory.read_mem_word)
	write_mem_byte = assert(binds.memory.write_mem_byte)
	write_mem_word = assert(binds.memory.write_mem_word)

	opcodes_fdcb.bind_io(binds)
end
-- TODO ESTO y FCCB.lua se duplican para IX e IY

-- ADD IY,BC
opcodes[0x09] = function (cpu)
	--logger.debug("ADD IY,BC")
	cpu.IY = ADD_16(cpu, cpu.IY, cpu.C | cpu.B << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- ADD IY,DE
opcodes[0x19] = function (cpu)
	--logger.debug("ADD IY,DE")
	cpu.IY = ADD_16(cpu, cpu.IY, cpu.E | cpu.D << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IY,nnnn
opcodes[0x21] = function (cpu)
	local word = read_mem_word(cpu, cpu.PC + 1)
	--logger.debug("LD IY,$%04x", word)
	cpu.IY = word
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- LD (nnnn),IY
opcodes[0x22] = function (cpu)
	local address = read_mem_word(cpu, cpu.PC + 1)
	--logger.debug("LD ($%04x),IY", address)
	write_mem_word(cpu, address, cpu.IY)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- INC IY
opcodes[0x23] = function (cpu)
	--logger.debug("INC IY")
	cpu.IY = (cpu.IY + 1) & 0xffff
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- INC IYh
opcodes[0x24] = function (cpu)
	--logger.debug("INC IYh")
	-- TODO: faltan los tstates
	local IYh, IYl = high_low(cpu.IY)
	IYh = INC(cpu, IYh)
	cpu.IY = IYl | IYh << 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- DEC
opcodes[0x25] = function (cpu)
	--logger.debug("DEC IYh")
	local IYh, IYl = high_low(cpu.IY)
	IYh = DEC(cpu, IYh)
	cpu.IY = IYl | IYh << 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYh,nn
opcodes[0x26] = function (cpu)
	--logger.debug("LD IYh,nn")
	local byte = read_mem_byte(cpu, cpu.PC + 1)
	cpu.IY = (byte << 8) | (cpu.IY & 0xff)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- ADD IY,IY
opcodes[0x29] = function (cpu)
	--logger.debug("ADD IY,IY")
	cpu.IY = ADD_16(cpu, cpu.IY, cpu.IY)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IY,(nnnn)
opcodes[0x2a] = function (cpu)
	local address = read_mem_word(cpu, cpu.PC + 1)
	--logger.debug("LD IY,($%04x)", address)
	cpu.IY = read_mem_word(cpu, address)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- DEC IY
opcodes[0x2b] = function (cpu)
	--logger.debug("DEC IY")
	cpu.IY = (cpu.IY - 1) & 0xffff
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- INC IYl
opcodes[0x2c] = function (cpu)
	--logger.debug("INC IYl")
	-- TODO: faltan los tstates
	local IYh, IYl = high_low(cpu.IY)
	IYl = INC(cpu, IYl)
	cpu.IY = IYl | IYh << 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- DEC IYl
opcodes[0x2d] = function (cpu)
	--logger.debug("DEC IYl")
	local IYh, IYl = high_low(cpu.IY)
	IYl = DEC(cpu, IYl)
	cpu.IY = IYl | IYh << 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYl,nn
opcodes[0x2e] = function (cpu)
	--logger.debug("LD IYl,nn")
	local byte = read_mem_byte(cpu, cpu.PC + 1)
	cpu.IY = (cpu.IY & 0xff00) | byte
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- INC (IY+nn)
opcodes[0x34] = function (cpu)
	--logger.debug("INC (IY+nn)")
	local offset = sign_extend(read_mem_byte(cpu, cpu.PC + 1))
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	local byte = read_mem_byte(cpu, cpu.IY + offset)
	contend_read_no_mreq(cpu, cpu.IY + offset, 1)
	byte = INC(cpu, byte)
	write_mem_byte(cpu, cpu.IY + offset, byte)

	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- DEC (IY+nn)
opcodes[0x35] = function (cpu)
	--logger.debug("DEC (IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )

	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	local byte = read_mem_byte(cpu, cpu.IY + offset)
	contend_read_no_mreq(cpu, cpu.IY + offset, 1)
	byte = DEC(cpu, byte)
	write_mem_byte(cpu, cpu.IY + offset, byte)

	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IY+dd),nn
opcodes[0x36] = function (cpu)
	--logger.debug("LD (IY+dd),nn")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	local byte = read_mem_byte(cpu, cpu.PC + 2)
	contend_read_no_mreq(cpu, cpu.PC + 2, 1)
	contend_read_no_mreq(cpu, cpu.PC + 2, 1)
	write_mem_byte(cpu, cpu.IY + offset, byte)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- ADD IY,SP
opcodes[0x39] = function (cpu)
	--logger.debug("ADD IY,SP")
	cpu.IY = ADD_16(cpu, cpu.IY, cpu.SP)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD B,IYh
opcodes[0x44] = function (cpu)
	--logger.debug("LD B,IYh")
	cpu.B = (cpu.IY & 0xff00) >> 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD B,IYl
opcodes[0x45] = function (cpu)
	--logger.debug("LD B,IYl")
	cpu.B = cpu.IY & 0x00ff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD B,(IY+nn)
opcodes[0x46] = function (cpu)
	--logger.debug("LD B,(IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	cpu.B = read_mem_byte(cpu, cpu.IY + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD C,IYh
opcodes[0x4c] = function (cpu)
	--logger.debug("LD C,IYh")
	cpu.C = (cpu.IY & 0xff00) >> 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD C,IYl
opcodes[0x4d] = function (cpu)
	--logger.debug("LD C,IYl")
	cpu.C = cpu.IY & 0x00ff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD C,(IY+nn)
opcodes[0x4e] = function (cpu)
	--logger.debug("LD C,(IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	cpu.C = read_mem_byte(cpu, cpu.IY + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD D,IYh
opcodes[0x54] = function (cpu)
	--logger.debug("LD D,IYh")
	cpu.D = (cpu.IY & 0xff00) >> 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD D,IYl
opcodes[0x55] = function (cpu)
	--logger.debug("LD D,IYl")
	cpu.D = cpu.IY & 0x00ff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD D,(IY+nn)
opcodes[0x56] = function (cpu)
	--logger.debug("LD D,(IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	cpu.D = read_mem_byte(cpu, cpu.IY + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD E,IYh
opcodes[0x5c] = function (cpu)
	--logger.debug("LD E,IYh")
	cpu.E = (cpu.IY & 0xff00) >> 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD E,IYl
opcodes[0x5d] = function (cpu)
	--logger.debug("LD E,IYl")
	cpu.E = cpu.IY & 0x00ff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD E,(IY+nn)
opcodes[0x5e] = function (cpu)
	--logger.debug("LD E,(IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	cpu.E = read_mem_byte(cpu, cpu.IY + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD IYh,B
opcodes[0x60] = function (cpu)
	--logger.debug("LD IYh,B")
	cpu.IY = (cpu.B << 8) | (cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYh,C
opcodes[0x61] = function (cpu)
	--logger.debug("LD IYh,C")
	cpu.IY = (cpu.C << 8) | (cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYh,D
opcodes[0x62] = function (cpu)
	--logger.debug("LD IYh,D")
	cpu.IY = (cpu.D << 8) | (cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYh,E
opcodes[0x63] = function (cpu)
	--logger.debug("LD IYh,E")
	cpu.IY = (cpu.E << 8) | (cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYh,IYh
opcodes[0x64] = function (cpu)
	--logger.debug("LD IYh,IYh")
	-- LD IYh,IYh
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYh,IYl
opcodes[0x65] = function (cpu)
	--logger.debug("LD IYh,IYl")
	local IYl = cpu.IY & 0x00ff
	cpu.IY = IYl | (IYl << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD H,(IY+nn)
opcodes[0x66] = function (cpu)
	--logger.debug("LD H,(IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	cpu.H = read_mem_byte(cpu, cpu.IY + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD IYh,A
opcodes[0x67] = function (cpu)
	--logger.debug("LD IYh,A")
	cpu.IY = (cpu.A << 8) | (cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYl,B
opcodes[0x68] = function (cpu)
	--logger.debug("LD IYl,B")
	cpu.IY = (cpu.IY & 0xff00) | cpu.B
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYl,C
opcodes[0x69] = function (cpu)
	--logger.debug("LD IYl,C")
	cpu.IY = (cpu.IY & 0xff00) | cpu.C
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYl,D
opcodes[0x6a] = function (cpu)
	--logger.debug("LD IYl,D")
	cpu.IY = (cpu.IY & 0xff00) | cpu.D
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYl,E
opcodes[0x6b] = function (cpu)
	--logger.debug("LD IYl,E")
	cpu.IY = (cpu.IY & 0xff00) | cpu.E
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYl,IYh
opcodes[0x6c] = function (cpu)
	--logger.debug("LD IYl,IYh")
	local high, low = high_low(cpu.IY)
	cpu.IY = high | (high << 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD IYl,IYl
opcodes[0x6d] = function (cpu)
	--logger.debug("LD IYl,IYl")
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD L,IY+nn
opcodes[0x6e] = function (cpu)
	--logger.debug("LD L,(IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	cpu.L = read_mem_byte(cpu, cpu.IY + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD IYl,A
opcodes[0x6f] = function (cpu)
	--logger.debug("LD IYl,A")
	cpu.IY = (cpu.IY & 0xff00) | cpu.A
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD (IY+nn),B
opcodes[0x70] = function (cpu)
	--logger.debug("LD (IY+nn),B")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IY + offset, cpu.B)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IY+nn),C
opcodes[0x71] = function (cpu)
	--logger.debug("LD (IY+nn),C")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IY + offset, cpu.C)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IY+nn),D
opcodes[0x72] = function (cpu)
	--logger.debug("LD (IY+nn),D")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IY + offset, cpu.D)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IY+nn),E
opcodes[0x73] = function (cpu)
	--logger.debug("LD (IY+nn),E")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IY + offset, cpu.E)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IY+nn),H
opcodes[0x74] = function (cpu)
	--logger.debug("LD (IY+nn),H")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IY + offset, cpu.H)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IY+nn),L
opcodes[0x75] = function (cpu)
	--logger.debug("LD (IY+nn),L")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IY + offset, cpu.L)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (IY+nn),A
opcodes[0x77] = function (cpu)
	--logger.debug("LD (IY+nn),A")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	write_mem_byte(cpu, cpu.IY + offset, cpu.A)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD A,IYh
opcodes[0x7c] = function (cpu)
	--logger.debug("LD A,IYh")
	cpu.A = (cpu.IY & 0xff00) >> 8
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD A,IYl
opcodes[0x7d] = function (cpu)
	--logger.debug("LD A,IYl")
	cpu.A = cpu.IY & 0x00ff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- LD A,(IY+nn)
opcodes[0x7e] = function (cpu)
	--logger.debug("LD A,(IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	cpu.A = read_mem_byte(cpu, cpu.IY + offset)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- ADD A,IYh
opcodes[0x84] = function (cpu)
	--logger.debug("ADD A,IYh")
	ADD_8(cpu, (cpu.IY & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- ADD A,IYl
opcodes[0x85] = function (cpu)
	--logger.debug("ADD A,IYl")
	ADD_8(cpu, cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- ADD A,(IY+nn)
opcodes[0x86] = function (cpu)
	--logger.debug("ADD A,(IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	local byte = read_mem_byte(cpu, cpu.IY + offset)
	ADD_8(cpu, byte)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- ADC A,IYh
opcodes[0x8c] = function (cpu)
	--logger.debug("ADC A,IYh")
	ADC_8(cpu, (cpu.IY & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- ADC A,IYl
opcodes[0x8d] = function (cpu)
	--logger.debug("ADC A,IYl")
	ADC_8(cpu, cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- ADC A,(IY+nn)
opcodes[0x8e] = function (cpu)
	--logger.debug("ADC A,(IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	ADC_8(cpu, read_mem_byte(cpu, cpu.IY + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- SUB IYh
opcodes[0x94] = function (cpu)
	--logger.debug("SUB IYh")
	SUB(cpu, (cpu.IY & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- SUB IYl
opcodes[0x95] = function (cpu)
	--logger.debug("SUB IYl")
	SUB(cpu, cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- SUB A,(IY+nn)
opcodes[0x96] = function (cpu)
	--logger.debug("SUB A,(IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	SUB(cpu, read_mem_byte(cpu, cpu.IY + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- SBC A,IYh
opcodes[0x9c] = function (cpu)
	--logger.debug("SBC A,IYh")
	SBC(cpu, (cpu.IY & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- SBC A,IYl
opcodes[0x9d] = function (cpu)
	--logger.debug("SBC A,IYl")
	SBC(cpu, cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- SBC A,(IY+nn)
opcodes[0x9e] = function (cpu)
	--logger.debug("SBC A,(IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	SBC(cpu, read_mem_byte(cpu, cpu.IY + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- AND IYh
opcodes[0xa4] = function (cpu)
	--logger.debug("AND IYh")
	AND(cpu, (cpu.IY & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- AND IYl
opcodes[0xa5] = function (cpu)
	--logger.debug("AND IYl")
	AND(cpu, cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- AND (IY+nn)
opcodes[0xa6] = function (cpu)
	--logger.debug("AND (IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	AND(cpu, read_mem_byte(cpu, cpu.IY + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- XOR IYh
opcodes[0xac] = function (cpu)
	--logger.debug("XOR IYh")
	XOR(cpu, (cpu.IY & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- XOR IYl
opcodes[0xad] = function (cpu)
	--logger.debug("XOR IYl")
	XOR(cpu, cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- XOR (IY+nn)
opcodes[0xae] = function (cpu)
	--logger.debug("XOR (IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	XOR(cpu, read_mem_byte(cpu, cpu.IY + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- OR IYh
opcodes[0xb4] = function (cpu)
	--logger.debug("OR IYh")
	OR(cpu, (cpu.IY & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- OR IYl
opcodes[0xb5] = function (cpu)
	--logger.debug("OR IYl")
	OR(cpu, cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- OR (IY+nn)
opcodes[0xb6] = function (cpu)
	--logger.debug("OR (IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	OR(cpu, read_mem_byte(cpu, cpu.IY + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- CP IYh
opcodes[0xbc] = function (cpu)
	--logger.debug("CP IYh")
	CP(cpu, (cpu.IY & 0xff00) >> 8)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CP IYl
opcodes[0xbd] = function (cpu)
	--logger.debug("CP IYl")
	CP(cpu, cpu.IY & 0x00ff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CP (IY+nn)
opcodes[0xbe] = function (cpu)
	--logger.debug("CP (IY+nn)")
	local offset = sign_extend( read_mem_byte(cpu, cpu.PC + 1) )
	for i=1,5 do
		contend_read_no_mreq(cpu, cpu.PC + 1, 1)
	end
	CP(cpu, read_mem_byte(cpu, cpu.IY + offset))
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- 0xCB FDCB opcodes
opcodes[0xcb] = function (cpu)
	--logger.warning("shifting FDCB")
	contend_read_no_mreq(cpu, cpu.PC + 1, 3)
	local offset = sign_extend( read_mem_byte_internal(cpu, cpu.PC + 1) )
	contend_read_no_mreq(cpu, cpu.PC + 2, 3)
	local opcode = read_mem_byte_internal(cpu, cpu.PC + 2)
	contend_read_no_mreq(cpu, cpu.PC + 2, 1)
	contend_read_no_mreq(cpu, cpu.PC + 2, 1)
	local f = opcodes_fdcb[opcode]
	if not f then
		--logger.fatal("Opcode FDCB 0x%0x (%d) not found", opcode, opcode)
		error("")
		return
	end
	f(cpu, offset)
	cpu.PC = (cpu.PC + 3) & 0xffff
end

-- POP IY
opcodes[0xe1] = function (cpu)
	--logger.debug("POP IY")
	local low  = read_mem_byte(cpu, cpu.SP)
	local high = read_mem_byte(cpu, cpu.SP + 1)
	cpu.IY = low | (high << 8)
	cpu.SP = (cpu.SP + 2) & 0xffff
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- EX (SP),IY
opcodes[0xe3] = function (cpu)
	--logger.debug("EX (SP),IY")
	local temp = cpu.IY
	cpu.IY = read_mem_word(cpu, cpu.SP)
	contend_read_no_mreq(cpu, cpu.SP + 1, 1)
	write_mem_byte(cpu, cpu.SP + 1, (temp & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, temp & 0xff)
	contend_write_no_mreq(cpu, cpu.SP, 1)
	contend_write_no_mreq(cpu, cpu.SP, 1)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- PUSH IY
opcodes[0xe5] = function (cpu)
	--logger.debug("PUSH IY")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)

	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, cpu.SP + 1, (cpu.IY & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, cpu.IY & 0xff)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- JP IY
opcodes[0xe9] = function (cpu)
	--logger.debug("JP IY")
	cpu.PC = cpu.IY
end

-- LD SP,IY
opcodes[0xf9] = function (cpu)
	--logger.debug("LD SP,IY")
	cpu.SP = cpu.IY
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.PC = (cpu.PC + 1) & 0xffff
end


return opcodes
