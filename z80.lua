--
-- Base opcodes
--
local logger = assert(require "logger")
local common_ops = require "opcodes.common"

---
-- Will be bound to the actual functions to be called later in bind_io
--
local contend
local contend_read_no_mreq
local contend_write_no_mreq
local read_mem_byte
local read_mem_byte_internal
local read_mem_word
local write_mem_byte
local write_mem_word

local write_port
local read_port



assert(logger)

local ADD_16 = common_ops.ADD_16
local ADD_8  = common_ops.ADD_8
local ADC_8  = common_ops.ADC_8
local SUB    = common_ops.SUB
local AND    = common_ops.AND
local OR     = common_ops.OR
local XOR    = common_ops.XOR
local INC    = common_ops.INC
local DEC    = common_ops.DEC
local SBC    = common_ops.SBC
local CP     = common_ops.CP

local opcodes_cb = require "opcodes.cb"
local opcodes_dd = require "opcodes.dd"
local opcodes_ed = require "opcodes.ed"
local opcodes_fd = require "opcodes.fd"
local daa_table = require "opcodes.daa"


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

local function sign_extend (v)
	return v < 128 and v or v - 256
end

local function high_low (value)
	return (value & 0xff00) >> 8, value & 0x00ff
end

local function incr_16 (value)
	return (value + 1) & 0xffff
end

local function decr_16 (value)
	return (value - 1) & 0xffff
end

local function LD_DE (cpu, value)
	cpu.D, cpu.E = high_low(value)
end

local function LD_HL_d (cpu, address)
	cpu.H, cpu.L = high_low(read_mem_word(cpu, address))
end

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



local opcodes = {}


function opcodes.bind_io (binds)
	contend = assert(binds.memory.contend)
	contend_read_no_mreq = assert(binds.memory.contend_read_no_mreq)
	contend_write_no_mreq = assert(binds.memory.contend_write_no_mreq)
	read_mem_byte = assert(binds.memory.read_mem_byte)
	read_mem_byte_internal = assert(binds.memory.read_mem_byte_internal)
	read_mem_word = assert(binds.memory.read_mem_word)
	write_mem_byte = assert(binds.memory.write_mem_byte)
	write_mem_word = assert(binds.memory.write_mem_word)

	write_port = assert(binds.ports.write_port)
	read_port = assert(binds.ports.read_port)

	opcodes_cb.bind_io(binds)
	opcodes_dd.bind_io(binds)
	opcodes_ed.bind_io(binds)
	opcodes_fd.bind_io(binds)
	common_ops.bind_io(binds)
end



-- NOP
opcodes[0] = function (cpu)
	--logger.debug("NOP")
end

-- LD BC,nnnn
opcodes[0x01] = function (cpu)
	local value = read_mem_word(cpu, cpu.PC)
	--logger.debug("LD BC, $%04x", value)
	cpu.B, cpu.C = high_low(value)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (BC),A
opcodes[0x02] = function (cpu)
	--logger.debug("LD (BC),A")
	write_mem_byte(cpu, cpu.C | cpu.B << 8, cpu.A)
end

--
opcodes[0x03] = function (cpu)
	--logger.debug("INC BC")
	cpu.B, cpu.C = increment_16(cpu.B, cpu.C)
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
end

-- INC B
opcodes[0x04] = function (cpu)
	--logger.debug("INC B")
	cpu.B = INC(cpu, cpu.B)
end

-- DEC B
opcodes[0x05] = function (cpu)
	--logger.debug("DEC B")
	cpu.B = DEC(cpu, cpu.B)
end

-- LD B,nn
opcodes[0x06] = function (cpu)
	--logger.debug("LD B,nn")
	cpu.B = read_mem_byte(cpu, cpu.PC)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RLCA
opcodes[0x07] = function (cpu)
	--logger.debug("RLCA")
	cpu.A = ( (cpu.A & 0x7f) << 1 ) | ( cpu.A >> 7 )
	cpu.F = ( cpu.F & ( 0x04 | 0x40 | 0x80 ) ) |
			( cpu.A & ( 0x01 | 0x08 | 0x20 ) )
end

-- EX AF,AF'
opcodes[0x08] = function (cpu)
	--logger.debug("EX AF,AF'")
	cpu.A, cpu.Ap = cpu.Ap, cpu.A
	cpu.F, cpu.Fp = cpu.Fp, cpu.F
end

-- ADD HL,BC
opcodes[0x09] = function (cpu)
	--logger.debug("ADD HL,BC")
	local result = ADD_16(cpu, cpu.L | cpu.H << 8, cpu.C | cpu.B << 8)
	cpu.H, cpu.L = high_low(result)
end

-- LD A,(BC)
opcodes[0x0a] = function (cpu)
	--logger.debug("LD A,(BC)")
	cpu.A = read_mem_byte(cpu, cpu.C | cpu.B << 8)
end

-- DEC BC
opcodes[0x0b] = function (cpu)
	--logger.debug("DEC BC")
	cpu.B, cpu.C = decrement_16(cpu.B, cpu.C)
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
end

-- INC C
opcodes[0x0c] = function (cpu)
	--logger.debug("INC C")
	cpu.C = INC(cpu, cpu.C)
end

-- DEC C
opcodes[0x0d] = function (cpu)
	--logger.debug("DEC C")
	cpu.C = DEC(cpu, cpu.C)
end

-- LD C,nn
opcodes[0x0e] = function (cpu)
	--logger.debug("LD C,nn")
	cpu.C = read_mem_byte(cpu, cpu.PC)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RRCA
opcodes[0x0f] = function (cpu)
	--logger.debug("RRCA")
	cpu.F = ( cpu.F & ( FLAG_PV | FLAG_Z | FLAG_S ) ) | ( cpu.A & FLAG_C )
	cpu.A = ( ( cpu.A >> 1) | ( cpu.A << 7 ) ) & 0xff
	cpu.F = cpu.F | ( cpu.A & ( FLAG_B3 | FLAG_B5 ) )
end

-- DJNZ
opcodes[0x10] = function (cpu)
	--logger.debug("DJNZ")
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	--cpu.tstates = cpu.tstates + 1
	cpu.B = (cpu.B - 1) & 0xff
	if cpu.B ~= 0 then
		local offset = sign_extend(read_mem_byte(cpu, cpu.PC))
		for i=1,5 do
			contend(cpu, cpu.PC, 1) -- pc+1:1
		end
		cpu.PC = (cpu.PC + offset + 1) & 0xffff
	else
		contend(cpu, cpu.PC, 3) -- pc+1:3
		cpu.PC = (cpu.PC + 1) & 0xffff
	end
end

-- 0x11 LD DE, nnnn
opcodes[0x11] = function (cpu)
	local value = read_mem_word(cpu, cpu.PC)
	--logger.debug("LD DE, $%04x", value)
	LD_DE(cpu, value)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (DE),A
opcodes[0x12] = function (cpu)
	--logger.debug("LD (DE),A")
	write_mem_byte(cpu, cpu.E | cpu.D << 8, cpu.A)
end

-- INC DE
opcodes[0x13] = function (cpu)
	--logger.debug("INC DE")
	cpu.D, cpu.E = increment_16(cpu.D, cpu.E)
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
end

-- INC D
opcodes[0x14] = function (cpu)
	--logger.debug("INC D")
	cpu.D = INC(cpu, cpu.D)
end

-- DEC D
opcodes[0x15] = function (cpu)
	--logger.debug("DEC D")
	cpu.D = DEC(cpu, cpu.D)
end

-- LD D,nn
opcodes[0x16] = function (cpu)
	--logger.debug("LD D,nn")
	cpu.D = read_mem_byte(cpu, cpu.PC)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RLA
opcodes[0x17] = function (cpu)
	--logger.debug("RLA")
	local byte = cpu.A
	cpu.A = ( (cpu.A & 0x7f) << 1 ) | ( cpu.F & 0x01 )
	cpu.F = ( cpu.F & ( 0x04 | 0x40 | 0x80 ) ) |
			( cpu.A & ( 0x08 | 0x20 ) ) | ( byte >> 7 )
end

-- JR
opcodes[0x18] = function (cpu)
	--logger.debug("JR")
	local offset = sign_extend(read_mem_byte(cpu, cpu.PC))
	for i=1,5 do
		contend(cpu, cpu.PC, 1)  -- pc+1:1
	end
	cpu.PC = (cpu.PC + offset + 1) & 0xffff
end

-- ADD HL,DE
opcodes[0x19] = function (cpu)
	--logger.debug("ADD HL,DE")
	local result = ADD_16(cpu, cpu.L | cpu.H << 8, cpu.E | cpu.D << 8)
	cpu.H, cpu.L = high_low(result)
end

-- LD A,(DE)
opcodes[0x1a] = function (cpu)
	--logger.debug("LD A,(DE)")
	cpu.A = read_mem_byte(cpu, cpu.E | cpu.D << 8)
end

-- DEC DE
opcodes[0x1b] = function (cpu)
	--logger.debug("DEC DE")
	cpu.D, cpu.E = decrement_16(cpu.D, cpu.E)
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
end

-- INC E
opcodes[0x1c] = function (cpu)
	--logger.debug("INC E")
	cpu.E = INC(cpu, cpu.E)
end

-- DEC E
opcodes[0x1d] = function (cpu)
	--logger.debug("DEC E")
	cpu.E = DEC(cpu, cpu.E)
end

-- LD E,nn
opcodes[0x1e] = function (cpu)
	--logger.debug("LD E,nn")
	cpu.E = read_mem_byte(cpu, cpu.PC)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RRA
opcodes[0x1f] = function (cpu)
	--logger.debug("RRA")
	local temp = cpu.A
	cpu.A = ( cpu.A >> 1 ) | ( (cpu.F & 0x01) << 7 )
	cpu.F = ( cpu.F & ( 0x04 | 0x40 | 0x80 ) ) |
			( cpu.A & ( 0x08 | 0x20 ) ) | ( temp & 0x01 )
end

-- JR NZ,offset
opcodes[0x20] = function (cpu)
	--logger.debug("JR NZ,offset")
	if cpu.F & FLAG_Z == 0 then  -- if(!(F & FLAG_Z)) {
		local offset = sign_extend( read_mem_byte(cpu, cpu.PC) )
		for i=1,5 do
			contend_read_no_mreq(cpu, cpu.PC, 1)	--pc+1:1
		end
		cpu.PC = (cpu.PC + offset + 1) & 0xffff
	else
		contend(cpu, cpu.PC, 3)
		cpu.PC = (cpu.PC + 1) & 0xffff
	end
end

-- LD HL,nnnn
opcodes[0x21] = function (cpu)
	local word = read_mem_word(cpu, cpu.PC)
	--logger.debug("LD HL,$%04x", word)
	cpu.H, cpu.L = high_low(word)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (nnnn),HL
opcodes[0x22] = function (cpu)
	local address = read_mem_word(cpu, cpu.PC)
	--logger.debug("LD ($%04x),HL", address)
	write_mem_word(cpu, address, cpu.L | cpu.H << 8)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- INC HL
opcodes[0x23] = function (cpu)
	--logger.debug("INC HL")
	cpu.H, cpu.L = increment_16(cpu.H, cpu.L)
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
end

-- INC H
opcodes[0x24] = function (cpu)
	--logger.debug("INC H")
	cpu.H = INC(cpu, cpu.H)
end

-- DEC H
opcodes[0x25] = function (cpu)
	--logger.debug("DEC H")
	cpu.H = DEC(cpu, cpu.H)
end

-- LD H,nn
opcodes[0x26] = function (cpu)
	--logger.debug("LD H,nn")
	cpu.H = read_mem_byte(cpu, cpu.PC)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- DAA
opcodes[0x27] = function (cpu)
	--logger.debug("DAA")
	local lookup = cpu.A
	if cpu.F & FLAG_C ~= 0 then lookup = lookup | 0x0100 end
	if cpu.F & FLAG_H ~= 0 then lookup = lookup | 0x0200 end
	if cpu.F & FLAG_N ~= 0 then lookup = lookup | 0x0400 end

	local word = daa_table[lookup]
	cpu.A, cpu.F = high_low(word)
end

-- JR Z,offset
opcodes[0x28] = function (cpu)
	--logger.debug("JR Z,offset")
	if cpu.F & FLAG_Z ~= 0 then
		local offset = sign_extend( read_mem_byte(cpu, cpu.PC) )
		for i=1,5 do
			contend(cpu, cpu.PC, 1)	--pc+1:1
		end
		cpu.PC = (cpu.PC + offset + 1) & 0xffff
	else
		contend(cpu, cpu.PC, 3)	--pc+1:3
		cpu.PC = (cpu.PC + 1) & 0xffff
	end
end

-- ADD HL,HL
opcodes[0x29] = function (cpu)
	--logger.debug("ADD HL,HL")
	local result = ADD_16(cpu, cpu.L | cpu.H << 8, cpu.L | cpu.H << 8)
	cpu.H, cpu.L = high_low(result)
end

-- 0x2A LD HL,(nnnn)
opcodes[0x2A] = function (cpu)
	local address = read_mem_word(cpu, cpu.PC)
	--logger.debug("LD HL,($%04x)", address)
	LD_HL_d(cpu, address)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- DEC HL
opcodes[0x2b] = function (cpu)
	--logger.debug("DEC HL")
	cpu.H, cpu.L = decrement_16(cpu.H, cpu.L)
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
end

-- INC L
opcodes[0x2c] = function (cpu)
	--logger.debug("INC L")
	cpu.L = INC(cpu, cpu.L)
end

-- DEC L
opcodes[0x2d] = function (cpu)
	--logger.debug("DEC L")
	cpu.L = DEC(cpu, cpu.L)
end

-- LD L,nn
opcodes[0x2e] = function (cpu)
	--logger.debug("LD L,nn")
	cpu.L = read_mem_byte(cpu, cpu.PC)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CPL
opcodes[0x2f] = function (cpu)
	--logger.debug("CPL")
	cpu.A = cpu.A ~ 0xff
	cpu.F = ( cpu.F & ( 0x01 | 0x04 | 0x40 | 0x80 ) ) |
			( cpu.A & ( 0x08 | 0x20 ) ) |
			( 0x02 | 0x10 )
end

-- JR NC,offset
opcodes[0x30] = function (cpu)
	--logger.debug("JR NC,offset")
	if cpu.F & FLAG_C == 0 then
		local offset = sign_extend( read_mem_byte(cpu, cpu.PC) )
		for i=1,5 do
			contend(cpu, cpu.PC, 1)	--pc+1:1
		end
		cpu.PC = (cpu.PC + offset + 1) & 0xffff
	else
		contend(cpu, cpu.PC, 3)	--pc+1:3
		cpu.PC = (cpu.PC + 1) & 0xffff
	end
end

-- LD SP,nnnn
opcodes[0x31] = function (cpu)
	--logger.debug("LD SP,nnnn")
	cpu.SP = read_mem_word(cpu, cpu.PC)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- LD (nnnn),A
opcodes[0x32] = function (cpu)
	local address = read_mem_word(cpu, cpu.PC)
	--logger.debug("LD ($%04x),A", address)
	write_mem_byte(cpu, address, cpu.A)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- INC SP
opcodes[0x33] = function (cpu)
	--logger.debug("INC SP")
	cpu.SP = (cpu.SP + 1) & 0xffff
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
end

-- INC (HL)
opcodes[0x34] = function (cpu)
	--logger.debug("INC (HL)")
	local HL = cpu.L | cpu.H << 8
	local byte = read_mem_byte(cpu, HL)
	contend(cpu, HL, 1)
	byte = INC(cpu, byte)
	write_mem_byte(cpu, HL, byte)
end

-- DEC (HL)
opcodes[0x35] = function (cpu)
	--logger.debug("DEC (HL)")
	local HL = cpu.L | cpu.H << 8
	local byte = read_mem_byte(cpu, HL)
	contend(cpu, HL, 1)
	byte = DEC(cpu, byte)
	write_mem_byte(cpu, HL, byte)
end

-- LD (HL),nn
opcodes[0x36] = function (cpu)
	--logger.debug("LD (HL),nn")
	local byte = read_mem_byte(cpu, cpu.PC)
	write_mem_byte(cpu, (cpu.H << 8) + cpu.L, byte )
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- SCF
opcodes[0x37] = function (cpu)
	--logger.debug("SCF")
	cpu.F = ( cpu.F & ( FLAG_PV | FLAG_Z | FLAG_S ) ) |
			( cpu.A & ( FLAG_B3 | FLAG_B5         ) ) |
			FLAG_C
end

-- JR C
opcodes[0x38] = function (cpu)
	--logger.debug("JR C")
	if cpu.F & FLAG_C ~= 0 then
		local offset = sign_extend(read_mem_byte(cpu, cpu.PC))
		for i=1,5 do
			contend(cpu, cpu.PC, 1)  -- pc+1:1
		end
		cpu.PC = (cpu.PC + offset + 1) & 0xffff
	else
		contend(cpu, cpu.PC, 3) -- pc+1:3
		cpu.PC = (cpu.PC + 1) & 0xffff
	end
end

-- ADD HL,SP
opcodes[0x39] = function (cpu)
	--logger.debug("ADD HL,SP")
	local result = ADD_16(cpu, cpu.L | cpu.H << 8, cpu.SP)
	cpu.H, cpu.L = high_low(result)
end

-- LD A,(nnnn)
opcodes[0x3a] = function (cpu)
	--logger.debug("LD A,(nnnn)")
	local address = read_mem_word(cpu, cpu.PC)
	cpu.A = read_mem_byte(cpu, address)
	cpu.PC = (cpu.PC + 2) & 0xffff
end

-- DEC SP
opcodes[0x3b] = function (cpu)
	--logger.debug("DEC SP")
	cpu.SP = (cpu.SP - 1) & 0xffff
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
end

-- INC A
opcodes[0x3c] = function (cpu)
	--logger.debug("INC A")
	cpu.A = INC(cpu, cpu.A)
end

-- DEC A
opcodes[0x3d] = function (cpu)
	--logger.debug("DEC A")
	cpu.A = DEC(cpu, cpu.A)
end

-- LD A,nn
opcodes[0x3E] = function (cpu)
	--logger.debug("LD A,nn")
	cpu.A = read_mem_byte(cpu, cpu.PC)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CCF
opcodes[0x3f] = function (cpu)
	--logger.debug("CCF")
	cpu.F = ( cpu.F & ( FLAG_PV | FLAG_Z | FLAG_S ) ) |
			( ( cpu.F & FLAG_C ~= 0 ) and FLAG_H or FLAG_C ) or ( cpu.A & ( FLAG_B3 | FLAG_B5 ) )
end

-- LD B,B
opcodes[0x40] = function (cpu)
	--logger.debug("LD B,B")
end

-- LD B,C
opcodes[0x41] = function (cpu)
	--logger.debug("LD B,C")
	cpu.B = cpu.C
end

-- LD B,D
opcodes[0x42] = function (cpu)
	--logger.debug("LD B,D")
	cpu.B = cpu.D
end

-- LD B,E
opcodes[0x43] = function (cpu)
	--logger.debug("LD B,E")
	cpu.B = cpu.E
end

-- LD B,H
opcodes[0x44] = function (cpu)
	--logger.debug("LD B,H")
	cpu.B = cpu.H
end

-- LD B,L
opcodes[0x45] = function (cpu)
	--logger.debug("LD B,L")
	cpu.B = cpu.L
end

-- LD B,(HL)
opcodes[0x46] = function (cpu)
	--logger.debug("LD B,(HL)")
	cpu.B = read_mem_byte(cpu, cpu.L | cpu.H << 8)
end

-- 0x47 LD B,A
opcodes[0x47] = function (cpu)
	--logger.debug("LD B,A")
	cpu.B = cpu.A
end

-- LD C,B
opcodes[0x48] = function (cpu)
	--logger.debug("LD C,B")
	cpu.C = cpu.B
end

-- LD C,C
opcodes[0x49] = function (cpu)
	--logger.debug("LD C,C")
end

-- LD C,D
opcodes[0x4a] = function (cpu)
	--logger.debug("LD C,D")
	cpu.C = cpu.D
end

-- LD C,E
opcodes[0x4b] = function (cpu)
	--logger.debug("LD C,E")
	cpu.C = cpu.E
end

-- LD C,H
opcodes[0x4c] = function (cpu)
	--logger.debug("LD C,H")
	cpu.C = cpu.H
end

-- LD C,L
opcodes[0x4d] = function (cpu)
	--logger.debug("LD C,L")
	cpu.C = cpu.L
end

-- LD C,(HL)
opcodes[0x4e] = function (cpu)
	--logger.debug("LD C,(HL)")
	cpu.C = read_mem_byte(cpu, cpu.L | cpu.H << 8)
end

-- LD C,A
opcodes[0x4f] = function (cpu)
	--logger.debug("LD C,A")
	cpu.C = cpu.A
end

-- LD D,B
opcodes[0x50] = function (cpu)
	--logger.debug("LD D,B")
	cpu.D = cpu.B
end

-- LD D,C
opcodes[0x51] = function (cpu)
	--logger.debug("LD D,C")
	cpu.D = cpu.C
end

-- LD D,D
opcodes[0x52] = function (cpu)
	--logger.debug("LD D,D")
end

-- LD D,E
opcodes[0x53] = function (cpu)
	--logger.debug("LD D,E")
	cpu.D = cpu.E
end

-- LD D,H
opcodes[0x54] = function (cpu)
	--logger.debug("LD D,H")
	cpu.D = cpu.H
end

-- LD D,L
opcodes[0x55] = function (cpu)
	--logger.debug("LD D,L")
	cpu.D = cpu.L
end

-- LD D,(HL)
opcodes[0x56] = function (cpu)
	--logger.debug("LD D,(HL)")
	cpu.D = read_mem_byte(cpu, cpu.L | cpu.H << 8)
end

-- LD D,A
opcodes[0x57] = function (cpu)
	--logger.debug("LD D,A")
	cpu.D = cpu.A
end

-- LD E,B
opcodes[0x58] = function (cpu)
	--logger.debug("LD E,B")
	cpu.E = cpu.B
end

-- LD E,C
opcodes[0x59] = function (cpu)
	--logger.debug("LD E,C")
	cpu.E = cpu.C
end

-- LD E,D
opcodes[0x5a] = function (cpu)
	--logger.debug("LD E,D")
	cpu.E = cpu.D
end

-- LD E,E
opcodes[0x5b] = function (cpu)
	--logger.debug("LD E,E")
end

-- LD E,H
opcodes[0x5c] = function (cpu)
	--logger.debug("LD E,H")
	cpu.E = cpu.H
end

-- LD E,L
opcodes[0x5d] = function (cpu)
	--logger.debug("LD E,L")
	cpu.E = cpu.L
end

-- LD E,(HL)
opcodes[0x5e] = function (cpu)
	--logger.debug("LD E,(HL)")
	cpu.E = read_mem_byte(cpu, cpu.L | cpu.H << 8)
end

-- LD E,A
opcodes[0x5f] = function (cpu)
	--logger.debug("LD E,A")
	cpu.E = cpu.A
end

-- LD H,B
opcodes[0x60] = function (cpu)
	--logger.debug("LD H,B")
	cpu.H = cpu.B
end

-- LD H,C
opcodes[0x61] = function (cpu)
	--logger.debug("LD H,C")
	cpu.H = cpu.C
end

-- LD H,D
opcodes[0x62] = function (cpu)
	--logger.debug("LD H,D")
	cpu.H = cpu.D
end

-- LD H,E
opcodes[0x63] = function (cpu)
	--logger.debug("LD H,E")
	cpu.H = cpu.E
end

-- LD H,H
opcodes[0x64] = function (cpu)
	--logger.debug("LD H,H")
end

-- LD H,L
opcodes[0x65] = function (cpu)
	--logger.debug("LD H,L")
	cpu.H = cpu.L
end

--LD H,(HL)
opcodes[0x66] = function (cpu)
	--logger.debug("LD H,(HL)")
	cpu.H = read_mem_byte(cpu, cpu.L | cpu.H << 8)
end

-- LD H,A
opcodes[0x67] = function (cpu)
	--logger.debug("LD H,A")
	cpu.H = cpu.A
end

-- LD L,B
opcodes[0x68] = function (cpu)
	--logger.debug("LD L,B")
	cpu.L = cpu.B
end

-- LD L,C
opcodes[0x69] = function (cpu)
	--logger.debug("LD L,C")
	cpu.L = cpu.C
end

-- LD L,D
opcodes[0x6a] = function (cpu)
	--logger.debug("LD L,D")
	cpu.L = cpu.D
end

-- LD L,E
opcodes[0x6b] = function (cpu)
	--logger.debug("LD L,E")
	cpu.L = cpu.E
end

-- LD L,H
opcodes[0x6c] = function (cpu)
	--logger.debug("LD L,H")
	cpu.L = cpu.H
end

-- LD L,L
opcodes[0x6d] = function (cpu)
	--logger.debug("LD L,L")
end

-- LD L,(HL)
opcodes[0x6e] = function (cpu)
	--logger.debug("LD L,(HL)")
	cpu.L = read_mem_byte(cpu, cpu.L | cpu.H << 8)
end

-- LD L,A
opcodes[0x6f] = function (cpu)
	--logger.debug("LD L,A")
	cpu.L = cpu.A
end

-- LD (HL),B
opcodes[0x70] = function (cpu)
	--logger.debug("LD (HL),B")
	write_mem_byte(cpu, cpu.L | cpu.H << 8, cpu.B)
end

-- LD (HL),C
opcodes[0x71] = function (cpu)
	--logger.debug("LD (HL),C")
	write_mem_byte(cpu, cpu.L | cpu.H << 8, cpu.C)
end

-- LD (HL),D
opcodes[0x72] = function (cpu)
	--logger.debug("LD (HL),D")
	write_mem_byte(cpu, cpu.L | cpu.H << 8, cpu.D)
end

-- LD (HL),E
opcodes[0x73] = function (cpu)
	--logger.debug("LD (HL),E")
	write_mem_byte(cpu, cpu.L | cpu.H << 8, cpu.E)
end

-- LD (HL),H
opcodes[0x74] = function (cpu)
	--logger.debug("LD (HL),H")
	write_mem_byte(cpu, cpu.L | cpu.H << 8, cpu.H)
end

-- LD (HL),L
opcodes[0x75] = function (cpu)
	--logger.debug("LD (HL),L")
	write_mem_byte(cpu, cpu.L | cpu.H << 8, cpu.L)
end

-- HALT
opcodes[0x76] = function (cpu)
	--logger.debug("HALT")
	cpu.halted = true
	-- remain here until an interrupt fires
	cpu.PC = (cpu.PC - 1) & 0xffff
end

-- LD (HL),A
opcodes[0x77] = function (cpu)
	--logger.debug("LD (HL),A")
	write_mem_byte(cpu, cpu.L | cpu.H << 8, cpu.A)
end

-- LD A,B
opcodes[0x78] = function (cpu)
	--logger.debug("LD A,B")
	cpu.A = cpu.B
end

-- LD A,C
opcodes[0x79] = function (cpu)
	--logger.debug("LD A,C")
	cpu.A = cpu.C
end

-- LD A,D
opcodes[0x7a] = function (cpu)
	--logger.debug("LD A,D")
	cpu.A = cpu.D
end

-- LD A,E
opcodes[0x7b] = function (cpu)
	--logger.debug("LD A,E")
	cpu.A = cpu.E
end

-- LD A,H
opcodes[0x7c] = function (cpu)
	--logger.debug("LD A,H")
	cpu.A = cpu.H
end

-- LD A,L
opcodes[0x7d] = function (cpu)
	--logger.debug("LD A,L")
	cpu.A = cpu.L
end

-- LD A,(HL)
opcodes[0x7e] = function (cpu)
	--logger.debug("LD A,(HL)")
	cpu.A = read_mem_byte(cpu, (cpu.H << 8) + cpu.L)
end

-- LD A,A
opcodes[0x7f] = function (cpu)
	--logger.debug("LD A,A")
end

-- ADD A,B
opcodes[0x80] = function (cpu)
	--logger.debug("ADD A,B")
	ADD_8(cpu, cpu.B)
end

-- ADD A,C
opcodes[0x81] = function (cpu)
	--logger.debug("ADD A,C")
	ADD_8(cpu, cpu.C)
end

-- ADD A,D
opcodes[0x82] = function (cpu)
	--logger.debug("ADD A,D")
	ADD_8(cpu, cpu.D)
end

-- ADD A,E
opcodes[0x83] = function (cpu)
	--logger.debug("ADD A,E")
	ADD_8(cpu, cpu.E)
end

-- ADD A,H
opcodes[0x84] = function (cpu)
	--logger.debug("ADD A,H")
	ADD_8(cpu, cpu.H)
end

-- ADD A,L
opcodes[0x85] = function (cpu)
	--logger.debug("ADD A,L")
	ADD_8(cpu, cpu.L)
end

-- ADD A,(HL)
opcodes[0x86] = function (cpu)
	--logger.debug("ADD A,(HL)")
	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	ADD_8(cpu, byte)
end

-- ADD A,A
opcodes[0x87] = function (cpu)
	--logger.debug("ADD A,A")
	ADD_8(cpu, cpu.A)
end

-- ADC A,B
opcodes[0x88] = function (cpu)
	--logger.debug("ADC A,B")
	ADC_8(cpu, cpu.B)
end

-- ADC A,C
opcodes[0x89] = function (cpu)
	--logger.debug("ADC A,C")
	ADC_8(cpu, cpu.C)
end

-- ADC A,D
opcodes[0x8a] = function (cpu)
	--logger.debug("ADC A,D")
	ADC_8(cpu, cpu.D)
end

-- ADC A,E
opcodes[0x8b] = function (cpu)
	--logger.debug("ADC A,E")
	ADC_8(cpu, cpu.E)
end

-- ADC A,H
opcodes[0x8c] = function (cpu)
	--logger.debug("ADC A,H")
	ADC_8(cpu, cpu.H)
end

-- ADC A,L
opcodes[0x8d] = function (cpu)
	--logger.debug("ADC A,L")
	ADC_8(cpu, cpu.L)
end

-- ADC A,(HL)
opcodes[0x8e] = function (cpu)
	--logger.debug("ADC A,(HL)")
	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	ADC_8(cpu, byte)
end

-- ADC A,A
opcodes[0x8f] = function (cpu)
	--logger.debug("ADC A,A")
	ADC_8(cpu, cpu.A)
end

-- SUB B
opcodes[0x90] = function (cpu)
	--logger.debug("SUB B")
	SUB(cpu, cpu.B)
end

-- SUB C
opcodes[0x91] = function (cpu)
	--logger.debug("SUB C")
	SUB(cpu, cpu.C)
end

-- SUB D
opcodes[0x92] = function (cpu)
	--logger.debug("SUB D")
	SUB(cpu, cpu.D)
end

-- SUB E
opcodes[0x93] = function (cpu)
	--logger.debug("SUB E")
	SUB(cpu, cpu.E)
end

-- SUB H
opcodes[0x94] = function (cpu)
	--logger.debug("SUB H")
	SUB(cpu, cpu.H)
end

-- SUB L
opcodes[0x95] = function (cpu)
	--logger.debug("SUB L")
	SUB(cpu, cpu.L)
end

-- SUB (HL)
opcodes[0x96] = function (cpu)
	--logger.debug("SUB (HL)")
	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	SUB(cpu, byte)
end

-- SUB A
opcodes[0x97] = function (cpu)
	--logger.debug("SUB A")
	SUB(cpu, cpu.A)
end

-- SBC A,B
opcodes[0x98] = function (cpu)
	--logger.debug("SBC A,B")
	SBC(cpu, cpu.B)
end

-- SBC A,C
opcodes[0x99] = function (cpu)
	--logger.debug("SBC A,C")
	SBC(cpu, cpu.C)
end

-- SBC A,D
opcodes[0x9a] = function (cpu)
	--logger.debug("SBC A,D")
	SBC(cpu, cpu.D)
end

-- SBC A,E
opcodes[0x9b] = function (cpu)
	--logger.debug("SBC A,E")
	SBC(cpu, cpu.E)
end

-- SBC A,H
opcodes[0x9c] = function (cpu)
	--logger.debug("SBC A,H")
	SBC(cpu, cpu.H)
end

-- SBC A,L
opcodes[0x9d] = function (cpu)
	--logger.debug("SBC A,L")
	SBC(cpu, cpu.L)
end

-- SBC A,(HL)
opcodes[0x9e] = function (cpu)
	--logger.debug("SBC A,(HL)")
	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	SBC(cpu, byte)
end

-- SBC A,A
opcodes[0x9f] = function (cpu)
	--logger.debug("SBC A,A")
	SBC(cpu, cpu.A)
end

-- AND B
opcodes[0xa0] = function (cpu)
	--logger.debug("AND B")
	AND(cpu, cpu.B)
end

-- AND C
opcodes[0xa1] = function (cpu)
	--logger.debug("AND C")
	AND(cpu, cpu.C)
end

-- AND D
opcodes[0xa2] = function (cpu)
	--logger.debug("AND D")
	AND(cpu, cpu.D)
end

-- AND E
opcodes[0xa3] = function (cpu)
	--logger.debug("AND E")
	AND(cpu, cpu.E)
end

-- AND H
opcodes[0xa4] = function (cpu)
	--logger.debug("AND H")
	AND(cpu, cpu.H)
end

-- AND L
opcodes[0xa5] = function (cpu)
	--logger.debug("AND L")
	AND(cpu, cpu.L)
end

-- AND (HL)
opcodes[0xa6] = function (cpu)
	--logger.debug("AND (HL)")
	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	AND(cpu, byte)
end

-- AND A
opcodes[0xa7] = function (cpu)
	--logger.debug("AND A")
	AND(cpu, cpu.A)
end

-- XOR B
opcodes[0xa8] = function (cpu)
	--logger.debug("XOR B")
	XOR(cpu, cpu.B)
end

-- XOR C
opcodes[0xa9] = function (cpu)
	--logger.debug("XOR C")
	XOR(cpu, cpu.C)
end

-- XOR D
opcodes[0xaa] = function (cpu)
	--logger.debug("XOR D")
	XOR(cpu, cpu.D)
end

-- XOR E
opcodes[0xab] = function (cpu)
	--logger.debug("XOR E")
	XOR(cpu, cpu.E)
end

-- XOR H
opcodes[0xac] = function (cpu)
	--logger.debug("XOR H")
	XOR(cpu, cpu.H)
end

-- XOR L
opcodes[0xad] = function (cpu)
	--logger.debug("XOR L")
	XOR(cpu, cpu.L)
end

-- XOR (HL)
opcodes[0xae] = function (cpu)
	--logger.debug("XOR (HL")
	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	XOR(cpu, byte)
end

-- XOR A
opcodes[0xAF] = function (cpu)
	--logger.debug("XOR A")
	cpu.A = 0
	cpu.F = 0x44
end

-- OR B
opcodes[0xb0] = function (cpu)
	--logger.debug("OR B")
	OR(cpu, cpu.B)
end

-- OR C
opcodes[0xb1] = function (cpu)
	--logger.debug("OR C")
	OR(cpu, cpu.C)
end

-- OR D
opcodes[0xb2] = function (cpu)
	--logger.debug("OR D")
	OR(cpu, cpu.D)
end

-- OR E
opcodes[0xb3] = function (cpu)
	--logger.debug("OR E")
	OR(cpu, cpu.E)
end

-- OR H
opcodes[0xb4] = function (cpu)
	--logger.debug("OR H")
	OR(cpu, cpu.H)
end

-- OR L
opcodes[0xb5] = function (cpu)
	--logger.debug("OR L")
	OR(cpu, cpu.L)
end

-- OR (HL)
opcodes[0xb6] = function (cpu)
	--logger.debug("OR (HL)")
	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	OR(cpu, byte)
end

-- OR A
opcodes[0xb7] = function (cpu)
	--logger.debug("OR A")
	OR(cpu, cpu.A)
end

-- CP B
opcodes[0xb8] = function (cpu)
	--logger.debug("CP B")
	CP(cpu, cpu.B)
end

-- CP C
opcodes[0xb9] = function (cpu)
	--logger.debug("CP C")
	CP(cpu, cpu.C)
end

-- CP D
opcodes[0xba] = function (cpu)
	--logger.debug("CP D")
	CP(cpu, cpu.D)
end

-- CP E
opcodes[0xbb] = function (cpu)
	--logger.debug("CP E")
	CP(cpu, cpu.E)
end

-- CP H
opcodes[0xbc] = function (cpu)
	--logger.debug("CP H")
	CP(cpu, cpu.H)
end

-- CP L
opcodes[0xbd] = function (cpu)
	--logger.debug("CP L")
	CP(cpu, cpu.L)
end

-- CP (HL)
opcodes[0xbe] = function (cpu)
	--logger.debug("CP (HL)")
	local byte = read_mem_byte(cpu, cpu.L | cpu.H << 8)
	CP(cpu, byte)
end

-- CP A
opcodes[0xbf] = function (cpu)
	--logger.debug("CP A")
	CP(cpu, cpu.A)
end

-- RET NZ
opcodes[0xc0] = function (cpu)
	--logger.debug("RET NZ")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	if cpu.F & FLAG_Z == 0 then
		cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
		cpu.SP = (cpu.SP + 2) & 0xffff
	end
end

-- POP BC
opcodes[0xc1] = function (cpu)
	--logger.debug("POP BC")
	cpu.C = read_mem_byte(cpu, cpu.SP)
	cpu.B = read_mem_byte(cpu, (cpu.SP + 1) & 0xffff)
	cpu.SP = (cpu.SP + 2) & 0xffff
end

-- JP NZ,nnnn
opcodes[0xc2] = function (cpu)
	if cpu.F & FLAG_Z == 0 then
		local address = read_mem_word(cpu, cpu.PC)
		--logger.debug("JP NZ,$%04x", address)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, cpu.PC + 1, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- JP nnnn
opcodes[0xC3] = function (cpu)
	--logger.debug("JP nnnn")
	cpu.PC = read_mem_word(cpu, cpu.PC)
end

-- CALL NZ
opcodes[0xc4] = function (cpu)
	--logger.debug("CALL NZ")
	if cpu.F & FLAG_Z == 0 then
		local address = read_mem_word(cpu, cpu.PC)
		contend(cpu, cpu.PC + 1, 1)
		cpu.SP = (cpu.SP - 2) & 0xffff
		local ret_address = cpu.PC + 2
		write_mem_byte(cpu, cpu.SP + 1, (ret_address & 0xff00) >> 8)
		write_mem_byte(cpu, cpu.SP, ret_address & 0xff)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, cpu.PC + 1, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- PUSH BC
opcodes[0xc5] = function (cpu)
	--logger.debug("PUSH BC")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1) -- part of opcode fetch
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, cpu.SP + 1, cpu.B)
	write_mem_byte(cpu, cpu.SP, cpu.C)
end

-- ADD A,nn
opcodes[0xc6] = function (cpu)
	--logger.debug("ADD A,nn")
	ADD_8(cpu, read_mem_byte(cpu, cpu.PC))
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RST 00
opcodes[0xc7] = function (cpu)
	--logger.debug("RST 00")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, cpu.SP + 1, (cpu.PC & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, cpu.PC & 0xff)
	cpu.PC = 0x0000  -- jumps to 00h
end

-- RET Z
opcodes[0xc8] = function (cpu)
	--logger.debug("RET Z")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	if cpu.F & FLAG_Z ~= 0 then
		cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, cpu.SP + 1) << 8
		cpu.SP = (cpu.SP + 2) & 0xffff
	end
end

-- RET
opcodes[0xc9] = function (cpu)
	--logger.debug("RET")
	cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
	cpu.SP = (cpu.SP + 2) & 0xffff
end

-- JP Z,nnnn
opcodes[0xca] = function (cpu)
	if cpu.F & FLAG_Z ~= 0 then
		local address = read_mem_word(cpu, cpu.PC)
		--logger.debug("JP Z,$%04x", address)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, cpu.PC + 1, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- 0xCB CB opcodes
opcodes[0xCB] = function (cpu)
	--logger.warning("shifting CB")
	contend(cpu, cpu.PC, 4)
	-- Increment R register, without modifying the most significant bit
	cpu.R = (cpu.R & 0x80) | ( (cpu.R + 1) & 0x7f)
	local opcode = read_mem_byte_internal(cpu, cpu.PC)
	local f = opcodes_cb[opcode]
	if not f then
		--logger.fatal("Opcode CB 0x%0x (%d) not found", opcode, opcode)
		error(1)
	end
	f(cpu)
end

-- CALL Z
opcodes[0xcc] = function (cpu)
	--logger.debug("CALL Z")
	if cpu.F & FLAG_Z ~= 0 then
		local address = read_mem_word(cpu, cpu.PC)
		contend(cpu, cpu.PC + 1, 1)
		cpu.SP = (cpu.SP - 2) & 0xffff
		local ret_address = cpu.PC + 2
		write_mem_byte(cpu, cpu.SP + 1, (ret_address & 0xff00) >> 8)
		write_mem_byte(cpu, cpu.SP, ret_address & 0xff)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, cpu.PC + 1, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- CALL nnnn
opcodes[0xcd] = function (cpu)
	--logger.debug("CALL nnnn")
	local address = read_mem_word(cpu, cpu.PC)
	contend(cpu, cpu.PC + 1, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	local ret_address = cpu.PC + 2
	write_mem_byte(cpu, cpu.SP + 1, (ret_address & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, ret_address & 0xff)
	cpu.PC = address
end

-- ADC A,nn
opcodes[0xce] = function (cpu)
	--logger.debug("ADC A,nn")
	local byte = read_mem_byte(cpu, cpu.PC)
	ADC_8(cpu, byte)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RST 08
opcodes[0xcf] = function (cpu)
	--logger.debug("RST 08")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, cpu.SP + 1, (cpu.PC & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, cpu.PC & 0xff)
	cpu.PC = 0x0008  -- jumps to 08h
end

-- RET NC
opcodes[0xd0] = function (cpu)
	--logger.debug("RET NC")
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	if cpu.F & FLAG_C == 0 then
		cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, cpu.SP + 1) << 8
		cpu.SP = (cpu.SP + 2) & 0xffff
	end
end

-- POP DE
opcodes[0xd1] = function (cpu)
	--logger.debug("POP DE")
	cpu.E = read_mem_byte(cpu, cpu.SP)
	cpu.D = read_mem_byte(cpu, cpu.SP + 1)
	cpu.SP = (cpu.SP + 2) & 0xffff
end

-- JP NC,nnnn
opcodes[0xd2] = function (cpu)
	if cpu.F & FLAG_C == 0 then
		local address = read_mem_word(cpu, cpu.PC)
		--logger.debug("JP NC,$%04x", address)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, cpu.PC + 1, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- 0xD3 OUT (nn),A
opcodes[0xD3] = function (cpu)
	--logger.debug("OUT (nn), A")
	local port = read_mem_byte(cpu, cpu.PC)
	--contend_io(port, 3)
	write_port(cpu, port + 256 * cpu.A, cpu.A)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CALL NC
opcodes[0xd4] = function (cpu)
	--logger.debug("CALL NC")
	if cpu.F & FLAG_C == 0 then
		local address = read_mem_word(cpu, cpu.PC)
		contend(cpu, cpu.PC + 1, 1)
		cpu.SP = (cpu.SP - 2) & 0xffff
		local ret_address = cpu.PC + 2
		write_mem_byte(cpu, cpu.SP + 1, (ret_address & 0xff00) >> 8)
		write_mem_byte(cpu, cpu.SP, ret_address & 0xff)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, cpu.PC + 1, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- PUSH DE
opcodes[0xd5] = function (cpu)
	--logger.debug("PUSH DE")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, cpu.D)
	write_mem_byte(cpu, cpu.SP, cpu.E)
end

-- SUB nn
opcodes[0xd6] = function (cpu)
	--logger.debug("SUB nn")
	SUB(cpu, read_mem_byte(cpu, cpu.PC))
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RST 10h
opcodes[0xd7] = function (cpu)
	--logger.debug("RST 10")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, (cpu.PC & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, cpu.PC & 0xff)
	cpu.PC = 0x0010  -- jumps to 10h
end

-- RET C
opcodes[0xd8] = function (cpu)
	--logger.debug("RET C")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	if cpu.F & FLAG_C ~= 0 then
		cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, cpu.SP + 1) << 8
		cpu.SP = (cpu.SP + 2) & 0xffff
	end
end

-- EXX
opcodes[0xd9] = function (cpu)
	--logger.debug("EXX")
	cpu.H, cpu.Hp = cpu.Hp, cpu.H
	cpu.L, cpu.Lp = cpu.Lp, cpu.L
	cpu.B, cpu.Bp = cpu.Bp, cpu.B
	cpu.C, cpu.Cp = cpu.Cp, cpu.C
	cpu.D, cpu.Dp = cpu.Dp, cpu.D
	cpu.E, cpu.Ep = cpu.Ep, cpu.E
end

-- JP C,nnnn
opcodes[0xda] = function (cpu)
	if cpu.F & FLAG_C ~= 0 then
		local address = read_mem_word(cpu, cpu.PC)
		--logger.debug("JP C,$%04x", address)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, cpu.PC + 1, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- IN A, nn
opcodes[0xdb] = function (cpu)
	--logger.debug("IN A,nn")
	local port = read_mem_byte(cpu, cpu.PC)
	--IN_A(cpu, port)
	cpu.A = read_port(cpu, cpu.A << 8 | port)
	--M_ContendIO((A << 8) + puerto, 3);
	--A = LeerPuerto((A << 8) + puerto);
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- CALL C
opcodes[0xdc] = function (cpu)
	--logger.debug("CALL C")
	if cpu.F & FLAG_C ~= 0 then
		local address = read_mem_word(cpu, cpu.PC)
		contend(cpu, cpu.PC + 1, 1)
		cpu.SP = (cpu.SP - 2) & 0xffff
		local ret_address = cpu.PC + 2
		write_mem_byte(cpu, cpu.SP + 1, (ret_address & 0xff00) >> 8)
		write_mem_byte(cpu, cpu.SP, ret_address & 0xff)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, cpu.PC + 1, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

--
opcodes[0xdd] = function (cpu)
	--logger.warning("shifting DD")
	contend(cpu, cpu.PC, 4)
	-- Increment R register, without modifying the most significant bit
	cpu.R = (cpu.R & 0x80) | ( (cpu.R + 1) & 0x7f)
	local opcode = read_mem_byte_internal(cpu, cpu.PC)
	local f = opcodes_dd[opcode]
	if not f then
		cpu.PC = (cpu.PC + 1) & 0xffff
		--logger.fatal("Opcode DD 0x%0x (%d) not found", opcode, opcode)
		return
		--error(1)
	end
	f(cpu)
end

-- SBC A,nn
opcodes[0xde] = function (cpu)
	--logger.debug("SBC A,nn")
	local byte = read_mem_byte(cpu, cpu.PC)
	SBC(cpu, byte)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RST 18
opcodes[0xdf] = function (cpu)
	--logger.debug("RST 18")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, cpu.SP + 1, (cpu.PC & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, cpu.PC & 0xff)
	cpu.PC = 0x0018  -- jumps to 18h
end

-- RET PO
opcodes[0xe0] = function (cpu)
	--logger.debug("RET PO")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	if cpu.F & FLAG_PV == 0 then
		cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
		cpu.SP = (cpu.SP + 2) & 0xffff
	end
end

-- POP HL
opcodes[0xe1] = function (cpu)
	--logger.debug("POP HL")
	cpu.L = read_mem_byte(cpu, cpu.SP)
	cpu.H = read_mem_byte(cpu, (cpu.SP + 1) & 0xffff)
	cpu.SP = (cpu.SP + 2) & 0xffff
end

-- JP PO,nnnn
opcodes[0xe2] = function (cpu)
	if cpu.F & FLAG_PV == 0 then
		local address = read_mem_word(cpu, cpu.PC)
		--logger.debug("JP PO,$%04x", address)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, (cpu.PC + 1) & 0xffff, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- EX (SP),HL
opcodes[0xe3] = function (cpu)
	--logger.debug("EX (SP),HL")
	local HL = cpu.L | cpu.H << 8
	cpu.H, cpu.L = high_low( read_mem_word(cpu, cpu.SP) )
	contend_read_no_mreq(cpu, (cpu.SP + 1) & 0xffff, 1)
	write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, (HL & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, HL & 0xff)
	contend_write_no_mreq(cpu, cpu.SP, 1)
	contend_write_no_mreq(cpu, cpu.SP, 1)
end

-- CALL PO
opcodes[0xe4] = function (cpu)
	--logger.debug("CALL PO")
	if cpu.F & FLAG_PV == 0 then
		local address = read_mem_word(cpu, cpu.PC)
		contend(cpu, (cpu.PC + 1) & 0xffff, 1)
		cpu.SP = (cpu.SP - 2) & 0xffff
		local ret_address = cpu.PC + 2
		write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, (ret_address & 0xff00) >> 8)
		write_mem_byte(cpu, cpu.SP, ret_address & 0xff)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, (cpu.PC + 1) & 0xffff, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- PUSH HL
opcodes[0xe5] = function (cpu)
	--logger.debug("PUSH HL")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, cpu.H)
	write_mem_byte(cpu, cpu.SP, cpu.L)
end

-- AND nn
opcodes[0xe6] = function (cpu)
	--logger.debug("AND nn")
	local byte = read_mem_byte(cpu, cpu.PC)
	AND(cpu, byte)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RST 20
opcodes[0xe7] = function (cpu)
	--logger.debug("RST 20")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, (cpu.PC & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, cpu.PC & 0xff)
	cpu.PC = 0x0020  -- jumps to 20h
end

-- RET PV
opcodes[0xe8] = function (cpu)
	--logger.debug("RET PV")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	if cpu.F & FLAG_PV ~= 0 then
		cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
		cpu.SP = (cpu.SP + 2) & 0xffff
	end
end

-- JP HL
opcodes[0xe9] = function (cpu)
	--logger.debug("JP HL")
	cpu.PC = cpu.L | cpu.H << 8
end

-- JP PE,nnnn
opcodes[0xea] = function (cpu)
	if cpu.F & FLAG_PV ~= 0 then
		local address = read_mem_word(cpu, cpu.PC)
		--logger.debug("JP PE,$%04x", address)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, (cpu.PC + 1) & 0xffff, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- EX DE,HL
opcodes[0xeb] = function (cpu)
	--logger.debug("EX DE,HL")
	cpu.D, cpu.E, cpu.H, cpu.L = cpu.H, cpu.L, cpu.D, cpu.E
end

-- CALL PE
opcodes[0xec] = function (cpu)
	--logger.debug("CALL PE")
	if cpu.F & FLAG_PV ~= 0 then
		local address = read_mem_word(cpu, cpu.PC)
		contend(cpu, (cpu.PC + 1) & 0xffff, 1)
		cpu.SP = (cpu.SP - 2) & 0xffff
		local ret_address = cpu.PC + 2
		write_mem_byte(cpu, cpu.SP + 1, (ret_address & 0xff00) >> 8)
		write_mem_byte(cpu, cpu.SP, ret_address & 0xff)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, (cpu.PC + 1) & 0xffff, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- 0xED ED opcodes
opcodes[0xED] = function (cpu)
	--logger.warning("shifting ED")
	contend(cpu, cpu.PC, 4)
	-- Increment R register, without modifying the most significant bit
	cpu.R = (cpu.R & 0x80) | ( (cpu.R + 1) & 0x7f)
	local opcode = read_mem_byte_internal(cpu, cpu.PC)
	local f = opcodes_ed[opcode]
	if not f then
		--logger.fatal("Opcode ED 0x%0x (%d) not found", opcode, opcode)
		os.exit(1)
	end
	f(cpu)
end

-- XOR nn
opcodes[0xee] = function (cpu)
	--logger.debug("XOR nn")
	local byte = read_mem_byte(cpu, cpu.PC)
	XOR(cpu, byte)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RST 28
opcodes[0xef] = function (cpu)
	--logger.debug("RST 28")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, (cpu.PC & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, cpu.PC & 0xff)
	cpu.PC = 0x0028  -- jumps to 28h
end

-- RET P
opcodes[0xf0] = function (cpu)
	--logger.debug("RET P")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	if cpu.F & FLAG_S == 0 then
		cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
		cpu.SP = (cpu.SP + 2) & 0xffff
	end
end

-- POP AF
opcodes[0xf1] = function (cpu)
	--logger.debug("POP AF")
	cpu.F = read_mem_byte(cpu, cpu.SP)
	cpu.A = read_mem_byte(cpu, (cpu.SP + 1) & 0xffff)
	cpu.SP = (cpu.SP + 2) & 0xffff
end

-- JP P,nnnn
opcodes[0xf2] = function (cpu)
	if cpu.F & FLAG_S == 0 then
		local address = read_mem_word(cpu, cpu.PC)
		--logger.debug("JP P,$%04x", address)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, (cpu.PC + 1) & 0xffff, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- 0xF3 DI
opcodes[0xF3] = function (cpu)
	--logger.debug("DI")
	-- deactivate both interruption flip-flops
	cpu.iff1 = 0
	cpu.iff2 = 0
end

-- CALL P
opcodes[0xf4] = function (cpu)
	--logger.debug("CALL P")
	if cpu.F & FLAG_S == 0 then
		local address = read_mem_word(cpu, cpu.PC)
		contend(cpu, (cpu.PC + 1) & 0xffff, 1)
		cpu.SP = (cpu.SP - 2) & 0xffff
		local ret_address = cpu.PC + 2
		write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, (ret_address & 0xff00) >> 8)
		write_mem_byte(cpu, cpu.SP, ret_address & 0xff)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, (cpu.PC + 1) & 0xffff, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- PUSH AF
opcodes[0xf5] = function (cpu)
	--logger.debug("PUSH AF")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, cpu.A)
	write_mem_byte(cpu, cpu.SP, cpu.F)
end

-- OR nn
opcodes[0xf6] = function (cpu)
	--logger.debug("OR nn")
	local byte = read_mem_byte(cpu, cpu.PC)
	OR(cpu, byte)
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RST 30
opcodes[0xf7] = function (cpu)
	--logger.debug("RST 30")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, (cpu.PC & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, cpu.PC & 0xff)
	cpu.PC = 0x0030  -- jumps to 30h
end

-- RET M
opcodes[0xf8] = function (cpu)
	--logger.debug("RET M")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	if cpu.F & FLAG_S ~= 0 then
		cpu.PC = read_mem_byte(cpu, cpu.SP) | read_mem_byte(cpu, (cpu.SP + 1) & 0xffff) << 8
		cpu.SP = (cpu.SP + 2) & 0xffff
	end
end

-- LD SP,HL
opcodes[0xf9] = function (cpu)
	--logger.debug("LD SP,HL")
	cpu.SP = cpu.L | cpu.H << 8
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
end

-- JP M,nnnn
opcodes[0xfa] = function (cpu)
	if cpu.F & FLAG_S ~= 0 then
		local address = read_mem_word(cpu, cpu.PC)
		--logger.debug("JP M,$%04x", address)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, (cpu.PC + 1) & 0xffff, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- EI
opcodes[0xfb] = function (cpu)
	--logger.debug("EI")
	cpu.iff1 = 1
	cpu.iff2 = 1
end

-- CALL M
opcodes[0xfc] = function (cpu)
	--logger.debug("CALL M")
	if cpu.F & FLAG_S ~= 0 then
		local address = read_mem_word(cpu, cpu.PC)
		contend(cpu, (cpu.PC + 1) & 0xffff, 1)
		cpu.SP = (cpu.SP - 2) & 0xffff
		local ret_address = cpu.PC + 2
		write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, (ret_address & 0xff00) >> 8)
		write_mem_byte(cpu, cpu.SP, ret_address & 0xff)
		cpu.PC = address
	else
		contend(cpu, cpu.PC, 3)
		contend(cpu, (cpu.PC + 1) & 0xffff, 3)
		cpu.PC = (cpu.PC + 2) & 0xffff
	end
end

-- 0xFD FD opcodes
opcodes[0xfd] = function (cpu)
	--logger.warning("shifting FD")
	contend(cpu, cpu.PC, 4)
	-- Increment R register, without modifying the most significant bit
	cpu.R = (cpu.R & 0x80) | ( (cpu.R + 1) & 0x7f)
	local opcode = read_mem_byte_internal(cpu, cpu.PC)
	local f = opcodes_fd[opcode]
	if not f then
		cpu.PC = (cpu.PC + 1) & 0xffff
		return
		--logger.fatal("Opcode FD 0x%0x (%d) not found", opcode, opcode)
		--os.exit(1)
	end
	f(cpu)
end

-- CP nn
opcodes[0xfe] = function (cpu)
	--logger.debug("CP nn")
	CP(cpu, read_mem_byte(cpu, cpu.PC))
	cpu.PC = (cpu.PC + 1) & 0xffff
end

-- RST 38
opcodes[0xff] = function (cpu)
	--logger.debug("RST 38")
	-- contend with IR register pair
	local I_high = cpu.I << 8
	contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	cpu.SP = (cpu.SP - 2) & 0xffff
	write_mem_byte(cpu, (cpu.SP + 1) & 0xffff, (cpu.PC & 0xff00) >> 8)
	write_mem_byte(cpu, cpu.SP, cpu.PC & 0xff)
	cpu.PC = 0x0038  -- jumps to 38h
end






local z80 = {
	A = 0,
	F = 0,
	H = 0,
	L = 0,
	B = 0,
	C = 0,
	D = 0,
	E = 0,
	IX = 0,
	IY = 0,

	Ap = 0,
	Fp = 0,
	Hp = 0,
	Lp = 0,
	Bp = 0,
	Cp = 0,
	Dp = 0,
	Ep = 0,

	PC = 0,
	iff1 = 0,
	iff2 = 0,
	I = 0,
	int_mode = 0,
	SP = 0,
	R = 0,
	tstates = 0,
	halted = false
}

local function reset (cpu)
	cpu.A = 0
	cpu.F = 0
	cpu.H = 0
	cpu.L = 0
	cpu.B = 0
	cpu.C = 0
	cpu.D = 0
	cpu.E = 0
	cpu.IX = 0
	cpu.IY = 0
	cpu.PC = 0
	cpu.iff1 = 0
	cpu.iff2 = 0
	cpu.I = 0
	cpu.int_mode = 0
	cpu.SP = 0
	cpu.R = 0
	cpu.tstates = 0
	cpu.halted = false

	cpu.Ap = 0
	cpu.Fp = 0
	cpu.Hp = 0
	cpu.Lp = 0
	cpu.Bp = 0
	cpu.Cp = 0
	cpu.Dp = 0
	cpu.Ep = 0
end

z80.reset = reset



---
-- Deals with maskable interrupts
--
local function  handle_interrupt (cpu, tstates_per_frame)
	cpu.tstates = cpu.tstates - tstates_per_frame
	--print(cpu.tstates, tstates_per_frame)

	-- if, HALTed, increment PC and disable HALT
	if cpu.halted then
		cpu.PC = cpu.PC + 1
		cpu.halted = false
		--logger.debug("Unhalt. intmode = %s", cpu.int_mode)
	end
	if cpu.iff1 == 0 then
		-- interruptions disabled
		return 1
	end
	-- En un INTACK, R se incrementa
	-- Increment R register, without modifying the most significant bit
	cpu.R = (cpu.R & 0x80) | ( (cpu.R + 1) & 0x7f)
	-- Tiene que ir acá, o sea, cuando se acepta la interrupción (igual para una NMI)
	--iff1 = 0;iff2 = 0;
	iff1 = 0; iff2 = 0;	-- en algún momento determiné que iff2 no se apagaba, pero todos los emuladores apagan ambos
	if cpu.int_mode == 0 then
		-- Takes 12 tstates
		local c = cpu.tstates
		cpu.SP = (cpu.SP - 2) & 0xffff
		write_mem_word(cpu, cpu.SP, cpu.PC)
		cpu.tstates = cpu.tstates + 7
		local c2 = cpu.tstates
		assert(cpu.tstates - c == 13, c2)	-- shouldn't that be 12 ?
		-- jump to 0038h
		cpu.PC = 0x0038
	
	elseif cpu.int_mode == 1 then
		local c = cpu.tstates
		-- Takes 13 tstates
		cpu.SP = (cpu.SP - 2) & 0xffff
		write_mem_word(cpu, cpu.SP, cpu.PC)
		cpu.tstates = cpu.tstates + 7
		assert(cpu.tstates - c == 13)
		-- jump to 0038h
		cpu.PC = 0x0038

	elseif cpu.int_mode == 2 then
		local c = cpu.tstates
		-- Takes 19 tstates
		--logger.info("Acknowledging interruption mode 2")
		--dump_registers(cpu)
		cpu.SP = (cpu.SP - 2) & 0xffff
		write_mem_word(cpu, cpu.SP, cpu.PC)
		local address = (0xff | cpu.I << 8) & 0xffff
		--logger.info("Reading address of service routine from 0x%04x", address)
		cpu.PC = read_mem_word(cpu, address)
		--logger.info("Jumping to service routine at 0x%04x", cpu.PC)
		cpu.tstates = cpu.tstates + 7
		assert(cpu.tstates - c == 19)
	end
	return 1
end

z80.handle_interrupt = handle_interrupt


z80.opcodes = opcodes

return z80
