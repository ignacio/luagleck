--
-- Common logical operations
--

---
-- Will be bound to the actual functions to be called later in bind_io
--
local contend_read_no_mreq

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


local sz53_table = { [0] = 0 }
local parity_table = { [0] = 0 }
local sz53p_table = { [0] = 0 }

--
-- Whether a half carry occurred or not can be determined by looking at
-- the 3rd bit of the two arguments and the result; these are hashed
-- into this table in the form r12, where r is the 3rd bit of the
-- result, 1 is the 3rd bit of the 1st argument and 2 is the
-- third bit of the 2nd argument; the tables differ for add and subtract
-- operations
local halfcarry_add_table = { [0] = 0, FLAG_H, FLAG_H, FLAG_H, 0, 0, 0, FLAG_H }
local halfcarry_sub_table = { [0] = 0, 0, FLAG_H, 0, FLAG_H, 0, FLAG_H, FLAG_H }

-- Similarly, overflow can be determined by looking at the 7th bits; again
-- the hash into this table is r12
local overflow_add_table = { [0] = 0, 0, 0, FLAG_PV, FLAG_PV, 0, 0, 0 }
local overflow_sub_table = { [0] = 0, FLAG_PV, 0, 0, 0, 0, FLAG_PV, 0 }

local function init_tables ()
	
	for i = 0, 0x100 do
		sz53_table[i] = i & ( FLAG_B3 | FLAG_B5 | FLAG_S )
		local j = i
		local parity = 0
		for k = 0, 7 do
			parity = parity ~ (j & 1)
			j = j >> 1
		end
		parity_table[i] = ( parity ~= 0 and 0 or FLAG_PV )
		sz53p_table[i] = sz53_table[i] | parity_table[i]
	end

	sz53_table[0]  = sz53_table[0] | FLAG_Z
	sz53p_table[0] = sz53p_table[0] | FLAG_Z
end

init_tables()


local function BIT (cpu, bit, operand)
	-- TODO: Revisar. Difiere respecto a Spectaculator y Fuse
	--[[
	local testBit = 1 << bit		-- 2 a la bit
	cpu.F = cpu.F & FLAG_C			-- enmascaro el flag C para conservarlo
	if operand & testBit ~= 0 then	-- ¿el bit es 1?
		if bit == 7 then			-- ¿Estoy testeando el bit 7?
			cpu.F = cpu.F | FLAG_S	-- Si, entonces prendo el flag S
		end
		if bit == 5 then			-- ¿Estoy testeando el bit 5?
			cpu.F = cpu.F | FLAG_B5	-- Si, entonces prendo el flag 5
		end
		if bit == 3 then			-- ¿Estoy testeando el bit 3?
			cpu.F = cpu.F | FLAG_B3	-- Si, entonces prendo el flag 3
		end
		cpu.F = cpu.F | FLAG_H		-- Prendo el flag H
	else
		-- entonces, es cero -> prendo los flags Z, H y PV
		cpu.F = cpu.F | FLAG_Z | FLAG_H | FLAG_PV
	end
	--]]
	cpu.F = ( cpu.F & FLAG_C ) | FLAG_H | ( operand & ( FLAG_B3 | FLAG_B5 ) )
	if ( operand & ( 0x01 << bit ) ) == 0 then
		cpu.F = cpu.F | FLAG_PV | FLAG_Z
	end
	if bit == 7 and operand & 0x80 ~= 0 then
		cpu.F = cpu.F | FLAG_S
	end
end

local function BIT_INDEX (cpu, bit, operand, address)
	cpu.F = ( cpu.F & FLAG_C ) | FLAG_H | ( (address >> 8) & ( FLAG_B3 | FLAG_B5 ) )
	if ( operand & ( 0x01 << bit ) ) == 0 then
		cpu.F = cpu.F | FLAG_PV | FLAG_Z
	end
	if bit == 7 and operand & 0x80 ~= 0 then
		cpu.F = cpu.F | FLAG_S
	end
end


local function RES (cpu, bit, byte)
	bit = 1 << bit -- 2^bit
	bit = bit ~ 0xff -- invert
	return byte & bit -- turn it off
end


local function SET (cpu, bit, byte)
	bit = 1 << bit -- 2^bit
	return byte | bit
end


local function ADD_16 (cpu, value1, value2)--, reg_low, reg_high)
	local add16temp = value1 + value2
	local lookup = 	( (    value1 & 0x0800 ) >> 11 ) |
					( (    value2 & 0x0800 ) >> 10 ) |
					( ( add16temp & 0x0800 ) >>  9 )
	local I_high = cpu.I << 8
	for i = 1, 7 do
		contend_read_no_mreq(cpu, I_high | cpu.R, 1)
	end
	--cpu[reg_high], cpu[reg_low] = high_low(add16temp)
	cpu.F = ( cpu.F & ( FLAG_PV | FLAG_Z | FLAG_S ) ) |
			( add16temp & 0x10000 ~= 0 and FLAG_C or 0 ) |
			( ( add16temp >> 8 ) & ( FLAG_B3 | FLAG_B5 ) ) |
			halfcarry_add_table[lookup]
	return add16temp & 0xffff
end


local function ADD_8 (cpu, value)
	local add = cpu.A + value
	local lookup = 	( ( cpu.A & 0x88 ) >> 3 ) |
					( ( value & 0x88 ) >> 2 ) |
					( (   add & 0x88 ) >> 1 )
	cpu.A = add & 0xff
	cpu.F = ( add & 0x100 ~= 0 and FLAG_C or 0 ) |
			halfcarry_add_table[lookup & 0x07] | overflow_add_table[lookup >> 4] |
			sz53_table[cpu.A]
end

local function ADC_8 (cpu, value)
	local temp = cpu.A + value + ( cpu.F & FLAG_C )
	local lookup =  ( ( cpu.A & 0x88 ) >> 3 ) |
					( ( value & 0x88 ) >> 2 ) |
					( (  temp & 0x88 ) >> 1 )
	cpu.A = temp & 0xff
	cpu.F = ( temp & 0x100 ~= 0 and FLAG_C or 0 ) |
			halfcarry_add_table[lookup & 0x07] | overflow_add_table[lookup >> 4] |
			sz53_table[cpu.A]
end


local function SUB (cpu, value)
	local sub = cpu.A - value
	local lookup =  ( ( cpu.A & 0x88 ) >> 3 ) |
					( ( value & 0x88 ) >> 2 ) |
					( (   sub & 0x88 ) >> 1 )
	cpu.A = sub & 0xff
	cpu.F = ( (sub & 0x100 ~= 0) and FLAG_C or 0 ) |
			FLAG_N |
			halfcarry_sub_table[lookup & 0x07] |
			overflow_sub_table[lookup >> 4] |
			sz53_table[cpu.A]
end


local function SBC (cpu, value)
	local temp = cpu.A - value - ( cpu.F & FLAG_C )
	local lookup =  ( ( cpu.A & 0x88 ) >> 3 ) |
					( ( value & 0x88 ) >> 2 ) |
					( (  temp & 0x88 ) >> 1 )
	cpu.A = temp & 0xff
	cpu.F = ( temp & 0x100 ~= 0 and FLAG_C or 0 ) |
			FLAG_N |
			halfcarry_sub_table[lookup & 0x07] |
			overflow_sub_table[lookup >> 4] |
			sz53_table[cpu.A]
end


local function AND (cpu, value)
	cpu.A = cpu.A & value
	cpu.F = FLAG_H | sz53p_table[cpu.A]
end

local function OR (cpu, value)
	cpu.A = cpu.A | value
	cpu.F = sz53p_table[cpu.A]
end

local function XOR (cpu, value)
	cpu.A = cpu.A ~ value
	cpu.F = sz53p_table[cpu.A]
end


local function INC (cpu, byte)
	assert(byte)

	byte = (byte + 1) & 0xff
	cpu.F = ( cpu.F & FLAG_C ) | ( byte == 0x80 and FLAG_PV or 0 ) |
			( (byte & 0x0f ~= 0) and 0 or FLAG_H ) | sz53_table[byte]
	return byte
end

local function DEC (cpu, byte)
	assert(byte)
	cpu.F = (cpu.F & FLAG_C) | ( ((byte & 0x0f) ~= 0) and 0 or FLAG_H ) | FLAG_N
	byte = (byte - 1) & 0xff
	cpu.F = cpu.F | ( byte == 0x7f and FLAG_PV or 0 ) | sz53_table[byte]
	return byte
end

-- Comparison instructions
local function CP (cpu, value)
	local cptemp = cpu.A - value
	local lookup =  ( (   cpu.A & 0x88 ) >> 3 ) |
					( ( (value) & 0x88 ) >> 2 ) |
					( (  cptemp & 0x88 ) >> 1 )

	if cptemp & 0x1000 ~= 0 then
		cpu.F = FLAG_C
	else
		if cptemp ~= 0 then
			cpu.F = 0
		else
			cpu.F = FLAG_Z
		end
	end

	cpu.F = cpu.F | FLAG_N |
				halfcarry_sub_table[lookup & 0x07] |
				overflow_sub_table[lookup >> 4] |
				( value & ( FLAG_B3 | FLAG_B5 ) ) |
				( cptemp & FLAG_S )
end

-- Rotation instructions
local function RL (cpu, value)
	local rltemp = value
	value = ( value << 1 ) | ( cpu.F & FLAG_C )
	value = value & 0xff
	cpu.F = ( rltemp >> 7 ) | sz53p_table[value]
	return value
end

local function RLC (cpu, value)
	value = ( (value & 0x7f) << 1 ) | ( value >> 7 )
	cpu.F = ( value & FLAG_C ) | sz53p_table[value]
	return value
end

local function RR (cpu, value)
	local temp = value
	value = ( value >> 1 ) | ( (cpu.F & 0x01) << 7 )
	cpu.F = ( temp & FLAG_C ) | sz53p_table[value]
	return value
end

local function RRC (cpu, value)
	cpu.F = value & FLAG_C
	value = ( value >> 1 ) | ( (value & 0x01) << 7 )
	cpu.F = cpu.F | sz53p_table[value]
	return value
end

local function SLA (cpu, value)
	cpu.F = value >> 7
	value = value << 1
	value = value & 0xff
	cpu.F = cpu.F | sz53p_table[value]
	return value
end

local function SLL (cpu, value)
	cpu.F = value >> 7
	value = ( value << 1 ) | 0x01
	value = value & 0xff
	cpu.F = cpu.F | sz53p_table[value]
	return value
end

local function SRA (cpu, value)
	cpu.F = value & FLAG_C
	value = ( value & 0x80 ) | ( value >> 1 )
	cpu.F = cpu.F | sz53p_table[value]
	return value
end

local function SRL (cpu, value)
	cpu.F = value & FLAG_C
	value = value >> 1
	cpu.F = cpu.F | sz53p_table[value]
	return value
end


local function bind_io (binds)
	contend_read_no_mreq = assert(binds.memory.contend_read_no_mreq)
end


return {
	bind_io = bind_io,
	BIT = BIT,
	BIT_INDEX = BIT_INDEX,
	RES = RES,
	SET = SET,
	ADD_16 = ADD_16,
	ADD_8 = ADD_8,
	ADC_8 = ADC_8,
	AND = AND,
	OR = OR,
	XOR = XOR,

	INC = INC,
	DEC = DEC,

	SUB = SUB,
	SBC = SBC,
	CP = CP,

	RL = RL,
	RLC = RLC,
	RR = RR,
	RRC = RRC,
	SLA = SLA,
	SLL = SLL,
	SRA = SRA,
	SRL = SRL,

	tables = {
		overflow_add_table = overflow_add_table,
		overflow_sub_table = overflow_sub_table,
		halfcarry_add_table = halfcarry_add_table,
		halfcarry_sub_table = halfcarry_sub_table,
		sz53p_table = sz53p_table,
		sz53_table = sz53_table,
		parity_table = parity_table
	}
}