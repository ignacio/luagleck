local logger = require "logger"
local port = require "port"
--local display = require "display"

local z80 = {}

local function high_low (value)
	return (value & 0xff00) >> 8, value & 0x00ff
end

---
-- TODO: Documentar que diablos hacia esto
local function GetPaginaBloque (blocknum)
	if blocknum == 0 then
		-- ROM
		return 0--3
	elseif blocknum == 4 then
		-- 0x8000 - 0xbfff
		return 2--1
	elseif blocknum == 5 then
		-- 0xc000 - 0xffff
		return 3--2
	elseif blocknum == 8 then
		-- 0x4000 - 0x7fff
		return 1--0
	end

	return 0
end

---
-- Decompresses and loads into RAM a possibly RLE compressed block.
--
local function uncompress_z80_block (machine, data, address, length)
	local offset = 0
	address = address or 0x4000 -- start at screem address by default
	length = length or #data

	while offset < length do
		--local ok, b1, b2, b3, b4 = pcall(string.unpack, "< B B B B", data, offset + 1)
		local b1, b2, b3, b4 = string.byte(data, offset + 1, offset + 5)
		-- if not ok then
		-- 	logger.error("#data= %s, offset + 1=%s,length=%s", #data, offset + 1, length)
		-- 	return
		-- end
		
		-- Check for '00eded00', the 'end of file' marker
		if b1 == 0 and b2 == 0xed and b3 == 0xed and b4 == 0 then
			return
		end

		if b1 == 0xed and b2 == 0xed then
			-- Found a compressed (RLE) block. It consists of four bytes:
			-- byte 0-1: ED, ED
			-- byte 2: block length (b3)
			-- byte 3: value (b4)
			-- So we write 'b3' times the value 'b4' to the memory
			local block_len = b3
			for off = 0, block_len - 1 do
				machine.memory.write_mem_byte_internal(machine.machine, (address + off) & 0xffff, b4)
				--memory.write_mem_byte_rom(cpu, (address + off) & 0xffff, b4)
			end
			--display.draw_screen(cpu)
			address = address + block_len
			offset = offset + 4
		
		else
			-- Found a regular value.
			machine.memory.write_mem_byte_internal(machine.cpu, address & 0xffff, b1)
			--memory.write_mem_byte_rom(cpu, address & 0xffff, b1)
			address = address + 1
			offset = offset + 1
		end
	end
end


---
--
local function load_z80_v2 (machine, data)
	local offset = 0
	local address

	while offset < #data do
		--display.draw_screen(cpu)
		local block_len, block_code = string.unpack("< I2 B", data, offset + 1)
		print("1:", offset, #data, block_code)
		offset = offset + 3
		--print("block_code", block_code)
		address = GetPaginaBloque(block_code) * 16384	-- TODO: that function belongs to a model
		if block_len == 0xffff then
			-- the block is 0x4000 bytes long and is not compressed
			error("ffff")
		else
			print("address", address, "block_len", block_len)
			uncompress_z80_block(machine, data:sub(offset + 1), address, block_len)
			offset = offset + block_len
		end
	end
end


---
--
local function load_z80_v3 (machine, data)
	error("load_z80_v3 not actually implemented")
	local offset = 0
	local address

	while offset < #data do
		--display.draw_screen(cpu)
		local block_len, block_code = string.unpack("< I2 B", data, offset + 1)
		print("1:", offset, #data, block_code)
		offset = offset + 3
		--print("block_code", block_code)
		address = GetPaginaBloque(block_code) * 16384	-- TODO: that function belongs to a model
		if block_len == 0xffff then
			-- the block is 0x4000 bytes long and is not compressed
			error("ffff")
		else
			print("address", address, "block_len", block_len)
			uncompress_z80_block(machine, data:sub(offset + 1), address, block_len)
			offset = offset + block_len
		end
	end
end


---
--
local function load_z80_v2_v3 (machine, data)
	-- header_len: //byte 30 y 31: largo de la cabecera (23 para v2, 54 para v3)
	-- PC: byte 32 y 33: PC
	-- model_type: /byte 34:		modo de hardware
	-- out_7ffd: byte 35: ultimo OUT a puerto 7FFD
	-- byte 36,37: we don't handle them
	-- out_fffd: byte 38:		ultimo OUT a puerto 7FFD
	-- ay_registers: bytes 39-54 state of each AY register

	local v2_v3_header = "<I2          I2  B           B         I2 B         c16 "
	local                  header_len, PC, model_type, out_7ffd, _, out_fffd, ay_registers
		= string.unpack(v2_v3_header, data)

	if header_len == 23 then
		-- z80_v2
		if model_type == 0 then
			-- 48K mode
			logger.info("TODO: Load 48K rom")
		elseif model_type == 1 then
			-- 48K mode + Interface 1
			logger.warning("Unsupported model combination: 48K + Interface 1")
			return false
		elseif model_type == 2 then
			-- 48K + SamRam
			logger.warning("Unsupported model combination: 48K + SamRam")
			return false
		elseif model_type == 3 then
			-- 128K mode
			logger.warning("Unsupported model: 128K")
			return false
		elseif model_type == 4 then
			-- 128K mode + Interface 1
			logger.warning("Unsupported model combination: 128K + Interface 1")
			return false
		else
			logger.error("Unknown model type %s", model_type)
			return false
		end
		load_z80_v2(machine, data:sub(string.packsize(v2_v3_header) + 1))

	elseif header_len == 54 then
		-- z80_v3
		if model_type == 0 then
			-- 48K mode
			logger.info("TODO: Load 48K rom")
		elseif model_type == 1 then
			-- 48K mode + Interface 1
			logger.warning("Unsupported model combination: 48K + Interface 1")
			return false
		elseif model_type == 2 then
			-- 48K + SamRam
			logger.warning("Unsupported model combination: 48K + SamRam")
			return false
		elseif model_type == 3 then
			-- 48K mode + MGT
			logger.warning("Unsupported model combination: 48K + MGT")
			return false
		elseif model_type == 4 then
			-- 128K mode
			logger.warning("Unsupported model: 128K")
			return false
		elseif model_type == 5 then
			-- 128K mode + Interface 1
			logger.warning("Unsupported model combination: 128K + Interface 1")
			return false
		elseif model_type == 6 then
			-- 48K mode + MGT
			logger.warning("Unsupported model combination: 128K + MGT")
			return false
		else
			logger.error("Unknown model type %s", model_type)
			return false
		end
		load_z80_v3(machine, data:sub(string.packsize(v2_v3_header) + 1))
	end

	machine.ports.write_port(machine.cpu, 0x7ffd, out_7ffd)

	-- Write AY registers
	for i=1, 16 do
		-- two writes: one to select which register to write, the second to write the value
		machine.ports.write_port(machine.cpu, 0xfffd, i)
		machine.ports.write_port(machine.cpu, 0xbffd, ay_registers:byte(i))
	end

	machine.ports.write_port(machine.cpu, 0xfffd, out_fffd)

	machine.cpu.tstates = 0
	machine.cpu.PC = PC

	return true
end

---
--
function z80.load (machine, data)
	
	local cpu = machine.cpu
	local memory = machine.memory

	cpu:reset()
	memory.reset_bank(1)
	memory.reset_bank(2)
	memory.reset_bank(3)
	
	-- Read the header (30 bytes)
	local header = "<B  B  I2  I2  I2  I2  B  B  B       I2  I2   I2   I2   I2   I2  I2  B     B     B"
	local            A, F, BC, HL, PC, SP, I, R, byte12, DE, BCp, DEp, HLp, AFp, IY, IX, iff1, iff2, int_mode =
		string.unpack(header, data)

	assert(string.packsize(header) == 30)

	-- for compatibility with other emulators, treat it as 1
	if byte12 == 0xff then byte12 = 1 end

	-- bit 0: bit 7 of R register
	if byte12 & 1 ~= 0 then cpu.R = cpu.R | 128 end

	-- if bit 5 is high and PC != 0 -> the snapshot is compressed
	local compressed = (byte12 & 0x20 ~= 0)

	port.port_fe = (byte12 & 0x0e) >> 1  -- border colour
	-- bit 4 - 1 if SamROM switched in (we don't care about this!)
	
	cpu.A = A
	cpu.F = F
	cpu.H, cpu.L = high_low(HL)
	
	cpu.B, cpu.C = high_low(BC)
	cpu.D, cpu.E = high_low(DE)
	cpu.IX = IX
	cpu.IY = IY

	cpu.iff1 = iff1 ~= 0
	cpu.iff2 = iff2 ~= 0
	
	cpu.I = I
	cpu.int_mode = int_mode & 0x03  -- 0, 1, or 2
	cpu.SP = SP
	cpu.PC = PC
	cpu.R = R
	cpu.tstates = 0
	cpu.halted = false
	

	cpu.Ap, cpu.Fp = high_low(AFp)
	cpu.Hp, cpu.Lp = high_low(HLp)
	cpu.Bp, cpu.Cp = high_low(BCp)
	cpu.Dp, cpu.Ep = high_low(DEp)

	data = data:sub(31)

	if cpu.PC == 0 then
		load_z80_v2_v3(machine, data)
	else
		-- should set ROM to spectrum 48k
		if compressed then
			uncompress_z80_block(machine, data)
		else
			local bank1, bank2, bank3 = string.unpack("< c16384 c16384 c16384", data)
			machine.memory.set_bank(1, bank1)
			machine.memory.set_bank(2, bank2)
			machine.memory.set_bank(3, bank3)
		end
	end
	return true
end

return z80
