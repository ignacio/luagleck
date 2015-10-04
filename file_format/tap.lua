local logger = require "logger"
local tap = {}

local FLAG_C      = 1
local FLAG_C_OFF  =	254

local blocks -- move this into a tap table
local current_block


local function compute_checksum (data, length)
	local checksum = 0
	for i = 1, length do
		checksum = checksum ~ data:byte(i)
	end
	return checksum
end


---
-- Loads and stoes in the memory a block of data. Also computes and returns the checksum.
-- The checksum is computed xor'ing each byte of data.
--
local function load_block_to_memory (machine, data, start_address, length)
	--logger.debug("[tap.load_block_to_memory] start_address: 0x%04x, length: %04x, data length: %04x",
	--	start_address, length, #data)
	
	local checksum = 0
	for i = 0, length - 1 do
		local byte = data:byte(i + 1)
		machine.memory.write_mem_byte_internal(machine.cpu, (start_address + i) & 0xffff, byte)
		checksum = checksum ~ byte
	end

	return checksum
end


---
-- block_id: byte
-- start_address: word
-- length: word
--
local function load_block (machine, block_id, start_address, length)

	if not blocks then return false end
	local current = blocks[current_block]
	if not current then
		current_block = 1
		return false
	end

	while current.flag ~= block_id do
		current_block = current_block + 1
		current = blocks[current_block]
		if not current then
			-- wrapped around tape while looking for block
			current_block = 1
			return false
		end
	end

	local cpu = machine.cpu

	if length <= current.dwBlockLength then
		-- found a block with the appropiate length
		--logger.debug("right block")
		local checksum = load_block_to_memory(machine, current.data:sub(2), start_address, length)
		checksum = checksum ~ current.flag

		--logger.debug("checksum = %s, block checksum = %s", checksum, current.bChecksum)
		cpu.H = (checksum == current.bChecksum) and 0 or 255
		cpu.IX = (cpu.IX + length) & 0xffff
		cpu.D, cpu.E = 0, 0
		-- go to next block
		current_block = current_block + 1
		return true
	else
		-- the block is shorter than the one requested.
		-- read it anyway, but signal an error
		logger.debug("wrong block")

		local checksum = load_block_to_memory(machine, current.data, start_address, current.dwBlockLength)
		checksum = checksum ~ current.flag

		cpu.H = (checksum == current.bChecksum) and 0 or 255
		cpu.IX = (cpu.IX + length) & 0xffff
		cpu.D, cpu.E = 0, 0
		return false
	end
end

---
-- called when the machine needs to hook into the tape loading process
--
function tap.load_hook (machine)

	local cpu = machine.cpu
	local DE = cpu.E | cpu.D << 8
	logger.debug("[tap.load_hook] Requested flag: %02x, destination: %04x, length: %04x",
		cpu.Ap, cpu.IX, DE)

	if not load_block(machine, cpu.Ap, cpu.IX, DE) then
		cpu.Fp = cpu.Fp & FLAG_C_OFF
	else
		cpu.F = cpu.F | FLAG_C
	end

	cpu.PC = 0x056b
end


---
-- Scans a TAP file, loading its blocks into memory.
--
local function scan_tap (data)
	local block_number = 0
	local blocks = {}

	local tape_length = #data

	local i = 1
	while i <= tape_length do
		local block = {}
		block.block_number = block_number
		block.dwPosicion = i
		local len, flag, type = string.unpack("<I2 B B", data, i)
		block.dwBlockLength = len - 2
		block.flag = flag
		block.type = type

		if block.flag == 0 then
			-- it's a header
			local filename, wDataLen, wParam1, wParam2, bChecksum =
				string.unpack("<c10 I2 I2 I2 B", data, i + 4)
			block.filename = filename
			block.wDataLen = wDataLen
			block.wParam1 = wParam1
			block.wParam2 = wParam2
			block.bChecksum = bChecksum
			block.data = data:sub(i + 2, i + 2 + block.dwBlockLength)

			assert(compute_checksum(block.data, #block.data) == bChecksum)
		else
			-- it's data
			block.data = data:sub(i + 2, i + 2 + block.dwBlockLength)
			block.bChecksum = string.byte(data, i + 2 + block.dwBlockLength + 1)
			assert(compute_checksum(block.data, #block.data) == block.bChecksum)
		end
		--logger.info("block_number = %s, length: %s, flag: %s, type: %s",
		--	block.block_number, block.dwBlockLength, block.flag, block.type)
		--logger.info("filename = %s, wDataLen: %s, wParam1: %s, wParam2: %s, checksum: %s",
		--	block.filename, block.wDataLen, block.wParam1, block.wParam2, block.bChecksum)

		i = i + block.dwBlockLength + 4
		table.insert(blocks, block)
		block_number = block_number + 1
	end

	return blocks
end


---
--
function tap.load (machine, data)
	
	blocks = scan_tap(data)

	current_block = 1
	return true
end

return tap
