local logger = require "logger"
local z80 = require "z80"

local display = require "display"
local SDL = require "SDL"

local port_handlers = require "port"

-- helpers
local function high_low (value)
	return (value & 0xff00) >> 8, value & 0x00ff
end



local lines_per_frame = 312
local lines_upper_border = 64
local cycles_left_border = 24
local cycles_right_border = 24
local cycles_per_line = 128
local cycles_horizontal_retrace = 48
local cycles_per_whole_line = cycles_per_line + cycles_right_border + cycles_horizontal_retrace + cycles_left_border
local tstates_per_frame = lines_per_frame * cycles_per_whole_line


local keyboard_issue = 3
local testVal = 0x18  -- a byte, to determine whether is issue 2 or 3
local issue = 0xff


----
-- Memory I/O
local memory = {}

local ram

function memory.set_ram (_ram)
	ram = _ram
end

---
-- Copies 16K of data in the given bank
function memory.set_bank (bank, data)
	local start = 16384 * bank
	for i = 0, 16383 do
		ram[start + i] = assert(string.byte(data:sub(i+1,i+1)))
	end
end

function memory.reset_bank (bank)
	local start = 16384 * bank
	for i = 0, 16383 do
		ram[start + i] = 0
	end
end

function memory.contend (cpu, address, tstates) -- (es una lectura)
	if address > 0xffff or address < 0 then error("contend: unclamped address: " .. address) end
	cpu.tstates = cpu.tstates + tstates
end

function memory.contend_read_no_mreq (cpu, address, tstates)
	if address > 0xffff or address < 0 then error("contend_read_no_mreq: unclamped address") end
	cpu.tstates = cpu.tstates + tstates
end

function memory.contend_write_no_mreq (cpu, address, tstates)
	if address > 0xffff or address < 0 then error("contend_write_no_mreq: unclamped address") end
	cpu.tstates = cpu.tstates + tstates
end

-- esta funcion luego va a depender del modelo
function memory.read_mem_byte_internal (cpu, address)
	if address > 0xffff or address < 0 then error("read_mem_byte_internal: unclamped address") end
	return assert(ram[address], "ram at address "..address.. " is nil")
end

-- esta funcion luego va a depender del modelo
function memory.read_mem_byte (cpu, address)
	memory.contend(cpu, address, 3)
	return memory.read_mem_byte_internal(cpu, address)
end

-- cuidado con direcciones cerca del fin de la memoria
-- puedo leer un byte de $ffff y el siguiente de $0000 (contemplar ese caso, y en
-- la escritura tambien)
function memory.read_mem_word_internal (cpu, address)
	--logger.debug("read_mem_word_internal: address = 0x%04x", address)
	if address > 0xffff or address < 0 then error("read_mem_word_internal: unclamped address") end
	if address + 1 > 0xffff or address + 1 < 0 then error("read_mem_word_internal: unclamped address") end
	--logger.debug( ram[address + 1], ram[address])
	--logger.debug( (ram[address + 1] << 8) + ram[address] )
	return (ram[address + 1] << 8) + ram[address]
end


function memory.read_mem_word (cpu, address)
	local high, low
	memory.contend(cpu, address, 3)
	low = memory.read_mem_byte_internal(cpu, address)
	memory.contend(cpu, address + 1, 3)
	high = memory.read_mem_byte_internal(cpu, address + 1)
	return low | (high << 8)
	--return memory.read_mem_word_internal(address)
end

function memory.write_mem_byte_internal (cpu, address, value)
	assert(value)
	if address < 0x4000 then return end -- do not write on ROM
	if address > 0xffff then error("write_mem_byte_internal: unclamped address") end

	ram[address] = value
end

function memory.write_mem_byte (cpu, address, value)
	memory.contend(cpu, address, 3)
	return memory.write_mem_byte_internal(cpu, address, value)
end

-- this allows writes on rom. used in tests
function memory.write_mem_byte_rom (cpu, address, value)
	assert(type(address) == "number")
	assert(type(value) == "number")
	if address > 0xffff then error("unclamped address") end

	--logger.info("write_mem_byte_rom", address, value)
	ram[address] = value
end

function memory.write_mem_word_internal (cpu, address, value)
	if address > 0xffff or address < 0 then error("unclamped address") end
	if address + 1 > 0xffff or address + 1 < 0 then error("unclamped address") end
	if address < 0x4000 then return end -- do not write on ROM
	ram[address + 1]  = (value & 0xff00) >> 8
	ram[address] = value & 0x00ff
end

function memory.write_mem_word (cpu, address, value)
	memory.contend(cpu, address, 3)
	memory.write_mem_byte_internal(cpu, address, value & 0xff)
	memory.contend(cpu, address + 1, 3)
	memory.write_mem_byte_internal(cpu, address + 1, (value & 0xff00) >> 8)
end

--
---

---
-- Ports I/O

local ports = {}

ports.port_fe = 0	-- a default value for port FE
ports.ear_bit = 0

-- esta funcion luego va a depender del modelo
-- port is a 16 bit value
function ports.contend_io (cpu, port, pre)
	assert(port)
	if port > 0xffff then error("unclamped port") end

	if pre then
		--if port & 0xc000 == 0x4000 then
		--	cpu.tstates = cpu.tstates + tstates
		--end
		cpu.tstates = cpu.tstates + 1
	else
		if port & 0x0001 ~= 0 then
			cpu.tstates = cpu.tstates + 3
		else
			cpu.tstates = cpu.tstates + 3
		end
	end
end

-- esta funcion luego va a depender del modelo
-- port is a 16 bit value
function ports.read_port (cpu, port)
	if port > 0xffff then error("unclamped port") end

	ports.contend_io(cpu, port, true)

	local high, low = (port & 0xff00) >> 8, port & 0x00ff
	local read = port_handlers.decode_port(cpu, high, low)

	ports.contend_io(cpu, port)
	return read
end

-- esta funcion luego va a depender del modelo
-- port is a 16 bit value
-- data is a 8 bit value
function ports.write_port (cpu, port, data)
	--logger.info("write_port: %04x %02x tstates %s", port, data, cpu.tstates)
	if port > 0xffff then error("unclamped port") end

	ports.contend_io(cpu, port, true)

	ports.contend_io(cpu, port)

	-- port FE?
	if port & 0x01 == 0 then
		ports.port_fe = data
		ports.ear_bit = data & 0x10 ~= 0

		-- Emulate Issue 3 and 2
		if data & testVal ~= 0 then
			issue = issue | 0x40
		else
			issue = issue & 0xbf
		end
	end
end

--
---



local function handle_interrupt (cpu)
	--print("handle interrupt", i)
	--beep(cpu)
	cpu:handle_interrupt(tstates_per_frame)

	display.flash_counter = display.flash_counter + 1
	if display.flash_counter == 16 then
		-- 25/50 = half second
		--display.toggle_flash()
		display.flash_counter = 0
	end

	--i = i + 1
	--if i == 2 then
		display.draw_screen(cpu)
		--i = 0
	--end
	--[[
	i = i + 1
	if i >= 115 then
		local f = io.open("out.scr", "wb")
		for j = 16384, 16384 + 6911 do
			local byte = memory.read_mem_byte_internal(cpu, j)
			f:write(string.char(byte))
		end
		os.exit()
	end
	--]]
end



local function run (machine, count_tstates)

	local cpu = assert(machine.cpu)
	local opcodes = cpu.opcodes

	--print("run for "..count_tstates .. " tstates")
	while cpu.tstates < count_tstates do

		-- opcode fetch (apply contention)
		memory.contend(cpu, cpu.PC, 4)
		-- Increment R register, without modifying the most significant bit
		cpu.R = (cpu.R & 0x80) | ( (cpu.R + 1) & 0x7f)
		-- R = (R & 0x80) | (R + 1 & 0x7f);		//Incremento R, sin modificar el bit más significativo
		local opcode = memory.read_mem_byte_internal(cpu, cpu.PC)
		-- increment PC before executing instruction
		cpu.PC = (cpu.PC + 1) & 0xffff
		local f = opcodes[opcode]
		if not f then
			logger.fatal("Opcode 0x%0x (%d) not found", opcode, opcode)
			break
		end
		--
		f(cpu)
		--logger.info("$%04x", cpu.PC)
		--if cpu.PC == 0x9854 then stop = true end
		--if cpu.PC < 1000 then
			--dump_registers(cpu)
			--os.execute("pause")
		--end

		if cpu.tstates >= tstates_per_frame then
			--print("handle interrupt", os.time())
			handle_interrupt(cpu)

			SDL.pumpEvents()
			local keys = SDL.getKeyboardState()
			if keys[SDL.scancode.Escape] then
				return
			end
		end
	end
end



local patch
local function patch_rom_48k (cpu)
	patch = memory.read_mem_word_internal(cpu, 0x0569)

	memory.write_mem_byte_rom(cpu, 0x0569, 0xdd)
	memory.write_mem_byte_rom(cpu, 0x056A, 0xff)
end

local function unpatch_rom_48k (cpu)
	local high, low = high_low(patch)
	memory.write_mem_byte_rom(cpu, 0x0569, low)
	memory.write_mem_byte_rom(cpu, 0x056A, high)
end




local function handle_tape (machine)
	require("file_format.tap").load_hook(machine)
end







local machine = {}

function machine.initialize ()

	logger.info("Loading Spectrum 48K ROM")
	local rom
	do
		local f = assert(io.open("48.ROM", "rb"))
		rom = f:read("*a")
		f:close()
	end

	local ram = {}
	logger.info("Copying ROM to RAM")
	for i = 0, 16383 do
		ram[i] = string.byte(rom:sub(i+1,i+1))
	end

	logger.info("Clearing the rest of the RAM")
	for i = 16384, 65535 do
		--ram[i] = 0
		ram[i] = math.random(0,255)
	end
	assert(ram[16384])

	memory.set_ram(ram)

	patch_rom_48k(z80)

	-- set functions to perform I/O to the cpu
	z80.opcodes.bind_io({
		memory = {
			contend = assert(memory.contend),
			contend_read_no_mreq = assert(memory.contend_read_no_mreq),
			contend_write_no_mreq = assert(memory.contend_write_no_mreq),
			read_mem_byte = assert(memory.read_mem_byte),
			read_mem_byte_internal = assert(memory.read_mem_byte_internal),
			read_mem_word = assert(memory.read_mem_word),
			write_mem_byte = assert(memory.write_mem_byte),
			write_mem_word = assert(memory.write_mem_word)
		},
		ports = {
			write_port = assert(ports.write_port),
			read_port = assert(ports.read_port)
		}
	})

	local machine_instance = {
		cpu = z80,
		run = run,
		ports = ports,
		memory = {
			read_mem_byte_internal = memory.read_mem_byte_internal,
			write_mem_byte_internal = memory.write_mem_byte_internal,
			set_bank = memory.set_bank,
			reset_bank = memory.reset_bank
		}
	}

	local opcodes_dd = require "opcodes.dd"
	opcodes_dd[0xff] = function(cpu)
		handle_tape(machine_instance)
		unpatch_rom_48k(cpu)
		machine_instance:run(2)
		patch_rom_48k(cpu)
	end
	
	return machine_instance
end

return machine
