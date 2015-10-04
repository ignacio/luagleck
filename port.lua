local logger = require "logger"

local SDL = require "SDL"

local read_port_handler = {}

local FLAG_S      = 128
local FLAG_Z      = 64
local FLAG_B5     = 32
local FLAG_H      = 16
local FLAG_B3     = 8
local FLAG_PV     = 4
local FLAG_N      = 2
local FLAG_C      = 1


local issue = 0xff

-- port is a 16 bit value
local function default_port_handler (cpu, port)
	--if(g_iNoLinea < 64 || g_iNoLinea > 64 + 192) {// || /*scanline < 24 ||*/ scanline > 128)
	--	return 0xff;
	--}
	--else {
	--	return g_memoria[g_iPaginaPantalla][(g_iScanline / 4 + D_AttribArray[g_iNoLinea - 64]) - 0x4000];
	--end
	--return 0xff
	-- TODO; Have access to the current scanline
	logger.info("default_port_handler: %s", cpu.tstates)
	return 0
end

---
-- Helper function to read a mid-row of the keyboard.
-- A "mid row" is, for instance, asdfg or qwert
local function read_key_row (keys, k1, k2, k3, k4, k5)
	return (keys[k1] and 0xfe or 0xff)
		& (keys[k2] and 0xfd or 0xff)
		& (keys[k3] and 0xfb or 0xff)
		& (keys[k4] and 0xf7 or 0xff)
		& (keys[k5] and 0xef or 0xff)

end

local function count_bits (value)
	-- count how many bits on
	local counter = 0
	for i = 0, 7 do
		if value & 0x01 ~= 0 then counter = counter + 1 end
		value = value >> 1
	end
	return counter
end


local function read_keyboard (cpu, port_high)
	-- get the currently pressed keys
	SDL.pumpEvents()
	local keys = SDL.getKeyboardState()

	local pressed_keys = issue -- depends on the keyboard issue (issue 2 or issue 3)

	local scancode = SDL.scancode

	--logger.info("read_keyboard: port: %02x %s", port_high, cpu.tstates)

	-- Map cetain key combinations to ease the usage
	-- If Backspace is pressed -> LeftShift + 0 are simulated
	-- If Capslock is pressed -> LeftShift + 2 are simulated
	-- If cursor Up is pressed -> LeftShift + 7 are simulated
	-- If cursor Down is pressed -> LeftShift + 6 are simulated
	-- If cursor Left is pressed -> LeftShift + 5 are simulated
	-- If cursor Right is pressed -> LeftShift + 8 are simulated

	if port_high & 0x01 == 0 then
		--logger.info("read_keyboard: row symbol_shift z x c v")
		-- scan the row symbol_shift z x c v
		pressed_keys = pressed_keys & read_key_row(keys, scancode.LeftShift, scancode.Z, scancode.X, scancode.C, scancode.V)
		-- Map Backspace, CapsLock and cursors to emulated keyboard. This combinations all fakes a LeftShift press
		if keys[scancode.Backspace] or keys[scancode.CapsLock] or keys[scancode.Up] or keys[scancode.Down] or
		   keys[scancode.Left] or keys[scancode.Right]
		then
			pressed_keys = pressed_keys & 0xfe
		end
	end
	if port_high & 0x02 == 0 then
		--logger.info("read_keyboard: row a s d f g")
		-- scan the row a s d f g
		pressed_keys = pressed_keys & read_key_row(keys, scancode.A, scancode.S, scancode.D, scancode.F, scancode.G)
	end
	if port_high & 0x04 == 0 then
		--logger.info("read_keyboard: row q w e r t")
		-- scan the row q w e r t
		pressed_keys = pressed_keys & read_key_row(keys, scancode.Q, scancode.W, scancode.E, scancode.R, scancode.T)
	end
	if port_high & 0x08 == 0 then
		--logger.info("read_keyboard: row 1 2 3 4 5")
		-- scan the row 1 2 3 4 5
		pressed_keys = pressed_keys & read_key_row(keys, scancode["1"], scancode["2"], scancode["3"], scancode["4"], scancode["5"])
		-- Map CapsLock and Cursors
		if keys[scancode.CapsLock] then pressed_keys = pressed_keys & 0xfd end	-- 2
		if keys[scancode.Left] then pressed_keys = pressed_keys & 0xef end		-- 5
	end
	if port_high & 0x10 == 0 then
		--logger.info("read_keyboard: row 6 7 8 9 0")
		-- scan the row 6 7 8 9 0 (backwards)
		pressed_keys = pressed_keys & read_key_row(keys, scancode["0"], scancode["9"], scancode["8"], scancode["7"], scancode["6"])
		-- Map Backspace and Cursors
		if keys[scancode.Backspace] then pressed_keys = pressed_keys & 0xfe end	-- 0
		if keys[scancode.Right] then pressed_keys = pressed_keys & 0xfb end		-- 8
		if keys[scancode.Up] then pressed_keys = pressed_keys & 0xf7 end		-- 7
		if keys[scancode.Down] then pressed_keys = pressed_keys & 0xef end		-- 6
	end
	if port_high & 0x20 == 0 then
		--logger.info("read_keyboard: row y u i o p")
		-- scan the row y u i o p (backwards)
		pressed_keys = pressed_keys & read_key_row(keys, scancode.P, scancode.O, scancode.I, scancode.U, scancode.Y)
	end
	if port_high & 0x40 == 0 then
		--logger.info("read_keyboard: row h j k l <enter>")
		-- scan the row h j k l <enter> (backwards)
		pressed_keys = pressed_keys & read_key_row(keys, scancode.Return, scancode.L, scancode.K, scancode.J, scancode.H)
	end
	if port_high & 0x80 == 0 then
		--logger.info("read_keyboard: row b n m symbol_shift space")
		-- scan the row b n m symbol_shift space
		pressed_keys = pressed_keys & read_key_row(keys, scancode.Space, scancode.RightShift, scancode.M, scancode.N, scancode.B)
	end

	--logger.info("read_keyboard: port: %02x %s pressed_keys %s", port_high, cpu.tstates, pressed_keys)

	-- mask carry flag so we don't touch it
	cpu.F = cpu.F & FLAG_C
	if pressed_keys == 0 then
		cpu.F = cpu.F | FLAG_Z
	elseif pressed_keys & 0x80 ~= 0 then
		cpu.F = cpu.F | FLAG_S
	end

	-- count how many bits on
	local count = count_bits(pressed_keys)
	if count & 0x01 == 0 then
		cpu.F = cpu.F | FLAG_PV
	end

	cpu.F = cpu.F | (pressed_keys & (FLAG_B5 | FLAG_B3))

	--return 0xbf
	return pressed_keys
end


---
-- Reads the kempston joystick (allows us to use the cursor keys of the keyboard in place of a joystick)
local function read_kempston (cpu, port_high)

	local keys = SDL.getKeyboardState()
	local scancode = SDL.scancode
	
	local value =     (keys[scancode.LeftControl] or keys[scancode.RightControl]) and 16 or 0
					| (keys[scancode.Up] and 8 or 0)
					| (keys[scancode.Down] and 4 or 0)
					| (keys[scancode.Left] and 2 or 0)
					| (keys[scancode.Right] and 1 or 0)

	-- mask carry flag so we don't touch it
	cpu.F = cpu.F & FLAG_C
	if value == 0 then
		cpu.F = cpu.F | FLAG_Z
	elseif value & 0x80 ~= 0 then
		cpu.F = cpu.F | FLAG_S
	end

	-- count how many bits on
	local count = count_bits(value)
	if count & 0x01 == 0 then
		cpu.F = cpu.F | FLAG_PV
	end

	cpu.F = cpu.F | (value & (FLAG_B5 | FLAG_B3))

	return value
end


local public = {}

function public.initialize_ports ()
	read_port_handler[0xfe] = read_keyboard
	--//ReadPortHandler[0xfe] = ReadPlaybackStream;
	read_port_handler[0x31] = read_kempston
	read_port_handler[0x7f] = read_fuller
	read_port_handler[0xfb] = read_alphacom_printer
	read_port_handler[0xbf] = read_light_gun

	--//archivoOut = fopen("c:\\tecOut.out", "wb+");
	--//archivoOut = fopen("c:\\tecOut.out", "rb");
	return true
end


---
-- Decodes and read a port
--
function public.decode_port (cpu, port_high, port_low)
	-- Kempston?
	-- bit 7,6,5 are low?
	if (port_low & 0xe0 == 0)
		or port_low == 0xdf -- some games rely on this
	then
		return read_kempston(cpu, port_high)
	end

	local handler = read_port_handler[port_low] or default_port_handler
	return handler(cpu, port_high)
end


return public
