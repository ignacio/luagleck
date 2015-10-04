local logger = require "logger"
local port = require "port"
local display = require "display"
local machine = require "machine"

local loader = require "file_format.loader"

local SDL = require "SDL"

local FLAG_S      = 128
local FLAG_Z      = 64
local FLAG_B5     = 32
local FLAG_H      = 16
local FLAG_B3     = 8
local FLAG_PV     = 4
local FLAG_N      = 2
local FLAG_C      = 1


---
-- helpers for troubleshooting
function dump_registers (cpu)
	logger.info([[dumping registers
Program counter, stack pointer
PC:  $%04x       SP:  $%04x
General registers
AF:  $%04x       AF':  $%04x
BC:  $%04x       BC':  $%04x
DE:  $%04x       DE':  $%04x
HL:  $%04x       HL':  $%04x
IX:  $%04x       IY:   $%04x
Flags (F)
S Z 5 H 3 V N C
%s %s %s %s %s %s %s %s
Interrupts, memory refresh
IR:  $%04x       IM:  %d (%s)
]],
	cpu.PC, cpu.SP,
	cpu.F | cpu.A << 8, cpu.Fp | cpu.Ap << 8,
	cpu.C | cpu.B << 8, cpu.Cp | cpu.Bp << 8,
	cpu.E | cpu.D << 8, cpu.Ep | cpu.Dp << 8,
	cpu.L | cpu.H << 8, cpu.Lp | cpu.Hp << 8,
	cpu.IX, cpu.IY,
	(cpu.F & FLAG_S ~= 0) and "x" or " ",
	(cpu.F & FLAG_Z ~= 0) and "x" or " ",
	(cpu.F & FLAG_B5 ~= 0) and "x" or " ",
	(cpu.F & FLAG_H ~= 0) and "x" or " ",
	(cpu.F & FLAG_B3 ~= 0) and "x" or " ",
	(cpu.F & FLAG_PV~= 0) and "x" or " ",
	(cpu.F & FLAG_N ~= 0) and "x" or " ",
	(cpu.F & FLAG_C ~= 0) and "x" or " ",
	cpu.R | cpu.I << 8, cpu.int_mode, (cpu.iff1 ~= 0 and cpu.iff2 ~= 0) and "EI" or "DI"
	)
end

function pause ()
	os.execute("pause")
end


port.initialize_ports()

---
-- Initialize SDL. Create the main window and initialize the display
--
SDL.init({ SDL.flags.Video, SDL.flags.Events })

-- screen without borders
--local win, err = SDL.createWindow({ title = "Lua Spec", height = 192, width = 256 })

-- screen with borders
local win, err = SDL.createWindow({ title = "Lua Spec", height = 192 + 64 + 56, width = 256 + 48 + 48,
									--flags = { SDL.window.Resizable }
									--flags	= { SDL.flags.OpenGL, SDL.window.Resizable }
								  })
if not win then
	error(err)
end

display.initialize(win)


local machine_instance = machine.initialize("spectrum_48k")

if arg[1] then
	if not loader.load(machine_instance, arg[1]) then
		print("failed")
		return
	end
end

print("Press ESC to quit...")
machine_instance:run(math.huge)

print "\nQuitting"
