local logger = require "logger"
local SDL = require "SDL"


-- Calculo tablas para acelerar el acceso al video del Spectrum
-- arrays de words

-- Given the vertical coordinate of a pixel, gives the starting memory address for the pixels of the corresponding
-- pixel row.
local D_PixelAddressArray = {}
for y = 0, 191 do
	D_PixelAddressArray[y] = 0x4000 + ((y >> 6) << 11) + (((y >> 3) & 0x07) << 5) + ((y & 0x07) << 8)
end

-- Given the vertical coordinate of a pixel, gives the starting memory address for the attributes of the corresponding
-- character row.
local D_AttribArray = {}
for y = 0, 191 do
	D_AttribArray[y] = 0x5800 + ((y >> 3) << 5)
end

-- Attribute matrix [32,24]
local D_Atributos = {}
for i = 0,31 do
	D_Atributos[i] = {}
end

--BYTE	D_Atributos[32][24];
-- hack para saber si algo cambio en la pantalla, muy berreta
local dirty_pixels = {}
local dirty_attributes = {}
for i = 0, 191 do
	dirty_attributes[i] = {}
end


---
-- TODO: Move this to a 'model' module. This values change with the machine type
--
local M_lineasBordeSup = 64
local M_lineasBordeInf = 56
local M_anchoBordeDer = 48
local M_anchoBordeIzq = 48




-- 24 bit colours
-- BRIGHT disabled
local colours_bright_off = {}
colours_bright_off[0] = (0x00000000);
colours_bright_off[1] = 159;
colours_bright_off[2] = (223<<16);
colours_bright_off[3] = ((224<<16)|(176));
colours_bright_off[4] = (208<<8);
colours_bright_off[5] = ((208<<8)|(208));
colours_bright_off[6] = ((207<<16)|(207<<8));
colours_bright_off[7] = ((192<<16)|(199<<8)|(192));

-- BRIGHT enabled
local colours_bright_on = {}
colours_bright_on[0] = (0x00000000);
colours_bright_on[1] = 175;
colours_bright_on[2] = (239<<16);
colours_bright_on[3] = ((255<<16)|(223));
colours_bright_on[4] = (239<<8);
colours_bright_on[5] = ((248<<8)|(255));
colours_bright_on[6] = ((255<<16)|(248<<8));
colours_bright_on[7] = ((255<<16)|(248<<8)|(255));


local display = {}

-- the one and only renderer
local rdr

display.flash_on = false
display.flash_counter = 0
display.machine = nil

local function draw_entire_screen_no_border (cpu)

	local read_mem_byte_internal = display.machine.memory.read_mem_byte_internal
	
	local D_tinta, D_papel
	for y = 0, 23 do
		for x = 0, 31 do
			D_Atributos[x][y] = read_mem_byte_internal(cpu, x + D_AttribArray[y << 3])
		end
	end

	for y = 0, 191 do
		local linea = y >> 3
		for x = 0, 31 do
			local D_Attrib = D_Atributos[x][linea]
			local D_PixelGroup = read_mem_byte_internal(cpu, x + D_PixelAddressArray[y])

			if display.flash_on or (D_Attrib & 0x80 ~= 0) then -- flash?
				D_tinta = (D_Attrib & 0x38) >> 3
				D_papel = D_Attrib & 0x07
			else
				D_tinta = D_Attrib & 0x07
				D_papel = (D_Attrib & 0x38) >> 3
			end

			-- Bright ON ?
			local foreground, background
			if D_Attrib & 0x40 ~= 0 then
				foreground = colours_bright_on[D_tinta]
				background = colours_bright_on[D_papel]
			else
				foreground = colours_bright_off[D_tinta]
				background = colours_bright_off[D_papel]
			end

			-- If the pixels or the attributes of this 8x1 line have changed, repaint
			if dirty_pixels[x + D_PixelAddressArray[y]] ~= D_PixelGroup or
				dirty_attributes[x][y] ~= D_Attrib
			then
				dirty_pixels[x + D_PixelAddressArray[y]] = D_PixelGroup
				dirty_attributes[x][y] = D_Attrib
				
				for i = 7, 0, -1 do
					if D_PixelGroup & (1 << i) ~= 0 then
						rdr:setDrawColor(foreground)
					else
						rdr:setDrawColor(background)
					end
					rdr:drawPoint({x=x * 8 + 7 - i, y=y})
				end
			end
		end
	end

	rdr:present()
end


local function draw_entire_screen_with_border (cpu)
	
	local color_borde = display.machine.ports.port_fe & 0x07
	local read_mem_byte_internal = display.machine.memory.read_mem_byte_internal

	rdr:setDrawColor(colours_bright_off[color_borde])
	for y = 0, M_lineasBordeSup - 1 do
		rdr:drawLine({x1 = 0, y1 = y, x2 = M_anchoBordeIzq + 256 + M_anchoBordeDer, y2 = y})
	end

	local D_tinta, D_papel
	for y = 0, 23 do
		for x = 0, 31 do
			D_Atributos[x][y] = read_mem_byte_internal(cpu, x + D_AttribArray[y << 3])
		end
	end

	for y = 0, 191 do
		local linea = y >> 3
		
		rdr:setDrawColor(colours_bright_off[color_borde])
		rdr:drawLine({x1 = 0, y1 = y + M_lineasBordeSup, x2 = M_anchoBordeIzq - 1, y2 = y + M_lineasBordeSup})
		
		for x = 0, 31 do
			local D_Attrib = D_Atributos[x][linea]
			local D_PixelGroup = read_mem_byte_internal(cpu, x + D_PixelAddressArray[y])

			-- TODO: flash does not work with the dirty attributes stuff
			if display.flash_on and (D_Attrib & 0x80 ~= 0) then -- flash?
				D_tinta = (D_Attrib & 0x38) >> 3
				D_papel = D_Attrib & 0x07
			else
				D_tinta = D_Attrib & 0x07
				D_papel = (D_Attrib & 0x38) >> 3
			end

			-- Bright ON ?
			local foreground, background
			if D_Attrib & 0x40 ~= 0 then
				foreground = colours_bright_on[D_tinta]
				background = colours_bright_on[D_papel]
			else
				foreground = colours_bright_off[D_tinta]
				background = colours_bright_off[D_papel]
			end

			-- If the pixels or the attributes of this 8x1 line have changed, repaint
			if dirty_pixels[x + D_PixelAddressArray[y] ] ~= D_PixelGroup or
			 	dirty_attributes[x][y] ~= D_Attrib
			then
			 	dirty_pixels[x + D_PixelAddressArray[y] ] = D_PixelGroup
			 	dirty_attributes[x][y] = D_Attrib
				
				-- draw the background
				rdr:setDrawColor(background)
				rdr:drawLine({x1 = x * 8 + M_anchoBordeIzq, y1 = y + M_lineasBordeSup,
							  x2 = x * 8 + 7 + M_anchoBordeIzq, y2 = y + M_lineasBordeSup})
				-- plot the foreground
				local points = {}
				rdr:setDrawColor(foreground)
				for i = 7, 0, -1 do
					if D_PixelGroup & (1 << i) ~= 0 then
						points[#points + 1] = {x = x * 8 + 7 - i + M_anchoBordeIzq, y = y + M_lineasBordeSup}
					else
					end
				end
				rdr:drawPoints(points)
			end
		end

		rdr:setDrawColor(colours_bright_off[color_borde])
		rdr:drawLine({x1 = M_anchoBordeIzq + 256, y1 = y + M_lineasBordeSup, x2 = M_anchoBordeIzq + 256 + M_anchoBordeDer, y2 = y + M_lineasBordeSup})
	end

	rdr:setDrawColor(colours_bright_off[color_borde])
	for y = 0, M_lineasBordeInf do
		local offset = M_lineasBordeSup + 192
		rdr:drawLine({x1 = 0, y1 =  offset + y, x2 = M_anchoBordeIzq + 256 + M_anchoBordeDer, y2 = offset + y})
	end

	rdr:present()
end



--display.draw_screen = draw_entire_screen_no_border
display.draw_screen = draw_entire_screen_with_border



---
--
function display.initialize (win)
	local err
	rdr, err = SDL.createRenderer(win, 0, 1)
	if not rdr then
		error(err)
	end
end


---
-- Signal that attributes needs to be reversed.
function display.toggle_flash ()
	dirty_pixels = {}
	display.flash_on = not display.flash_on
end

function display.set_machine (machine)
	display.machine = machine
end

return display
