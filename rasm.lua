local args = {...}
local _floor = math.floor

local model = {
	name = "R16K1S60",
	run = {-62, 27},
	pause = {-50, 27},
	resume = {-38, 27},
	reset = {-26, 27},
	ram = {16, 4},
	ramsize = {64, 16},
	iron = {-76, -142},
	ironsize = {223, 170},
	isrunning = function(selectedpos)
		return not sim.partID(selectedpos[1] + 131, selectedpos[2] - 86) and true
	end
}
do
	local function infuncgen(val_and, val_or, coord)
		return function(selectedpos)
			local partID = sim.partID(selectedpos[1] + coord[1], selectedpos[2] + coord[2])
			if not partID then return 0 end
			local ctype = sim.partProperty(partID, "ctype")
			return bit.bor(val_or, bit.band(val_and, ctype))
		end
	end
	local function outfuncgen(val_and, val_or, coords)
		return function(selectedpos, rawoutval)
			local outval = bit.bor(val_or, bit.band(val_and, rawoutval))
			for ix = 1, #coords do
				local partID = sim.partID(selectedpos[1] + coords[ix][1], selectedpos[2] + coords[ix][2])
				if partID then
					sim.partProperty(partID, "ctype", outval)
				end
			end
		end
	end
	model.editables = {
		{
			name = "A:", infunc = infuncgen(0xFFFF, 0, {-63, -128}),
			outfunc = outfuncgen(0xFFFF, 0x20000000, {{-63, -128}, {-63, -14}})
		},
		{
			name = "B:", infunc = infuncgen(0xFFFF, 0, {-64, -128}),
			outfunc = outfuncgen(0xFFFF, 0x20000000, {{-64, -128}, {-64, -14}})
		},
		{
			name = "C:", infunc = infuncgen(0xFFFF, 0, {-65, -128}),
			outfunc = outfuncgen(0xFFFF, 0x20000000, {{-65, -128}, {-65, -14}})
		},
		{
			name = "D:", infunc = infuncgen(0xFFFF, 0, {-66, -128}),
			outfunc = outfuncgen(0xFFFF, 0x20000000, {{-66, -128}, {-66, -14}})
		},
		{
			name = "E:", infunc = infuncgen(0xFFFF, 0, {-67, -128}),
			outfunc = outfuncgen(0xFFFF, 0x20000000, {{-67, -128}, {-67, -14}})
		},
		{
			name = "S:", infunc = infuncgen(0xFFFF, 0, {-68, -128}),
			outfunc = outfuncgen(0xFFFF, 0x20000000, {{-68, -128}, {-68, -14}, {-67, -119}})
		},
		{
			name = "I:", infunc = infuncgen(0xFFFF, 0, {-69, -128}),
			outfunc = outfuncgen(0xFFFF, 0x20000000, {{-69, -128}, {-69, -14}, {-68, -116}, {-68, -76}})
		},
		{
			name = "F:", infunc = infuncgen(0x7F, 0, {-24, -74}),
			outfunc = outfuncgen(0x7F, 0x20000000, {{-24, -74}, {-24, -120}, {-24, -122}})
		}
	}
end
model.cellcount = model.ramsize[1] * model.ramsize[2]

local content, invoked_from_rasmui
if args[4] == "model" then -- model data request
	return model
elseif args[4] == "content" or args[4] == "content_rasmui" then -- assemble argument
	--! TODO: release this
	-- invoked_from_rasmui = args[4] == "content_rasmui"
	content = args[3]
else -- assemble file named by argument
	local handle = io.open(args[3], "r")
	if not handle then
		print("Failed to open " .. args[3])
		return
	end
	content = handle:read("*a")
	handle:close()
end

local errors_ordered = {}
local translating = true
local error_encountered, current_line
local line_to_error = setmetatable({}, {__newindex = function(tbl, key, value)
	errors_ordered[#errors_ordered + 1] = {
		line = key,
		message = value
	}
	rawset(tbl, key, value)
	translating = false
end})

local function refmatch(ref, ...)
	local matches = {...}
	ref.matches = #matches ~= 0 and matches
	return ref.matches and true
end
local templates = {
	{name = "opneut2", text = "", pattern = "^()([%w_ %.%-\"]+)%s*,%s*([%w_ %.%-\"]+)$"},
	{name = "opneut1", text = "", pattern = "^()([%w_ %.%-\"]+)$"},
	{name = "opneut0", text = "", pattern = "^()$"},
	{name = "opneut3", text = "", pattern = "^()([%w_ %.%-\"]+)%s*,%s*([%w_ %.%-\"]+)%s*,%s*([%w_ %.%-\"]+)$"},
	{name = "opadr1p", text = "", pattern = "^()%[%s*([%w_ %.%-\"]+)%s*%+%s*([%w_ %.%-\"]+)%s*%]%s*,%s*([%w_ %.%-\"]+)$"},
	{name = "opadr2p", text = "", pattern = "^()([%w_ %.%-\"]+)%s*,%s*%[%s*([%w_ %.%-\"]+)%s*%+%s*([%w_ %.%-\"]+)%s*%]$"},
	{name = "opadr1n", text = "", pattern = "^()%[%s*([%w_ %.%-\"]+)%s*%-%s*([%w_ %.%-\"]+)%s*%]%s*,%s*([%w_ %.%-\"]+)$"},
	{name = "opadr2n", text = "", pattern = "^()([%w_ %.%-\"]+)%s*,%s*%[%s*([%w_ %.%-\"]+)%s*%-%s*([%w_ %.%-\"]+)%s*%]$"},
	{name = "opadr1z", text = "", pattern = "^()%[%s*([%w_ %.%-\"]+)%s*%]%s*,%s*([%w_ %.%-\"]+)$"},
	{name = "opadr2z", text = "", pattern = "^()([%w_ %.%-\"]+)%s*,%s*%[%s*([%w_ %.%-\"]+)%s*%]$"}
}

local modifiers = {
	["flags"] = true,
	["noflags"] = true
}

local registers = {
	["ax"] = 1,
	["bx"] = 2,
	["cx"] = 3,
	["dx"] = 4,
	["ex"] = 5,
	["sp"] = 6,
	["ip"] = 7
}

local labels = {}
local last_global_label
local strings = {}
local stringslen = 0
local function stringcap(cap)
	stringslen = stringslen + 1
	local strfun = loadstring("return \"" .. (cap or "") .. "\"")
	if not strfun then
		line_to_error[current_line] = "malformed string"
		error_encountered = true
		return ""
	end
	strings[tostring(stringslen)] = strfun()
	return "\"" .. stringslen .. "\""
end
local cell_to_line = {}
local labelpatches = {}

local function initdata_byte(instruction)
	local result_8 = {}
	for ix = 1, #instruction.operands do
		local ixOperand = instruction.operands[ix]
		if ixOperand.oper_type == "number" then
			result_8[#result_8 + 1] = ixOperand.oper_val % 0x100
		elseif ixOperand.oper_type == "label" then
			result_8.errormsg = "a label cannot be used to initialize 8-bit data"
			break
		elseif ixOperand.oper_type == "register" then
			result_8.errormsg = "a register cannot be used to initialize data"
			break
		elseif ixOperand.oper_type == "string" then
			if #result_8 % 2 == 1 then result_8[#result_8 + 1] = 0 end
			for letter in strings[ixOperand.oper_val]:gmatch(".") do
				result_8[#result_8 + 1] = letter:byte()
			end
			if #ixOperand.oper_val % 2 == 1 then result_8[#result_8 + 1] = 0 end
		end
	end
	local result = {}
	local result_8_len = #result_8
	for ix = 2, result_8_len, 2 do
		result[ix / 2] = (result_8[ix - 1]) % 0x100 * 0x100 + (result_8[ix]) % 0x100
	end
	if result_8_len % 2 == 1 then
		result[#result + 1] = result_8[result_8_len]
	end
	return result
end

local function initdata_word(instruction)
	local result = {}
	for ix = 1, #instruction.operands do
		local ixOperand = instruction.operands[ix]
		if ixOperand.oper_type == "number" then
			result[#result + 1] = ixOperand.oper_val % 0x10000
		elseif ixOperand.oper_type == "label" then
			labelpatches[{instruction = instruction, offset = ix - 1}] = labels[ixOperand.oper_val]
			result[#result + 1] = 0
		elseif ixOperand.oper_type == "register" then
			result.errormsg = "a register cannot be used to initialize data"
			break
		elseif ixOperand.oper_type == "string" then
			for letter in strings[ixOperand.oper_val]:gmatch(".") do
				result[#result + 1] = letter:byte()
			end
		end
	end
	return result
end

local initdata_mnemonics = {
	["db"] = initdata_byte,
	["dw"] = initdata_word
}

local template_datalist = {name = "datalist", text = ""}

local instructions = {}
local labelqueue = {}
local modifierqueue = {}
local instructionslen = 0
current_line = 1
for linebreakstart, linebreakend, linestr in content:gmatch("()\n*()([^\n]+)") do
	error_encountered = false
	local instruction
	current_line = current_line + linebreakend - linebreakstart
	linestr = linestr:gsub("\"([^\"]*)\"", stringcap):gsub(";.*$", "")
	if error_encountered then
		break
	end
	while true do
		local gotdot, labelname, labelrest = linestr:match("^%s*(%.?)([%a_][%w_]*)%s*:(.*)$")
		if labelname then
			gotdot = #gotdot ~= 0
			if gotdot then
				if not last_global_label then
					line_to_error[current_line] = "no global label defined yet"
					error_encountered = true
					break
				end
				labelname = last_global_label .. "." .. labelname
			end
			if labels[labelname] or labelqueue[labelname] then
				line_to_error[current_line] = "label '" .. labelname .. "' already defined"
				error_encountered = true
				break
			end
			if not gotdot then
				last_global_label = labelname
			end
			labelqueue[labelname] = true
			linestr = labelrest
		else
			break
		end
	end
	if error_encountered then break end
	linestr = linestr:gsub("^%s*(.-)%s*$", "%1")
	while true do
		local modifier, rest = linestr:match("^@([%a_][%w_%(%)]*)%s*(.+)$")
		if modifier then
			if modifier == "org" then
				local org_str, rest_ = rest:match("^(.-)%s*(.+)$")
				if not orgstr then org_str, rest_ = rest, "" end
				if not org_str then
					line_to_error[current_line] = "missing org value"
					error_encountered = true
					break
				end
				local org_override = tonumber(org_str)
				if not org_override then
					line_to_error[current_line] = "org value is malformed"
					error_encountered = true
					break
				end
				modifierqueue.org = org_override
				linestr = rest_
			else
				if not modifiers[modifier] then
					line_to_error[current_line] = "invalid modifier '" .. modifier .. "'"
					error_encountered = true
					break
				end
				modifierqueue[modifier] = true
				linestr = rest
			end
		else
			break
		end
	end
	if error_encountered then break end
	linestr = linestr:gsub("^%s*(.-)%s*$", "%1")
	if not linestr:match("^%s*$") then
		local mnemonic, opers_rest = linestr:match("^([%a_][%w_]*)%s*(.*)$")
		if mnemonic then
			local operands, template
			local ref = {}
			if initdata_mnemonics[mnemonic] then
				template = template_datalist
				operands = {}
				for oper_parts in opers_rest:gmatch("[^,]+") do
					operands[#operands + 1] = oper_parts
				end
			else
				for ix = 1, #templates do
					local ixTemplate = templates[ix]
					if refmatch(ref, opers_rest:match(ixTemplate.pattern)) then
						template = ixTemplate
						operands = ref.matches
						table.remove(operands, 1)
						break
					end
				end
			end
			if template then
				instruction = {
					mnemonic = mnemonic,
					operands = {},
					template = template,
					line = current_line,
					modifiers = modifierqueue,
					global_label = last_global_label
				}
				modifierqueue = {}
				for key, value in pairs(operands) do
					value = value:gsub("^%s*(.-)%s*$", "%1")
					local typedoperand = {}
					repeat
						local regref = registers[value:lower()]
						if regref then
							typedoperand.oper_type = "register"
							typedoperand.oper_val = regref
							break
						end
						local numstr = value:match("^%-?%s*%d+$") or value:lower():match("^0x*[%dabcdef]+$")
						if numstr then
							local numberval = tonumber(numstr)
							if numberval and _floor(numberval) == numberval then
								typedoperand.oper_type = "number"
								typedoperand.oper_val = numberval
							else
								line_to_error[current_line] = "malformed number '" .. numstr .. "'"
								error_encountered = true
								break
							end
							break
						end
						local stringstr = value:match("^\"(%d+)\"$")
						if stringstr then
							typedoperand.oper_type = "string"
							typedoperand.oper_val = stringstr
							break
						end
						local gotdot, labelstr = value:match("^(%.?)([%a_][%w_]*)$")
						if not labelstr then
							labelstr = value:match("^([%a_][%w_]*%.[%a_][%w_]*)$")
							gotdot = ""
						end
						if not labelstr then
							labelstr = value:match("^([%a_][%w_]*)$")
						end
						if labelstr then
							gotdot = #gotdot ~= 0
							if gotdot then
								if not last_global_label then
									line_to_error[current_line] = "no global label defined yet"
									error_encountered = true
									break
								end
								labelstr = last_global_label .. "." .. labelstr
							end
							typedoperand.oper_type = "label"
							typedoperand.oper_val = labelstr
							break
						end
					until true
					if not error_encountered then
						if next(typedoperand) then
							instruction.operands[key] = typedoperand
						else
							line_to_error[current_line] = "invalid operand #" .. key
							error_encountered = true
						end
					end
				end
				if not error_encountered then
					instructionslen = instructionslen + 1
					instructions[instructionslen] = instruction
				end
			else
				line_to_error[current_line] = "invalid operand list"
			end
		else
			line_to_error[current_line] = "mnemonic expected"
		end
	end
	if instruction then
		for key in pairs(labelqueue) do
			labels[key] = instruction
		end
		labelqueue = {}
	end
end

local function generic_operand(cellresult, operandlevel, instruction, operandnum)
	local operand = instruction.operands[operandnum]
	if cellresult.errormsg then return end
	if operand.oper_type == "register" then
		if operandlevel == 3 and operand.oper_val > 3 then
			cellresult.errormsg = "'" .. operand.oper_val .. "' cannot be used as a tertiary operand"
		else
			cellresult[1] = cellresult[1] + operand.oper_val * 2 ^ (10 - operandlevel * 3) -- reference register
		end
	elseif operand.oper_type == "label" then
		local labeledinstr = labels[operand.oper_val]
		if labeledinstr then
			if labeledinstr ~= instructions[1] then -- immediates default to 0
				labelpatches[{instruction = instruction, offset = 1}] = labeledinstr
				cellresult[1] = cellresult[1] + 1 -- supply immediate
				cellresult[2] = 0
			end
			if cellresult.imm then
				cellresult.errormsg = "only one immediate can be encoded"
			else
				cellresult.imm = true
			end
		else
			cellresult.errormsg = "unknown label '" .. operand.oper_val .. "'"
		end
	elseif operand.oper_type == "number" then
		local numberval = operand.oper_val
		if numberval ~= 0 then -- immediates default to 0
			cellresult[1] = cellresult[1] + 1 -- supply immediate
			cellresult[2] = numberval % 0x10000
		end
		if cellresult.imm then
			cellresult.errormsg = "only one immediate can be encoded"
		else
			cellresult.imm = true
		end
	else
		cellresult.errormsg = "invalid operand '" .. operand.oper_val .. "'"
	end
end

local function generate_mnemonic(opcode, operandmap, rejectedModifiers)
	return function(instruction)
		local result = {opcode}
		for key, value in pairs(operandmap) do if value then
			generic_operand(result, key, instruction, value)
		end end
		if instruction.modifiers["flags"] then
			if rejectedModifiers and rejectedModifiers["flags"] then
				return {errormsg = "@flags cannot be used here"}
			else
				result[1] = bit.bor(result[1], 0x0008)
			end
		end
		if instruction.modifiers["noflags"] then
			if rejectedModifiers and rejectedModifiers["noflags"] then
				return {errormsg = "@noflags cannot be used here"}
			else
				result[1] = bit.band(result[1], bit.bnot(0x0008))
			end
		end
		return result
	end
end

local memop_rejects_flagset = {flags = true, noflags = true}
local mnemonics = {
	["nop"] = {opneut0 = generate_mnemonic(0x0000, {})},
	["xchg"] = {opneut2 = generate_mnemonic(0x0008, {1, 2})},
	["hlt"] = {opneut0 = generate_mnemonic(0x1000, {})},
	["call"] = {opneut1 = generate_mnemonic(0x2000, {false, 1})},
	["push"] = {opneut1 = generate_mnemonic(0x2400, {false, 1})},
	["ret"] = {opneut0 = generate_mnemonic(0x2800, {})},
	["pop"] = {opneut1 = generate_mnemonic(0x2C00, {false, 1})},
	["jb"] = {opneut1 = generate_mnemonic(0x4000, {false, 1})},
	["jnb"] = {opneut1 = generate_mnemonic(0x4100, {false, 1})},
	["jo"] = {opneut1 = generate_mnemonic(0x4200, {false, 1})},
	["jno"] = {opneut1 = generate_mnemonic(0x4300, {false, 1})},
	["js"] = {opneut1 = generate_mnemonic(0x4400, {false, 1})},
	["jns"] = {opneut1 = generate_mnemonic(0x4500, {false, 1})},
	["je"] = {opneut1 = generate_mnemonic(0x4600, {false, 1})},
	["jne"] = {opneut1 = generate_mnemonic(0x4700, {false, 1})},
	["jle"] = {opneut1 = generate_mnemonic(0x4800, {false, 1})},
	["jnle"] = {opneut1 = generate_mnemonic(0x4900, {false, 1})},
	["jl"] = {opneut1 = generate_mnemonic(0x4A00, {false, 1})},
	["jnl"] = {opneut1 = generate_mnemonic(0x4B00, {false, 1})},
	["jbe"] = {opneut1 = generate_mnemonic(0x4C00, {false, 1})},
	["jnbe"] = {opneut1 = generate_mnemonic(0x4D00, {false, 1})},
	["jmp"] = {opneut1 = generate_mnemonic(0x4E00, {false, 1})},
	["send"] = {opneut2 = generate_mnemonic(0x5000, {2, 1})},
	["recv"] = {opneut2 = generate_mnemonic(0x5400, {1, 2})},
	["bump"] = {opneut1 = generate_mnemonic(0x5800, {false, 1})},
	["wait"] = {opneut1 = generate_mnemonic(0x5C00, {false, 1}),
		opneut0 = generate_mnemonic(0x5C00, {})},
	["add"] = {opneut2 = generate_mnemonic(0x8008, {1, 2})},
	["sub"] = {opneut2 = generate_mnemonic(0x8408, {1, 2})},
	["cmp"] = {opneut2 = generate_mnemonic(0x9408, {1, 2})},
	["adc"] = {opneut2 = generate_mnemonic(0x8808, {1, 2})},
	["sbb"] = {opneut2 = generate_mnemonic(0x8C08, {1, 2})},
	["shl"] = {opneut2 = generate_mnemonic(0xA008, {1, false, 2}),
		opneut3 = generate_mnemonic(0xA008, {1, 2, 3})},
	["shr"] = {opneut2 = generate_mnemonic(0xA408, {1, false, 2}),
		opneut3 = generate_mnemonic(0xA408, {1, 2, 3})},
	["shld"] = {opneut3 = generate_mnemonic(0xA808, {1, 2, 3})},
	["rol"] = {opneut2 = generate_mnemonic(0xA808, {1, 1, 2})},
	["shrd"] = {opneut3 = generate_mnemonic(0xAC08, {1, 2, 3})},
	["ror"] = {opneut2 = generate_mnemonic(0xAC08, {1, 1, 2})},
	["mov"] = {opneut2 = generate_mnemonic(0xC000, {1, 2}),
		opadr1z = generate_mnemonic(0x3000, {1, 2}),
		opadr1p = generate_mnemonic(0x3008, {1, 3, 2}, memop_rejects_flagset),
		opadr1n = generate_mnemonic(0x3408, {1, 3, 2}, memop_rejects_flagset),
		opadr2z = generate_mnemonic(0x3800, {2, 1}),
		opadr2p = generate_mnemonic(0x3808, {2, 1, 3}, memop_rejects_flagset),
		opadr2n = generate_mnemonic(0x3C08, {2, 1, 3}, memop_rejects_flagset)},
	["and"] = {opneut2 = generate_mnemonic(0xC408, {1, 2})},
	["test"] = {opneut2 = generate_mnemonic(0xD408, {1, 2})},
	["or"] = {opneut2 = generate_mnemonic(0xC808, {1, 2})},
	["xor"] = {opneut2 = generate_mnemonic(0xCC08, {1, 2})}
}
mnemonics["jna"] = mnemonics["jbe"]
mnemonics["ja"] = mnemonics["jnbe"]
mnemonics["jng"] = mnemonics["jle"]
mnemonics["jg"] = mnemonics["jnle"]
mnemonics["jnge"] = mnemonics["jl"]
mnemonics["jge"] = mnemonics["jnl"]
mnemonics["jz"] = mnemonics["je"]
mnemonics["jnz"] = mnemonics["jne"]
mnemonics["jc"] = mnemonics["jb"]
mnemonics["jnae"] = mnemonics["jb"]
mnemonics["jnc"] = mnemonics["jnb"]
mnemonics["jae"] = mnemonics["jnb"]
for key, value in pairs(initdata_mnemonics) do mnemonics[key] = {datalist = value} end

local cell_count = 0
local cells = {}
for ix = 0, model.cellcount - 1 do cells[ix] = 0 end
for ixInst = 1, instructionslen do
	local instruction = instructions[ixInst]
	local mnemonicindex = mnemonics[instruction.mnemonic:lower()]
	if mnemonicindex then
		local templateindex = mnemonicindex[instruction.template.name]
		if templateindex then
			local cellresult = templateindex(instruction)
			if instruction.modifiers.org then
				cell_count = instruction.modifiers.org
			end
			instruction.address = cell_count
			if cellresult.errormsg then
				line_to_error[instruction.line] = cellresult.errormsg
			else
				instruction.cellresult = cellresult
				if translating then
					for ixCell = 1, #cellresult do
						if cell_count == model.cellcount then
							line_to_error[instruction.line] = "out of space"
						end
						cells[cell_count] = cellresult[ixCell]
						cell_count = cell_count + 1
					end
				end
			end
		else
			line_to_error[instruction.line] = "mnemonic does not accept argument list '" .. instruction.template.text .. "'"
		end
	else
		line_to_error[instruction.line] = "invalid mnemonic '" .. instruction.mnemonic .. "'"
	end
end

if translating then
	for key, value in pairs(labelpatches) do
		cells[key.instruction.address + key.offset] = value.address
	end
	for key, value in pairs(cells) do
		local partID = sim.partID(args[1] + model.ram[1] + key % model.ramsize[1], args[2] + model.ram[2] + _floor(key / model.ramsize[1]))
		if not partID then
			print("Burn failed")
			break
		end
		sim.partProperty(partID, "ctype", 0x20000000 + value)
	end
end

if invoked_from_rasmui then
	return line_to_error
else
	if translating then
		print("Success - " .. instructionslen .. " instructions, " .. cell_count .. " cells")
	else
		print("Error: line " .. errors_ordered[1].line .. ": " .. errors_ordered[1].message)
		local errors_ordered_len = #errors_ordered
		if errors_ordered_len > 1 then
			print("    and " .. (errors_ordered_len - 1) .. " more")
		end
	end
end

