-- * TODO: don't let scrollbar go small

local tab_to_space_ = "    "
local rasm_path = debug.getinfo(1, "S").source:sub(2):gsub("\\", "/")
do
	local rasm_name = "rasm.lua"
	if rasm_path:find("/") then -- * We **do** need this here.
		-- * Now this is a weird one. For some reason, if :gsub doesn't find anything here,
		--   it messes up the stack and we get a "chunk has too many syntax levels" error.
		rasm_path = rasm_path:gsub("/[^/]+$", "/" .. rasm_name)
	else
		rasm_path = rasm_name
	end
end

local kbdlayouts = {
	["en"] = {["default"]={[96]=96,[49]=49,[50]=50,[51]=51,[52]=52,[53]=53,[54]=54,[55]=55,[56]=56,[57]=57,[8]=8,[277]=277,[278]=278,[280]=280,[9]=9,[113]=113,[119]=119,[101]=101,[114]=114,[116]=116,[121]=121,[117]=117,[105]=105,[111]=111,[112]=112,[13]=13,[127]=127,[279]=279,[281]=281,[97]=97,[115]=115,[100]=100,[102]=102,[103]=103,[104]=104,[106]=106,[107]=107,[108]=108,[304]=304,[122]=122,[120]=120,[99]=99,[118]=118,[98]=98,[110]=110,[109]=109,[44]=44,[46]=46,[47]=47,[303]=303,[273]=273,[306]=306,[308]=308,[32]=32,[307]=307,[319]=319,[305]=305,[276]=276,[274]=274,[275]=275},["shift"]={[96]=126,[49]=33,[50]=64,[51]=35,[52]=36,[53]=37,[54]=94,[55]=38,[56]=42,[57]=40,[48]=41,[45]=95,[61]=43,[8]=8,[277]=277,[278]=278,[280]=280,[9]=9,[113]=113,[119]=119,[101]=101,[114]=114,[116]=116,[121]=121,[117]=117,[105]=105,[111]=111,[112]=112,[13]=13,[127]=127,[279]=279,[281]=281,[97]=97,[115]=115,[100]=100,[102]=102,[103]=103,[104]=104,[106]=106,[107]=107,[108]=108,[304]=304,[122]=122,[120]=120,[99]=99,[118]=118,[98]=98,[110]=110,[109]=109,[44]=60,[46]=62,[47]=63,[303]=303,[273]=273,[306]=306,[308]=308,[32]=32,[307]=307,[319]=319,[305]=305,[276]=276,[274]=274,[275]=275,[91]=123,[93]=125,[59]=58,[39]=34,[92]=124},["altgr"]={}},
	["hu"] = {["default"]={[96]=48,[49]=49,[50]=50,[51]=51,[52]=52,[53]=53,[54]=54,[55]=55,[56]=56,[57]=57,[8]=8,[277]=277,[278]=278,[280]=280,[9]=9,[113]=113,[119]=119,[101]=101,[114]=114,[116]=116,[121]=122,[117]=117,[105]=105,[111]=111,[112]=112,[13]=13,[127]=127,[279]=279,[281]=281,[97]=97,[115]=115,[100]=100,[102]=102,[103]=103,[104]=104,[106]=106,[107]=107,[108]=108,[304]=304,[122]=121,[120]=120,[99]=99,[118]=118,[98]=98,[110]=110,[109]=109,[44]=44,[46]=46,[47]=45,[303]=303,[273]=273,[306]=306,[308]=308,[32]=32,[307]=307,[319]=319,[305]=305,[276]=276,[274]=274,[275]=275},["shift"]={[49]=39,[50]=34,[51]=43,[52]=33,[53]=37,[54]=47,[55]=61,[56]=40,[57]=41,[8]=8,[277]=277,[278]=278,[280]=280,[9]=9,[113]=113,[119]=119,[101]=101,[114]=114,[116]=116,[121]=122,[117]=117,[105]=105,[111]=111,[112]=112,[13]=13,[127]=127,[279]=279,[281]=281,[97]=97,[115]=115,[100]=100,[102]=102,[103]=103,[104]=104,[106]=106,[107]=107,[108]=108,[304]=304,[122]=121,[120]=120,[99]=99,[118]=118,[98]=98,[110]=110,[109]=109,[44]=63,[46]=58,[47]=95,[303]=303,[273]=273,[306]=306,[308]=308,[32]=32,[307]=307,[319]=319,[305]=305,[276]=276,[274]=274,[275]=275},["altgr"]={[49]=126,[51]=94,[8]=8,[277]=277,[278]=278,[280]=280,[9]=9,[113]=92,[119]=124,[13]=13,[127]=127,[279]=279,[281]=281,[97]=97,[102]=91,[103]=93,[304]=304,[60]=60,[122]=62,[120]=35,[99]=38,[118]=64,[98]=123,[110]=125,[44]=59,[47]=42,[303]=303,[273]=273,[306]=306,[308]=308,[32]=32,[307]=307,[319]=319,[305]=305,[276]=276,[274]=274,[275]=275}}
}

local tooltip_timeout = 60
local keyboard_delay = 30
local keyboard_interval = 2
local keyboard_layout = kbdlayouts[tpt.RASMUI_KBDLAYOUT or "en"]
if not keyboard_layout then
	print("\008lKeyboard layout '" .. tpt.RASMUI_KBDLAYOUT .. "' not found, falling back to 'en'")
	keyboard_layout = kbdlayouts["en"]
end

local function invokeRasm(...)
	if not fs.exists(rasm_path) then
		print("\008lFailed to invoke RASM, file not found")
		return
	end
	local rasmfunc, errstr = loadfile(rasm_path)
	if not rasmfunc then
		print("\008l" .. errstr)
		return
	end
	local pcallres = {pcall(rasmfunc, ...)}
	if not pcallres[1] then
		print("\008l" .. pcallres[2])
		return
	end
	return unpack(pcallres, 2)
end

local model = invokeRasm(nil, nil, nil, "model")
if not model then return end

local color_yellow = {1, 1, 0}
local color_red = {1, 0, 0}
local color_green = {0, 1, 0}
local color_cyan = {0, 1, 1}
local _most_likely_windows = not fs.exists("C:/")

local smartPath
do
	local spathMetatable
	local spathMetatableIndex = {}
	function spathMetatableIndex:equals(other)
		if #other ~= #self then return false end
		for ix = 1, #other do
			if other[ix] ~= self[ix] then return false end
		end
		return true
	end
	function spathMetatableIndex:isParentOf(other)
		if #other >= #self then return false end
		for ix = 1, #self do
			if other[ix] ~= self[ix] then return false end
		end
		return true
	end
	function spathMetatableIndex:isChildOf(other)
		return other:isParentOf(self)
	end
	function spathMetatableIndex:combine(other)
		local newpath = {}
		for ix = 1, #self do
			newpath[ix] = self[ix]
		end
		if type(other) == "string" then
			newpath[#newpath + 1] = other
		elseif getmetatable(other) == spathMetatable then for ix = 1, #other do
			newpath[#newpath + 1] = other[ix]
		end end
		return setmetatable(newpath, spathMetatable)
	end
	function spathMetatableIndex:bake()
		return table.concat(self, "/")
	end
	function spathMetatableIndex:front(level)
		local newpath = {}
		for ix = #self - (level or 1) + 1, #self do
			newpath[#newpath + 1] = self[ix]
		end
		return setmetatable(newpath, spathMetatable)
	end
	function spathMetatableIndex:back(level)
		local newpath = {}
		for ix = 1, #self - (level or 1) do
			newpath[ix] = self[ix]
		end
		return setmetatable(newpath, spathMetatable)
	end
	spathMetatable = { -- local
		__index = spathMetatableIndex,
		__tostring = spathMetatableIndex.bake,
		__concat = spathMetatableIndex.combine,
		__sub = spathMetatableIndex.back,
		__eq = spathMetatableIndex.equals,
		__lt = spathMetatableIndex.isChildOf
	}
	function smartPath(path)
		local nobackslashes = path:gsub("\\", "/"):gsub("//*", "/")
		local newpath = {}
		local lpos2 = 1
		while true do
			local pos1, pos2 = nobackslashes:match("()/()", lpos2)
			newpath[#newpath + 1] = pos1 and nobackslashes:sub(lpos2, pos1 - 1) or nobackslashes:sub(lpos2)
			if not pos1 then break end
			lpos2 = pos2
		end
		if newpath[#newpath] == "" then newpath[#newpath] = nil end
		return setmetatable(newpath, spathMetatable)
	end
end


local _floor, _min, _max = math.floor, math.min, math.max
local gfx_width_, gfx_height_ = gfx.WIDTH, gfx.HEIGHT


local function rslashwrapper(fsfunc)
	return function(path)
		local altpath = path:match("^(.*)[\\/]$") or path .. "/"
		return fsfunc(path) or fsfunc(altpath)
	end
end

local fs_isdirectory = rslashwrapper(fs.isDirectory)
local fs_isfile = rslashwrapper(fs.isFile)
local fs_exists = rslashwrapper(fs.exists)
local fs_list = rslashwrapper(_most_likely_windows and function(path)
	if path == "" then
		local result = {}
		for ix = ("A"):byte(), ("Z"):byte() do
			local drv = string.char(ix) .. ":"
			if fs.exists(drv .. "/") then
				result[#result + 1] = drv
			end
		end
		return result
	else
		return fs.list(path)
	end
end or fs.list)


local safe_ipairs, safe_pairs
do
	local function inext(itbl, ikey)
		ikey = ikey + 1
		local ivalue = itbl[ikey]
		if not ivalue then return nil end
		return ikey, ivalue
	end
	local function backwards_inext(itbl, ikey)
		if ikey == 0 then return nil end
		local ivalue = itbl[ikey]
		return ikey - 1, ivalue
	end
	function safe_ipairs(tbl, backwards)
		local tbl_backup = {}
		for key, value in pairs(tbl) do
			tbl_backup[key] = value
		end
		if backwards then
			return backwards_inext, tbl_backup, #tbl_backup
		end
		return inext, tbl_backup, 0
	end
	
	function safe_pairs(tbl)
		local tbl_backup = {}
		for key, value in pairs(tbl) do
			tbl_backup[key] = value
		end
		return next, tbl_backup
	end
end

local highlighting = {
	{
		["t"] = {"xchg", "nop", "hlt", "call", "ret", "push", "pop", "jnb", "jae", "jnc", "jb", "jnae",
		"jc", "jno", "jo", "jns", "js", "jne", "jnz", "je", "jz", "jg", "jnle", "jle", "jng",
		"jge", "jnl", "jl", "jnge", "ja", "jnbe", "jbe", "jna", "jmp", "send", "recv", "shl",
		"shr", "shld", "shrd", "rol", "ror", "add", "adc", "sub", "sbb", "bump", "wait",
		"cmp", "mov", "and", "or", "xor", "test", "@flags", "@noflags", "dw", "db", "@org"},
		["g"] = {"ax", "bx", "cx", "dx", "ex", "sp", "ip"},
		["o"] = {"%d+", "0x[%daAbBcCdDeEfF]+"}
	},
	{
		["l"] = {";.*"}
	},
	{
		["r"] = {"\"\"", "\"[^\"]+\""}
	}
}
local do_highlighting
do
	local key -- I hate states
	local function gsubcapret(cap1, cap2, cap3)
		return (type(cap1) == "string" and cap1 or "") .. "\008" .. key .. cap2:gsub("\008.", "") .. "\008w" .. (type(cap3) == "string" and cap3 or "")
	end
	local function do_highlighting_gsub(str, smatch)
		return str:gsub("^()(" .. smatch .. ")()$", gsubcapret):gsub("([^_%w])(" .. smatch .. ")()$", gsubcapret):gsub("^()(" .. smatch .. ")([^_%w])", gsubcapret):gsub("([^_%w])(" .. smatch .. ")([^_%w])", gsubcapret)
	end
	function do_highlighting(partialline_hl)
		for ix = 1, #highlighting do
			for key_, value in pairs(highlighting[ix]) do
				for thing, smatch in pairs(value) do
					key = key_
					partialline_hl = do_highlighting_gsub(partialline_hl, smatch)
				end
			end
		end
		return partialline_hl
	end
end

local function check_rect(a, b, c, d, e, f)
	return a <= e and b <= f and (a + c) > e and (b + d) > f
end

local function draw_outline(a, b, c, d, j, k, l, m, n, o)
	gfx.fillRect(a, b, c, d, j, k, l)
	c, d = a + c - 1, b + d - 1
	gfx.drawLine(a, b, a, d, m, n, o)
	gfx.drawLine(a, b, c, b, m, n, o)
	gfx.drawLine(c, b, c, d, m, n, o)
	gfx.drawLine(a, d, c, d, m, n, o)
end

local _tptrasmui
if tpt.rasmui then
	_tptrasmui = tpt.rasmui
else
	local class
	do
		local classmetatable = {__index = function(tbl, key)
			local result
			for ix = 1, #tbl.baseorder do
				result = tbl.baseorder[ix][key]
				if result then break end
			end
			return result
		end}
		local basemaskmetatable = {__index = function(tbl, key)
			local result = tbl.base[key]
			return type(result) == "function" and function(mask, ...)
				return result(tbl.target, ...)
			end or result
		end}
		local function instantiate(self, ...)
			local result = setmetatable({__basemasks = {}}, self.instancemetatable)
			local constructor = result.construct
			if constructor then
				local ok, err = pcall(constructor, result, ...)
				if not ok then
					print(err)
					return nil, err
				end
			end
			return result
		end
		function class(...)
			local bases = {...}
			local result = setmetatable({bases = {}, baseorder = {}}, classmetatable)
			for ix = 1, #bases do
				local tbase = bases[ix]
				if getmetatable(tbase) ~= classmetatable then
					error("base #" .. ix .. ": invalid class", 2)
				end
				for key in pairs(tbase.bases) do result.bases[key] = true end
				result.bases[tbase] = true
				result.baseorder[#result.baseorder + 1] = tbase
			end
			result.new = instantiate
			result.instancemetatable = {__index = function(tbl, key)
				return result[key]
			end, __call = function(tbl, base)
				if getmetatable(base) ~= classmetatable then
					error("attempt to cast to invalid class", 2)
				end
				if not result.bases[base] then
					error("attempt to cast to unrelated type", 2)
				end
				return setmetatable({target = tbl, base = base}, basemaskmetatable)
			end}
			return result
		end
	end

	local rootNode
	local elem_local = {}

	local function passKeyboardEvent(keystring, keynumber, modifier, event)
		if not tpt.rasmui then return end
		local focus_ = _tptrasmui.focus
		local killevent
		if focus_ and focus_.visible and focus_.enabled then
			killevent = focus_:keypress_focus(false, keystring, keynumber, modifier, event) == false
		end
		rootNode:keypress(keystring, keynumber, modifier, event)
		if killevent then
			return false -- kill event
		end
	end
	
	local khrepeat_keystring, khrepeat_keynumber, khrepeat_modifier
	local khrepeat_delaycnt, khrepeat_intervalcnt
	tpt.rasmui = {
		lastmodifierstate = 0,
		focus = nil,
		hover = nil,
		hook_step = function()
			if tpt.boost then
				tpt.boost.lock(_tptrasmui.focus and true)
			end
			
			_tptrasmui.hover = rootNode:get_hover(tpt.mousex, tpt.mousey)
			rootNode:step()
			rootNode:draw()
			if not next(rootNode.children) then
				rootNode:destruct()
			end
			if tpt.rasmui and _tptrasmui.hover.been_hover_since_ > tooltip_timeout and
			_tptrasmui.hover.tooltip then
				local tooltipText = _tptrasmui.hover.tooltip
				local dtw, dth = gfx.textSize(tooltipText)
				local x_, y_ = tpt.mousex, tpt.mousey
				if x_ + dtw + 7 > gfx_width_ then x_ = x_ - dtw - 7 end
				if y_ - dth - 6 < 0 then y_ = y_ + dth + 6 end
				draw_outline(x_, y_ - dth - 6, dtw + 7, dth + 6, 50, 50, 50, 255, 255, 255)
				gfx.drawText(x_ + 4, y_ - dth - 2, tooltipText)
			end
			if khrepeat_delaycnt then
				if khrepeat_delaycnt == keyboard_delay then
					if khrepeat_intervalcnt == 0 then
						passKeyboardEvent(khrepeat_keystring, khrepeat_keynumber, khrepeat_modifier, 3)
					end
					khrepeat_intervalcnt = (khrepeat_intervalcnt + 1) % keyboard_interval
				else
					khrepeat_delaycnt = khrepeat_delaycnt + 1
				end
			end
			if tpt.rasmui and _tptrasmui.do_single_step then
				tpt.set_pause(1)
				_tptrasmui.do_single_step = nil
			end
		end,
		hook_mouseclick = function(mousex, mousey, ...)
			local hover_ = rootNode:get_hover(mousex, mousey)
			_tptrasmui.hover = hover_
			if hover_ and hover_.visible and hover_.enabled then
				hover_:mouseclick_hover(false, mousex, mousey, ...)
			end
			rootNode:mouseclick(mousex, mousey, ...)
			if hover_ and hover_ ~= _tptrasmui.rootNode then
				return false -- kill event
			end
		end,
		hook_keypress = function(keystring, keynumber, modifier, event)
			_tptrasmui.lastmodifierstate = modifier
			local shift_ = bit.band(modifier, 3) ~= 0
			local modifiergroup = shift_ and keyboard_layout.shift or keyboard_layout.default
			if bit.band(modifier, 64) ~= 0 and bit.band(modifier, 768) ~= 0 then
				modifiergroup = keyboard_layout.altgr
			end
			local keynumber_ = modifiergroup[keynumber]
			if keynumber_ then
				keynumber = keynumber_
				keystring = keynumber < 256 and string.char(keynumber) or ""
			end
			if bit.band(modifier, 8192) ~= 0 then
				keystring = shift_ and keystring:lower() or keystring:upper()
			else
				keystring = shift_ and keystring:upper() or keystring:lower()
			end
			if keystring == "\r" then keystring = "\n" end
			if event == 1 then
				khrepeat_keystring, khrepeat_keynumber, khrepeat_modifier = keystring, keynumber, modifier
				khrepeat_delaycnt, khrepeat_intervalcnt = 0, 0
			elseif event == 2 then
				khrepeat_delaycnt = nil
			end
			if keynumber > 256 then keystring = "" end
			return passKeyboardEvent(keystring, keynumber, modifier, event)
		end,
		elem = elem_local
	}
	_tptrasmui = tpt.rasmui
	
	elem_local.Common = class()
	function elem_local.Common:construct(data)
		data = data or {}
		self.children = self.children or {}
		self.tooltip = data.tooltip
		self.userdata = data.userdata
		self.is_hover_ = false
		self.posX = data.posX or 0
		self.posY = data.posY or 0
		self.width = data.width or 0
		self.height = data.height or 0
		self.layoutfunc = data.layoutfunc
		self.visible = true
		if data.visible ~= nil then self.visible = data.visible end
		self.enabled = true
		if data.enabled ~= nil then self.enabled = data.enabled end
		if data.parent then
			self:set_parent(data.parent)
		end
		self.been_hover_since_ = 0
		self.in_focus_ = false
		self.in_focus_tree_ = false
		if data.focus ~= nil then self:set_focus(data.focus) end
		self:layout_change()
	end
	function elem_local.Common:hover_change()
		self.been_hover_since_ = 0
	end
	function elem_local.Common:step()
		self.been_hover_since_ = self.been_hover_since_ + 1
		local current_is_hover_ = _tptrasmui.hover == self
		if self.is_hover_ ~= current_is_hover_ then
			self.is_hover_ = current_is_hover_
			self:hover_change()
		end
		for key, value in safe_pairs(self.children) do
			value:step()
		end
	end
	function elem_local.Common:draw()
		if not self.visible then return end
		for key, value in safe_ipairs(self.children) do
			value:draw()
		end
	end
	function elem_local.Common:get_hover(mousex, mousey)
		if not self.visible or not check_rect(self.dpx_, self.dpy_, self.width, self.height, mousex, mousey) then
			return -- nothing under the mouse
		end
		for key, value in safe_ipairs(self.children, true) do
			local result = value:get_hover(mousex, mousey) -- child under the mouse
			if result then return result end
		end
		return self -- self under the mouse
	end
	function elem_local.Common:mouseclick(...)
		for key, value in safe_pairs(self.children) do
			value:mouseclick(...)
		end
	end
	function elem_local.Common:mouseclick_hover(bubblingUp, ...)
		if self.parent then
			self.parent:mouseclick_hover(true, ...)
		end
	end
	function elem_local.Common:keypress(...)
		for key, value in safe_pairs(self.children) do
			value:keypress(...)
		end
	end
	function elem_local.Common:keypress_focus(bubblingUp, ...)
		-- print(tostring(self) .. " " .. (self.text or ""))
		if self.parent then
			self.parent:keypress_focus(true, ...)
		end
	end
	function elem_local.Common:bring_to_front()
		if not self.parent then return end
		local killthis
		for key, value in pairs(self.parent.children) do
			if value == self then
				killthis = key
				break
			end
		end
		table.remove(self.parent.children, killthis)
		table.insert(self.parent.children, self)
	end
	function elem_local.Common:set_focus(inFocus)
		if inFocus then
			if _tptrasmui.focus then
				_tptrasmui.focus:set_focus(false)
			end
			_tptrasmui.focus = self
			self.in_focus_ = true
			self:set_in_focus_tree_(false, true)
		else
			_tptrasmui.focus = nil
			self.in_focus_ = false
			self:set_in_focus_tree_(false, false)
		end
	end
	function elem_local.Common:set_in_focus_tree_(bubblingUp, inFocusTree)
		self.in_focus_tree_ = inFocusTree
		if inFocusTree then
			self:bring_to_front()
		end
		if self.parent then
			self.parent:set_in_focus_tree_(true, inFocusTree)
		end
	end
	function elem_local.Common:layout_change()
		if not self.parent then return end
		if self.layoutfunc then
			self.layoutfunc(self)
		end
		self.dpx_ = self.parent.dpx_ + self.posX
		self.dpy_ = self.parent.dpy_ + self.posY
		self.dqx_ = self.dpx_ + self.width - 1
		self.dqy_ = self.dpy_ + self.height - 1
		for key, value in safe_pairs(self.children) do
			value:layout_change()
		end
	end
	function elem_local.Common:set_parent(parent)
		if self.parent then
			local killthis
			for key, value in pairs(self.parent.children) do
				if value == self then
					killthis = key
					break
				end
			end
			if killthis then
				table.remove(self.parent.children, killthis)
			end
		end
		self.parent = parent
		if self.parent then
			table.insert(self.parent.children, self)
		end
	end
	function elem_local.Common:destruct_recursive()
		for key, value in safe_pairs(self.children) do
			value:destruct_recursive()
		end
		self:destruct()
	end
	function elem_local.Common:destruct()
		self:set_parent(nil)
		for key, value in safe_pairs(self.children) do
			value:set_parent(nil)
		end
		if _tptrasmui.focus == self then
			_tptrasmui.focus = nil
		end
	end
	
	elem_local.StaticText = class(elem_local.Common)
	function elem_local.StaticText:construct(data)
		data = data or {}
		self(elem_local.Common):construct(data)
		self.text = data.text or ""
		self.textoffsetY = data.textoffsetY or 0
	end
	function elem_local.StaticText:draw()
		if not self.visible then return end
		local dtw_, dth_ = gfx.textSize(self.text)
		local dty_ = self.dpy_ + _floor((self.height - dth_) / 2) + 2 + self.textoffsetY
		if self.textColor then
			gfx.drawText(self.dpx_, self.dpy_, self.text,
				255 * self.textColor[1], 255 * self.textColor[2], 255 * self.textColor[3])
		else
			gfx.drawText(self.dpx_, dty_, self.text)
		end
		self(elem_local.Common):draw()
	end
	
	elem_local.Clickable = class(elem_local.Common)
	function elem_local.Clickable:construct(data)
		self(elem_local.Common):construct(data)
		self.lastclick = 0
	end
	function elem_local.Clickable:step()
		self(elem_local.Common):step()
		self.lastclick = self.lastclick + 1
	end
	function elem_local.Clickable:click()
		if self.lastclick < 15 then
			self:doubleclick()
		end
		self.lastclick = 0
	end
	function elem_local.Clickable:doubleclick()
	end
	function elem_local.Clickable:mouseclick(x_, y_, button, event, wheel)
		if button == 1 and event == 2 then
			self.lmousedown_ = false
		end
		self(elem_local.Common):mouseclick(x_, y_, button, event, wheel)
	end
	function elem_local.Clickable:mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
		if not bubblingUp then
			if event == 1 then
				self:set_focus(true)
			end
			if button == 1 then
				if event == 1 then
					self.lmousedown_ = true
				elseif event == 2 and self.lmousedown_ then
					self:click()
				end
			end
		end
		self(elem_local.Common):mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
	end
	
	elem_local.Draggable = class(elem_local.Clickable)
	function elem_local.Draggable:drag(diffX, diffY)
	end
	function elem_local.Draggable:mouseclick(x_, y_, button, event, wheel)
		self(elem_local.Clickable):mouseclick(x_, y_, button, event, wheel)
		if self.lmousedown_ and button == 1 and event == 3 then
			local compDiffX, compDiffY = self:drag(x_ - self.dragmoveX_, y_ - self.dragmoveY_)
			self.dragmoveX_ = self.dragmoveX_ + compDiffX
			self.dragmoveY_ = self.dragmoveY_ + compDiffY
		end
	end
	function elem_local.Draggable:mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
		self(elem_local.Clickable):mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
		if not bubblingUp then
			if button == 1 and event == 1 then
				self.dragmoveX_ = x_
				self.dragmoveY_ = y_
			end
		end
	end
	
	elem_local.Root = class(elem_local.Clickable)
	function elem_local.Root:construct(data)
		-- print("RasmUI initialized")
		self(elem_local.Clickable):construct(data)
	end
	function elem_local.Root:destruct()
		self(elem_local.Common):destruct()
		tpt.unregister_step(_tptrasmui.hook_step)
		tpt.unregister_mouseclick(_tptrasmui.hook_mouseclick)
		tpt.unregister_keypress(_tptrasmui.hook_keypress)
		tpt.rasmui = nil
		_tptrasmui = nil
		-- print("RasmUI uninitialized")
	end
	function elem_local.Root:step()
		if self.stepbutton then
			tpt.set_pause(1)
			self.stepbutton = nil
		end
		self(elem_local.Common):step()
	end
	function elem_local.Root:layout_change()
		self.dpx_ = self.posX
		self.dpy_ = self.posY
		for key, value in safe_pairs(self.children) do
			value:layout_change()
		end
	end
	
	elem_local.Button = class(elem_local.Clickable)
	function elem_local.Button:construct(data)
		data = data or {}
		self.stuck = false
		if data.stuck ~= nil then self.stuck = data.stuck end
		self.textoffsetX = data.textoffsetX or 0
		self.textoffsetY = data.textoffsetY or 0
		self.textColor = data.textColor
		self.text = data.text or ""
		self.clickfunc = data.clickfunc
		self(elem_local.Clickable):construct(data)
	end
	function elem_local.Button:click()
		if self.clickfunc then
			self.clickfunc(self)
		end
	end
	function elem_local.Button:draw()
		if not self.visible then return end
		local enabled_ = self.enabled
		local dtw_, dth_ = gfx.textSize(self.text)
		local dtx_ = self.dpx_ + _floor((self.width - dtw_) / 2) + self.textoffsetX
		local dty_ = self.dpy_ + _floor((self.height - dth_) / 2) + 2 + self.textoffsetY
		local selfhover_ = _tptrasmui.hover == self
		local bg_ = enabled_ and (selfhover_ and (self.lmousedown_ and 255 or 20) or 0) or 10
		local ol_ = enabled_ and (selfhover_ and (self.lmousedown_ and 235 or 255) or 200) or 100
		local tc_ = enabled_ and ((selfhover_ and self.lmousedown_) and 0 or 255) or 100
		if self.stuck then bg_, ol_, tc_ = 255, 235, 0 end
		draw_outline(self.dpx_, self.dpy_, self.width, self.height, bg_, bg_, bg_, ol_, ol_, ol_)
		if self.textColor then
			gfx.drawText(dtx_, dty_, self.text,
				tc_ * self.textColor[1], tc_ * self.textColor[2], tc_ * self.textColor[3])
		else
			gfx.drawText(dtx_, dty_, self.text, tc_, tc_, tc_)
		end
		self(elem_local.Common):draw()
	end
	
	elem_local.WindowResizer = class(elem_local.Draggable)
	function elem_local.WindowResizer:construct(data)
		self(elem_local.Clickable):construct(data)
		self.width = 15
		self.height = 15
		self.posY = 2
		self:layout_change()
	end
	function elem_local.WindowResizer:draw()
		if not self.visible then return end
		for ix = 2, 11, 3 do
			gfx.drawLine(self.dpx_ + 15 - ix, self.dpy_ + 1, self.dpx_ + 13, self.dpy_ + ix - 1)
		end
		self(elem_local.Common):draw()
	end
	function elem_local.WindowResizer:drag(diffX, diffY)
		local width_, height_ = self.parent.width, self.parent.height
		self.parent.width = _min(_min(_max(self.parent.width + diffX, self.parent.min_width),
			self.parent.max_width), self.parent.parent.width - self.parent.posX)
		self.parent.height = _min(_min(_max(self.parent.height - diffY, self.parent.min_height),
			self.parent.max_height), self.parent.height + self.parent.posY)
		local compDiffX, compDiffY = (self.parent.width - width_), (height_ - self.parent.height)
		self.parent.posY = self.parent.posY + compDiffY
		self.parent:layout_change()
		return compDiffX, compDiffY
	end
	function elem_local.WindowResizer:layout_change()
		self.posX = self.parent.width - 17
		self(elem_local.Draggable):layout_change()
	end
	
	elem_local.CtrlReceiver = class(elem_local.Common)
	function elem_local.CtrlReceiver:construct(data)
		data = data or {}
		self.ctrl_receivers = {}
		for key in pairs(data.ctrl_receivers or {}) do
			self.ctrl_receivers[key] = true
		end
		self(elem_local.Common):construct(data)
	end
	function elem_local.CtrlReceiver:keypress_focus(bubblingUp, keystring, keynumber, modifier, event)
		local killevent
		if event == 1 and bit.band(modifier, 192) ~= 0 then
			for receiver in pairs(self.ctrl_receivers) do
				killevent = (receiver(self, bubblingUp, keynumber, modifier) == false) or killevent
			end
		end
		self(elem_local.Common):keypress_focus(bubblingUp, keystring, keynumber, modifier, event)
		if killevent then
			return false
		end
	end
	
	elem_local.Window = class(elem_local.Draggable, elem_local.CtrlReceiver)
	function elem_local.Window:construct(data)
		data = data or {}
		self:set_text(data.text or "")
		self.min_width = data.min_width or 100
		self.min_height = data.min_height or 19
		self.max_width = data.max_width or math.huge
		self.max_height = data.max_height or math.huge
		local resizable = true
		if data.resizable ~= nil then
			resizable = data.resizable
		end
		self(elem_local.Clickable):construct(data)
		self.closebutton = elem_local.Button:new({
			parent = self,
			posX = 2, posY = 2, width = 15, height = 15,
			text = "\238\128\170", textoffsetY = -1,
			tooltip = "Close",
			clickfunc = function()
				self:destruct_recursive()
			end
		})
		self.maximizebox = elem_local.Button:new({
			parent = self,
			posX = 18, posY = 2, width = 15, height = 15,
			textoffsetY = -1,
			clickfunc = function(caller)
				caller.parent:set_maximized(not caller.parent.maximized)
			end
		})
		self.windowResizer = elem_local.WindowResizer:new({
			parent = self
		})
		self:set_resizable(resizable)
		self:set_maximized(false)
		if data.maximized ~= nil then
			self:set_maximized(data.maximized)
		end
		self(elem_local.CtrlReceiver):construct(data)
	end
	function elem_local.Window:keypress_focus(...)
		return self(elem_local.CtrlReceiver):keypress_focus(...)
	end
	function elem_local.Window:set_maximized(maximized)
		if not self.maximized then
			self.nonmaximizedstate = {
				posX = self.posX,
				posY = self.posY,
				width = self.width,
				height = self.height
			}
		end
		self.maximized = maximized
		if not self.maximized then
			self.posX = self.nonmaximizedstate.posX
			self.posY = self.nonmaximizedstate.posY
			self.width = self.nonmaximizedstate.width
			self.height = self.nonmaximizedstate.height
		end
		self.maximizebox.text = self.maximized and "\238\128\162" or "\238\128\135"
		self.maximizebox.tooltip = self.maximized and "Restore" or "Maximize"
		self.windowResizer.visible = self.resizable and not self.maximized
		self:layout_change()
	end
	function elem_local.Window:set_resizable(resizable)
		self.resizable = resizable
		self.windowResizer.visible = self.resizable and not self.maximized
	end
	function elem_local.Window:doubleclick()
		self:set_maximized(not self.maximized)
	end
	function elem_local.Window:drag(diffX, diffY)
		local _posx, _posy = self.posX, self.posY
		self.posX = _min(_max(self.posX + diffX, 0), self.parent.width - self.width)
		self.posY = _min(_max(self.posY + diffY, 0), self.parent.height - self.height)
		self:layout_change()
		return self.posX - _posx, self.posY - _posy
	end
	function elem_local.Window:set_text(text)
		self.text = text
		self.dtw_, self.dth_ = gfx.textSize(text)
	end
	function elem_local.Window:layout_change()
		if self.maximized then
			self.posX = 0
			self.posY = 0
			self.width = self.parent.width
			self.height = self.parent.height
		end
		self(elem_local.Common):layout_change()
	end
	function elem_local.Window:draw()
		if not self.visible then return end
		local ol_ = self.enabled and (self.in_focus_tree_ and 255 or 200) or 100
		draw_outline(self.dpx_, self.dpy_, self.width, self.height, 0, 0, 0, ol_, ol_, ol_)
		gfx.drawText(self.dpx_ + 38, self.dpy_ + 6, self.text)
		self(elem_local.Common):draw()
	end
	
	elem_local.ScrollbarThumb = class(elem_local.Draggable)
	function elem_local.ScrollbarThumb:construct(data)
		self.posX = 0
		self.posY = 0
		self(elem_local.Clickable):construct(data)
	end
	function elem_local.ScrollbarThumb:drag(diffX, diffY)
		local posy_ = self.posY
		self.posY = _min(_max(self.posY + diffY, 0), self.parent.height - self.height)
		self.parent.scrollpos = self.posY / (self.parent.height - self.height)
		self.parent.parent:set_scrollpos(self.parent.scrollpos)
		self:layout_change()
		return 0, self.posY - posy_
	end
	function elem_local.ScrollbarThumb:layout_change()
		self.width = self.parent.width
		self.height = _max(_floor(self.parent.height * self.parent.parent.viewport_ratio), 10)
		self.posY = (self.parent.height - self.height) * self.parent.scrollpos
		self(elem_local.Common):layout_change()
	end
	function elem_local.ScrollbarThumb:set_focus(inFocus)
		if inFocus then
			self.parent.parent:set_focus(true)
			return
		end
		self(elem_local.Common):step(false)
	end
	function elem_local.ScrollbarThumb:draw()
		gfx.fillRect(self.dpx_, self.dpy_, self.width, self.height, 255, 255, 255)
		self(elem_local.Common):draw()
	end
	
	elem_local.Scrollbar = class(elem_local.Clickable)
	function elem_local.Scrollbar:construct(data)
		self(elem_local.Clickable):construct(data)
		self.posY = 1
		self.width = 0
		self.scrollpos = 0
		self.thumb = elem_local.ScrollbarThumb:new({
			parent = self
		})
		self:layout_change()
	end
	function elem_local.Scrollbar:layout_change()
		self.posX = self.parent.width - self.width - 1
		self.height = self.parent.height - 2
		self(elem_local.Common):layout_change()
	end
	function elem_local.Scrollbar:set_scrollpos(scrollpos)
		self.scrollpos = scrollpos
		self:layout_change()
	end
	function elem_local.Scrollbar:step()
		local hover_ = _tptrasmui.hover
		local should_be_visible_ = self.parent.viewport_ratio < 1 and (hover_ == self.parent or hover_ == self or hover_ == self.thumb or self.lmousedown_ or self.thumb.lmousedown_)
		if should_be_visible_ and self.width < 6 then
			self.width = self.width + 1
			self:layout_change()
		end
		if not should_be_visible_ and self.width > 0 then
			self.width = self.width - 1
			self:layout_change()
		end
		if self.lmousedown_ then
			if self.thumb.dpy_ > tpt.mousey then
				self.thumb:drag(0, -2)
			end
			if (self.thumb.dpy_ + self.thumb.height - 1) < tpt.mousey then
				self.thumb:drag(0, 2)
			end
		end
		self(elem_local.Clickable):step()
	end
	function elem_local.Scrollbar:set_focus(inFocus)
		if inFocus then
			self.parent:set_focus(true)
			return
		end
		self(elem_local.Common):step(false)
	end
	function elem_local.Scrollbar:draw()
		if self.width == 0 or not self.visible then return end
		gfx.fillRect(self.dpx_, self.dpy_, self.width, self.height, 255, 255, 255, 48)
		self(elem_local.Common):draw()
	end
	
	elem_local.ScrollableList = class(elem_local.Clickable)
	function elem_local.ScrollableList:construct(data)
		data = data or {}
		self.borderR = data.borderR or 4
		self.borderL = data.borderL or 4
		self.borderT = data.borderT or 3
		self.borderB = data.borderB or 3
		self.top_line = 1
		self.scrollpos = 0
		self.lines = {""}
		self.number_of_lines = 1
		self(elem_local.Clickable):construct(data)
		self.scrollbar = elem_local.Scrollbar:new({
			parent = self
		})
		self:set_top_line(1)
	end
	function elem_local.ScrollableList:set_height(height)
		self.height_in_lines = height
	end
	function elem_local.ScrollableList:set_top_line(top_line)
		if self.number_of_lines <= self.height_in_lines then
			self.top_line = 1
			self.scrollpos = 0
		else
			self.top_line = _min(_max(top_line, 1), self.number_of_lines - self.height_in_lines + 1)
			self.scrollpos = (self.top_line - 1) / (self.number_of_lines - self.height_in_lines)
		end
		if self.scrollbar then
			self.scrollbar:set_scrollpos(self.scrollpos)
		end
	end
	function elem_local.ScrollableList:update_viewport_ratio()
		self.viewport_ratio = _min(self.height_in_lines / self.number_of_lines, 1)
		self:set_top_line(self.top_line)
	end
	function elem_local.ScrollableList:layout_change()
		self(elem_local.Clickable):layout_change()
		local teststring = ""
		while true do
			local thing, height = gfx.textSize(teststring)
			if height > self.height - self.borderT - self.borderB then
				break
			end
			teststring = teststring .. "\n"
		end
		self:set_height(#teststring)
		self:update_viewport_ratio()
		if not self.scrollbar then return end
		if self.top_line + self.height_in_lines - 1 > self.number_of_lines then
			self.top_line = self.number_of_lines - self.height_in_lines + 1
		end
		self:set_top_line(self.top_line)
	end
	function elem_local.ScrollableList:set_scrollpos(scrollpos)
		self.scrollpos = scrollpos
		self.top_line = _floor((self.number_of_lines - self.height_in_lines) * self.scrollpos + 0.5) + 1
	end
	function elem_local.ScrollableList:mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
		self(elem_local.Clickable):mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
		if wheel ~= 0 then
			self:set_top_line(self.top_line - wheel * 3)
		end
	end
	
	elem_local.TextEditor = class(elem_local.ScrollableList, elem_local.CtrlReceiver)
	function elem_local.TextEditor:construct(data)
		data = data or {}
		self.borderInnerL = 22
		self.textchangefunc = data.textchangefunc
		self(elem_local.ScrollableList):construct(data)
		self.cursor_line = 1
		self.cursor_char = 0
		self:sanitize_cursor(true)
		self:set_text(data.text or "")
		self.overwrite = false
		self(elem_local.CtrlReceiver):construct(data)
		self.ctrl_receivers[self.ctrl_selection] = true
	end
	function elem_local.TextEditor:textchange()
		if self.textchangefunc then
			self:textchangefunc()
		end
	end
	function elem_local.TextEditor:mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
		self(elem_local.ScrollableList):mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
		if not bubblingUp then
			if button == 1 then
				if event == 1 then
					self.cursor_line, self.cursor_char = self:get_cursor_by_mouse(x_, y_)
					self.selection_start_line = self.cursor_line
					self.selection_start_char = self.cursor_char
					self:sanitize_selection()
				end
			end
		end
	end
	function elem_local.TextEditor:mouseclick(x_, y_, button, event, wheel)
		self(elem_local.Clickable):mouseclick(x_, y_, button, event, wheel)
		if not bubblingUp then
			if button == 1 then
				if self.lmousedown_ and (event == 2 or event == 3) then
					self.cursor_line, self.cursor_char = self:get_cursor_by_mouse(x_, y_)
					self:follow_cursor()
					self.selection_end_line = self.cursor_line
					self.selection_end_char = self.cursor_char
					self:sanitize_selection()
				end
			end
		end
	end
	function elem_local.TextEditor:sanitize_cursor(fixselstart)
		self.cursor_line = _min(_max(self.cursor_line, 1), self.number_of_lines)
		self.cursor_char = _min(_max(self.cursor_char, 0), #self.lines[self.cursor_line])
		self:follow_cursor()
		if fixselstart then
			self.selection_start_line = self.cursor_line
			self.selection_start_char = self.cursor_char
		end
		self.selection_end_line = self.cursor_line
		self.selection_end_char = self.cursor_char
		self:sanitize_selection()
	end
	function elem_local.TextEditor:sanitize_selection()
		if self.selection_start_line > self.selection_end_line or (self.selection_start_line == self.selection_end_line and self.selection_start_char > self.selection_end_char) then
			self.selection_first_line = self.selection_end_line
			self.selection_first_char = self.selection_end_char
			self.selection_last_line = self.selection_start_line
			self.selection_last_char = self.selection_start_char
			return
		end
		self.selection_first_line = self.selection_start_line
		self.selection_first_char = self.selection_start_char
		self.selection_last_line = self.selection_end_line
		self.selection_last_char = self.selection_end_char
	end
	function elem_local.TextEditor:get_cursor_by_mouse(x_, y_)
		local cursor_line = _min(_max(_floor((y_ - self.dpy_ - self.borderT) / 12) +
			self.top_line, 1), self.number_of_lines)
		x_ = x_ - self.dpx_ - self.borderInnerL
		local clinestr = self.lines[cursor_line]
		local clinestrlen = #clinestr
		local cursor_char = 0
		while true do
			if cursor_char == clinestrlen then
				break
			end
			if x_ < gfx.textSize(clinestr:sub(1, cursor_char + 1):gsub("\t", tab_to_space_)) - 1 then
				break
			end
			cursor_char = cursor_char + 1
		end
		return cursor_line, cursor_char
	end
	function elem_local.TextEditor:follow_cursor()
		local topline_ = self.top_line
		if self.cursor_line < self.top_line then
			self:set_top_line(self.cursor_line)
		end
		if self.cursor_line > (self.top_line + self.height_in_lines - 1) then
			self:set_top_line(self.cursor_line + 1 - self.height_in_lines)
		end
	end
	function elem_local.TextEditor:get_charline_pos(pline, pchar)
		local lpos = 0
		for ix = 1, pline - 1 do
			lpos = lpos + #self.lines[ix] + 1
		end
		return lpos + pchar
	end
	function elem_local.TextEditor:get_cursor()
		return self:get_charline_pos(self.cursor_line, self.cursor_char)
	end
	function elem_local.TextEditor:get_selection_first()
		return self:get_charline_pos(self.selection_first_line, self.selection_first_char)
	end
	function elem_local.TextEditor:get_selection_last()
		return self:get_charline_pos(self.selection_last_line, self.selection_last_char)
	end
	function elem_local.TextEditor:cpos_to_charline(cpos)
		local pchar
		cpos = _max(cpos, 0)
		local ix = 1
		while true do
			if ix > self.number_of_lines then
				ix = ix - 1
				pchar = #self.lines[ix]
				break
			end
			local linelen = #self.lines[ix] + 1
			if cpos < linelen then
				pchar = cpos
				break
			end
			ix = ix + 1
			cpos = cpos - linelen
		end
		return ix, pchar
	end
	function elem_local.TextEditor:set_cursor(cpos, dontfixcstart)
		self.cursor_line, self.cursor_char = self:cpos_to_charline(cpos)
		self:sanitize_cursor(not dontfixcstart)
	end
	function elem_local.TextEditor:set_selection_first(cpos)
		self.selection_first_line, self.selection_first_char = self:cpos_to_charline(cpos)
	end
	function elem_local.TextEditor:set_selection_last(cpos)
		self.selection_last_line, self.selection_last_char = self:cpos_to_charline(cpos)
	end
	function elem_local.TextEditor:ctrl_selection(bubblingUp, keynumber)
		if not bubblingUp then
			if keynumber == 97 then
				self:set_selection_first(0)
				self:set_selection_last(#self:get_text())
			elseif keynumber == 99 or keynumber == 120 or keynumber == 118 then
				local first_, last_, cursor_ = self:get_selection_first(), self:get_selection_last(), self:get_cursor()
				local text_ = self:get_text()
				if keynumber == 99 or keynumber == 120 then
					tpt.set_clipboard(text_:sub(first_ + 1, last_))
				end
				if keynumber == 120 or keynumber == 118 then
					local cliptext = keynumber == 118 and tpt.get_clipboard():gsub("\r\n?", "\n") or ""
					self:set_text(text_:sub(1, first_) .. cliptext .. text_:sub(last_ + 1))
					self:set_cursor(first_ + #cliptext)
				end
			end
		end
	end
	function elem_local.TextEditor:keypress_focus(bubblingUp, keystring, keynumber, modifier, event)
		-- print(keystring, keynumber, modifier, event)
		if self(elem_local.CtrlReceiver):keypress_focus(bubblingUp,
		keystring, keynumber, modifier, event) == false then
			return false
		end
		if not bubblingUp then
			local cursormoved
			local shift_ = bit.band(modifier, 3) ~= 0
			if (event == 1 or event == 3) and not (bit.band(modifier, 192) ~= 0 and bit.band(modifier, 768) == 0) then
				if keynumber == 273 then
					self.cursor_line = _max(self.cursor_line - 1, 1)
					cursormoved = true
				elseif keynumber == 274 then
					self.cursor_line = _min(self.cursor_line + 1, self.number_of_lines)
					cursormoved = true
				elseif keynumber == 275 then
					self:set_cursor(self:get_cursor() + 1, shift_)
				elseif keynumber == 276 then
					self:set_cursor(self:get_cursor() - 1, shift_)
				elseif keynumber == 278 then
					self.cursor_char = 0
					cursormoved = true
				elseif keynumber == 279 then
					self.cursor_char = #self.lines[self.cursor_line]
					cursormoved = true
				elseif keynumber == 280 then
					self.cursor_line = _max(self.cursor_line - self.height_in_lines, 1)
					cursormoved = true
				elseif keynumber == 281 then
					self.cursor_line = _min(self.cursor_line + self.height_in_lines, self.number_of_lines)
					cursormoved = true
				elseif keynumber == 277 then
					self.overwrite = not self.overwrite
				elseif keynumber == 127 then
					local first_, last_, cursor_ = self:get_selection_first(), self:get_selection_last(), self:get_cursor()
					local text_ = self:get_text()
					if first_ == last_ then
						self:set_text(text_:sub(1, cursor_) .. text_:sub(cursor_ + 2))
					else
						self:set_text(text_:sub(1, first_) .. text_:sub(last_ + 1))
						self:set_cursor(first_)
					end
				elseif keynumber == 8 then
					local first_, last_, cursor_ = self:get_selection_first(), self:get_selection_last(), self:get_cursor()
					local text_ = self:get_text()
					if first_ == last_ then
						if cursor_ ~= 0 then
							self:set_text(text_:sub(1, cursor_ - 1) .. text_:sub(cursor_ + 1))
							self:set_cursor(cursor_ - 1)
						end
					else
						self:set_text(text_:sub(1, first_) .. text_:sub(last_ + 1))
						self:set_cursor(first_)
					end
				elseif keystring ~= "" then
					local first_, last_, cursor_ = self:get_selection_first(), self:get_selection_last(), self:get_cursor()
					local text_ = self:get_text()
					if first_ == last_ then
						self:set_text(text_:sub(1, cursor_) .. keystring .. text_:sub(cursor_ + (self.overwrite and 2 or 1)))
						self:set_cursor(cursor_ + 1)
					else
						self:set_text(text_:sub(1, first_) .. keystring .. text_:sub(last_ + 1))
						self:set_cursor(first_ + #keystring)
					end
				end
			end
			if cursormoved then
				self:sanitize_cursor(not shift_)
			end
			return false
		end
	end
	function elem_local.TextEditor:hack_text(beforepos, newstr, afterpos)
		local newstr = newstr or ""
		local text_ = self:get_text()
		--! TODO: well, this
		self:set_text(text_:sub(1, beforepos) .. (newstr or "") .. text_:sub(afterpos), true)
	end
	function elem_local.TextEditor:set_text(text, _keep_linemsgs)
		self.lines = {}
		if not _keep_linemsgs then
			self.linemsgs = {}
		end
		self.number_of_lines = 1
		for linestr in text:gmatch("([^\n]*)\n") do
			self.lines[self.number_of_lines] = linestr
			self.number_of_lines = self.number_of_lines + 1
		end
		self.lines[self.number_of_lines] = text:match("\n([^\n]*)$") or text
		self:update_viewport_ratio()
		self:sanitize_cursor(true)
		self:textchange()
	end
	function elem_local.TextEditor:get_text()
		return table.concat(self.lines, "\n")
	end
	function elem_local.TextEditor:step()
		self(elem_local.ScrollableList):step()
		self.borderInnerL = _max(22, 10 + #tostring(_min(_max(self.top_line + self.height_in_lines - 1, 0), self.number_of_lines)) * 6)
	end
	function elem_local.TextEditor:draw()
		if not self.visible then return end
		local ol_ = self.enabled and (self.in_focus_tree_ and 255 or 200) or 100
		draw_outline(self.dpx_, self.dpy_, self.width, self.height, 0, 0, 0, ol_, ol_, ol_)
		gfx.fillRect(self.dpx_ + 1, self.dpy_ + 1, self.borderInnerL - self.borderL, self.height - 2, 100, 100, 100)
		for ix = 0, _min(self.number_of_lines, self.height_in_lines) - 1 do
			local lineindex = ix + self.top_line
			local lineindexstr = tostring(lineindex)
			-- local linecolor = self.lines[lineindex]
			-- if linecolor then
				-- gfx.fillRect(self.dpx_ + 1, self.dpy_ + self.borderT + ix * 12 - 2, self.borderInnerL - self.borderL,
					-- 13, linecolor[1], linecolor[2], linecolor[3])
			-- end
			gfx.drawText(self.dpx_ + self.borderInnerL - gfx.textSize(lineindexstr) - 5,
				self.dpy_ + self.borderT + ix * 12 + 1, lineindexstr)
			local fullline = self.lines[lineindex] .. (lineindex == self.number_of_lines and "" or " ")
			local linemax, linelen = 0, #fullline
			while true do
				if linemax == linelen then
					break
				end
				if gfx.textSize(fullline:sub(1, linemax + 1):gsub("\t", tab_to_space_)) > self.width - self.borderInnerL - self.borderR then
					break
				end
				linemax = linemax + 1
			end
			local partialline = fullline:sub(1, linemax)
			local partialline_hl = do_highlighting(partialline)
			gfx.drawText(self.dpx_ + self.borderInnerL, self.dpy_ + self.borderT + ix * 12 + 1,
				(partialline_hl:gsub("\t", tab_to_space_)))
			if lineindex >= self.selection_first_line and lineindex <= self.selection_last_line then
				local firstchar = lineindex > self.selection_first_line and 0 or self.selection_first_char
				local lastchar = lineindex < self.selection_last_line and linemax or self.selection_last_char
				local firstoffset = gfx.textSize(partialline:sub(1, firstchar):gsub("\t", tab_to_space_))
				local lastoffset = gfx.textSize(partialline:sub(1, lastchar):gsub("\t", tab_to_space_))
				gfx.fillRect(self.dpx_ + self.borderInnerL + firstoffset,
					self.dpy_ + self.borderT + ix * 12, lastoffset - firstoffset, 10)
				gfx.drawText(self.dpx_ + self.borderInnerL + firstoffset, self.dpy_ + self.borderT + ix * 12 + 1,
					(partialline:sub(firstchar + 1, lastchar):gsub("\t", tab_to_space_)), 0, 0, 0)
			end
			if self.in_focus_ and lineindex == self.cursor_line and linemax >= self.cursor_char then
				local textoutsize = gfx.textSize(partialline:sub(1, self.cursor_char):gsub("\t", tab_to_space_))
				local rposx_, rposy_ = self.dpx_ + self.borderInnerL + textoutsize,
					self.dpy_ + self.borderT + (self.cursor_line - self.top_line) * 12
				if self.overwrite then
					gfx.fillRect(rposx_, rposy_ - 1, (gfx.textSize(string.sub(partialline .. " ", self.cursor_char + 1,
						self.cursor_char + 1):gsub("\t", tab_to_space_))), 12)
				else
					gfx.drawLine(rposx_, rposy_ - 1, rposx_, rposy_ + 10)
				end
			end
		end
		self(elem_local.Common):draw()
	end
	
	elem_local.ListBox = class(elem_local.ScrollableList, elem_local.CtrlReceiver)
	function elem_local.ListBox:construct(data)
		data = data or {}
		self.selectfunc = data.selectfunc
		self.selectdoublefunc = data.selectdoublefunc
		self(elem_local.ScrollableList):construct(data)
		self.selection = nil
	end
	function elem_local.ListBox:set_list(list)
		local oldselectionvalue = self.selection and self.lines[self.selection] or nil
		self.lines = {}
		for key, value in pairs(list) do
			self.lines[key] = value
		end
		self.number_of_lines = #list
		self:update_viewport_ratio()
		local newselection
		for key, value in pairs(self.lines) do
			if value == oldselectionvalue then
				newselection = key
				break
			end
		end
		self:select(newselection)
	end
	function elem_local.ListBox:get_lineindex_by_mouse(y_)
		return _min(_max(_floor((y_ - self.dpy_ - self.borderT) / 12) +
			self.top_line, 1), self.number_of_lines)
	end
	function elem_local.ListBox:mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
		self(elem_local.ScrollableList):mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
		if not bubblingUp then
			if button == 1 then
				if event == 1 then
					self:select(self:get_lineindex_by_mouse(y_))
				end
			end
		end
	end	
	function elem_local.ListBox:select(selection)
		self.selection = selection
		if self.selectfunc then
			self:selectfunc()
		end
	end
	function elem_local.ListBox:doubleclick()
		if self.selectdoublefunc then
			self:selectdoublefunc()
		end
	end
	function elem_local.ListBox:draw()
		if not self.visible then return end
		local ol_ = self.enabled and (self.in_focus_tree_ and 255 or 200) or 100
		draw_outline(self.dpx_, self.dpy_, self.width, self.height, 0, 0, 0, ol_, ol_, ol_)
		for ix = 0, _min(self.number_of_lines, self.height_in_lines) - 1 do
			local lineindex = ix + self.top_line
			local highlighted = (self.highlight and self.highlight[lineindex]) and 0 or 255
			local fullline = self.lines[lineindex]
			local linemax, linelen = 0, #fullline
			while true do
				if linemax == linelen then
					break
				end
				if gfx.textSize(fullline:sub(1, linemax + 1)) > self.width - self.borderL - self.borderR then
					break
				end
				linemax = linemax + 1
			end
			local partialline = fullline:sub(1, linemax)
			local sel_ = self.selection == lineindex and 0 or nil
			if sel_ then
				gfx.fillRect(self.dpx_ + 1, self.dpy_ + self.borderT + ix * 12,
					self.width - 2, 10)
			end
			gfx.drawText(self.dpx_ + self.borderL, self.dpy_ + self.borderT + ix * 12 + 1, partialline, sel_ or highlighted, sel_ or 255, sel_ or 255)
		end
		self(elem_local.Common):draw()
	end
	
	elem_local.TextBox = class(elem_local.Clickable, elem_local.CtrlReceiver)
	function elem_local.TextBox:construct(data)
		data = data or {}
		self.borderR = data.borderR or 4
		self.borderL = data.borderL or 4
		self.borderT = data.borderT or 3
		self.borderB = data.borderB or 3
		self.textColor = data.textColor
		self.textchangefunc = data.textchangefunc
		self.returnhitfunc = data.returnhitfunc
		self.escapehitfunc = data.escapehitfunc
		self(elem_local.Clickable):construct(data)
		self.scrollpos = 0
		self.cursor = 0
		self.text = ""
		self.overwrite = false
		self:sanitize_cursor(true)
		self:set_text(data.text or "")
		self(elem_local.CtrlReceiver):construct(data)
		self.ctrl_receivers[self.ctrl_selection] = true
		self:layout_change()
	end
	function elem_local.TextBox:layout_change()
		self(elem_local.Clickable):layout_change(data)
		if self.cursor then
			self:follow_cursor()
		end
	end
	function elem_local.TextBox:ctrl_selection(bubblingUp, keynumber)
		if not bubblingUp then
			if keynumber == 97 then
				self.selection_first = 0
				self.selection_last = #self.text
			elseif keynumber == 99 or keynumber == 120 or keynumber == 118 then
				local first_, last_, cursor_ = self.selection_first, self.selection_last, self.cursor
				local text_ = self.text
				if keynumber == 99 or keynumber == 120 then
					tpt.set_clipboard(text_:sub(first_ + 1, last_))
				end
				if keynumber == 120 or keynumber == 118 then
					local clipboardtext = tpt.get_clipboard():gsub("\r\n?", "\n")
					clipboardtext = clipboardtext:match("^([^\n]*)\n")
					local cliptext = keynumber == 118 and clipboardtext or ""
					self:set_text(text_:sub(1, first_) .. cliptext .. text_:sub(last_ + 1))
					self:set_cursor(first_ + #cliptext)
				end
			end
		end
	end
	function elem_local.TextBox:textchange()
		if self.textchangefunc then
			self:textchangefunc()
		end
	end
	function elem_local.TextBox:returnhit()
		if self.returnhitfunc then
			self:returnhitfunc()
		end
	end
	function elem_local.TextBox:escapehit()
		if self.escapehitfunc then
			self:escapehitfunc()
		end
	end
	function elem_local.TextBox:set_cursor(cpos, dontfixcstart)
		self.cursor = cpos
		self:sanitize_cursor(not dontfixcstart)
	end
	function elem_local.TextBox:set_text(text)
		self.text = text
		self:sanitize_cursor(true)
		self:textchange()
	end
	function elem_local.TextBox:keypress_focus(bubblingUp, keystring, keynumber, modifier, event)
		if self(elem_local.CtrlReceiver):keypress_focus(bubblingUp, keystring, keynumber, modifier, event) == false then
			return false
		end
		if not bubblingUp then
			local cursormoved
			local shift_ = bit.band(modifier, 3) ~= 0
			if (event == 1 or event == 3) and not (bit.band(modifier, 192) ~= 0 and bit.band(modifier, 768) == 0) then
				if keynumber == 275 then
					self:set_cursor(self.cursor + 1, shift_)
				elseif keynumber == 276 then
					self:set_cursor(self.cursor - 1, shift_)
				elseif keynumber == 278 then
					self.cursor = 0
					cursormoved = true
				elseif keynumber == 279 then
					self.cursor = #self.text
					cursormoved = true
				elseif keynumber == 27 then
					self:escapehit()
				elseif keynumber == 13 then
					self:returnhit()
				elseif keynumber == 277 then
					self.overwrite = not self.overwrite
				elseif keynumber == 127 then
					local first_, last_, cursor_ = self.selection_first, self.selection_last, self.cursor
					local text_ = self.text
					if first_ == last_ then
						self:set_text(text_:sub(1, cursor_) .. text_:sub(cursor_ + 2))
					else
						self:set_text(text_:sub(1, first_) .. text_:sub(last_ + 1))
						self:set_cursor(first_)
					end
				elseif keynumber == 8 then
					local first_, last_, cursor_ = self.selection_first, self.selection_last, self.cursor
					local text_ = self.text
					if first_ == last_ then
						if cursor_ ~= 0 then
							self:set_text(text_:sub(1, cursor_ - 1) .. text_:sub(cursor_ + 1))
							self:set_cursor(cursor_ - 1)
						end
					else
						self:set_text(text_:sub(1, first_) .. text_:sub(last_ + 1))
						self:set_cursor(first_)
					end
				elseif keystring ~= "" then
					local first_, last_, cursor_ = self.selection_first, self.selection_last, self.cursor
					local text_ = self.text
					if first_ == last_ then
						self:set_text(text_:sub(1, cursor_) .. keystring .. text_:sub(cursor_ + (self.overwrite and 2 or 1)))
						self:set_cursor(cursor_ + 1)
					else
						self:set_text(text_:sub(1, first_) .. keystring .. text_:sub(last_ + 1))
						self:set_cursor(first_ + #keystring)
					end
				end
			end
			if cursormoved then
				self:sanitize_cursor(not shift_)
			end
			return false
		end
	end
	function elem_local.TextBox:sanitize_cursor(fixselstart)
		self.cursor = _min(_max(self.cursor, 0), #self.text)
		self:follow_cursor()
		if fixselstart then
			self.selection_start = self.cursor
		end
		self.selection_end = self.cursor
		self:sanitize_selection()
	end
	function elem_local.TextBox:sanitize_selection()
		if self.selection_start > self.selection_end then
			self.selection_first = self.selection_end
			self.selection_last = self.selection_start
			return
		end
		self.selection_first = self.selection_start
		self.selection_last = self.selection_end
	end
	function elem_local.TextBox:get_cursor_by_mouse(x_)
		x_ = x_ - self.dpx_ - self.borderL
		local clinestr = self.text:sub(self.scrollpos + 1)
		local clinestrlen = #clinestr
		local cursor = 0
		while true do
			if cursor == clinestrlen then
				break
			end
			if x_ < gfx.textSize(clinestr:sub(1, cursor + 1)) - 1 then
				break
			end
			cursor = cursor + 1
		end
		return cursor + self.scrollpos
	end
	function elem_local.TextBox:mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
		self(elem_local.Clickable):mouseclick_hover(bubblingUp, x_, y_, button, event, wheel)
		if not bubblingUp then
			if button == 1 then
				if event == 1 then
					self.cursor = self:get_cursor_by_mouse(x_)
					self.selection_start = self.cursor
					self:sanitize_selection()
				end
			end
		end
	end
	function elem_local.TextBox:mouseclick(x_, y_, button, event, wheel)
		self(elem_local.Clickable):mouseclick(x_, y_, button, event, wheel)
		if not bubblingUp then
			if button == 1 then
				if self.lmousedown_ and (event == 2 or event == 3) then
					self.cursor = self:get_cursor_by_mouse(x_)
					self:follow_cursor()
					self.selection_end = self.cursor
					self:sanitize_selection()
				end
			end
		end
	end
	function elem_local.TextBox:follow_cursor()
		if self.cursor < self.scrollpos then
			self.scrollpos = self.cursor
			return
		end
		local text_ = self.text
		while true do
			local fullline = text_:sub(self.scrollpos + 1)
			local linemax, linelen = 0, #fullline
			while true do
				if linemax == linelen then
					break
				end
				if gfx.textSize(fullline:sub(1, linemax + 1)) > self.width - self.borderL - self.borderR then
					break
				end
				linemax = linemax + 1
			end
			if self.cursor <= self.scrollpos + linemax then
				break
			end
			self.scrollpos = self.scrollpos + 1
		end
		while true do
			if gfx.textSize(text_:sub(self.scrollpos)) >= self.width - self.borderL - self.borderR then
				break
			end
			if self.scrollpos == 0 then
				break
			end
			self.scrollpos = self.scrollpos - 1
		end
	end
	function elem_local.TextBox:draw()
		if not self.visible then return end
		local ol_ = self.enabled and (self.in_focus_tree_ and 255 or 200) or 100
		draw_outline(self.dpx_, self.dpy_, self.width, self.height, 0, 0, 0, ol_, ol_, ol_)
		
		local text_ = self.text
		local fullline = text_:sub(self.scrollpos + 1)
		local linemax, linelen = 0, #fullline
		while true do
			if linemax == linelen then
				break
			end
			if gfx.textSize(fullline:sub(1, linemax + 1)) > self.width - self.borderL - self.borderR then
				break
			end
			linemax = linemax + 1
		end
		local partialline = fullline:sub(1, linemax)
		
		-- line
		if self.textColor then
			gfx.drawText(self.dpx_ + self.borderL, self.dpy_ + self.borderT + 1, partialline, self.textColor[1] * 255, self.textColor[2] * 255, self.textColor[3] * 255)
		else
			gfx.drawText(self.dpx_ + self.borderL, self.dpy_ + self.borderT + 1, partialline)
		end
		
		-- selection
		local firstchar = _max(self.selection_first - self.scrollpos, 0)
		local lastchar = _max(self.selection_last - self.scrollpos, 0)
		local firstoffset = gfx.textSize(partialline:sub(1, firstchar))
		local lastoffset = gfx.textSize(partialline:sub(1, lastchar))
		gfx.fillRect(self.dpx_ + self.borderL + firstoffset,
			self.dpy_ + self.borderT, lastoffset - firstoffset, 10)
		gfx.drawText(self.dpx_ + self.borderL + firstoffset, self.dpy_ + self.borderT + 1,
			(partialline:sub(firstchar + 1, lastchar)), 0, 0, 0)
			
		-- cursor
		if self.in_focus_ then
			local textoutsize = gfx.textSize(text_:sub(self.scrollpos + 1, self.cursor))
			local rposx_, rposy_ = self.dpx_ + self.borderL + textoutsize, self.dpy_ + self.borderT
			if self.overwrite then
				gfx.fillRect(rposx_, rposy_ - 1, (gfx.textSize(string.sub(text_ .. " ", self.cursor + 1,
					self.cursor + 1))), 12)
			else
				gfx.drawLine(rposx_, rposy_ - 1, rposx_, rposy_ + 10)
			end
		end
		
		self(elem_local.Common):draw()
	end
	
	rootNode = elem_local.Root:new({
		posX = 0,
		posY = 0,
		width = gfx_width_,
		height = gfx_height_
	})
	_tptrasmui.rootNode = rootNode
	
	tpt.register_step(_tptrasmui.hook_step)
	tpt.register_mouseclick(_tptrasmui.hook_mouseclick)
	tpt.register_keypress(_tptrasmui.hook_keypress)
end

local selectedpos = {}

local defunct = true
local rasmdata_lte

local defaultWidth = 262
local defaultHeight = 148
local editorwintext = "RasmUI"

local editorwin, texteditor_asm

local depends_on_defunct = {}
local depends_on_debug = {}
local visibility_groups = {
	["all"] = {},
	["edit"] = {},
	["fsact"] = {}
}
local function select_visibility_group(group)
	for key in pairs(visibility_groups["all"]) do key.visible = false end
	for key, value in pairs(visibility_groups[group]) do key.visible = value end
end

local selectedcolor = {}

local width_, height_ = gfx_width_, gfx_height_
local searchdir = 1
local searchpos = 0
local maxsearchpos = width_ * height_

local textbox_fspath, listbox_files, button_save, button_reloadsource

local lastpath, lastsavepath, saving, unsavedchanges
local function set_lastsavepath(path)
	lastsavepath = path
	button_reloadsource.enabled = lastsavepath and true
end
local function set_unsavedchanges(obviously)
	unsavedchanges = obviously
	button_save.textColor = unsavedchanges and color_red
end

local function fsort_casehack(one, other)
	while true do
		if #other == 0 then return false end
		if #one == 0 then return true end
		if one:lower():byte() < other:lower():byte() then return true end
		if one:lower():byte() > other:lower():byte() then return false end
		if one:byte() < other:byte() then return true end
		if one:byte() > other:byte() then return false end
		one, other = one:sub(2), other:sub(2)
	end
end

local function bump_window()
	editorwin:set_text(editorwintext .. " - " .. (lastsavepath and lastsavepath:front():bake() or "new source"))
end

local function open_source(path_)
	local path = path_:bake()
	local handle = io.open(path, "r")
	if not handle then
		tpt.throw_error("Failed to open source")
		return false
	end
	texteditor_asm:set_text(handle:read("*a"):gsub("\r\n?", "\n"))
	texteditor_asm:set_cursor(0)
	handle:close()
	lastpath = path_
	set_lastsavepath(path_)
	set_unsavedchanges(false)
	bump_window()
	return true
end
local function save_source(path_)
	local path = path_:bake()
	local handle = io.open(path, "w")
	if not handle then
		tpt.throw_error("Failed to save source")
		return false
	end
	handle:write(texteditor_asm:get_text())
	handle:close()
	lastpath = path_
	set_lastsavepath(path_)
	set_unsavedchanges(false)
	bump_window()
	return true
end
local listpath
local function set_listpath(listpath_)
	listpath = listpath_
	local flist = fs_list(listpath:bake()) or {}
	local files, dirs = {}, {}
	for key, value in pairs(flist) do
		local concatres = listpath:combine(value):bake()
		if fs_isfile(concatres) then files[#files + 1] = value end
		if fs_isdirectory(concatres) then dirs[#dirs + 1] = value end
	end
	table.sort(files, fsort_casehack)
	table.sort(dirs, fsort_casehack)
	local dirslen = #dirs
	local sortedflist = {}
	local highlight = {}
	for key, value in pairs(dirs) do
		sortedflist[key] = value
		highlight[key] = true
	end
	for key, value in pairs(files) do
		sortedflist[key + dirslen] = value
	end
	listbox_files:set_list(sortedflist)
	listbox_files.highlight = highlight
	textbox_fspath:set_text(listpath:bake())
	textbox_fspath:set_cursor(#textbox_fspath.text)
end
local function init_filedialog(saving_)
	saving = saving_
	set_listpath((lastpath and lastpath:back()) or smartPath("."))
	listbox_files:set_top_line(1)
	listbox_files:select(nil)
	select_visibility_group("fsact")
end
local function uninit_filedialog(saving_)
	select_visibility_group("edit")
	texteditor_asm:set_focus(true)
end

local function rasmdata_lte_bump(_rasmdata_lte)
	rasmdata_lte = _rasmdata_lte
	-- local newlinecolors = {}
	-- for key in pairs(rasmdata_lte) do
		-- newlinecolors[key] = {150, 0, 0}
	-- end
	-- texteditor_asm.linecolors = newlinecolors
end

local function search()
	local searchpos_ = searchpos
	local selectedmachine = nil
	while true do
		searchpos = (searchpos + searchdir) % maxsearchpos
		local posX, posY = searchpos % width_, _floor(searchpos / width_)
		local quartzID = sim.partID(posX, posY)
		if quartzID and sim.partProperty(quartzID, "type") == elem.DEFAULT_PT_QRTZ and
		sim.partProperty(quartzID, "ctype") == 0x1864A205 then
			selectedpos[1], selectedpos[2] = posX, posY
			local machine = ""
			while true do
				posX = posX + 1
				local nextID = sim.partID(posX, posY)
				if not nextID then break end
				local nextCtype = sim.partProperty(nextID, "ctype")
				if nextCtype == 0 then
					posX = posX + 1
					local lastID = sim.partID(posX, posY)
					if not lastID then break end
					local lastCtype = sim.partProperty(lastID, "ctype")
					local checksum = 0
					for letter in machine:gmatch(".") do
						checksum = checksum + letter:byte()
					end
					if checksum ~= lastCtype then break end
					selectedmachine = machine
				end
				machine = machine .. string.char(nextCtype)
			end
			if selectedmachine == model.name then
				defunct = false
				break
			end
		end
		if searchpos == searchpos_ then
			defunct = true
			break
		end
	end
	-- editorwin:set_text(defunct and ("%s - No supported machine found"):format(editorwintext) or
		-- ("%s - %s @ (%d; %d)"):format(editorwintext, selectedmachine, selectedpos[1], selectedpos[2]))
	for key in pairs(depends_on_defunct) do
		key.enabled = not defunct
		key.stuck = key.stuck and not defunct
	end
end

editorwin = _tptrasmui.elem.Window:new({
	parent = _tptrasmui.rootNode,
	posX = _floor((gfx_width_ - defaultWidth) / 2),
	posY = _floor((gfx_height_ - defaultHeight) / 2),
	width = defaultWidth,
	height = defaultHeight,
	min_width = defaultWidth,
	min_height = defaultHeight,
	text = editorwintext,
	focus = true
})

local function lof_dockBottom(self)
	self.posY = self.parent.height - self.userdata.posY
end
local function lof_dockRight(self)
	self.posX = self.parent.width - self.userdata.posX
end
local function lof_dockBottomRight(self)
	lof_dockRight(self)
	lof_dockBottom(self)
end

local button_fscancel = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, posX = 2, userdata = {posY = 17}, layoutfunc = lof_dockBottom,
	text = "\238\128\170", textoffsetY = -1,
	tooltip = "Cancel",
	clickfunc = function()
		uninit_filedialog()
	end
})
visibility_groups["all"][button_fscancel] = true
visibility_groups["fsact"][button_fscancel] = true
local button_open = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, posX = 2, userdata = {posY = 17}, layoutfunc = lof_dockBottom,
	text = "\238\128\129", textoffsetY = -1,
	tooltip = "Open source file",
	clickfunc = function()
		init_filedialog(false)
	end
})
visibility_groups["all"][button_open] = true
visibility_groups["edit"][button_open] = true
local function savebutton_action(has_shift_)
	if lastsavepath and not has_shift_ then
		save_source(lastsavepath)
	else
		init_filedialog(true)
	end
end
button_save = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, posX = 20, userdata = {posY = 17}, layoutfunc = lof_dockBottom,
	text = "\238\128\130", textoffsetY = -1,
	tooltip = "Save source file",
	clickfunc = function()
		savebutton_action(bit.band(_tptrasmui.lastmodifierstate, 3) ~= 0)
	end
})
local button_save_draw = button_save.draw
function button_save.draw(self, ...)
	local textColor = self.textColor
	if bit.band(_tptrasmui.lastmodifierstate, 3) ~= 0 then
		self.textColor = color_green
	end
	button_save_draw(self, ...)
	self.textColor = textColor
end
visibility_groups["all"][button_save] = true
visibility_groups["edit"][button_save] = true
local button_new = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, posX = 38, userdata = {posY = 17}, layoutfunc = lof_dockBottom,
	text = "\238\128\146", textoffsetY = -2,
	tooltip = "New source file",
	clickfunc = function(caller)
		set_lastsavepath(nil)
		texteditor_asm:set_cursor(0)
		texteditor_asm:set_text("")
		set_unsavedchanges(false)
		bump_window()
	end
})
visibility_groups["all"][button_new] = true
visibility_groups["edit"][button_new] = true
button_reloadsource = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, posX = 56, userdata = {posY = 17}, layoutfunc = lof_dockBottom,
	text = "\238\128\145", textoffsetY = -2,
	enabled = false,
	tooltip = "Reload source",
	clickfunc = function()
		if lastsavepath then open_source(lastsavepath) end
	end
})
visibility_groups["all"][button_reloadsource] = true
visibility_groups["edit"][button_reloadsource] = true
local button_reloadonassemble = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, posX = 74, userdata = {posY = 17}, layoutfunc = lof_dockBottom,
	text = "R", textoffsetX = 1,
	tooltip = "Reload source on Assemble",
	clickfunc = function(caller)
		caller.stuck = not caller.stuck
	end
})
visibility_groups["all"][button_reloadonassemble] = true
visibility_groups["edit"][button_reloadonassemble] = true

local button_prev = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 156, posY = 17}, layoutfunc = lof_dockBottomRight,
	text = "\238\128\150", textoffsetY = -1,
	tooltip = "Select previous machine",
	clickfunc = function()
		searchdir = -1
		search()
	end
})
visibility_groups["all"][button_prev] = true
visibility_groups["edit"][button_prev] = true
local button_next = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 138, posY = 17}, layoutfunc = lof_dockBottomRight,
	text = "\238\128\149", textoffsetY = -1,
	tooltip = "Select next machine",
	clickfunc = function()
		searchdir = 1
		search()
	end
})
visibility_groups["all"][button_next] = true
visibility_groups["edit"][button_next] = true

local button_toggledebug
local function bump_debug()
	for key in pairs(depends_on_debug) do
		visibility_groups["edit"][key] = button_toggledebug.stuck
		key.visible = button_toggledebug.stuck
	end
	editorwin:layout_change()
end
button_toggledebug = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 109, posY = 17}, layoutfunc = lof_dockBottomRight,
	text = "\238\129\130", textoffsetY = -2,
	tooltip = "Show debug controls",
	clickfunc = function(caller)
		caller.stuck = not caller.stuck
		bump_debug()
	end
})
visibility_groups["all"][button_toggledebug] = true
visibility_groups["edit"][button_toggledebug] = true
local editableboxes = {}
local button_hexadec = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 109, posY = 33}, layoutfunc = lof_dockBottomRight,
	text = "0x",
	tooltip = "Show hexadecimal numbers",
	stuck = true,
	clickfunc = function(caller)
		caller.stuck = not caller.stuck
		bump_debug()
	end
})
visibility_groups["all"][button_hexadec] = true
visibility_groups["edit"][button_hexadec] = true
depends_on_debug[button_hexadec] = true
local button_fpsboost = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 91, posY = 33}, layoutfunc = lof_dockBottomRight,
	text = "\238\128\153", textoffsetY = -1,
	tooltip = "Remove FPS cap while executing code",
	clickfunc = function(caller)
		caller.stuck = not caller.stuck
	end
})
visibility_groups["all"][button_fpsboost] = true
visibility_groups["edit"][button_fpsboost] = true
depends_on_debug[button_fpsboost] = true
local button_showoverlay = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 73, posY = 33}, layoutfunc = lof_dockBottomRight,
	text = "\238\129\147", textoffsetX = 1, textoffsetY = -1,
	tooltip = "Draw overlay over selected machine",
	clickfunc = function(caller)
		caller.stuck = not caller.stuck
	end
})
visibility_groups["all"][button_showoverlay] = true
visibility_groups["edit"][button_showoverlay] = true
depends_on_debug[button_showoverlay] = true
local button_color = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 55, posY = 33}, layoutfunc = lof_dockBottomRight,
	text = "\238\128\136", textoffsetY = -2,
	tooltip = "Randomize overlay color",
	textColor = {0, 0, 0},
	clickfunc = function(caller)
		selectedcolor[1] = math.random(0, 155) + 100
		selectedcolor[2] = math.random(0, 155) + 100
		selectedcolor[3] = math.random(0, 155) + 100
		caller.textColor[1] = selectedcolor[1] / 255
		caller.textColor[2] = selectedcolor[2] / 255
		caller.textColor[3] = selectedcolor[3] / 255
	end
})
visibility_groups["all"][button_color] = true
visibility_groups["edit"][button_color] = true
depends_on_debug[button_color] = true
local button_stepper = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 37, posY = 33}, layoutfunc = lof_dockBottomRight,
	text = "F", textoffsetX = 1,
	tooltip = "Advance the simulation by a single frame",
	clickfunc = function(caller)
		if tpt.set_pause() == 0 then return end
		_tptrasmui.do_single_step = true
		tpt.set_pause(0)
	end
})
visibility_groups["all"][button_stepper] = true
visibility_groups["edit"][button_stepper] = true
depends_on_debug[button_stepper] = true
local button_followexec = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 19, posY = 33}, layoutfunc = lof_dockBottomRight,
	text = "\238\129\128", textoffsetX = 1, textoffsetY = -2,
	--! TODO: release this
	-- tooltip = "Follow execution in editor",
	tooltip = "Follow execution in editor (NYI)",
	enabled = false,
	clickfunc = function(caller)
		caller.stuck = not caller.stuck
	end
})
visibility_groups["all"][button_followexec] = true
visibility_groups["edit"][button_followexec] = true
--! TODO: release this
-- depends_on_debug[button_followexec] = true

local button_assemble = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 91, posY = 17}, layoutfunc = lof_dockBottomRight,
	text = "\238\128\154", textoffsetY = -1,
	tooltip = "Assemble source",
	clickfunc = function()
		if button_reloadonassemble.stuck then
			button_reloadsource:click()
		end
		rasmdata_lte_bump(invokeRasm(selectedpos[1], selectedpos[2], texteditor_asm:get_text(), "content_rasmui"))
	end
})
visibility_groups["all"][button_assemble] = true
visibility_groups["edit"][button_assemble] = true
local button_run = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 73, posY = 17}, layoutfunc = lof_dockBottomRight,
	text = "\238\128\128", textoffsetY = -1,
	tooltip = "Start execution",
	clickfunc = function()
		if defunct then return end
		sim.partCreate(-2, selectedpos[1] + model.run[1],
			selectedpos[2] + model.run[2], elem.DEFAULT_PT_SPRK)
	end
})
visibility_groups["all"][button_run] = true
visibility_groups["edit"][button_run] = true
local button_pause = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 55, posY = 17}, layoutfunc = lof_dockBottomRight,
	text = "\238\128\136", textoffsetY = -2,
	tooltip = "Stop execution",
	clickfunc = function()
		if defunct then return end
		sim.partCreate(-2, selectedpos[1] + model.pause[1],
			selectedpos[2] + model.pause[2], elem.DEFAULT_PT_SPRK)
	end
})
visibility_groups["all"][button_pause] = true
visibility_groups["edit"][button_pause] = true
local button_resume = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 37, posY = 17}, layoutfunc = lof_dockBottomRight,
	text = "\238\129\162", textoffsetY = -2,
	tooltip = "Resume execution",
	clickfunc = function()
		if defunct then return end
		sim.partCreate(-2, selectedpos[1] + model.resume[1],
			selectedpos[2] + model.resume[2], elem.DEFAULT_PT_SPRK)
	end
})
visibility_groups["all"][button_resume] = true
visibility_groups["edit"][button_resume] = true
local button_reset = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 19, posY = 17}, layoutfunc = lof_dockBottomRight,
	text = "\238\128\145", textoffsetY = -2,
	tooltip = "Reset machine",
	clickfunc = function()
		if defunct then return end
		sim.partCreate(-2, selectedpos[1] + model.reset[1],
			selectedpos[2] + model.reset[2], elem.DEFAULT_PT_SPRK)
	end
})
visibility_groups["all"][button_reset] = true
visibility_groups["edit"][button_reset] = true

local button_fsok
textbox_fspath = _tptrasmui.elem.TextBox:new({
	parent = editorwin, posX = 20, height = 15, layoutfunc = function(caller)
		caller.posY = caller.parent.height - 17
		caller.width = caller.parent.width - 76
	end,
	textColor = color_red,
	textchangefunc = function(caller)
		local path = caller.text
		if fs_exists(path) then
			if fs_isfile(path) then
				caller.textColor = saving and color_yellow or color_green
			elseif fs_isdirectory(path) then
				caller.textColor = color_cyan
			else
				caller.textColor = color_red
			end
		else
			caller.textColor = color_red
		end
	end,
	returnhitfunc = function()
		button_fsok:click()
	end,
	escapehitfunc = function()
		button_fscancel:click()
	end
})
visibility_groups["all"][textbox_fspath] = true
visibility_groups["fsact"][textbox_fspath] = true

local button_fsgoup = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 55, posY = 17}, layoutfunc = lof_dockBottomRight,
	text = "\238\128\147", textoffsetY = -2,
	tooltip = "Go up",
	clickfunc = function()
		set_listpath(listpath:back())
	end
})
visibility_groups["all"][button_fsgoup] = true
visibility_groups["fsact"][button_fsgoup] = true
local button_fsrefresh = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 37, posY = 17}, layoutfunc = lof_dockBottomRight,
	text = "\238\128\145", textoffsetY = -2,
	tooltip = "Refresh list",
	clickfunc = function()
		set_listpath(listpath)
	end
})
visibility_groups["all"][button_fsrefresh] = true
visibility_groups["fsact"][button_fsrefresh] = true
button_fsok = _tptrasmui.elem.Button:new({
	parent = editorwin,
	width = 17, height = 15, userdata = {posX = 19, posY = 17}, layoutfunc = lof_dockBottomRight,
	text = "\238\129\143", textoffsetY = -1,
	clickfunc = function()
		local path = textbox_fspath.text
		local path_ = smartPath(path)
		local success
		if fs_isfile(path) then
			success = saving and save_source(path_) or open_source(path_)
		elseif fs_isdirectory(path) then
			set_listpath(path_)
		elseif not fs_exists(path) and saving then
			success = save_source(path_)
		end
		if success then
			uninit_filedialog()
		end
	end
})
visibility_groups["all"][button_fsok] = true
visibility_groups["fsact"][button_fsok] = true

do
	local function textchangeonbox(caller)
		caller.textColor = tonumber(caller.text) and color_green or color_red
	end
	local function resetregonbox(caller)
		caller:set_text((button_hexadec.stuck and "0x%04X" or "%i"):format(model.editables[caller.userdata.target].infunc(selectedpos)))
	end
	local function setregbybox(caller)
		local numval = tonumber(caller.text)
		if not numval then return end
		model.editables[caller.userdata.target].outfunc(selectedpos, numval)
		resetregonbox(caller)
	end
	for ix = 1, #model.editables do
		local static_ = _tptrasmui.elem.StaticText:new({
			parent = editorwin,
			posY = 18 + _floor((ix - 1) / 2) * 16, width = 8, height = 15, userdata = {posX = 117 - ((ix - 1) % 2) * 60}, layoutfunc = lof_dockRight,
			text = model.editables[ix].name
		})
		visibility_groups["all"][static_] = true
		visibility_groups["edit"][static_] = true
		depends_on_debug[static_] = true
		local textbox_ = _tptrasmui.elem.TextBox:new({
			parent = editorwin,
			posY = 18 + _floor((ix - 1) / 2) * 16, width = 45, height = 15, userdata = {posX = 107 - ((ix - 1) % 2) * 60, target = ix}, layoutfunc = lof_dockRight,
			textchangefunc = textchangeonbox,
			returnhitfunc = setregbybox,
			escapehitfunc = resetregonbox
		})
		editableboxes[textbox_] = true
		visibility_groups["all"][textbox_] = true
		visibility_groups["edit"][textbox_] = true
		depends_on_debug[textbox_] = true
		depends_on_defunct[textbox_] = true
	end
end

local function lof_fillRest(self)
	self.width = self.parent.width - 4
	self.height = self.parent.height - 36
end
local function lof_fillRestRem(self)
	self.width = self.parent.width - (button_toggledebug.stuck and 123 or 4)
	self.height = self.parent.height - 36
end

listbox_files = _tptrasmui.elem.ListBox:new({
	parent = editorwin,
	-- visible = false,
	posX = 2, posY = 18, layoutfunc = lof_fillRest,
	selectfunc = function(caller)
		if not caller.selection then return end
		textbox_fspath:set_text(listpath:combine(caller.lines[caller.selection]):bake())
		textbox_fspath:set_cursor(#textbox_fspath.text)
	end,
	selectdoublefunc = function()
		button_fsok:click()
	end,
})
visibility_groups["all"][listbox_files] = true
visibility_groups["fsact"][listbox_files] = true

texteditor_asm = _tptrasmui.elem.TextEditor:new({
	parent = editorwin,
	posX = 2, posY = 18, layoutfunc = lof_fillRestRem,
	textchangefunc = function()
		set_unsavedchanges(true)
	end
})
local function ctrl_fileops(caller, bubblingUp, keynumber, modifier)
	if keynumber == 110 then
		button_new:click()
	elseif keynumber == 111 then
		button_open:click()
	elseif keynumber == 115 then
		savebutton_action(bit.band(modifier, 3) ~= 0)
	end
end
editorwin.ctrl_receivers[ctrl_fileops] = true
visibility_groups["all"][texteditor_asm] = true
visibility_groups["edit"][texteditor_asm] = true

depends_on_defunct[button_assemble] = true
depends_on_defunct[button_run] = true
depends_on_defunct[button_pause] = true
depends_on_defunct[button_resume] = true
depends_on_defunct[button_reset] = true
--! TODO: release this
-- depends_on_defunct[button_followexec] = true
depends_on_defunct[button_showoverlay] = true

bump_debug()

do
	local fpsboost = false
	local editorwin_step = editorwin.step
	function editorwin:step()
		editorwin_step(self)
		if not defunct then
			for key in pairs(editableboxes) do
				if not key.in_focus_ then
					key:escapehit()
				end
			end
		end
		local fpsboost_ = fpsboost
		fpsboost = ((not defunct and model.isrunning(selectedpos)) and button_fpsboost.stuck) or false
		if fpsboost_ ~= fpsboost then
			tpt.setfpscap(fpsboost and 2 or 60)
		end
	end
	local editorwin_draw = editorwin.draw
	function editorwin:draw()
		if not defunct and self.visible and self.in_focus_tree_ and button_toggledebug.stuck and button_showoverlay.stuck then
			gfx.fillRect(selectedpos[1] + model.iron[1],
				selectedpos[2] + model.iron[2],
				model.ironsize[1], model.ironsize[2],
				selectedcolor[1], selectedcolor[2], selectedcolor[3], 100)
		end
		editorwin_draw(self)
	end
end

select_visibility_group("edit")
button_new:click()
button_color:click()
button_next:click()

