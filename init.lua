local load_time_start = os.clock()

function vector.pos_to_string(pos)
	return "{x="..pos.x.."; y="..pos.y.."; z="..pos.z.."}"
end

local r_corr = 0.25 --remove a bit more nodes (if shooting diagonal) to let it look like a hole (sth like antialiasing)

-- this doesn't need to be calculated every time
local f_1 = 0.5-r_corr
local f_2 = 0.5+r_corr

--returns information about the direction
local function get_used_dir(dir)
	local abs_dir = {x=math.abs(dir.x), y=math.abs(dir.y), z=math.abs(dir.z)}
	local dir_max = math.max(abs_dir.x, abs_dir.y, abs_dir.z)
	if dir_max == abs_dir.x then
		local tab = {"x", {x=1, y=dir.y/dir.x, z=dir.z/dir.x}}
		if dir.x >= 0 then
			tab[3] = "+"
		end
		return tab
	end
	if dir_max == abs_dir.y then
		local tab = {"y", {x=dir.x/dir.y, y=1, z=dir.z/dir.y}}
		if dir.y >= 0 then
			tab[3] = "+"
		end
		return tab
	end
	local tab = {"z", {x=dir.x/dir.z, y=dir.y/dir.z, z=1}}
	if dir.z >= 0 then
		tab[3] = "+"
	end
	return tab
end

local function node_tab(z, d)
	local n1 = math.floor(z*d+f_1)
	local n2 = math.floor(z*d+f_2)
	if n1 == n2 then
		return {n1}
	end
	return {n1, n2}
end

local function return_line(pos, dir, range) --range ~= length
	local tab = {}
	local num = 1
	local t_dir = get_used_dir(dir)
	local dir_typ = t_dir[1]
	if t_dir[3] == "+" then
		f_tab = {0, range, 1}
	else
		f_tab = {0, -range, -1}
	end
	local d_ch = t_dir[2]
	if dir_typ == "x" then
		for d = f_tab[1],f_tab[2],f_tab[3] do
			local x = d
			local ytab = node_tab(d_ch.y, d)
			local ztab = node_tab(d_ch.z, d)
			for _,y in ipairs(ytab) do
				for _,z in ipairs(ztab) do
					tab[num] = {x=pos.x+x, y=pos.y+y, z=pos.z+z}
					num = num+1
				end
			end
		end
	elseif dir_typ == "y" then
		for d = f_tab[1],f_tab[2],f_tab[3] do
			local xtab = node_tab(d_ch.x, d)
			local y = d
			local ztab = node_tab(d_ch.z, d)
			for _,x in ipairs(xtab) do
				for _,z in ipairs(ztab) do
					tab[num] = {x=pos.x+x, y=pos.y+y, z=pos.z+z}
					num = num+1
				end
			end
		end
	else
		for d = f_tab[1],f_tab[2],f_tab[3] do
			local xtab = node_tab(d_ch.x, d)
			local ytab = node_tab(d_ch.y, d)
			local z = d
			for _,x in ipairs(xtab) do
				for _,y in ipairs(ytab) do
					tab[num] = {x=pos.x+x, y=pos.y+y, z=pos.z+z}
					num = num+1
				end
			end
		end
	end
	return tab
end

local function table_contains2(t, v)
	for i = #t, 1, -1 do
		if t[i] == v then
			return true
		end
	end
	return false
end

local function return_fine_line(pos, dir, range, scale)
	local ps1 = return_line(vector.round(vector.multiply(pos, scale)), dir, range*scale)
	local ps2 = {}
	local ps2_num = 1
	for _,p1 in ipairs(ps1) do
		local p2 = vector.round(vector.divide(p1, scale))
		if not table_contains2(ps2, p2) then
			ps2[ps2_num] = p2
			ps2_num = ps2_num+1
		end
	end
	return ps2
end

function vector.fine_line(pos, dir, range, scale)
	--assert_vector(pos)
	if not range then --dir = pos2
		dir = vector.direction(pos, dir)
		range = vector.distance(pos, dir)
	end
	return return_fine_line(pos, dir, range, scale)
end

function vector.line(pos, dir, range, alt)
	--assert_vector(pos)
	if alt then
		if not range then --dir = pos2
			dir, range = vector.direction(pos, dir), vector.distance(pos, dir)
		end
		return return_line(pos, dir, range)
	end
	if range then --dir = pos2
		dir = vector.round(vector.multiply(dir, range))
	else
		dir = vector.subtract(dir, pos)
	end
	local line,n = {},1
	for _,i in ipairs(vector.threeline(dir.x, dir.y, dir.z)) do
		line[n] = {x=pos.x+i[1], y=pos.y+i[2], z=pos.z+i[3]}
		n = n+1
	end
	return line
end

local twolines = {}
function vector.twoline(x, y)
	local pstr = x.." "..y
	local line = twolines[pstr]
	if line then
		return line
	end
	line = {}
	local n = 1
	local dirx = 1
	if x < 0 then
		dirx = -dirx
	end
	local ymin, ymax = 0, y
	if y < 0 then
		ymin, ymax = ymax, ymin
	end
	local m = y/x --y/0 works too
	local dir = 1
	if m < 0 then
		dir = -dir
	end
	for i = 0,x,dirx do
		local p1 = math.max(math.min(math.floor((i-0.5)*m+0.5), ymax), ymin)
		local p2 = math.max(math.min(math.floor((i+0.5)*m+0.5), ymax), ymin)
		for j = p1,p2,dir do
			line[n] = {i, j}
			n = n+1
		end
	end
	twolines[pstr] = line
	return line
end

local threelines = {}
function vector.threeline(x, y, z)
	local pstr = x.." "..y.." "..z
	local line = threelines[pstr]
	if line then
		return line
	end
	if x ~= math.floor(x) then
		minetest.log("error", "[vector_extras] INFO: The position used for vector.threeline isn't round.")
	end
	local two_line = vector.twoline(x, y)
	line = {}
	local n = 1
	local zmin, zmax = 0, z
	if z < 0 then
		zmin, zmax = zmax, zmin
	end
	local m = z/math.hypot(x, y)
	local dir = 1
	if m < 0 then
		dir = -dir
	end
	for _,i in ipairs(two_line) do
		local px, py = unpack(i)
		local ph = math.hypot(px, py)
		local z1 = math.max(math.min(math.floor((ph-0.5)*m+0.5), zmax), zmin)
		local z2 = math.max(math.min(math.floor((ph+0.5)*m+0.5), zmax), zmin)
		for pz = z1,z2,dir do
			line[n] = {px, py, pz}
			n = n+1
		end
	end
	threelines[pstr] = line
	return line
end

function vector.sort(ps, preferred_coords)
	preferred_coords = preferred_coords or {"z", "y", "x"}
	local a,b,c = unpack(preferred_coords)
	local function ps_sorting(p1, p2)
		if p1[a] == p2[a] then
			if p1[b] == p2[a] then
				if p1[c] < p2[c] then
					return true
				end
			elseif p1[b] < p2[b] then
				return true
			end
		elseif p1[a] < p2[a] then
			return true
		end
	end
	table.sort(ps, ps_sorting)
	return ps
end

function vector.scalarproduct(v1, v2)
	return v1.x*v2.x + v1.y*v2.y + v1.z*v2.z
end

function vector.crossproduct(v1, v2)
	return {
		x = v1.y*v2.z - v1.z*v2.y,
		y = v1.z*v2.x - v1.x*v2.z,
		z = v1.x*v2.y - v1.y*v2.x
	}
end

function vector.mirror(pos, v, vb)
	if vb then
		v = vector.normalize(vector.crossproduct(v, vb))
	end
	local dx,dy,dz = v.x,v.y,v.z
	local x,y,z = pos.x,pos.y,pos.z
	local dif = (dx*x+dy*y+dz*z)/(dx*dx+dy*dy+dz*dz)
	if dif == 0 then
		return pos
	end
	dif = -dif*2
	return vector.add(pos, vector.multiply(v, dif))
end

--local areas = {}
function vector.area(ps)
	-- sort positions and imagine the first one as vector.zero
	ps = vector.sort(ps)
	local pos = ps[1]
	local B = vector.subtract(ps[2], pos)
	local C = vector.subtract(ps[3], pos)

	-- get the positions for the fors
	local cube_p1 = {x=0, y=0, z=0}
	local cube_p2 = {x=0, y=0, z=0}
	for i in pairs(cube_p1) do
		cube_p1[i] = math.min(B[i], C[i], 0)
		cube_p2[i] = math.max(B[i], C[i], 0)
	end

	local vn = vector.normalize(vector.crossproduct(B, C))

	--[[bB.x*C.x+bB.y*C.y+C.z = 0
	bB.x*vn.x+bB.y*vn.y+vn.z = 0

	bB.x = -(bB.y*C.y+C.z)/*C.x
	bB.x = -(bB.y*vn.y+vn.z)/vn.x

	(bB.y*C.y+C.z)/*C.x = (bB.y*vn.y+vn.z)/vn.x
	(bB.y*C.y+C.z)*vn.x = C.x*(bB.y*vn.y+vn.z)
	bB.y*C.y*vn.x+C.z*vn.x = C.x*bB.y*vn.y+C.x*vn.z
	bB.y*C.y*vn.x-C.x*bB.y*vn.y = C.x*vn.z-C.z*vn.x
	bB.y*(C.y*vn.x-C.x*vn.y) = C.x*vn.z-C.z*vn.x
	bB.y = (C.x*vn.z-C.z*vn.x)/(C.y*vn.x-C.x*vn.y)

	bB.y = -(bB.x*C.x+C.z)/C.y
	bB.y = -(bB.x*vn.x+vn.z)/vn.y]]

	--[[
	local v3 = vector.subtract(C, B)

	local bB = {
		x = (C.y*vn.z-C.z*vn.y)/(C.x*vn.y-C.y*vn.x),
		y = (C.x*vn.z-C.z*vn.x)/(C.y*vn.x-C.x*vn.y),
		z = 1,
	}
	local cC = {
		x = (B.y*vn.z-B.z*vn.y)/(B.x*vn.y-B.y*vn.x),
		y = (B.x*vn.z-B.z*vn.x)/(B.y*vn.x-B.x*vn.y),
		z = 1,
	}

	local aA = {
		x = (v3.y*vn.z-v3.z*vn.y)/(v3.x*vn.y-v3.y*vn.x),
		y = (v3.x*vn.z-v3.z*vn.x)/(v3.y*vn.x-v3.x*vn.y),
		z = 1,
	}]]
	local B2 = vector.mirror(B, vn, C)
	local C2 = vector.mirror(C, vn, B)

	local BC = vector.subtract(C, B)
	local BA = vector.multiply(B, -1)
	local A2 = vector.add(vector.mirror(BA, vn, BC), B)

	local nB = vector.normalize(B)
	local nC = vector.normalize(C)
	local angle_BC = math.acos(vector.scalarproduct(nB, nC))

	local BA = vector.normalize(vector.multiply(B, -1))
	local BC = vector.normalize(vector.subtract(C, B))
	local angle_AC = math.acos(vector.scalarproduct(BA, BC))

	local bB = {z=1}
	local area = {}
	for z = cube_p1.z, cube_p2.z do
		for y = cube_p1.y, cube_p2.y do
			for x = cube_p1.x, cube_p2.x do
				local p = {x=x, y=y, z=z}
				local d = {
					x = (p.y*vn.y+p.z*vn.z)/vn.x+p.x,
					y = (p.x*vn.x+p.z*vn.z)/vn.y+p.y,
					z = (p.x*vn.x+p.y*vn.y)/vn.z+p.z,
				}
				local dmin = math.min(math.abs(d.x), math.abs(d.y), math.abs(d.z))
				if dmin <= 0.5 then
					--[[ep.x*vn.x+ep.y*vn.y+ep.z*vn.z = 0
					ep.x = p.x+n*vn.x
					ep.y = p.y+n*vn.y
					ep.z = p.z+n*vn.z

					(p.x+n*vn.x)*vn.x+(p.y+n*vn.y)*vn.y+(p.z+n*vn.z)*vn.z = 0
					p.x*vn.x+n*vn.x*vn.x+p.y*vn.y+n*vn.y*vn.y+p.z*vn.z+n*vn.z*vn.z = 0
					n*vn.x*vn.x+n*vn.y*vn.y+n*vn.z*vn.z = -(p.x*vn.x+p.y*vn.y+p.z*vn.z)
					n*(vn.x*vn.x+vn.y*vn.y+vn.z*vn.z) = -(p.x*vn.x+p.y*vn.y+p.z*vn.z)--]]
					n = -(p.x*vn.x+p.y*vn.y+p.z*vn.z)/(vn.x*vn.x+vn.y*vn.y+vn.z*vn.z)
					local ep = vector.new(p)
					ep = vector.add(ep, vector.multiply(vn, n))
					--[[for n,i in pairs(d) do
						if math.abs(i) == dmin then
							ep[n] = ep[n]+i
							break
						end
					end--]]
					local nep = vector.normalize(ep)
					local angle_Bep = math.acos(vector.scalarproduct(nB, nep))
					local angle_Cep = math.acos(vector.scalarproduct(nC, nep))
					local angldif = angle_BC - (angle_Bep+angle_Cep)
					if math.abs(angldif) < 0.01 then
						ep = vector.subtract(ep, B)
						nep = vector.normalize(ep)
						local angle_Aep = math.acos(vector.scalarproduct(BA, nep))
						local angle_Cep = math.acos(vector.scalarproduct(BC, nep))
						local angldif = angle_AC - (angle_Aep+angle_Cep)
						if math.abs(angldif) < 0.01 then
							table.insert(area, vector.add(pos, p))
						end
					end
				end
			end
		end
	end
	return area
end

function vector.straightdelay(s, v, a)
	if not a then
		return s/v
	end
	return (math.sqrt(v*v+2*a*s)-v)/a
end

vector.zero = {x=0, y=0, z=0}

function vector.sun_dir(time)
	if not time then
		time = minetest.get_timeofday()
	end
	local t = (time-0.5)*5/6+0.5 --the sun rises at 5 o'clock, not at 6
	if t < 0.25
	or t > 0.75 then
		return
	end
	local tmp = math.cos(math.pi*(2*t-0.5))
	return {x=tmp, y=math.sqrt(1-tmp*tmp), z=0}
end

function vector.inside(pos, minp, maxp)
	for _,i in pairs({"x", "y", "z"}) do
		if pos[i] < minp[i]
		or pos[i] > maxp[i] then
			return false
		end
	end
	return true
end

function vector.minmax(p1, p2)
	local p1 = vector.new(p1)
	local p2 = vector.new(p2)
	for _,i in ipairs({"x", "y", "z"}) do
		if p1[i] > p2[i] then
			p1[i], p2[i] = p2[i], p1[i]
		end
	end
	return p1, p2
end

function vector.move(p1, p2, s)
	return vector.round(
		vector.add(
			vector.multiply(
				vector.direction(
					p1,
					p2
				),
				s
			),
			p1
		)
	)
end

local explosion_tables = {}
function vector.explosion_table(r)
	local table = explosion_tables[r]
	if table then
		return table
	end

	local t1 = os.clock()
	local tab, n = {}, 1

	local tmp = r*r + r
	for x=-r,r do
		for y=-r,r do
			for z=-r,r do
				local rc = x*x+y*y+z*z 
				if rc <= tmp then
					local np={x=x, y=y, z=z}
					if math.floor(math.sqrt(rc) +0.5) > r-1 then
						tab[n] = {np, true}
					else
						tab[n] = {np}
					end
					n = n+1
				end
			end
		end
	end
	explosion_tables[r] = tab
	minetest.log("info", string.format("[vector_extras] table created after ca. %.2fs", os.clock() - t1))
	return tab
end

local circle_tables = {}
function vector.circle(r)
	local table = circle_tables[r]
	if table then
		return table
	end

	local t1 = os.clock()
	local tab, n = {}, 1

	for i = -r, r do
		for j = -r, r do
			if math.floor(math.sqrt(i*i+j*j)+0.5) == r then
				tab[n] = {x=i, y=0, z=j}
				n = n+1
			end
		end
	end
	circle_tables[r] = tab
	minetest.log("info", string.format("[vector_extras] table created after ca. %.2fs", os.clock() - t1))
	return tab
end

local ring_tables = {}
function vector.ring(r)
	local table = ring_tables[r]
	if table then
		return table
	end

	local t1 = os.clock()
	local tab, n = {}, 1

	local tmp = r*r
	local p = {x=math.floor(r+0.5), z=0}
	while p.x > 0 do
		tab[n] = p
		n = n+1
		local p1, p2 = {x=p.x-1, z=p.z}, {x=p.x, z=p.z+1}
		local dif1 = math.abs(tmp-p1.x*p1.x-p1.z*p1.z)
		local dif2 = math.abs(tmp-p2.x*p2.x-p2.z*p2.z)
		if dif1 <= dif2 then
			p = p1
		else
			p = p2
		end
	end

	local tab2, n = {}, 1
	for _,i in ipairs(tab) do
		for _,j in ipairs({
			{i.x, i.z},
			{-i.z, i.x},
			{-i.x, -i.z},
			{i.z, -i.x},
		}) do
			tab2[n] = {x=j[1], y=0, z=j[2]}
			n = n+1
		end
	end
	ring_tables[r] = tab2
	minetest.log("info", string.format("[vector_extras] table created after ca. %.2fs", os.clock() - t1))
	return tab2
end

function vector.chunkcorner(pos)
	return {x=pos.x-pos.x%16, y=pos.y-pos.y%16, z=pos.z-pos.z%16}
end

dofile(minetest.get_modpath("vector_extras").."/vector_meta.lua")

minetest.log("info", string.format("[vector_extras] loaded after ca. %.2fs", os.clock() - load_time_start))
