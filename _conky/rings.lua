--[[
Ring Meters by londonali1010 (2009)

This script draws percentage meters as rings. It is fully customisable; all options are described in the script.

IMPORTANT: if you are using the 'cpu' function, it will cause a segmentation fault if it tries to draw a ring straight away. The if statement on line 145 uses a delay to make sure that this doesn't happen. It calculates the length of the delay by the number of updates since Conky started. Generally, a value of 5s is long enough, so if you update Conky every 1s, use update_num > 5 in that if statement (the default). If you only update Conky every 2s, you should change it to update_num > 3; conversely if you update Conky every 0.5s, you should use update_num > 10. ALSO, if you change your Conky, is it best to use "killall conky; conky" to update it, otherwise the update_num will not be reset and you will get an error.

To call this script in Conky, use the following (assuming that you save this script to ~/scripts/rings.lua):
    lua_load ~/scripts/rings-v1.2.1.lua
    lua_draw_hook_pre ring_stats

Changelog:
+ v1.2.1 -- Fixed minor bug that caused script to crash if conky_parse() returns a nil value (20.10.2009)
+ v1.2 -- Added option for the ending angle of the rings (07.10.2009)
+ v1.1 -- Added options for the starting angle of the rings, and added the "max" variable, to allow for variables that output a numerical value rather than a percentage (29.09.2009)
+ v1.0 -- Original release (28.09.2009)
]]

settings_table = {
    --{
    --    -- Edit this table to customise your rings.
    --    -- You can create more rings simply by adding more elements to settings_table.
    --    -- "name" is the type of stat to display; you can choose from 'cpu', 'memperc', 'fs_used_perc', 'battery_used_perc'.
    --    name='time',
    --    -- "arg" is the argument to the stat type, e.g. if in Conky you would write ${cpu cpu0}, 'cpu0' would be the argument. If you would not use an argument in the Conky variable, use ''.
    --    arg='%d',
    --    -- "max" is the maximum value of the ring. If the Conky variable outputs a percentage, use 100.
    --    max=31,
    --    -- "bg_colour" is the colour of the base ring.
    --    bg_colour=0xcccccc,
    --    -- "bg_alpha" is the alpha value of the base ring.
    --    bg_alpha=0.0,
    --    -- "fg_colour" is the colour of the indicator part of the ring.
    --    fg_colour=0xe43526,
    --    -- "fg_alpha" is the alpha value of the indicator part of the ring.
    --    fg_alpha=0.8,
    --    -- "x" and "y" are the x and y coordinates of the centre of the ring, relative to the top left corner of the Conky window.
    --    x=170, y=170,
    --    -- "radius" is the radius of the ring.
    --    radius=120,
    --    -- "thickness" is the thickness of the ring, centred around the radius.
    --    thickness=1,
    --    -- "start_angle" is the starting angle of the ring, in degrees, clockwise from top. Value can be either positive or negative.
    --    start_angle=0,
    --    -- "end_angle" is the ending angle of the ring, in degrees, clockwise from top. Value can be either positive or negative, but must be larger (e.g. more clockwise) than start_angle.
    --    end_angle=360,
	--sectors = 31,
	--gap_sectors=1
    --},
    --{
    --    name='time',
    --    arg='%H.%M',
    --    max=23,
    --    bg_colour=0xcccccc,
    --    bg_alpha=0.1,
    --    fg_colour=0xcccccc,
    --    fg_alpha=0.8,
    --    x=170, y=170,
    --    radius=80,
    --    thickness=10,
    --    start_angle=0,
    --    end_angle=360
    --},
    --{
    --    name='time',
    --    arg='%M.%S',
    --    max=59,
    --    bg_colour=0xcccccc,
    --    bg_alpha=0.1,
    --    fg_colour=0xcccccc,
    --    fg_alpha=0.8,
    --    x=170, y=170,
    --    radius=70,
    --    thickness=5,
    --    start_angle=0,
    --    end_angle=360
    --},
    --{
    --    name='time',
    --    arg='%S',
    --    max=59,
    --    bg_colour=0xcccccc,
    --    bg_alpha=0.1,
    --    fg_colour=0xcccccc,
    --    fg_alpha=0.8,
    --    x=170, y=170,
    --    radius=65,
    --    thickness=3,
    --    start_angle=0,
    --    end_angle=360
    --},
    {
        name='cpu',
        arg='cpu1',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.1,
        fg_colour=0x00d317,
        fg_alpha=0.2,
        x=1173, y=285,
        radius=35,
        thickness=5,
        start_angle=60,
        end_angle=360
    },
    {
        name='cpu',
        arg='cpu2',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.1,
        fg_colour=0x00d317,
        fg_alpha=0.2,
        x=1264, y=285,
        radius=35,
        thickness=5,
        start_angle=60,
        end_angle=360
    },
    --[[{
        name='battery_percent',
        arg='BAT1',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.1,
        fg_colour=0xffffff,
        fg_alpha=0.6,
        x=165, y=170,
        radius=72,
        thickness=11,
        start_angle=122,
        end_angle=210
    }, ]]
    {
        name='memperc',
        arg='',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.1,
        fg_colour=0x00d317,
        fg_alpha=0.2,
        x=1235, y=387,
        radius=53,
        thickness=7,
        start_angle=0,
        end_angle=180
    },
    --[[{
        name='time',
        arg='%d',
        max=31,
        bg_colour=0xffffff,
        bg_alpha=0.1,
        fg_colour=0xffffff,
        fg_alpha=0.8,
        x=165, y=170,
        radius=70,
        thickness=5,
        start_angle=212,
        end_angle=360
    },
    {
        name='time',
        arg='%m',
        max=12,
        bg_colour=0xffffff,
        bg_alpha=0.1,
        fg_colour=0xffffff,
        fg_alpha=0.8,
        x=165, y=170,
        radius=76,
        thickness=5,
        start_angle=212,
        end_angle=360
    },]]
    {
        name='fs_used_perc',
        arg='/home',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.1,
        fg_colour=0x00d317,
        fg_alpha=0.2,
        x=1235, y=493,
        radius=53,
        thickness=7,
        start_angle=180,
        end_angle=360
    },
    {
        name='fs_used_perc',
        arg='/',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.1,
        fg_colour=0x00d317,
        fg_alpha=0.2,
        x=1235, y=599,
        radius=53,
        thickness=7,
        start_angle=0,
        end_angle=180
    },
    --[[{
        name='fs_used_perc',
        arg='/swap',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.1,
        fg_colour=0x00d317,
        fg_alpha=0.2,
        x=1235, y=588,
        radius=53,
        thickness=7,
        start_angle=180,
        end_angle=360
    },]]
}

require 'cairo'

function rgb_to_r_g_b(colour,alpha)
    return ((colour / 0x10000) % 0x100) / 255., ((colour / 0x100) % 0x100) / 255., (colour % 0x100) / 255., alpha
end

function draw_ring(cr,t,pt)
    local w,h=conky_window.width,conky_window.height

    local xc,yc,ring_r,ring_w,sa,ea=pt['x'],pt['y'],pt['radius'],pt['thickness'],pt['start_angle'],pt['end_angle']
    local bgc, bga, fgc, fga=pt['bg_colour'], pt['bg_alpha'], pt['fg_colour'], pt['fg_alpha']

    local angle_0=sa*(2*math.pi/360)-math.pi/2
    local angle_f=ea*(2*math.pi/360)-math.pi/2
    local t_arc=t*(angle_f-angle_0)

    -- Draw background ring

    cairo_arc(cr,xc,yc,ring_r,angle_0,angle_f)
    cairo_set_source_rgba(cr,rgb_to_r_g_b(bgc,bga))
    cairo_set_line_width(cr,ring_w)
    cairo_stroke(cr)

    -- Draw indicator ring

    cairo_arc(cr,xc,yc,ring_r,angle_0,angle_0+t_arc)
    cairo_set_source_rgba(cr,rgb_to_r_g_b(fgc,fga))
    cairo_stroke(cr)        
end

function conky_ring_stats()
    local function setup_rings(cr,pt)
        local str=''
        local value=0

        str=string.format('${%s %s}',pt['name'],pt['arg'])
        str=conky_parse(str)

        value=tonumber(str)
        if value == nil then value = 0 end
        pct=value/pt['max']

        draw_ring(cr,pct,pt)
    end

    if conky_window==nil then return end
    local cs=cairo_xlib_surface_create(conky_window.display,conky_window.drawable,conky_window.visual, conky_window.width,conky_window.height)

    local cr=cairo_create(cs)    

    local updates=conky_parse('${updates}')
    update_num=tonumber(updates)

    if update_num>5 then
        for i in pairs(settings_table) do
            setup_rings(cr,settings_table[i])
        end
    end
end

--[[ This is a script made for draw a transaprent background for conky ]]
-- Change these settings to affect your background.
-- "corner_r" is the radius, in pixels, of the rounded corners. If you don't want rounded corners, use 0.
 
corner_r=300
 
-- Set the colour and transparency (alpha) of your background.
 
bg_colour=0x000000
bg_alpha=0.2
 
function conky_draw_bg()
	if conky_window==nil then return end
	local w=conky_window.width
	local h=conky_window.height
	local cs=cairo_xlib_surface_create(conky_window.display, conky_window.drawable, conky_window.visual, w, h)
	cr=cairo_create(cs)
 
	cairo_move_to(cr,corner_r,0)
	cairo_line_to(cr,w-corner_r,0)
	cairo_curve_to(cr,w,0,w,0,w,corner_r)
	cairo_line_to(cr,w,h-corner_r)
	cairo_curve_to(cr,w,h,w,h,w-corner_r,h)
	cairo_line_to(cr,corner_r,h)
	cairo_curve_to(cr,0,h,0,h,0,h-corner_r)
	cairo_line_to(cr,0,corner_r)
	cairo_curve_to(cr,0,0,0,0,corner_r,0)
	cairo_close_path(cr)
 
	cairo_set_source_rgba(cr,rgb_to_r_g_b(bg_colour,bg_alpha))
	cairo_fill(cr)
end

function conky_main()
	--conky_draw_bg()
	conky_ring_stats()
end

