
local utils = require("utils")

local kbd = {}
local dbus_switch = "dbus-send --dest=ru.gentoo.KbddService /ru/gentoo/KbddService ru.gentoo.kbdd.set_layout uint32:"

function kbd.set_layout(number)
    utils.run(dbus_switch..number)
--    utils.run("qdbus ru.gentoo.KbddService /ru/gentoo/KbddService  ru.gentoo.kbdd.set_layout "..number)
end

function kbd.switcher(layouts)
    local cur = 1
    return function()
        cur = cur - math.floor(cur/#layouts)*#layouts + 1
        kbd.set_layout(layouts[cur])
    end
end

return kbd

