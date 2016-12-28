local naughty = require('naughty')
local beautiful = require('beautiful')
local wibox = require('wibox')

local battery = {}

local function get_file(file)
    local f = io.open(file)
    if not f then
        return nil
    end

    local res = f:read()
    f:close()
    return res
end

function battery.get_widget(adapter)
    local battery_widget = wibox.widget.textbox()
    battery_widget:set_align("right")

    local function battery_info(adapter)
        local cur = get_file("/sys/class/power_supply/"..adapter.."/charge_now")
        local cap = get_file("/sys/class/power_supply/"..adapter.."/charge_full")
        local sta = get_file("/sys/class/power_supply/"..adapter.."/status")
        if not cur or not cap or not sta then
            return nil
        end

        local battery = math.floor(cur * 100 / cap)

        local function show_info(color, dir)
            battery_widget:set_markup('<span color="' .. color .. '">' ..
                                          ' Bat: ' .. dir .. battery .. '%' .. dir .. ' </span>')
        end

        if sta:match("Discharging") then
            if battery < 10 then
                naughty.notify
                { title      = "Battery Warning"
                  , text       = "Battery low! " .. battery .. "% left!"
                  , timeout    = 5
                  , position   = "top_right"
                  , fg         = beautiful.fg_focus
                  , bg         = beautiful.bg_focus
                }
            end
            show_info(battery < 15 and 'red' or 'orange', "↓")
        elseif sta:match("Charging") then
            show_info('lightgreen', "↑")
        else
            battery_widget:set_markup("")
        end
    end

    battery_info(adapter)
    local battery_timer = timer{timeout = 20}
    battery_timer:connect_signal("timeout", function() battery_info(adapter) end)
    battery_timer:start()

    return battery_widget
end

return battery
