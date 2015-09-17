-- volume widget
volume_widget = widget({ type = "textbox", name = "tb_volume", align = "right" })

function update_volume(widget)
    local fd = io.popen("amixer sget Master")
    local status = fd:read("*all")
    fd:close()
    
    local volume = tonumber(string.match(status, "(%d?%d?%d)%%"))
    volume = string.format("%02d", volume)

    status = string.match(status, "%[(o[^%]]*)%]")

    if string.find(status, "on", 1, true) then
        volume = volume .. "%"
    else
        volume = volume .. "M"
    end
    widget.text = "V " .. volume
end

update_volume(volume_widget)
awful.hooks.timer.register(1, function () update_volume(volume_widget) end)

-- weather widget
weatherwidget = widget({type = "textbox" })
vicious.register(weatherwidget, vicious.widgets.weather, '${tempf}F', 307, "KLMO")
 
separator = widget({ type = "textbox" })
separator.text = " :: "

cpuwidget = widget({ type = "textbox" })
vicious.register(cpuwidget, vicious.widgets.cpu,
    function (widget ,args) 
        if args[3] == nil then
            return ("CPU %02d%%"):format(args[2])
        else
            return ("CPU %02d%% %02d%%"):format(args[2],args[3])
        end
    end)

    --return 

