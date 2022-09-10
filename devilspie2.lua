-- somewhat helpful sites:
-- https://fransdejonge.com/2012/12/vlc-control-clone-window-with-devilspie2/
-- https://softsolder.com/2013/04/27/dual-monitors-devilspie2-ftw/
-- https://www.justingedge.com/linux/devilspie2-automatic-window-placement/
--
-- try to get matplotlib windows to appear on second monitor in some reasonable fashion
--

if (figure_next_x == nil) then figure_next_x = 0 end
if (figure_next_y == nil) then figure_next_y = 0 end

debug_print("Window Name: " .. get_window_name());
if (get_window_name():find("^Figure ") ~= nil) then
    debug_print("Application name: " .. get_application_name())
    x, y, width, height = get_window_geometry()
    set_window_geometry(1920 + figure_next_x, figure_next_y, width, height)
    figure_next_x = (figure_next_x + 35) % 500
    figure_next_y = (figure_next_y + 35) % 500
    debug_print("x " .. figure_next_x .. " " .. figure_next_y)
end
debug_print("Workspace Count: " .. get_workspace_count())

-- I want my Xfce4-terminal to the right on the second screen of my two-monitor
-- setup. (String comparison are case sensitive, please note this when
-- creating rule scripts.)
--if (get_window_name() == "Terminal") then
--   -- x,y, xsize, ysize
--   set_window_geometry(1600, 300, 900, 700);
--end

-- Make Firefox always start maximized.
--if (get_application_name() == "Firefox") then
--   maximize();
--end
