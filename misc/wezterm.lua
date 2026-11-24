local wezterm = require 'wezterm'
local act = wezterm.action
local config = wezterm.config_builder()

config.font = wezterm.font_with_fallback { 'Menlo', 'PingFang SC' }
config.font_size = 14
config.cell_width = 16 / 17
-- A ratio in WezTerm; approximate Alacritty's extra 9 Retina pixels.
config.line_height = 1.28
config.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }
config.hide_tab_bar_if_only_one_tab = true
config.default_prog = { '/bin/zsh', '-l' }
config.scrollback_lines = 10000
config.alternate_buffer_wheel_scroll_speed = 3
config.default_cursor_style = 'BlinkingBlock'
config.cursor_blink_rate = 750
config.force_reverse_video_cursor = true
config.bold_brightens_ansi_colors = false
config.use_ime = true
config.send_composed_key_when_left_alt_is_pressed = false
config.send_composed_key_when_right_alt_is_pressed = false
config.window_close_confirmation = 'NeverPrompt'
config.quick_select_alphabet = 'jfkdls;ahgurieowpq'

config.colors = {
  foreground = '#d8d8d8', background = '#181818',
  selection_fg = '#181818', selection_bg = '#d8d8d8',
  ansi = { '#181818', '#ac4242', '#90a959', '#f4bf75', '#6a9fb5', '#aa759f', '#75b5aa', '#d8d8d8' },
  brights = { '#6b6b6b', '#c55555', '#aac474', '#feca88', '#82b8c8', '#c28cb8', '#93d3c3', '#f8f8f8' },
}

config.keys = {}
local function bind(mods, key, action)
  table.insert(config.keys, { mods = mods, key = key, action = action })
end

-- Send the same control bytes as Alacritty, keeping Cmd+Q for quit and Cmd+V for paste.
for key in ('abcdefghijklmnoprstuwxyz'):gmatch('.') do
  bind('CMD', key, act.SendString(string.char(key:byte() - 96)))
end
bind('CMD', 'q', act.QuitApplication)
bind('CMD', 'v', act.PasteFrom 'Clipboard')
for _, mods in ipairs({ 'CTRL', 'CMD' }) do
  bind(mods, ';', act.SendString('\x1b[27;5;59~'))
  bind(mods, '/', act.SendString('\x1f'))
end
bind('CMD', 'LeftArrow', act.SendString('\x1b\x1fTb\x1f'))
bind('CMD|SHIFT', 'LeftArrow', act.SendString('\x1b\x1fTc\x1f'))
bind('CMD', 'RightArrow', act.SendString('\x1b\x1fUB\x1f'))
bind('CMD|SHIFT', 'RightArrow', act.SendString('\x1b\x1fUC\x1f'))
bind('CMD|SHIFT', 'n', act.SpawnWindow)
bind('CMD|SHIFT', 't', act.SpawnTab 'CurrentPaneDomain')
bind('CMD|SHIFT', 'UpArrow', act.ActivateTabRelative(-1))
bind('CMD|SHIFT', 'DownArrow', act.ActivateTabRelative(1))
bind('CMD|SHIFT', 'c', act.CopyTo 'Clipboard')
bind('CMD|SHIFT', 'v', act.PasteFrom 'Clipboard')
bind('CMD|SHIFT', '+', act.IncreaseFontSize)
bind('CMD|SHIFT', '_', act.DecreaseFontSize)
bind('CMD|SHIFT', ')', act.ResetFontSize)

config.mouse_bindings = {
  { event = { Up = { streak = 1, button = 'Left' } }, mods = 'NONE', action = act.CompleteSelection 'Clipboard' },
}
for button, action in pairs({ Left = act.IncreaseFontSize, Right = act.DecreaseFontSize, Middle = act.ResetFontSize }) do
  table.insert(config.mouse_bindings, {
    event = { Down = { streak = 1, button = button } }, mods = 'CTRL', action = action,
  })
end

wezterm.on('gui-startup', function(cmd)
  local _, _, window = wezterm.mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

return config
