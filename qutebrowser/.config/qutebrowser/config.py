
c.content.blocking.method = 'both'
c.content.cookies.accept = 'no-3rdparty'
c.content.cookies.store = True

c.aliases = {
    'q': 'close',
    'qa': 'quit',
    'w': 'session-save',
    'wq': 'quit --save',
    'wqa': 'quit --save'
}

c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.policy.images = 'never'

# c.url.default_page = 'about:blank'

c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.com/?q={}',
    'ddg': 'https://duckduckgo.com/?q={}',
    'aw': 'https://wiki.archlinux.org/?search={}',
    'goog': 'https://www.google.com/search?q={}',
    'wiki': 'https://en.wikipedia.org/wiki/{}',
    'syt': 'https://www.youtube.com/results?search_query={}',
    'cyt': 'https://www.youtube.com/c/{}/videos',
    'enit': 'https://www.wordreference.com/enit/{}',
    'iten': 'https://www.wordreference.com/iten/{}'
}

c.statusbar.show = 'always'

c.tabs.tabs_are_windows = True
c.tabs.show = 'never'
c.tabs.last_close = 'close'

c.fonts.default_family = '"JetBrains Mono"'
c.fonts.default_size = '11pt'
c.fonts.completion.entry = '11pt "JetBrains Mono"'
c.fonts.debug_console = '11pt "JetBrains Mono"'
c.fonts.prompts = 'default_size sans-serif'
c.fonts.statusbar = '11pt "JetBrains Mono"'

config.bind('xb', 'config-cycle statusbar.show always never')
config.bind('xt', 'config-cycle tabs.show always never')
config.bind('xx', 'config-cycle statusbar.show always never;; config-cycle tabs.show always never')
config.bind('M', 'hint links spawn mpv {hint-url}')


config.bind('<Ctrl-g>', 'leave-mode', mode='insert')
config.bind('<Ctrl-g>', 'leave-mode', mode='command')
config.bind('<Ctrl-g>', 'leave-mode', mode='prompt')
config.bind('<Ctrl-g>', 'leave-mode', mode='hint')

config.bind('<Ctrl-j>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-k>', 'completion-item-focus --history prev', mode='command')
  
config.load_autoconfig()
