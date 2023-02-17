
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
c.colors.webpage.preferred_color_scheme = 'dark'

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

c.tabs.tabs_are_windows = False
c.tabs.show = 'never'
c.tabs.last_close = 'close'

c.fonts.default_family = '"JetBrains Mono"'
c.fonts.default_size = '11pt'
c.fonts.completion.entry = '11pt "JetBrains Mono"'
c.fonts.debug_console = '11pt "JetBrains Mono"'
c.fonts.prompts = 'default_size sans-serif'
c.fonts.statusbar = '11pt "JetBrains Mono"'

# bug in qtwebengine not yet fixed in guix version
c.qt.args=['disable-seccomp-filter-sandbox']

config.bind('xb', 'config-cycle statusbar.show always never')
config.bind('xt', 'config-cycle tabs.show always never')
config.bind('xx', 'config-cycle statusbar.show always never;; config-cycle tabs.show always never')
config.bind('M', 'hint links spawn mpv {hint-url}')

config.bind('<Ctrl-g>', 'clear-keychain ;; search ;; fullscreen --leave')
config.bind('<Ctrl-g>', 'mode-leave', mode='insert')
config.bind('<Ctrl-g>', 'mode-leave', mode='command')
config.bind('<Ctrl-g>', 'mode-leave', mode='prompt')
config.bind('<Ctrl-g>', 'mode-leave', mode='hint')
config.bind('<Ctrl-g>', 'mode-leave', mode='caret')

config.bind('<Ctrl-j>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-k>', 'completion-item-focus --history prev', mode='command')
  
# config.source('themes/nord-qutebrowser.py')

config.load_autoconfig()
