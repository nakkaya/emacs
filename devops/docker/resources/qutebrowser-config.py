# Always restore open sites when qutebrowser is reopened.
c.auto_save.session = True

# Open new tabs (middleclick/ctrl+click) in the background.
c.tabs.background = True

c.url.start_pages = ["https://google.com"]

c.url.searchengines["DEFAULT"] = "https://www.google.com/search?q={}"
c.url.searchengines["g"] = "https://www.google.com/search?q={}"

# Font
monospace = "16px 'Jet Brains Mono'"

# Font used in the completion categories.
c.fonts.completion.category = f"bold {monospace}"

# Font used in the completion widget.
c.fonts.completion.entry = monospace

# Font used for the debugging console.
c.fonts.debug_console = monospace

# Font used for the downloadbar.
c.fonts.downloads = monospace

# Font used in the keyhint widget.
c.fonts.keyhint = monospace

# Font used for error messages.
c.fonts.messages.error = monospace

# Font used for info messages.
c.fonts.messages.info = monospace

# Font used for warning messages.
c.fonts.messages.warning = monospace

# Font used for prompts.
c.fonts.prompts = monospace

# Font used in the statusbar.
c.fonts.statusbar = monospace

# Font used in the tab bar.
c.fonts.tabs = monospace

# Font used for the hints.
c.fonts.hints = "bold 16px 'Jet Brains Mono'"

