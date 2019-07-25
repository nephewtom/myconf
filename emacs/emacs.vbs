'I keep this file in C:\Users\etomort
'And create a shortcut like the one in shortcut-for-EmacsWSL.png

'Pages I had a look at:
'https://blog.ropnop.com/configuring-a-pretty-and-usable-terminal-emulator-for-wsl/
'https://stackoverflow.com/questions/41225711/wsl-run-linux-from-windows-without-spawning-a-cmd-window

args = "-c" & " -l " & """DISPLAY=:0 emacs"""
WScript.CreateObject("Shell.Application").ShellExecute "bash", args, "", "open", 0
