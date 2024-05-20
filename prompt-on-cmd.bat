:: This is to set a nice prompt on CMD with path in one line
:: and the prompt on next line with > and a space

:: https://stackoverflow.com/questions/12028372/how-do-i-change-the-command-line-prompt-in-windows
:: $P    Current drive and path
:: $_    Carriage return and linefeed
:: $G    > (greater-than sign)
:: $S      (space)
set PROMPT=$P$_$G$S

:: And these to open a new split tab on same directory
:: https://learn.microsoft.com/en-us/windows/terminal/tutorials/new-tab-same-directory
set PROMPT=$e]9;9;$P$e\%PROMPT%
setx PROMPT "%PROMPT%"
