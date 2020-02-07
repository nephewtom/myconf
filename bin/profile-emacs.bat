@echo off
set file=%1
if "%file%" equ "" (
   echo file is empty
   set file="~/myconf/emacs/basic.el"   
) else (
   echo file is NOT empty
   echo The path has to use slashes, e.g.: ~/myconf/emacs/basic.el
)
echo Profiling file:%file%

set profile=~/myconf/emacs/profile-dotemacs.el

runemacs.exe -Q -l %profile% --eval "(setq profile-dotemacs-file (setq load-file-name \"%file%\"))" -f profile-dotemacs
