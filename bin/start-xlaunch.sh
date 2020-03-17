echo "Check if vcxsrv is running..."
cmd.exe /c tasklist.exe /FI "IMAGENAME eq vcxsrv.exe" | grep vcxsrv
if [[ $? -ne 0 ]]; then
    echo "X11 Server not found. Starting VcXsrv now..."
    xlaunch.exe -run ../config.xlaunch
    # vcxsrv.exe :0 -ac -terminate -lesspointer -multiwindow -clipboard -wgl -dpi auto
else
    echo "X11 Server already running!"
fi

