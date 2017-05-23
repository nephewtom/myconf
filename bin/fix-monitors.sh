#!/bin/bash
# -------------------------------------------------
#  Get monitors configuration from monitor.xml and apply it for current user session.
#
#  See http://bernaerts.dyndns.org/linux/74-ubuntu/309-ubuntu-dual-display-monitor-position-lost
# -------------------------------------------------

# monitor.xml path
#MONITOR_XML="$HOME/.config/monitors.xml"
MONITOR_XML="$HOME/bin/monitors.xml"

echo "Info from" $MONITOR_XML
echo "--------------------------------------------"

# get number of declared monitors
NUM=$(xmllint --xpath 'count(//monitors/configuration['1']/output)' $MONITOR_XML)
echo "NUM:"$NUM

# loop thru declared monitors to create the command line parameters
for (( i=1; i<=$NUM; i++)); do
  # get attributes of current monitor (name and x & y positions)
  NAME=$(xmllint --xpath 'string(//monitors/configuration['1']/output['$i']/@name)' $MONITOR_XML 2>/dev/null)
  POS_X=$(xmllint --xpath '//monitors/configuration['1']/output['$i']/x/text()' $MONITOR_XML 2>/dev/null)
  POS_Y=$(xmllint --xpath '//monitors/configuration['1']/output['$i']/y/text()' $MONITOR_XML 2>/dev/null)
  ROTATE=$(xmllint --xpath '//monitors/configuration['1']/output['$i']/rotation/text()' $MONITOR_XML 2>/dev/null)
  WIDTH=$(xmllint --xpath '//monitors/configuration['1']/output['$i']/width/text()' $MONITOR_XML 2>/dev/null)
  HEIGHT=$(xmllint --xpath '//monitors/configuration['1']/output['$i']/height/text()' $MONITOR_XML 2>/dev/null)
  RATE=$(xmllint --xpath '//monitors/configuration['1']/output['$i']/rate/text()' $MONITOR_XML 2>/dev/null)
  PRIMARY=$(xmllint --xpath '//monitors/configuration['1']/output['$i']/primary/text()' $MONITOR_XML 2>/dev/null)

  # if position is defined for current monitor, add its position and orientation to command line parameters
  if [ -n "$POS_X" ]; then
      PARAM_ARR=("${PARAM_ARR[@]}" "--output" "$NAME" "--pos" "${POS_X}x${POS_Y}" "--mode" "${WIDTH}x${HEIGHT}" "--rate" "$RATE" "--rotate" "$ROTATE")
      echo "name:$NAME | pos_x:$POS_X, pos_y:$POS_Y | width:$WIDTH, height:$HEIGHT rate:$RATE | primary:$PRIMARY"
  fi

  # if monitor is defined as primary, adds it to command line parameters
  # [ "$PRIMARY" = "yes" ] && PARAM_ARR=("${PARAM_ARR[@]}" "--primary")
done

echo -e "\nExecuting xrandr to fix monitors and resolutions:"
echo -n "xrandr "
for i in "${PARAM_ARR[@]}"; do
    echo -n "$i "
done
echo; echo

xrandr "${PARAM_ARR[@]}"

echo "This is what I used in the past"
echo "xrandr --output VGA1 --pos 1680x0 --mode 1280x1024 --rate 75"
echo "xrandr --output HDMI2 --pos 0x0 --mode 1680x1050 --rate 60 --rotate normal --primary"
