#!/bin/bash

BATTERY=0
BATTERY_INFO=$(acpi -b | grep "Battery ${BATTERY}")
BATTERY_STATE=$(echo "${BATTERY_INFO}" | grep -wo "Full\|Charging\|Discharging")
BATTERY_POWER=$(echo "${BATTERY_INFO}" | grep -o '[0-9]\+%' | tr -d '%')

URGENT_VALUE=10
LOW_VALUE=25
MIDDLE_VALUE=65
HIGH_VALUE=90

if [[ "${BATTERY_STATE}" = "Charging" ]]; then
echo " ${BATTERY_POWER}%"

elif [[ "${BATTERY_STATE}" = "Discharging" ]]; then
  if [[ "${BATTERY_POWER}" -le "${URGENT_VALUE}" ]]; then
    echo " ${BATTERY_POWER}%"

  elif [[ "${BATTERY_POWER}" -ge "${URGENT_VALUE}"  ]]; then
    echo " ${BATTERY_POWER}%"

  elif [[ "${BATTERY_POWER}" -ge "${LOW_VALUE}"  ]]; then
    echo " ${BATTERY_POWER}%"

  elif [[ "${BATTERY_POWER}" -ge "${MIDDLE_VALUE}"  ]]; then
    echo "  ${BATTERY_POWER}%"

  elif [[ "${BATTERY_POWER}" -ge "${HIGH_VALUE}"  ]]; then
    echo "  ${BATTERY_POWER}%"

  fi

else
  echo "${BATTERY_POWER}%"
  echo "${BATTERY_POWER}%"
  echo ""
fi
