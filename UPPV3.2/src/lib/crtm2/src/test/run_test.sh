#!/bin/bash
#
# $Id: run_test.sh 22707 2012-11-21 21:09:10Z paul.vandelst@noaa.gov $
#
#  NAME:
#       run_test.sh
#
#  PURPOSE:
#       Shell script file to run the CRTM Forward model example programs.
#
#  CALLING SEQUENCE:
#       run_test.sh [test type]
#
#

# -----------------------------
# Error message output function
# -----------------------------
error_message()
{
  SCRIPT_NAME="`basename $0`"
  MESSAGE=$1
  echo; echo "${SCRIPT_NAME}(ERROR): ${MESSAGE}"
}



########################################################################
#                           MAIN SCRIPT BEGINS                         #
########################################################################

TEST_TYPE=""
if [ $# -gt 0 ]; then
  TEST_TYPE="$1 "
fi

# Set up
SUCCESS=0
FAILURE=1

# Initialise overall test counters
N_ALL_PASS=0
N_ALL_FAIL=0
N_ALL_TOTAL=0

# Directory for results
RESULTS_PATH="results"

# Loop over each example
EXAMPLE_DIR=`ls -d Example*_*`
for EXAMPLE in ${EXAMPLE_DIR}; do

  # Enter the current example directory
  cd ${EXAMPLE}
  
  # Get the list of available sensors
  # ...Get the SpcCoeff file list
  SPCCOEFF_FILE=`ls coefficients/*SpcCoeff.bin 2>/dev/null`
  if [ -z "${SPCCOEFF_FILE}" ]; then
    error_message "No coefficient files found for ${EXAMPLE}. Skipping..."
    cd ..
    continue
  fi
  # ...Extract out the sensor ids
  SENSOR_ID=""
  for SPCCOEFF in ${SPCCOEFF_FILE}; do
    SENSOR_ID="${SENSOR_ID}`basename ${SPCCOEFF} .SpcCoeff.bin` "
  done

  # Check that the example codes have been compiled
  # and are executable by the user.
  if [ ! -f ${EXAMPLE} ]; then
    error_message "${EXAMPLE} executable not found. Weird."
    exit ${FAILURE}
  else
    if [ ! -x ${EXAMPLE} ]; then
      error_message "${EXAMPLE} found, but it does not have execute permission. That's even weirder."
      exit ${FAILURE}
    fi
  fi

  # Initialise individual test counters
  N_PASS=0
  N_FAIL=0
  N_TOTAL=0

  # Loop over test sensors
  echo
  echo "----------------------------------------------"
  echo "Running ${TEST_TYPE}${EXAMPLE} example"
  echo "  Sensors: ${SENSOR_ID}"
  echo "----------------------------------------------"
  for SID in ${SENSOR_ID}; do
    # Delete the sensor signal file
    SIGNALFILE="${RESULTS_PATH}/${SID}.signal"
    if [ -f ${SIGNALFILE} ]; then
      rm -f ${SIGNALFILE}
    fi
    echo
    # Run the example code
    OUTFILE="${SID}.output"
    ./${EXAMPLE} <<-NoMoreInput > ${OUTFILE}
	${SID}
	NoMoreInput
    if [ $? -ne 0 ]; then
      echo "  Run failed for the ${SID} instrument!"
      ((N_FAIL = N_FAIL + 1))
      continue
    fi
    # Check the output results
    echo "Checking results for the ${SID} instrument..."
    if [ -f ${SIGNALFILE} ]; then
      echo "  The results are the same!"
      ((N_PASS = N_PASS + 1))
      rm -f ${SIGNALFILE} > /dev/null
    else
      echo "  The results are different! Check the output in ${OUTFILE}"
      ((N_FAIL = N_FAIL + 1))
    fi
    ((N_TOTAL = N_PASS + N_FAIL))
  done

  # Accumulate overall test results
  ((N_ALL_PASS = N_ALL_PASS + N_PASS))  
  ((N_ALL_FAIL = N_ALL_FAIL + N_FAIL))
  ((N_ALL_TOTAL = N_ALL_TOTAL + N_TOTAL))

  # Output individual test comparison results
  if [ ${N_FAIL} -gt 0 ]; then
    WARNING="  <----<<<  **WARNING**"
  else
    WARNING=""
  fi
  echo
  echo "----------------------------------------------"
  echo "Summary of ${EXAMPLE} results"
  echo "----------------------------------------------"
  echo
  echo "Passed ${N_PASS} of ${N_TOTAL} tests."
  echo "Failed ${N_FAIL} of ${N_TOTAL} tests.${WARNING}"
  echo

  # Return to parent directory
  cd ..
done

# Output overall test comparison results
if [ ${N_ALL_FAIL} -gt 0 ]; then
  WARNING="  <----<<<  **WARNING**"
else
  WARNING=""
fi
echo
echo "======================"
echo "SUMMARY OF ALL RESULTS"
echo "======================"
echo
echo "Passed ${N_ALL_PASS} of ${N_ALL_TOTAL} tests."
echo "Failed ${N_ALL_FAIL} of ${N_ALL_TOTAL} tests.${WARNING}"
echo

