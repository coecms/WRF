#!/bin/bash
# Runs all the WPS steps for the JAN00 testcase
# Please modify as necessary.

#PBS -q express
#PBS -l walltime=25:00
#PBS -l ncpus=4
#PBS -l mem=4gb
#PBS -l wd
#PBS -l storage=scratch/$PROJECT+gdata/$PROJECT+gdata/sx70
#PBS -W umask=0022

set -eu

# ------- USER UPDATE NECESSARY
# To allow running on /scratch with the source code on /home or /g/data
# we now add the path to the WRF executables to the path.
# You can either set the WRF_ROOT variable value via the `-v` option
# for qsub or change the value below within the if statement.
if [ -z "${WRF_ROOT+x}" ]; then
    WRF_ROOT=$HOME/WRF
fi
# -------------------------------

if [ -x "${WRF_ROOT}/WPS/geogrid.exe" ]; then
    export PATH="${WRF_ROOT}/WPS:${PATH}"
else
    echo "ERROR: WPS not found"
    exit 1
fi

source ${WRF_ROOT}/build.env

geogrid.exe

link_grib.csh /g/data/sx70/data/JAN00_v4/fnl_2000012

ungrib.exe

metgrid.exe

echo "WPS finished."
