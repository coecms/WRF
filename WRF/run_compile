#!/bin/bash

#---------------------------------------
# For help using this script please run:
# ./run_compile -h
#---------------------------------------

#
# Default values for variables populated by command-line argument
debug=""
opt=""
chem=""
help_text=0
compile_case=em_real
architecture=79
nest=1
clean=0
test=""

# Read in command line arguments (code is a mix from https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash and man getopt example) 
SHORT=Ddsfhta:n:c
LONG=compile_case:,architecture:,nest:,help,chem,clean,netcdf4,test

PARSED=$(getopt --options $SHORT --longoptions $LONG --name "$0" -- "$@")
if [[ $? != 0 ]] ; then echo "Wrong arguments. Terminating..." >&2 ; exit 1 ; fi


eval set -- "$PARSED"

while true; do
    case "$1" in
	-D) debug=-D; shift ;;
	-d) debug=-d; shift ;;
	-s) opt=-s; shift ;;
	-f) opt=-f; shift ;;
    -t|--test) test="-W block=true"; shift ;;
	--chem) chem=1; shift ;;
	--compile_case) compile_case="$2"; shift 2 ;;
	-a|--architecture) architecture="$2"; shift 2 ;;
	-n|--nest) nest="$2"; shift 2 ;;
    -c|--clean) clean=1; shift ;;
	-h|--help) help_text=1; shift ;;
	--) shift; break ;;
	*) echo "Programming error"; exit 1 ;;
    esac
done

if [[ ${help_text} == 1 ]]; then
    echo "The optional arguments are:"

    # Architecture
    echo "-a, --architecture  followed by a number (default 79). "
    echo "    This is the first input required by configure to choose the compilation options."
    echo "    There are 8 different possibilities. "
    echo "    There are 2 optimisation options and each have 4 parallelisation options."
    echo "    The parallelisation options are:"
    echo "        serial  no parallelisation, runs on 1 processor"
    echo "        smpar   shared memory parallelisation"
    echo "        dmpar   distributed memory parallelisation"
    echo "        sm+dm   shared and distributed memory parallelisation"
    
    echo "    The architecture choices are:"
    echo "        76-79    less optimisations for Cascade Lake and Broadwell processors (-O2)"
    echo "                  (express/normal, expressbw/normalbw queues)"
    echo "                  (76=serial, 77=smpar, 78=dmpar, 79=sm+dm)"
    echo "        72-75    optimisation for Cascade Lake and Broadwell processors (-O3)"
    echo "                  (express/normal, expressbw/normalbw queues)"
    echo "                  (72=serial, 73=smpar, 74=dmpar, 75=sm+dm)"
    echo "        13-16    Intel compiler, least optimisations, slow code (no -xHOST)"
    echo "                  (13=serial, 14=smpar, 15=dmpar, 16=sm+dm)"
    echo 

    # Nest
    echo "-n, --nest  followed by a number (default 1)."
    echo "    This option defines the type of nest used by WRF"
    echo "        0   no nest"
    echo "        1   basic"
    echo "        2   preset moves"
    echo "        3   vortex following"
    echo 

    # Debugging options
    echo "-d   build with debugging information and no optimization" 
    echo "-D   build with -d AND floating traps, traceback, uninitialized variables"
    echo

    # Optimisation options
    echo "-s   build less modules with full optimisation options"
    echo "-f   (default) build WRF with default level of optimisation"
    echo

    # Chemistry
    echo "--chem  build WRF-Chem"
    echo

    # Compile case
    echo "--compile_case  followed by the WRF case to build. Default is em_real"
    echo

    # Clean option
    echo "-c, --clean   will call './clean -a' before configure and compilation"
    echo

    # Help
    echo "-h, --help  writes this help text"
    echo

    # Test
    echo "-t, --test  option for Jenkins tests only"
    exit 0
fi

# Some debugging outputs
if [[ 0 == 1 ]]; then
    echo "All the arguments values are: "
    echo "debug: "$debug
    echo "opt: "$opt
    echo "chem: "$chem
    echo "compile_case: "$compile_case
    echo "architecture: "$architecture
    echo "nest: "$nest
    echo "clean: "$clean
    echo "help_text: "${help_text}
    exit 1
fi

# Source environment
source ../build.env

#netcdf4?
if [[ $netcdf4 == 1 ]]; then
    export NETCDF4=1
fi

# Clean if asked for
if [[ $clean == 1 ]]; then
    ./clean -a
fi

# WRF now supports large files per default. Option to turn off the support.
#export WRFIO_NCD_NO_LARGE_FILE_SUPPORT=1

# Un-comment to compile with chemistry
if [[ $chem == 1 ]]; then
    export WRF_CHEM=1
    config_chem=chem
fi

# run configure
./configure $debug $opt $config_chem << EOF_configure
$architecture
$nest
EOF_configure

# Select queue depending on architecture option
QUEUE=express
NPROC=4

# Find current directory project and path
fs=`pwd -P`
if [ ${fs:1:1} = "s" ]
then
    # On scratch
    proj="+scratch/${fs:9:3}"
else
    if [ ${fs:1:1} = "g" ]
    then
        #On gdata
        proj="+gdata/${fs:8:3}"
    fi # If in home, no need to add anything
fi

# submit compile to the queue.
echo Submitting the compilation to the express queue. Allow for about 40min to finish. Use the qstat command to check status.
qsub $test -N compile_job <<EOF_compile
#!/bin/bash
#PBS -l walltime=3:30:00
#PBS -l mem=16GB
#PBS -l ncpus=$NPROC
#PBS -j oe
#PBS -q $QUEUE
#PBS -l wd
#PBS -W umask=0022
#PBS -l software=intel-compiler
#PBS -l storage=gdata/$PROJECT+scratch/$PROJECT$proj

# Set the value of compile_case
compile_case=${compile_case}

# get the environment
source ../build.env

# For multi-processors compilation
export J="-j \$PBS_NCPUS"

echo "#********************************************"
echo Start compilation of \${compile_case} for WRF.
./compile \${compile_case}
EOF_compile
