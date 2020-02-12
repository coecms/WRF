# WRF
WRF ported on NCI machine: Gadi

Installing
==========
You can install WRF on any filesystem on Gadi: /home, /g/data or /scratch.
Keep in mind /scratch is purged regularly. It isn't a problem if you are using
WRF without modifications (easy to reinstall from here) but you would need to 
save any code modifications on a different filesystem (or Github etc.)
To use on Gadi, run:
```
     git clone -b <VERSION> https://github.com/coecms/WRF.git
```
or
```
    git clone -b <VERSION> git@github.com:coecms/WRF.git      
```
where version is the WRF version you want to use.

Versions currently available:
=============================
* V4.1.3
* V4.1.2
* V4.1.1
* V4.0.2
* V3.9.1.1
* V3.9
* V3.7.1
* V3.6.1
* V3.6

Building WRF (ARW)
==================
Go to the WRFV3/ subdirectory, run:
```
cd WRF/WRFV3/
```
There is a *run_compile* script to help you configure and compile WRF. This script by default will compile the WRFV3 software for a real experiment, with some speed optimisations for the normal and express queues and with basic nesting. There is a range of command-line arguments accepted by the script. These are the command-line arguments accepted by the *configure* script, an option to clean everything prior to compilation, the choice of compilation flags and nesting type for the *configure* script and finally the compilation option for the *compile* script. You can access a full list and description by running:
```
./run_compile -h
```
To configure and compile WRFV3 with the default options, you simply run:
```
./run_compile
```
If you want more aggressive optimisation options on Cascade Lake nodes, refer to the help screen of run_compile for the version you are using. Note, those options will result in a faster code at the cost of reproducibility of the results.

The configure step will be run interactively with the output on your screen, then the compilation step will be submitted to the express queue automatically.

After a successful build, one should see the following executable files for WRF (the ARW core):
```
[abc123@gadi WRFV3]$ ls -l main/*.exe

-rwx------ 1 abc123 wrf 35559897 Aug 22 17:14 main/ndown.exe
-rwx------ 1 abc123 wrf 35471615 Aug 22 17:14 main/real.exe
-rwx------ 1 abc123 wrf 35081246 Aug 22 17:14 main/tc.exe
-rwx------ 1 abc123 wrf 38763058 Aug 22 17:13 main/wrf.exe
```

**Tip**: if you are only running an idealised case and you are getting tired of specifying the option on the command-line, you can:
1. change the default value of compile_case at the start of the run_compile script OR
2. create an alias for the command in your .bashrc or .cshrc file (it is a good idea to change the name of the command for the alias so it is clear it is an alias if you need help later on):
bash syntax:
```
alias my_compile='run_compile --compile_case <replace with the compile option here>'
```
csh/tcsh syntax:
```
alias my_compile 'run_compile --compile_case <replace with the compile option here>'
```
Building WPS
============
NOTE: because of current unavailability of some software on Gadi, not all executables are built. This will be solved when the software needed are made available.

Go to the WPS/ subdirectory, run:
```
cd WRF/WPS/
```
There is another *run_compile* script to help with configuring and compiling WPS. You can again choose the configure input from the command-line option as well as to clean a previous compilation of WPS. The default is to compile with GRIB2 support and a distributed memory code. See the inline help for *run_compile* for a full explanation:
```
./run_compile -h
```

To configure and compile WPS with the default options, you simply run:
```
./run_compile
```
A file configure.wps should be created after configuring has completed.

After a successful build, one should see the following executable files:
```
[abc123@gadi WPS]$ ls -l *.exe

lrwxrwxrwx 1 abc123 wrf 23 Aug 23 10:19 geogrid.exe -> geogrid/src/geogrid.exe
lrwxrwxrwx 1 abc123 wrf 23 Aug 23 10:20 metgrid.exe -> metgrid/src/metgrid.exe
lrwxrwxrwx 1 abc123 wrf 21 Aug 23 10:20 ungrib.exe -> ungrib/src/ungrib.exe

[abc123@gadi WPS]$ ls -l */src/*.exe

-rwx------ 1 abc123 wrf 3412782 Aug 23 10:19 geogrid/src/geogrid.exe
-rwx------ 1 abc123 wrf 3206974 Aug 23 10:20 metgrid/src/metgrid.exe
-rwx------ 1 abc123 wrf 1209622 Aug 23 10:20 ungrib/src/g1print.exe
-rwx------ 1 abc123 wrf 1407507 Aug 23 10:20 ungrib/src/g2print.exe
-rwx------ 1 abc123 wrf 2169010 Aug 23 10:20 ungrib/src/ungrib.exe
-rwx------ 1 abc123 wrf 1184660 Aug 23 10:20 util/src/avg_tsfc.exe
-rwx------ 1 abc123 wrf 1250303 Aug 23 10:20 util/src/calc_ecmwf_p.exe
-rwx------ 1 abc123 wrf 1217842 Aug 23 10:20 util/src/height_ukmo.exe
-rwx------ 1 abc123 wrf 953821 Aug 23 10:20 util/src/int2nc.exe
-rwx------ 1 abc123 wrf 1118699 Aug 23 10:20 util/src/mod_levs.exe
-rwx------ 1 abc123 wrf 3704627 Aug 23 10:20 util/src/plotfmt.exe
-rwx------ 1 abc123 wrf 3409302 Aug 23 10:20 util/src/plotgrids.exe
-rwx------ 1 abc123 wrf 916613 Aug 23 10:20 util/src/rd_intermediate.exe
```
Note that there are some warnings in the build output like "warning: overriding commands for target '.c.o'." These are harmless and can be ignored.

Tests run
=========
## Compilation
* WRF compilation with dmpar
* WRF compilation with dm+sm
* WPS compilation with distributed memory with dm+sm compilation of WRF
## Tests simulations
Tests simulations have been done on [Jenkins](https://accessdev.nci.org.au/jenkins/job/WRF/job/WRF-Core/). Tests simulations were performed for WRF version 4 and over. The simulations were:
* First tutorial case (Jan 00) with 1 nest
* First tutorial case (Jan 00) with 2 nests
* First tutorial case (Jan 00) with additional diagnostics turned on.
* First tutorial case (Jan 00) with quiting I/O.
* First tutorial case (Jan 00) with restart
WRF outputs for these tests can be found under /projects/WRF/data/KGO/. The inputs for the tests can be found in https://github.com/coecms/wrf-testing
Note: wps-era is currently being tested for WRF v4.1.1 compatibility to create boundary conditions from ERA-Interim.
