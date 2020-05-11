#!/bin/bash

CRTMDIR='/glade/p/work/guerrett/data/CRTM/REL-2.3.0'
ENDIAN='Big_Endian'
INST=( \
'abi_g16' 'abi_gr' 'ahi_himawari8' \
'airs281_aqua' \
'amsr2_gcom-w1' 'amsre_aqua' \
'amsua_aqua' 'amsua_metop-a' 'amsua_metop-b' 'amsua_metop-c' \
'amsua_n15' 'amsua_n16' 'amsua_n17' 'amsua_n18' 'amsua_n19' \
'amsub_n15' 'amsub_n16' 'amsub_n17' \
'atms_npp' \
'hirs2_n06' 'hirs2_n07' 'hirs2_n08' 'hirs2_n09' 'hirs2_n10' 'hirs2_n11' 'hirs2_n12' 'hirs2_n14' \
'hirs2_tirosn' 'hirs3_n15' 'hirs3_n16' 'hirs3_n17' \
'hirs4_metop-a' 'hirs4_metop-b' 'hirs4_n18' 'hirs4_n19' \
'hsb_aqua' \
'iasi616_metop-a' 'iasi616_metop-b' \
'imgr_g13' 'imgr_g14' 'imgr_g15' \
'mhs_metop-a' 'mhs_metop-b' 'mhs_metop-c' \
'mhs_n18' 'mhs_n19' \
'mwhs_fy3a' 'mwhs_fy3b' \
'mwts_fy3a' 'mwts_fy3b' \
'seviri_m08' 'seviri_m09' 'seviri_m10' \
'ssmis_f16' 'ssmis_f17' 'ssmis_f18' 'ssmis_f19' 'ssmis_f20' \
)

mkdir ODPS
for instrument in ${INST[@]}
do
   cp $CRTMDIR/fix/SpcCoeff/$ENDIAN/$instrument".Spc"* ./
   cp $CRTMDIR/fix/TauCoeff/ODPS/$ENDIAN/$instrument".Tau"* ./ODPS/
done
ln -sf ODPS/* ./
cp $CRTMDIR/fix/AerosolCoeff/$ENDIAN/* ./
cp $CRTMDIR/fix/CloudCoeff/$ENDIAN/* ./
cp $CRTMDIR/fix/EmisCoeff/*/$ENDIAN/* ./
cp $CRTMDIR/fix/EmisCoeff/*/SEcategory/$ENDIAN/* ./

