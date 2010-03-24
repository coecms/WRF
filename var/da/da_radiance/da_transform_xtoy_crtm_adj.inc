subroutine da_transform_xtoy_crtm_adj ( cv_size, cv, iv, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! PURPOSE: transform gradient from obs space to model grid space.
   !
   ! METHOD:  jo_grad_x = H^T jo_grad_y =  - H^T R^-1 ( d - H delta_x )
   !           1. input gradient in obs space and reference state of RTTOV
   !           2. call adjoint of RTM
   !           3. adjoint of interpolation from model grid to obs loc
   !
   !  HISTORY: 11/19/2006 - Creation                       Zhiquan Liu
   !           01/11/2008 - Add crtm_cloud                 Xiaoyan Zhang
   !           11/25/2008 - Zero Jacobian for top level    Tom Auligne
   !
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)            :: cv_size         ! Size of cv array.
   real, intent(inout)            :: cv(1:cv_size)   ! control variables.
   type (x_type), intent(inout)   :: jo_grad_x ! 
   type (y_type),  intent(in)     :: jo_grad_y ! H' delta_x
   type (iv_type), intent(in)     :: iv        ! O-B structure.

   integer, parameter             :: AIRS_Max_Channels = 281
   
#ifdef CRTM

   integer                        :: icld, jcld, i
   integer                        :: k  ! Index dimension.
   integer                        :: num_rad  ! Number of radiance obs
   integer                        :: inst, nchanl, n
   integer                        :: ipred, npred, gammapred, id
   real                           :: cv_local(1:cv_size)
   real, allocatable              :: q_ad(:,:)
   real, allocatable              :: t_ad(:,:)
   real, allocatable              :: p_ad(:)
   real, allocatable              :: xb_q(:,:)

!! for crtm_cloud
   real, allocatable              :: qcw(:,:)
   real, allocatable              :: qci(:,:)
   real, allocatable              :: qrn(:,:)
   real, allocatable              :: qsn(:,:)
   real, allocatable              :: qgr(:,:)

!   type(infa_type), pointer       :: info                
!   integer                        :: i,j
!   real                           :: dx,dy,dxm,dym


   ! CRTM local varaibles and types
   integer :: wmo_sensor_id, Error_Status, Allocate_Status
   type (CRTM_RTSolution_type ), allocatable :: RTSolution(:,:),RTSolution_AD(:,:)
   type (CRTM_Atmosphere_type ), allocatable :: Atmosphere(:), Atmosphere_AD(:)
   type (CRTM_Surface_type ),    allocatable :: Surface(:), Surface_AD(:)
   type (CRTM_GeometryInfo_type ), allocatable :: GeometryInfo(:)
   type (CRTM_Options_type ) , allocatable   :: Options(:)
   
   integer                        :: ts_index
   integer                        :: nclouds, ncv
   real, allocatable              :: cc_ad(:)
   real*8                         :: rad_clr            ! RT clear/cloudy radiances
   real, allocatable              :: rad_ovc(:)         ! RT overcast radiances
   real*8                         :: rad_ad, tb_ad

   ! Initializations for AIRS (MMR) Cloud Detection
   integer                        :: Band_Size(5), Bands(AIRS_Max_Channels,5) 
  
      Band_Size(1:5) = (/86, 0, 0, 16, 0 /)
      Bands(:,:)     = 0  
      Bands(1:Band_Size(1),1) = &
&    (/                                                 &              !&      1,   6,   7,  10,  11,  15,  16,  17,  20,  21, &
&                                                       &              !&     22,  24,  27,  28,  30,  36,  39,  40,  42,  51, &
&                                                       &              !&     52,  54,  55,  56,  59,  62,  63,  68,  69,  71, &
&                                                       &              !&     72,  73,  74,  75,  76,  77,  78,  79,  80,  82, &
&                     92,  93,  98,  99, 101, 104, 105, &              !&     83,  84,  86,  92,  93,  98,  99, 101, 104, 105, &
&     108, 110, 111, 113, 116, 117, 123, 124, 128, 129, &
&     138, 139, 144, 145, 150, 151, 156, 157, 159, 162, &
&     165, 168, 169, 170, 172, 173, 174, 175, 177, 179, &
&     180, 182, 185, 186, 190, 192,      198, 201, 204, &              !&     180, 182, 185, 186, 190, 192, 193, 198, 201, 204, &
&     207, 210,      215, 216,      221,      226, 227, &              !&     207, 210, 213, 215, 216, 218, 221, 224, 226, 227, &
&     232,                     252, 253, 256, 257, 261, &              !&     232, 239, 248, 250, 251, 252, 253, 256, 257, 261, &
&     262, 267, 272, 295, 299,      305,           310, &              !&     262, 267, 272, 295, 299, 300, 305, 308, 309, 310, &
&          321, 325, 333, 338, 355, 362, 375, 453, 475, &              !&     318, 321, 325, 333, 338, 355, 362, 375, 453, 475, &
&     484, 497, 528, 587, 672, 787, 791, 843, 870, 914, &
&     950 /)

!      Bands(1:Band_Size(2),2) = &
!&    (/ 1003, 1012, 1019, 1024, 1030, 1038, 1048, 1069, 1079, 1082,  &
!&       1083, 1088, 1090, 1092, 1095, 1104, 1111, 1115, 1116, 1119,  &
!&       1120, 1123, 1130, 1138, 1142, 1178, 1199, 1206, 1221, 1237,  &
!&       1252, 1260, 1263, 1266, 1278, 1285 /)

!      Bands(1:Band_Size(3),3) = &
!&    (/       1301, 1304, 1329, 1371, 1382, 1415, 1424, 1449, 1455, &  !&    1290, 1301, 1304, 1329, 1371, 1382, 1415, 1424, 1449, 1455, &  
!&       1466,       1477,             1500, 1519,       1538, 1545, &  !&    1466, 1471, 1477, 1479, 1488, 1500, 1519, 1520, 1538, 1545, &  
!&       1565, 1574, 1583, 1593,       1627, 1636,       1652, 1669, &  !&    1565, 1574, 1583, 1593, 1614, 1627, 1636, 1644, 1652, 1669, & 
!&                   1694, 1708,       1723, 1740, 1748,       1756, &  !&    1674, 1681, 1694, 1708, 1717, 1723, 1740, 1748, 1751, 1756, &
!&             1766, 1771, 1777,       1783, 1794, 1800,       1806, &  !&    1763, 1766, 1771, 1777, 1780, 1783, 1794, 1800, 1803, 1806, &
!&             1826, 1843  /)                                           !&    1812, 1826, 1843  /)

      Bands(1:Band_Size(4),4) = &
&    (/ 1852, 1865, 1866,       1868, 1869, 1872, 1873,       1876, &  !&    1852, 1865, 1866, 1867, 1868, 1869, 1872, 1873, 1875, 1876, 
&             1881, 1882, 1883,                   1911, 1917, 1918, &  !&    1877, 1881, 1882, 1883, 1884, 1897, 1901, 1911, 1917, 1918, &
&                   1924, 1928        /)                               !&    1921, 1923, 1924, 1928, 1937  /)   

!      Bands(1:Band_Size(5),5) = &
!&    (/ 1938, 1939, 1941, 1946, 1947, 1948, 1958, 1971, 1973, 1988, &
!&       1995, 2084, 2085, 2097, 2098, 2099, 2100, 2101, 2103, 2104, &
!&       2106, 2107, 2108, 2109, 2110, 2111, 2112, 2113, 2114, 2115, &
!&       2116, 2117, 2118, 2119, 2120, 2121, 2122, 2123, 2128, 2134, &
!&       2141, 2145, 2149, 2153, 2164, 2189, 2197, 2209, 2226, 2234, &
!&       2280, 2318, 2321, 2325, 2328, 2333, 2339, 2348, 2353, 2355, &
!&       2363, 2370, 2371, 2377  /)  


!---------------------------------------------------------

   if ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_crtm_adj")
   
   cv_local(:) = 0.0
   
   sensors_loop: do inst = 1, iv%num_inst                 ! loop for sensor

      num_rad = iv%instid(inst)%info%n2 - iv%instid(inst)%info%n1 + 1
      if ( num_rad < 1 ) cycle

      allocate (Atmosphere   (iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                Atmosphere_AD(iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting Atmosphere"/))
      end if
      allocate (Surface      (iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                Surface_AD   (iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting Surface"/))
      end if
      allocate (GeometryInfo (iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting GeometryInfo"/))
      end if
      allocate (Options      (iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting Options"/))
      end if

!----------------------------------------------------------------------------
! CRTM allocation
!
! Atmosphere structure
!
      do n = iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel
         Atmosphere(n)%n_Layers=(kte-kts)+1   ! number of vertical levels
         Atmosphere(n)%n_Absorbers=2
         Atmosphere(n)%n_Clouds=0
         Atmosphere(n)%n_Aerosols=0
         if (crtm_cloud) Atmosphere(n)%n_Clouds=6

         Error_Status = CRTM_Allocate_Atmosphere( Atmosphere(n)%n_Layers, &
                                                  Atmosphere(n)%n_Absorbers, &
                                                  Atmosphere(n)%n_Clouds, &
                                                  Atmosphere(n)%n_Aerosols, &
                                                  Atmosphere(n))
         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
              (/"Error in allocatting CRTM Atmosphere Structure"/))
         end if
         Atmosphere(n)%Absorber_ID(1)=H2O_ID
         Atmosphere(n)%Absorber_ID(2)=O3_ID
         Atmosphere(n)%Climatology=crtm_atmosphere

         if (crtm_cloud) then
            Atmosphere(n)%Cloud(1)%Type=WATER_CLOUD
            Atmosphere(n)%Cloud(2)%Type=ICE_CLOUD
            Atmosphere(n)%Cloud(3)%Type=RAIN_CLOUD
            Atmosphere(n)%Cloud(4)%Type=SNOW_CLOUD
            Atmosphere(n)%Cloud(5)%Type=GRAUPEL_CLOUD
            Atmosphere(n)%Cloud(6)%Type=HAIL_CLOUD
         end if
      end do

!-------------------------------------------------------------------------------

      allocate (q_ad(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      allocate (t_ad(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      allocate (p_ad(iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      allocate (xb_q(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))

      if (crtm_cloud) then
         allocate (qcw(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qci(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qrn(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qsn(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qgr(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      end if
      
      q_ad = 0.0
      t_ad = 0.0
      p_ad = 0.0
      xb_q = 0.0

!      info => iv%instid(inst)%info

  ! CRTM channel information structure
      ! Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptor(inst),ChannelInfo)
      ! if ( Error_Status /= 0) then
      !    call da_error(__FILE__,__LINE__, &
      !     (/"Error in calling CRTM_Set_ChannelInfo"/))
      ! end if
      nchanl    = ChannelInfo(inst)%n_channels
 
  ! Allocate forward model solution RTSolution array to number of channels
      allocate( RTSolution( ChannelInfo(inst)%n_Channels, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2 )   , &
                RTSolution_AD( ChannelInfo(inst)%n_Channels, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
               STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
                      (/"Error in allocatting RTSolution"/))
      END IF
 
  ! CRTM Surface Structure
      if (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsua') then
         wmo_sensor_id=WMO_AMSUA
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsub') then
         wmo_sensor_id=WMO_AMSUB
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsre') then
         wmo_sensor_id=WMO_AMSRE
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='ssmi') then
         wmo_sensor_id=WMO_SSMI
#ifndef CRTM_1_1
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='ssmis') then
         wmo_sensor_id=WMO_SSMIS   ! available since CRTM_1_2
#endif
      else
         wmo_sensor_id=INVALID_WMO_SENSOR_ID
      end if
 
      do n =  iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel

         Error_Status = CRTM_Allocate_Surface( nchanl,     &  ! Input
                                      Surface(n))  ! Output
         if ( Error_Status /= 0 ) then
           call da_error(__FILE__,__LINE__, &
             (/"Error in allocatting CRTM Surface Structure"/))
         end if
 
         ! CRTM Options structure
         Options(n)%n_channels = nchanl
         if ( use_antcorr(inst) ) Options(n)%Antenna_Correction = 1   ! SET = 1 in CRTM_Parameters.f90
         Error_Status = CRTM_Allocate_Options( nchanl,    &  ! Input
                                               Options(n))      ! Output
         if ( Error_Status /= 0 ) then
           call da_error(__FILE__,__LINE__, &
             (/"Error in allocatting CRTM Options Structure"/))
         endif

         ! Gamma correction from VarBC
         !----------------------------
#ifdef CRTM_MODIF
         if (use_varbc) then
           gammapred = iv%instid(inst)%varbc_info%gammapred 
           do k = 1, nchanl
              npred = iv%instid(inst)%varbc(k)%npred
              if (npred <= 0) cycle                                           ! VarBC channels only
              if (iv%instid(inst)%varbc_info%npredmax < gammapred)  cycle     ! Gamma channels only
              if (iv%instid(inst)%varbc(k)%pred_use(gammapred) < 0) cycle     ! Gamma channels only
              do ipred = 1, npred
                 if (iv%instid(inst)%varbc(k)%ipred(ipred) /= gammapred) cycle
                 RTSolution(k,n)%Gamma = iv%instid(inst)%varbc(k)%param(ipred)
              end do
           end do
         end if
         RTSolution_AD(:,n)%Gamma    = 0.0
#endif
      end do

      do n =  iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel

         ! [1.0] Extract base state Atmosphere variables
         Atmosphere(n)%level_pressure(0) = iv%instid(inst)%pf(0,n)
         do k=1,Atmosphere(n)%n_layers
            Atmosphere(n)%pressure(k) = iv%instid(inst)%pm(k,n)
            Atmosphere(n)%level_pressure(k) = iv%instid(inst)%pf(k,n)
            Atmosphere(n)%temperature(k) = iv%instid(inst)%tm(k,n)
            Atmosphere(n)%absorber(k,1) = iv%instid(inst)%qm(k,n)
            xb_q(k,n) = 0.001*iv%instid(inst)%qm(k,n)/(1.0+iv%instid(inst)%qm(k,n)) ! specific humidity
         end do

         if (crtm_cloud) then
            do k=1,Atmosphere(n)%n_layers
               Atmosphere(n)%cloud(1)%water_content(k)=iv%instid(inst)%qcw(k,n)
               Atmosphere(n)%cloud(2)%water_content(k)=iv%instid(inst)%qci(k,n)
               Atmosphere(n)%cloud(3)%water_content(k)=iv%instid(inst)%qrn(k,n)
               Atmosphere(n)%cloud(4)%water_content(k)=iv%instid(inst)%qsn(k,n)
               Atmosphere(n)%cloud(5)%water_content(k)=iv%instid(inst)%qgr(k,n)
               Atmosphere(n)%cloud(6)%water_content(k)=iv%instid(inst)%qhl(k,n)
               Atmosphere(n)%cloud(1)%effective_radius(k)=iv%instid(inst)%rcw(k,n)
               Atmosphere(n)%cloud(2)%effective_radius(k)=iv%instid(inst)%rci(k,n)
               Atmosphere(n)%cloud(3)%effective_radius(k)=iv%instid(inst)%rrn(k,n)
               Atmosphere(n)%cloud(4)%effective_radius(k)=iv%instid(inst)%rsn(k,n)
               Atmosphere(n)%cloud(5)%effective_radius(k)=iv%instid(inst)%rgr(k,n)
               Atmosphere(n)%cloud(6)%effective_radius(k)=iv%instid(inst)%rhl(k,n)
            end do
         end if

         ! [1.1] User-supplied emissivity
         ! Options%emissivity_switch = 1
         ! Options%emissivity(1:Options%n_channels) = &
         !     iv%instid(inst)%emiss(1:Options%n_channels,n)

         ! [1.1] CRTM Surface parameter data
         Surface(n)%Land_Coverage=iv%instid(inst)%land_coverage(n)
         Surface(n)%Water_Coverage=iv%instid(inst)%water_coverage(n)
         Surface(n)%Snow_Coverage=iv%instid(inst)%snow_coverage(n)
         Surface(n)%Ice_Coverage=iv%instid(inst)%ice_coverage(n)

         if (Surface(n)%Land_Coverage > 0.0) then
            Surface(n)%Land_Type=GRASS_SOIL           ! land type (User guide appendix 3)
            Surface(n)%Land_Temperature=iv%instid(inst)%ts(n)      ! K
            Surface(n)%Soil_Moisture_Content=iv%instid(inst)%smois(n)  !0.05    ! volumetric water content (g/cm**3)
            !Surface(n)%Canopy_Water_Content=0.05      ! gravimetric water content
            Surface(n)%Vegetation_Fraction=iv%instid(inst)%vegfra(n)
            Surface(n)%Soil_Temperature=iv%instid(inst)%tslb(n)
         end if
         if (Surface(n)%Water_Coverage > 0.0) then
            !Surface(n)%Water_Type=SEA_WATER          ! (Currently NOT used)
            Surface(n)%Water_Temperature=iv%instid(inst)%ts(n)     ! K
            Surface(n)%Wind_Speed=sqrt((iv%instid(inst)%u10(n))**2+ &
                               (iv%instid(inst)%v10(n))**2)  ! m/sec
            !surface(n)%Wind_Direction=0.0            ! NOT used
            Surface(n)%Salinity=33.0                  ! ppmv
         end if
         if (Surface(n)%Snow_Coverage > 0.0) then
            Surface(n)%Snow_Type=NEW_SNOW             ! User guide appendix 3
            Surface(n)%Snow_Temperature=iv%instid(inst)%ts(n)      ! K
            Surface(n)%Snow_Depth=iv%instid(inst)%snowh(n)         ! mm
            !Surface(n)%Snow_Density=0.2               ! g/cm**3
            !Surface(n)%Snow_Grain_Size=2.0            ! mm
         end if
         if (Surface(n)%Ice_Coverage > 0.0) then
            !Surface(n)%Ice_Type=FRESH_ICE             ! NO Table offered, single example is FRESH_ICE
            Surface(n)%Ice_Temperature=iv%instid(inst)%ts(n)       ! K
            Surface(n)%Ice_Thickness=10.0              ! mm
            !Surface(n)%Ice_Density=0.9                ! g/cm**3
            !Surface(n)%Ice_Roughness=0.0               ! NO Table offered, single example is ZERO
         end if
         Surface(n)%SensorData%n_channels = nchanl
#ifdef CRTM_1_1
         Surface(n)%SensorData%Sensor_Id  = wmo_sensor_id             ! CRTM_1.1
#else
         Surface(n)%SensorData%Select_WMO_Sensor_Id  = wmo_sensor_id  ! CRTM_1.2
#endif
         Surface(n)%SensorData%Tb(1:nchanl) = iv%instid(inst)%tb_inv(1:nchanl,n) + &
                                              iv%instid(inst)%tb_xb(1:nchanl,n)
     
         ! -- Copy the adjoint atmosphere structure
         Error_Status = CRTM_Assign_Atmosphere( Atmosphere(n), Atmosphere_AD(n) )

         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
                         (/"Error copying Atmosphere_AD structure"/))
         END IF

         ! -- Copy the adjoint surface structure
         Error_Status = CRTM_Assign_Surface( Surface(n), Surface_AD(n) )

         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
                         (/"Error copying Surface_AD structure"/))
         END IF

         ! -- Zero the Adjoint outputs 
         ! Important: adjoint variables must be initialized
         call CRTM_Zero_Atmosphere( Atmosphere_AD(n) )
         call CRTM_Zero_Surface( Surface_AD(n) )

         ! [1.2] CRTM GeometryInfo Structure
         GeometryInfo(n)%Sensor_Zenith_Angle=iv%instid(inst)%satzen(n)
         GeometryInfo(n)%Source_Zenith_Angle=iv%instid(inst)%solzen(n)
         GeometryInfo(n)%iFOV=iv%instid(inst)%scanpos(n)
   !     GeometryInfo(n)%Satellite_Height=830.0
   !     GeometryInfo(n)%Sensor_Scan_Angle=
   !     GeometryInfo(n)%Sensor_Zenith_Angle=
   !     GeometryInfo(n)%Sensor_Scan_Angle=
   !     GeometryInfo(n)%Source_Zenith_Angle=
	 
         ! [1.3] assign tb = R^-1 Re :

         do i = 1, ChannelInfo(inst)%n_Channels
            RTSolution_AD(i,n)%brightness_temperature = jo_grad_y%instid(inst)%tb(i,n)
            RTSolution_AD(i,n)%radiance = 0.0 ! must assign zero, since each call of AD model will return non-zero value
         end do

      end do

      ! [1.4] Call CRTM_AD model
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( n )
      do n =  iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel
         if (.not. use_crtm_kmatrix) then
            call da_crtm_ad (1, nchanl, 1, Atmosphere(n),   &
                               Surface(n),      &
                               RTSolution_AD(:,n),&
                               GeometryInfo(n), &
                               ChannelInfo(inst:inst),  &
                               Atmosphere_AD(n),&
                               Surface_AD(n),   &
                               RTSolution(:,n),   &
                               Options(n))
         else
            do i = 1, ChannelInfo(inst)%n_Channels
               Atmosphere_AD(n)%Level_Pressure(Atmosphere(n)%n_layers) = &
                       Atmosphere_AD(n)%Level_Pressure(Atmosphere(n)%n_layers) + &
                       iv%instid(inst)%ps_jacobian(i,n) * RTSolution_AD(i,n)%brightness_temperature   
            end do

            do k = kts , kte
               do i = 1, ChannelInfo(inst)%n_Channels
                  Atmosphere_AD(n)%Temperature(k) = Atmosphere_AD(n)%Temperature(k) + &
                                iv%instid(inst)%t_jacobian(i,k,n) * RTSolution_AD(i,n)%brightness_temperature
                  Atmosphere_AD(n)%absorber(k,1) = Atmosphere_AD(n)%absorber(k,1) + &
                                iv%instid(inst)%q_jacobian(i,k,n) * RTSolution_AD(i,n)%brightness_temperature
               end do
            end do
         endif
      end do
      !$OMP END PARALLEL DO

      do n =  iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel

      ! Skin Temperature
      !-----------------
	 if (use_satcv(1)) then
            ts_index = iv%instid(inst)%cv_index(n)%ts 	       
  	    if (Surface(n)%Land_Coverage  > 0.0) cv(ts_index) = Surface_AD(n)%Land_Temperature
            if (Surface(n)%Water_Coverage > 0.0) cv(ts_index) = Surface_AD(n)%Water_Temperature
            if (Surface(n)%Snow_Coverage  > 0.0) cv(ts_index) = Surface_AD(n)%Snow_Temperature
            if (Surface(n)%Ice_Coverage   > 0.0) cv(ts_index) = Surface_AD(n)%Ice_Temperature
	 end if 

      ! Cloud cover(s)
      !---------------
	 if (use_satcv(2)) then
	    nclouds = iv%instid(inst)%cv_index(n)%nclouds
	    ncv     = iv%instid(inst)%cv_index(n)%ncv
	    allocate (rad_ovc (nclouds))
	    allocate (cc_ad   (ncv))
	    do k = 1, nchanl
               if (ALL(iv%instid(inst)%ichan(k) /= Bands(:,1))) cycle   ! Only Channels in Band 1 	       
               rad_clr      = iv%instid(inst)%rad_xb(k,n)             
               do icld = kte-nclouds+1, kte
                  rad_ovc(icld)   = iv%instid(inst)%rad_ovc(k,icld,n)
               end do
	       rad_ad       = 0.0
	       tb_ad        = jo_grad_y%instid(inst)%tb(k,n)
	       call CRTM_Planck_Temperature_AD(inst,k,rad_clr,tb_ad,rad_ad)

 	       do icld = 1, nclouds
	          cc_ad(icld)        = rad_ad * (rad_ovc(icld)-rad_clr)
               end do
!	       !---------------------------------------------------------------
!              ! Change of variable (preconditioning) 
!              !---------------------------------------------------------------
!	       do icld = 1, ncv
!	          cc_ad(icld) = SUM(rad_ad * (rad_ovc(:)-rad_clr) * &
!		                iv%instid(inst)%cv_index(n)%vtox(icld,:) )
!	       end do

 	       do icld = 1, ncv
	          cv(iv%instid(inst)%cv_index(n)%cc(icld)) = cv(iv%instid(inst)%cv_index(n)%cc(icld)) + cc_ad(icld)
               end do
	    end do	
	    deallocate(rad_ovc, cc_ad)
	 end if 

      ! [1.5] Scale transformation and fill zero for no-control variable

         if (crtm_cloud) then
            do k=kts,kte
               do icld=1,Atmosphere(n)%n_Clouds
                  Atmosphere_AD(n)%Cloud(icld)%Water_Content(k) = &
		        Atmosphere_AD(n)%Cloud(icld)%Water_Content(k) * &
		        (Atmosphere(n)%Level_Pressure(k)- Atmosphere(n)%Level_Pressure(k-1)) / gravity
               enddo
            enddo
         end if

       ! [1.6] Adjoint of Interpolate horizontally from ob to grid:

         if (crtm_cloud) then
            do k=kts,kte ! from bottom to top
               qcw(k,n)=Atmosphere_AD(n)%Cloud(1)%Water_Content(kte-k+1)
               qci(k,n)=Atmosphere_AD(n)%Cloud(2)%Water_Content(kte-k+1)
               qrn(k,n)=Atmosphere_AD(n)%Cloud(3)%Water_Content(kte-k+1)
               qsn(k,n)=Atmosphere_AD(n)%Cloud(4)%Water_Content(kte-k+1)
               qgr(k,n)=Atmosphere_AD(n)%Cloud(5)%Water_Content(kte-k+1)
            end do
         end if

         do k=kts,kte ! from bottom to top
            if (atmosphere(n)%pressure(kte-k+1) >= 75.0) &        ! Zero Jacobian for top level(s)
               q_ad(k,n) = Atmosphere_AD(n)%Absorber(kte-k+1,1) * 1000.0 /  &
                           (1.0-xb_q(kte-k+1,n))**2               ! in g/kg
            if (k < kte) &                                        ! Zero Jacobian for top level
               t_ad(k,n)=Atmosphere_AD(n)%Temperature(kte-k+1)
         end do

         p_ad(n) = Atmosphere_AD(n)%Level_Pressure(atmosphere(n)%n_layers) * 0.01   ! in hPa

         Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_AD(n) )
         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
                         (/"Error in deallocatting CRTM Atmosphere_AD Structure"/))
         end if

         Error_Status = CRTM_Destroy_Surface(Surface_AD(n))
         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
                         (/"Error in deallocatting CRTM Surface_AD Structure"/))
         end if

      end do       !  end loop for pixels
	 
         ! [1.7] Gamma correction to VarBC
#ifdef CRTM_MODIF
         if (use_varbc) then
           do k = 1, nchanl
              npred = iv%instid(inst)%varbc(k)%npred
              if (npred <= 0) cycle                                               ! VarBC channels only
              if (iv%instid(inst)%varbc_info%npredmax < gammapred)  cycle         ! Gamma channels only
              if (iv%instid(inst)%varbc(k)%pred_use(gammapred) < 0) cycle         ! Gamma channels only
              do ipred = 1, npred
                 if (iv%instid(inst)%varbc(k)%ipred(ipred) /= gammapred) cycle
                 id = iv%instid(inst)%varbc(k)%index(ipred)
                 cv_local(id) = cv_local(id) + &
                    SUM(RTSolution_AD(k,n)%Gamma * iv%instid(inst)%varbc(k)%vtox(ipred,1:npred))
              end do   
           end do	    
         end if

       ! Gathering Gamma correction across processors
       !--------------------------------------------- 
!!!        call wrf_dm_sum_reals(cv_local, cv)
#endif
      
        if (crtm_cloud) then
           call da_interp_lin_2d_adj_partial(jo_grad_x%qcw(:,:,kts:kte),iv%instid(inst)%info, kts,kte, qcw)
           call da_interp_lin_2d_adj_partial(jo_grad_x%qci(:,:,kts:kte),iv%instid(inst)%info, kts,kte, qci)
           call da_interp_lin_2d_adj_partial(jo_grad_x%qrn(:,:,kts:kte),iv%instid(inst)%info, kts,kte, qrn)
           call da_interp_lin_2d_adj_partial(jo_grad_x%qsn(:,:,kts:kte),iv%instid(inst)%info, kts,kte, qsn)
           call da_interp_lin_2d_adj_partial(jo_grad_x%qgr(:,:,kts:kte),iv%instid(inst)%info, kts,kte, qgr)
        endif

         call da_interp_lin_2d_adj_partial(jo_grad_x%t(:,:,kts:kte),    iv%instid(inst)%info, kts,kte, t_ad)
         call da_interp_lin_2d_adj_partial(jo_grad_x%q(:,:,kts:kte),    iv%instid(inst)%info, kts,kte, q_ad)
         call da_interp_lin_2d_adj_partial(jo_grad_x%psfc, iv%instid(inst)%info, 1,1, p_ad)

         deallocate (q_ad)
         deallocate (t_ad)
         deallocate (p_ad)
         deallocate (xb_q)

         if (crtm_cloud) then
            deallocate (qcw)
            deallocate (qci)
            deallocate (qrn)
            deallocate (qsn)
            deallocate (qgr)
         end if

      !-------------------------------------------------------------------
      ! [2.0] Deallocating CRTM structures
      !-------------------------------------------------------------------
         deallocate( RTSolution, RTSolution_AD, STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
              (/"Error in deallocatting RTSolution"/))
         END IF

      do n =  iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel

         Error_Status = CRTM_Destroy_Options(Options(n))
         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Options Structure"/))
         end if

         Error_Status = CRTM_Destroy_Surface(Surface(n))
         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Surface Structure"/))
         end if

      !-------------------------------------------------------------------
      ! [3.0] Deallocating CRTM Atmosphere structures
      !-------------------------------------------------------------------
         Error_Status = CRTM_Destroy_Atmosphere( Atmosphere(n) )
         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
                         (/"Error in deallocatting CRTM Atmosphere Structure"/))
         end if

      end do

         deallocate( Atmosphere, Atmosphere_AD, STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
              (/"Error in deallocatting Atmosphere"/))
         END IF

         deallocate( Surface, Surface_AD, STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
              (/"Error in deallocatting Surface"/))
         END IF

         deallocate( Options, STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
              (/"Error in deallocatting Options"/))
         END IF

         deallocate( GeometryInfo, STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
              (/"Error in deallocatting GeometryInfo"/))
         END IF

   end do sensors_loop       ! end loop for sensor

       
   if (trace_use) call da_trace_exit("da_transform_xtoy_crtm_adj")
#else
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#endif
 
end subroutine da_transform_xtoy_crtm_adj
