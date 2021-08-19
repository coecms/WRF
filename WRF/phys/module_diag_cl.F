#if (NMM_CORE == 1)
MODULE module_diag_cl
CONTAINS
   SUBROUTINE diag_cl_stub
   END SUBROUTINE diag_cl_stub
END MODULE module_diag_cl
#else
!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_diag_cl
CONTAINS

   SUBROUTINE clwrf_output_calc(                                      &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      ips,ipe, jps,jpe, kps,kpe,                      & ! patch  dims
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                     ,is_restart                                      & ! CLWRF
                     ,clwrfH,t2,q2,u10,v10, skintemp                  & ! CLWRF
                     ,t2clmin,t2clmax,tt2clmin,tt2clmax               & ! CLWRF
                     ,t2clmean,t2clstd                                & ! CLWRF
                     ,q2clmin,q2clmax,tq2clmin,tq2clmax               & ! CLWRF
                     ,q2clmean,q2clstd                                & ! CLWRF
                     ,u10clmax,v10clmax,spduv10clmax,tspduv10clmax    & ! CLWRF
                     ,u10clmean,v10clmean,spduv10clmean               & ! CLWRF
                     ,u10clstd,v10clstd,spduv10clstd                  & ! CLWRF
                     ,raincclmax,rainncclmax,traincclmax,trainncclmax & ! CLWRF
                     ,raincclmean,rainncclmean,raincclstd,rainncclstd & ! CLWRF
                     ,skintempclmin,skintempclmax                     & ! CLWRF
                     ,tskintempclmin,tskintempclmax                   & ! CLWRF
                     ,skintempclmean,skintempclstd                    & ! CLWRF
                     ,raincv,rainncv                                  &
                     ,dt,xtime,curr_secs2                             &
                     ,nsteps                                          &
                                                                      )
!----------------------------------------------------------------------

  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
  USE module_configure 

   IMPLICIT NONE
!======================================================================
! Definitions
!-----------
!-- DT            time step (second)
!-- XTIME         forecast time
!-- curr_secs2    current time in seconds since simulation restart
!
!-- RAINCV        cumulus scheme precipitation in one time step (mm)
!-- RAINNCV       explicit scheme precipitation in one time step (mm)
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- ips           start index for i in patch
!-- ipe           end index for i in patch
!-- jps           start index for j in patch
!-- jpe           end index for j in patch
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!
! CLWRF-UC May.09 definitions
!-----------
! is_restart: whether if simulation is a restart
! clwrfH: Interval (hour) of accumulation for computations 
! [var]cl[min/max]: [minimum/maximum] of variable [var] during interval
! t[var]cl[min/max]: Time (minutes) of [minimum/maximum] of variable 
!    [var] during interval 
! [var]clmean: mean of variable [var] during interval
! [var]clstd: standard dev. of variable [var] during interval
!    Variables are written on aux_hist_out7 (established
!    in Registry)
!
!======================================================================

   INTEGER,      INTENT(IN   )                     ::            &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      ips,ipe, jps,jpe, kps,kpe, &
                                                        kts,kte, &
                                                      num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN)       :: i_start,   &
                                      i_end,j_start,j_end

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           & 
                                      RAINNCV, RAINCV, SKINTEMP 

   REAL,  INTENT(IN   )                            :: DT, XTIME
   REAL,  INTENT(IN   )                            :: curr_secs2

! LOCAL  VAR

   INTEGER                                   :: i,j,k,its,ite,jts,jte,ij
   INTEGER                                   :: idp,jdp

   REAL                                      :: xtimep
   LOGICAL, EXTERNAL                         :: wrf_dm_on_monitor

!!-------------------
!! CLWRF-UC Nov.09

   CHARACTER (LEN=80)                        :: timestr

   REAL, DIMENSION( ims:ime , jms:jme ),                                          & 
                          INTENT(IN)         :: t2, q2, u10, v10 
   REAL, DIMENSION( ims:ime , jms:jme ),                                          &
                          INTENT(OUT)        :: t2clmin, t2clmax, tt2clmin,       &
                          tt2clmax, t2clmean, t2clstd,                            & 
                          q2clmin, q2clmax, tq2clmin, tq2clmax, q2clmean, q2clstd,&
                          u10clmax, v10clmax, spduv10clmax, tspduv10clmax,        &
                          u10clmean, v10clmean, spduv10clmean,                    &
                          u10clstd, v10clstd, spduv10clstd, skintempclmin,        &
                          skintempclmax, tskintempclmin, tskintempclmax,          &
                          skintempclmean, skintempclstd
   REAL, DIMENSION( ims:ime , jms:jme ),                                          &
                          INTENT(OUT)        :: raincclmax, rainncclmax,          &
                          traincclmax, trainncclmax, raincclmean, rainncclmean,   & 
                          raincclstd, rainncclstd 
   REAL, PARAMETER                           :: minimum0= 1000000.,               &
                          maximum0= -1000000. 
   REAL                                      :: value
   INTEGER, INTENT(IN)                       :: clwrfH
   CHARACTER (LEN=1024)                      :: message
   INTEGER, INTENT(INOUT)                    :: nsteps
   LOGICAL                                   :: is_restart
! local vars
   REAL                                      :: t273

!-----------------------------------------------------------------

   t273  = 273.

! Initialize [var] values
! SET START AND END POINTS FOR TILES
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
!  IF ( MOD(NINT(XTIME), clwrfH) == 0 ) THEN
! IF (( MOD(NINT(curr_secs2/dt),NINT(clwrfH*60./dt)) == 0) .AND. (.NOT.is_restart)) THEN
  IF (( MOD(NINT(curr_secs2/dt),NINT(clwrfH*60./dt)) == 0) ) THEN
    DO ij = 1 , num_tiles
      IF  ( wrf_dm_on_monitor() ) THEN
          CALL wrf_debug(0, 'Re-initializing accumulation arrays')
      ENDIF
      nsteps = 1
      DO j = j_start(ij), j_end(ij)
        DO i = i_start(ij), i_end(ij)
          t2clmin(i,j)=t2(i,j)-t273
          t2clmax(i,j)=t2(i,j)-t273
          t2clmean(i,j)=t2(i,j)-t273
          t2clstd(i,j)=(t2(i,j)-t273)*(t2(i,j)-t273)
          q2clmin(i,j)=q2(i,j)
          q2clmax(i,j)=q2(i,j)
          q2clmean(i,j)=q2(i,j)
          q2clstd(i,j)=q2(i,j)*q2(i,j)
          spduv10clmax(i,j)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
          u10clmean(i,j)=u10(i,j)
          v10clmean(i,j)=v10(i,j)
          spduv10clmean(i,j)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
          u10clstd(i,j)=u10(i,j)*u10(i,j)
          v10clstd(i,j)=v10(i,j)*v10(i,j)
          spduv10clstd(i,j)=u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j)
          raincclmax(i,j)=raincv(i,j)/dt
          rainncclmax(i,j)=rainncv(i,j)/dt
          raincclmean(i,j)=raincv(i,j)/dt
          rainncclmean(i,j)=rainncv(i,j)/dt
          raincclstd(i,j)=(raincv(i,j)/dt)*(raincv(i,j)/dt)
          rainncclstd(i,j)=(rainncv(i,j)/dt)*(rainncv(i,j)/dt)
          skintempclmin(i,j)=skintemp(i,j)-t273
          skintempclmax(i,j)=skintemp(i,j)-t273
          skintempclmean(i,j)=skintemp(i,j)-t273
          skintempclstd(i,j)=(skintemp(i,j)-t273)*(skintemp(i,j)-t273)
          ! reinitialize times
          tt2clmin(i,j)       = xtime + dt/60.  ! value at end of timestep 
          tt2clmax(i,j)       = xtime + dt/60.
          tq2clmin(i,j)       = xtime + dt/60.
          tq2clmax(i,j)       = xtime + dt/60.
          tspduv10clmax(i,j)  = xtime + dt/60.
          traincclmax(i,j)    = xtime + dt/60.
          trainncclmax(i,j)   = xtime + dt/60.
          tskintempclmin(i,j) = xtime + dt/60.
          tskintempclmax(i,j) = xtime + dt/60.
        ENDDO
      ENDDO
    ENDDO
!    nsteps=clwrfH*60./dt
  ELSE
    xtimep = xtime + dt/60.   ! value at end of timestep for time info
    nsteps=nsteps+1
! Temperature
          CALL varstatistics(t2-t273,xtimep,ime-ims+1,jme-jms+1,t2clmin,t2clmax,   &
            tt2clmin,tt2clmax,t2clmean,t2clstd)
! Water vapor mixing ratio
          CALL varstatistics(q2,xtimep,ime-ims+1,jme-jms+1,q2clmin,q2clmax,   &
            tq2clmin,tq2clmax,q2clmean,q2clstd)
! Wind speed
          CALL varstatisticsWIND(u10,v10,xtimep,ime-ims+1,jme-jms+1,u10clmax, &
            v10clmax,spduv10clmax,tspduv10clmax,u10clmean,v10clmean,         &
            spduv10clmean,u10clstd,v10clstd,spduv10clstd)
! Precipitation flux
          CALL varstatisticsMAX(raincv/dt,xtimep,ime-ims+1,jme-jms+1,         &
            raincclmax,traincclmax,raincclmean,raincclstd) 
          CALL varstatisticsMAX(rainncv/dt,xtimep,ime-ims+1,jme-jms+1,        &
            rainncclmax,trainncclmax,rainncclmean,rainncclstd)
! Skin Temperature 
          CALL varstatistics(skintemp-t273,xtimep,ime-ims+1,jme-jms+1,skintempclmin,&
            skintempclmax,tskintempclmin,tskintempclmax,skintempclmean,       &
            skintempclstd)

!          IF (MOD(NINT(XTIME),clwrfH) == 0) THEN
!          IF (MOD(NINT(XTIME+dt/60.),clwrfH) == 0) THEN
           IF (MOD(NINT((curr_secs2+dt)/dt),NINT(clwrfH*60./dt)) == 0) THEN
             IF  ( wrf_dm_on_monitor() ) &
               PRINT *,'nsteps=',nsteps,' xtime:', xtime,' clwrfH:',clwrfH
               t2clmean=t2clmean/nsteps
               t2clstd=SQRT(MAX(t2clstd/nsteps-t2clmean**2.,0.))
               t2clmean=t2clmean+t273
               t2clmin=t2clmin+t273
               t2clmax=t2clmax+t273
               q2clmean=q2clmean/nsteps
               q2clstd=q2clstd/nsteps-q2clmean*q2clmean
               q2clstd=MAX(q2clstd,0.)
               q2clstd=SQRT(q2clstd)
               u10clmean=u10clmean/nsteps
               v10clmean=v10clmean/nsteps
               spduv10clmean=spduv10clmean/nsteps
               u10clstd=SQRT(MAX(u10clstd/nsteps-u10clmean**2., 0.))
               v10clstd=SQRT(MAX(v10clstd/nsteps-v10clmean**2., 0.))
               spduv10clstd=SQRT(MAX(spduv10clstd/nsteps-                        &
                 spduv10clmean**2, 0.))
               raincclmean=raincclmean/nsteps
               rainncclmean=rainncclmean/nsteps
               raincclstd=SQRT(MAX(raincclstd/nsteps-raincclmean**2., 0.))
               rainncclstd=SQRT(MAX(rainncclstd/nsteps-rainncclmean**2., 0.))
               skintempclmean=skintempclmean/nsteps
               skintempclstd=skintempclstd/nsteps-skintempclmean*skintempclmean
               skintempclstd=MAX(skintempclstd,0.)
               skintempclstd=SQRT(skintempclstd)
               skintempclmean=skintempclmean+t273
               skintempclmin=skintempclmin+t273
               skintempclmax=skintempclmax+t273
             IF  ( wrf_dm_on_monitor() ) THEN
               DO ij = 1 , num_tiles
               idp = i_start(ij)+(i_end(ij)-i_start(ij))/2
               jdp = j_start(ij)+(j_end(ij)-j_start(ij))/2
               WRITE(message, *)'CLWRFdiag - T2; tile: ',ij,          &
                 ' T2clmin:', t2clmin(idp,jdp),                       &
                 ' T2clmax:', t2clmax(idp,jdp),                       &
                 ' TT2clmin:', tt2clmin(idp,jdp),                     &
                 ' TT2clmax:', tt2clmax(idp,jdp),                     &
                 ' T2clmean:', t2clmean(idp,jdp),                     &
                 ' T2clstd:', t2clstd(idp,jdp)
               CALL wrf_debug(0, message)
               WRITE(message, *)'CLWRFdiag - Q2; tile: ',ij,          &
                 ' Q2clmin:', q2clmin(idp,jdp),                       &
                 ' Q2clmax:', q2clmax(idp,jdp),                       &
                 ' TQ2clmin:', tq2clmin(idp,jdp),                     &
                 ' TQ2clmax:', tq2clmax(idp,jdp),                     &
                 ' Q2clmean:', q2clmean(idp,jdp),                     &
                 ' Q2clstd:', q2clstd(idp,jdp)
               CALL wrf_debug(75, message)
               WRITE(message, *)'CLWRFdiag - WINDSPEED; tile: ',ij,   &
                 ' U10clmax:', u10clmax(idp,jdp),                     &
                 ' V10clmax:', v10clmax(idp,jdp),                     &
                 ' SPDUV10clmax:', spduv10clmax(idp,jdp),             &
                 ' TSPDUV10clmax:', tspduv10clmax(idp,jdp),           &
                 ' U10clmean:', u10clmean(idp,jdp),                   &
                 ' V10clmean:', v10clmean(idp,jdp),                   &
                 ' SPDUV10clmean:', spduv10clmean(idp,jdp),           &
                 ' U10clstd:', u10clstd(idp,jdp),                     &
                 ' V10clstd:', v10clstd(idp,jdp),                     &
                 ' SPDUV10clstd:', spduv10clstd(idp,jdp)
               CALL wrf_debug(75, message)
               WRITE(message, *)'CLWRFdiag - RAIN; tile: ',ij,        &
                 ' RAINCclmax:',raincclmax(idp,jdp),                  &
                 ' RAINNCclmax:',rainncclmax(idp,jdp),                &
                 ' TRAINCclmax:',traincclmax(idp,jdp),                &
                 ' TRAINNCclmax:',trainncclmax(idp,jdp),              &
                 ' RAINCclmean:',raincclmean(idp,jdp),                &
                 ' RAINNCclmean:',rainncclmean(idp,jdp),              &
                 ' RAINCclstd:',raincclstd(idp,jdp),                  &
                 ' RAINNCclstd:',rainncclstd(idp,jdp)
               CALL wrf_debug(75, message)
               WRITE(message,*)'CLWRFdiag - SKINTEMP; tile: ',ij,     &
                 ' SKINTEMPclmin:',skintempclmin(idp,jdp),            &
                 ' SKINTEMPclmax:',skintempclmax(idp,jdp),            &
                 ' TSKINTEMPclmin:',tskintempclmin(idp,jdp),          &
                 ' TSKINTEMPclmax:',tskintempclmax(idp,jdp),          &
                 ' SKINTEMPclmean:',skintempclmean(idp,jdp),          &
                 ' SKINTEMPclstd:',skintempclstd(idp,jdp)
               CALL wrf_debug(75, message)
               ENDDO
             ENDIF
           END IF
!        ENDDO
!      ENDDO
  ENDIF
!  !$OMP END PARALLEL DO

   END SUBROUTINE clwrf_output_calc

! UC.CLWRF Nov.09
SUBROUTINE varstatisticsWIND(varu, varv, tt, dx, dy, varumax, varvmax,       &
  varuvmax, tvaruvmax, varumean, varvmean, varuvmean, varustd, varvstd,     & 
  varuvstd) 
! Subroutine to compute variable statistics for a wind somponents 

IMPLICIT NONE

INTEGER                                                        :: i, j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: varu, varv
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varumax,   &
  varvmax, varuvmax, tvaruvmax, varumean, varvmean, varuvmean, varustd,      & 
  varvstd, varuvstd
REAL                                                           :: varuv

DO i=1,dx
  DO j=1,dy
    varuv=sqrt(varu(i,j)*varu(i,j)+varv(i,j)*varv(i,j))
      IF (varuv > varuvmax(i,j)) THEN
        varumax(i,j)=varu(i,j)
        varvmax(i,j)=varv(i,j)
        varuvmax(i,j)=varuv
        tvaruvmax(i,j)=tt
      END IF
    varuvmean(i,j)=varuvmean(i,j)+varuv
    varuvstd(i,j)=varuvstd(i,j)+varuv**2
  END DO
END DO
varumean=varumean+varu
varvmean=varvmean+varv
varustd=varustd+varu*varu
varvstd=varvstd+varv*varv

END SUBROUTINE varstatisticsWIND

SUBROUTINE varstatisticsMAX(var, tt, dx, dy, varmax, tvarmax, varmean,       &
   varstd)
! Subroutine to compute variable statistics for a max only variable values

IMPLICIT NONE

INTEGER                                                        :: i,j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: var
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varmax,    &
  tvarmax, varmean, varstd

DO i=1,dx
  DO j=1,dy
    IF (var(i,j) > varmax(i,j)) THEN
      varmax(i,j)=var(i,j)
      tvarmax(i,j)=tt
    END IF
  END DO
END DO
varmean=varmean+var
varstd=varstd+var*var

END SUBROUTINE varstatisticsMAX 

SUBROUTINE varstatistics(var, tt, dx, dy, varmin, varmax, tvarmin, tvarmax,  & 
  varmean, varstd) 
! Subroutine to compute variable statistics

IMPLICIT NONE

INTEGER                                                        :: i,j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: var
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varmin,    &
  varmax, tvarmin, tvarmax, varmean, varstd

DO i=1,dx
  DO j=1,dy
    IF (var(i,j) < varmin(i,j)) THEN
      varmin(i,j)=var(i,j)
      tvarmin(i,j)=tt
    END IF
    IF (var(i,j) > varmax(i,j)) THEN
      varmax(i,j)=var(i,j)
      tvarmax(i,j)=tt
    END IF
  END DO
END DO
varmean=varmean+var
varstd=varstd+var*var

END SUBROUTINE varstatistics

SUBROUTINE diagnostic_clwrf_clim_new(                 &
  ids,ide, jds,jde, kds,kde                        &
  ,ims,ime, jms,jme, kms,kme                       &
  ,ips,ipe, jps,jpe, kps,kpe                       &
  ,i_start,i_end,j_start,j_end,num_tiles           &
  ,kts,kte                                         &
  ,max_window                                      &
  ,raincv,rainncv                                  &
  ,u10,v10                                         &
  ,swdown                                          &
  ,histout, histstart_h, histstart_m               &
  ,histstart_s, histstart                          &
  ,do_rain5, prmax5, tprmax5                       &
  ,do_rain10, prmax10, tprmax10                    &
  ,do_rain20, prmax20, tprmax20                    &
  ,do_rain30, prmax30, tprmax30                    &
  ,do_rain60, prmax60, tprmax60                    &
  ,do_wind5, uv10max5, tuv10max5                   &
  ,do_wind10, uv10max10, tuv10max10                &
  ,do_wind20, uv10max20, tuv10max20                &
  ,do_wind30, uv10max30, tuv10max30                &
  ,do_wind60, uv10max60, tuv10max60                &
  ,track_rain, track_wind, track_dt                &
  ,dsunshine, initime_sunshine                     &
  ,is_restart                                      &
  ,dt,xtime                                        &
  )
!----------------------------------------------------------------------

USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval

IMPLICIT NONE
!======================================================================
! Definitions
!-----------
!-- DT            time step (second)
!-- XTIME         forecast time
!
!-- RAINCV        cumulus scheme precipitation in one time step (mm)
!-- RAINNCV       explicit scheme precipitation in one time step (mm)
!-- TRACK_RAIN    records rain over max_window time-steps so we can calculate
!                 the moving averages.
!-- TRACK_WIND    records wind speeds over max_window time-steps to calculate
!                 the moving averages.
!-- TRACK_DT      records the time-step lengths in case of varying
!                 time-step.
!
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- num_tiles     number of tiles
!
!======================================================================
! is_restart: whether if simulation is a restart

! Number of time windows 
INTEGER, PARAMETER :: ndiag=5
! Time windows in minutes
INTEGER, DIMENSION( ndiag ), PARAMETER :: time_diag=(/5,10,20,30,60/)

INTEGER, INTENT(IN)                                 :: ims,ime, jms,jme, kms,kme  &
                                                      ,ids,ide, jds,jde, kds,kde  &
                                                      ,ips,ipe, jps,jpe, kps,kpe  &
                                                      ,kts,kte                    &
                                                      ,num_tiles, max_window

INTEGER, DIMENSION(num_tiles), INTENT(IN)             :: i_start,i_end,j_start,j_end 

REAL,  INTENT(IN)                                     :: DT, XTIME

INTEGER, INTENT(IN)                                   :: histout
INTEGER, INTENT(IN)                                   :: histstart_h &
      ,histstart_m &
                                                      ,histstart_s &
                                                      ,histstart
REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: raincv, rainncv, swdown
REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) :: u10, v10

INTEGER, TARGET, INTENT(IN)                      :: do_rain5, do_rain10  &
                                                    ,do_rain20, do_rain30 &
                                                    ,do_rain60

REAL, DIMENSION( ims:ime , jms:jme ), TARGET, INTENT(INOUT) :: prmax5 , tprmax5  &
                                                      ,prmax10, tprmax10 &
                                                      ,prmax20, tprmax20 &
                                                      ,prmax30, tprmax30 &
                                                      ,prmax60, tprmax60 

REAL, DIMENSION( ims:ime,1:max_window,jms:jme ), INTENT(INOUT) :: track_rain

INTEGER, TARGET, INTENT(IN)                      :: do_wind5, do_wind10  &
                                                    ,do_wind20, do_wind30 &
                                                    ,do_wind60

REAL, DIMENSION( ims:ime , jms:jme ), TARGET, INTENT(INOUT) :: uv10max5 , tuv10max5  &
                                                      ,uv10max10, tuv10max10 &
                                                      ,uv10max20, tuv10max20 &
                                                      ,uv10max30, tuv10max30 &
                                                      ,uv10max60, tuv10max60 

REAL, DIMENSION( ims:ime,1:max_window,jms:jme ), INTENT(INOUT) :: track_wind
REAL, DIMENSION( 1:max_window ), INTENT(INOUT) :: track_dt

      
REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: dsunshine, initime_sunshine
LOGICAL, INTENT(IN)                                   :: is_restart

! LOCAL  VAR
TYPE IND_DIAG
    INTEGER, POINTER :: do_diag
    REAL, POINTER, DIMENSION(:,:) :: maxivalue
    REAL, POINTER, DIMENSION(:,:) :: maxtime
END TYPE IND_DIAG

TYPE(IND_DIAG), DIMENSION( ndiag )                    :: do_rain_diag
TYPE(IND_DIAG), DIMENSION( ndiag )                    :: do_wind_diag


INTEGER                                               :: i,j,ij,im2,jm2,n
INTEGER                                               :: istart, iend, jstart, jend

INTEGER                                               :: histstart_mloc

INTEGER            :: ndt_rain_max
INTEGER            :: ndt_wind_max

REAL, DIMENSION(5) :: per_rain
REAL, DIMENSION(5) :: per_wind
REAL               :: per_rain_max
REAL               :: per_wind_max

REAL               :: begin_time
REAL               :: first_out, time_since_last_out
REAL, SAVE         :: XTIME0

REAL, PARAMETER                                       :: sunshine_start=120.
REAL, DIMENSION( ims:ime , jms:jme )                  :: total
REAL                                                  :: tmp

CHARACTER(LEN=1024)                                   :: outstring

LOGICAL, SAVE                                         :: FIRST=.TRUE.
LOGICAL                                               :: is_reset
LOGICAL, EXTERNAL                                     :: wrf_dm_on_monitor

!-----------------------------------------------------------------

! Transform individual variables in arrays to make code easier to maintain
! Allocate pointers
do_rain_diag(1)%do_diag => do_rain5
do_rain_diag(2)%do_diag => do_rain10
do_rain_diag(3)%do_diag => do_rain20
do_rain_diag(4)%do_diag => do_rain30
do_rain_diag(5)%do_diag => do_rain60

do_rain_diag(1)%maxivalue => prmax5
do_rain_diag(2)%maxivalue => prmax10
do_rain_diag(3)%maxivalue => prmax20
do_rain_diag(4)%maxivalue => prmax30
do_rain_diag(5)%maxivalue => prmax60

do_rain_diag(1)%maxtime => tprmax5
do_rain_diag(2)%maxtime => tprmax10
do_rain_diag(3)%maxtime => tprmax20
do_rain_diag(4)%maxtime => tprmax30
do_rain_diag(5)%maxtime => tprmax60


do_wind_diag(1)%do_diag => do_wind5
do_wind_diag(2)%do_diag => do_wind10
do_wind_diag(3)%do_diag => do_wind20
do_wind_diag(4)%do_diag => do_wind30
do_wind_diag(5)%do_diag => do_wind60

do_wind_diag(1)%maxivalue => uv10max5
do_wind_diag(2)%maxivalue => uv10max10
do_wind_diag(3)%maxivalue => uv10max20
do_wind_diag(4)%maxivalue => uv10max30
do_wind_diag(5)%maxivalue => uv10max60

do_wind_diag(1)%maxtime => tuv10max5
do_wind_diag(2)%maxtime => tuv10max10
do_wind_diag(3)%maxtime => tuv10max20
do_wind_diag(4)%maxtime => tuv10max30
do_wind_diag(5)%maxtime => tuv10max60



! For printing purposes only
im2=INT(ims+(ime-ims)/2)
jm2=INT(jms+(jme-jms)/2)

! Get the maximum period for rain and wind diagnostics.
IF (FIRST) THEN
    XTIME0 = XTIME
    FIRST  = .FALSE.
ENDIF

! Gives 0 if the diagnostic is not on for this nest. Else gives the time in seconds.
do n=1,ndiag
    per_rain(n) = time_diag(n)*do_rain_diag(n)%do_diag*60.
    per_wind(n) = time_diag(n)*do_wind_diag(n)%do_diag*60.
end do
! Get the longer time period for this nest and the corresponding max number of 
! time steps we need to keep for the diagnostic
per_rain_max = MAXVAL(per_rain)
ndt_rain_max = CEILING(per_rain_max/dt)

per_wind_max = MAXVAL(per_wind)
ndt_wind_max = CEILING(per_wind_max/dt)

! Check if max_window is big enough
IF (max_window < MAX(ndt_rain_max,ndt_wind_max)) THEN
    WRITE(outstring,*) 'max_window is too small. It should be ', &
        MAX(ndt_rain_max,ndt_wind_max)
    CALL wrf_error_fatal(outstring)
ENDIF

! Calculate the first time step we want the diagnostics output. 
! If the diagnostics are on only after some simulation time, we might
! not need to start calculating the diagnostic from the start.
begin_time = 0.
histstart_mloc = histstart_m
if (histstart_mloc == 0.) then
    histstart_mloc = histstart
end if
begin_time = ((histstart_h*60. + histstart_mloc)*60. + histstart_s)

first_out = XTIME0*60. + begin_time

    
! Reset the max and time of max at the time step after writing to file.
! The namelist options auxhist5_interval and auxhist5_begin_h are used 
! to define the output frequency of this diagnostic.
tmp = XTIME*60.
is_reset = (MOD(NINT(tmp-first_out), NINT(histout*60.)) == 0) .OR. (XTIME==0.)

time_since_last_out = MOD((tmp-first_out),(histout*60.))/60.

! Only calculate diagnostic after first_out - histout time
IF (tmp >= (first_out - (histout*60./dt))) THEN

! SET START AND END POINTS FOR TILES
!$OMP PARALLEL DO   &
!$OMP PRIVATE ( ij )

    DO ij = 1 , num_tiles
      istart=i_start(ij)
      iend=i_end(ij)
      jstart=j_start(ij)
      jend=j_end(ij)
      
      ! If at least one of the diagnostics is switched on, we update the
      ! array to track the timestep. We do this in case the diagnostics
      ! straddle a restart where the timestep was changed.
      IF (ndt_rain_max >0. .or. ndt_wind_max >0.) THEN
          track_dt = cshift(track_dt,1,1)
          track_dt(max_window) = dt
      END IF
      
      IF (ndt_rain_max > 0.) THEN
          ! We want some rain diagnostics so we need to keep track
          ! of precipitations. The precip. of the current time step is 
          ! always in the last index of track_rain.
          DO j=jstart,jend
            DO i=istart,iend
                track_rain(i,:,j) = cshift(track_rain(i,:,j),1,1)
                track_rain(i,max_window,j) = raincv(i,j) + rainncv(i,j)
            enddo
          ENDDO
          
          ! Loop over diagnostics for rain
          DO n=1,ndiag
            IF (DO_RAIN_DIAG(n)%DO_DIAG == 1) THEN
                CALL ACCUM_DRIVER(                 &
                    xtime,time_since_last_out,dt  &
                    ,do_rain_diag(n)%maxivalue       &
                    ,do_rain_diag(n)%maxtime      &
                    ,track_rain, track_dt         &
                    ,per_rain(n), max_window      &
                    ,is_reset                     &
                    ,ims, ime, jms, jme           &
                    ,istart, iend, jstart, jend   & 
                    )
            ENDIF
          ENDDO
      ENDIF
      
      IF (ndt_wind_max > 0.) THEN
          ! We want some wind diagnostics so we need to keep track
          ! of wind speeds. The wind speed of the current time step is 
          ! always in the last index of track_wind.
          DO j=jstart,jend
            DO i=istart,iend
                track_wind(i,:,j) = cshift(track_wind(i,:,j),1,1)
                track_wind(i,max_window,j) = sqrt(u10(i,j)**2+v10(i,j)**2)*dt
            enddo
          ENDDO
          
          ! Loop over diagnostics for wind
          DO n=1,ndiag
            IF (DO_WIND_DIAG(n)%DO_DIAG == 1) THEN
                CALL ACCUM_DRIVER(                 &
                    xtime,time_since_last_out, dt &
                    ,do_wind_diag(n)%maxivalue       &
                    ,do_wind_diag(n)%maxtime      &
                    ,track_wind, track_dt         &
                    ,per_wind(n), max_window      &
                    ,is_reset                     &
                    ,ims, ime, jms, jme           &
                    ,istart, iend, jstart, jend   & 
                    )
            ENDIF
          ENDDO
      ENDIF
  
      ! Calculate sunshine. Needs to be set to 0 at midnight.
!       IF ((MOD(NINT((XTIME*60.+start_sec)/dt),NINT(histout*60./dt)) == 0 .AND. &
!           (.NOT.is_restart)) .OR. XTIME == 0 ) THEN 
      IF (is_reset) THEN 
          DO j=jstart,jend
            DO i=istart,iend
                dsunshine(i,j) = 0.
                initime_sunshine(i,j) = 0.                 
            ENDDO
          ENDDO
      ENDIF
      
      DO j=jstart,jend
          DO i=istart,iend
            IF ((swdown(i,j) >= sunshine_start) .AND. (initime_sunshine(i,j) == 0.)) THEN
                initime_sunshine(i,j)=time_since_last_out
                dsunshine(i,j)=dt
            ELSE IF ((swdown(i,j) >= sunshine_start)) THEN
                dsunshine(i,j)=dsunshine(i,j)+dt
            ENDIF
          END DO
      END DO
    END DO
END IF  ! To delay start of diagnostic.

IF ( wrf_dm_on_monitor() ) THEN
    WRITE(outstring,*) 'CLWRFclim values: ',ij,' dsunshine:', dsunshine(im2, jm2),      &
        ' time sunshine:', initime_sunshine(im2, jm2)
    CALL wrf_debug ( 75, TRIM(outstring) )
END IF

END SUBROUTINE diagnostic_clwrf_clim_new

SUBROUTINE ACCUM_DRIVER(            &
  xtime, time_since_last_out, dt &
  ,maxper, tmaxper               &
  ,track, track_dt               &
  ,P_ask, nwind                  &
  ,is_reset                      &
  ,ims, ime, jms, jme            &
  ,istart, iend, jstart, jend    &
  )

INTEGER, INTENT(IN)                                     :: ims,ime, jms,jme &
      , nwind
INTEGER, INTENT(IN)                                     :: istart, iend, jstart, jend
REAL,  INTENT(IN)                                       :: DT, XTIME, time_since_last_out

REAL, INTENT(IN)                                        :: P_ask
REAL, DIMENSION( ims:ime,1:nwind, jms:jme ), INTENT(IN) :: track
REAL, DIMENSION( 1:nwind ), INTENT(IN)                  :: track_dt
LOGICAL, INTENT(IN)                                     :: is_reset
REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)     :: maxper, tmaxper

INTEGER                                                 :: ndt
REAL                                                    :: time_sum ! Cum. sum of time-steps
                                                                    ! to find # of t.s. in P_ask.
REAL, DIMENSION( ims:ime , jms:jme )                    :: rate
REAL, DIMENSION( ims:ime,1:nwind, jms:jme )             :: loc_track

INTEGER                                                 :: i,j

LOGICAL, EXTERNAL                                       :: wrf_dm_on_monitor
CHARACTER(LEN=1024)                                     :: outstring

INTEGER                                                 :: im2, jm2
REAL                                                    :: Fn, tmp

IF (is_reset) THEN
    ! Reset max and time to 0.
    DO j=jstart,jend
      DO i=istart,iend
          maxper(i,j) = 0.
          tmaxper(i,j) = 0.                      
      ENDDO
    ENDDO
ENDIF

IF (sum(track_dt) >= P_ask) THEN
    im2=INT(ims+(ime-ims)/2)
    jm2=INT(jms+(jme-jms)/2)


    ! Get number of time steps in the accumulation window.
    ! Sum the dt in track_dt from last until we reach time >= P_ask
    ndt = 0
    time_sum = track_dt(nwind)
    do while ( time_sum < P_ask )
      ndt = ndt + 1
      time_sum = time_sum + track_dt(nwind - ndt)
    end do
    
    
    ! Fraction of time for the last time step <= 1
    tmp = sum(track_dt(nwind-ndt:nwind-1))
    Fn = (P_ask - tmp)/track_dt(nwind)    
    
    ! If we have a varying time-step then maybe we can have Fn <0,
    ! when new time-step < old time-step. In this case, it means there
    ! is nothing to be done at this time-step and we just need to 
    ! exit the subroutine.
    if (Fn > 0) then
      ! Accumulate the precip. or wind over the ndt last time steps.
      ! We should have: (ndt-1)+Fn = P_ask/dt.
      loc_track = track/P_ask
      rate = sum(loc_track(:,nwind-ndt:nwind-1,:),2)+loc_track(:,nwind,:)*Fn
      
      ! Find maximum.
      DO j=jstart,jend
          DO i=istart,iend
            IF (rate(i,j) > maxper(i,j)) THEN
                maxper(i,j)=rate(i,j)
                tmaxper(i,j)=time_since_last_out+Fn*track_dt(nwind)/60.
            END IF
          END DO
      END DO
    
!!!!!  !!!!!  !!!!!
      ! Printing some results
      !!
      IF ( wrf_dm_on_monitor() ) THEN
          WRITE(outstring,*) 'CLWRFclim values: maxper:', maxper(im2, jm2),      &
              ' tmaxper:', tmaxper(im2, jm2), 'period:', P_ask
          CALL wrf_debug ( 75, TRIM(outstring) )
      END IF
    end if
ENDIF

END SUBROUTINE ACCUM_DRIVER

END MODULE module_diag_cl
#endif
