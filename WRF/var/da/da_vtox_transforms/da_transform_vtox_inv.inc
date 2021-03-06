subroutine da_transform_vtox_inv(grid, cv_size, xbx, be, ep, cv, vv, vp)

   !--------------------------------------------------------------------------
   ! Purpose: Inverse control variable transform v = U^{-1} x'. 
   !--------------------------------------------------------------------------

   implicit none

   type(domain),   intent(inout) :: grid
   integer,        intent(in)    :: cv_size ! Size of cv array.
   type(xbx_type), intent(in)    :: xbx  ! For header & non-grid arrays.
   type(be_type),  intent(in)    :: be   ! background errors.
   type(ep_type),  intent(in)    :: ep   ! Ensemble perturbations.
   real,           intent(out)   :: cv(1:cv_size)   ! control variables.
   type(vp_type),  intent(out)   :: vv   ! grdipt/eof cv (local).
   type(vp_type),  intent(inout) :: vp   ! grdipt/level cv (local).

   if (trace_use) call da_trace_entry("da_transform_vtox_inv")

   call da_zero_x (grid%xa)
   
   if (.not. use_background_errors) then
      if (trace_use) call da_trace_exit("da_transform_vtox_inv")
      return
   end if

   !----------------------------------------------------------------------
   ! [1.0]: Perform inverse of balance tranform: vp = u_p^{-1} dx
   !----------------------------------------------------------------------

   if ( cv_options /= 7 ) call da_transform_vptox_inv(grid, vp, be, ep)

   !----------------------------------------------------------------------
   ! [2.0]: Perform inverse of vertical transform: vv = L^{-1/2} E^T vp
   !----------------------------------------------------------------------

   !if (  cv_options == 3 ) then
   !
   !   call da_apply_be( be, cv, vp, grid)
   !   call da_transform_bal( vp, be, grid)
   !
   !else

   if (vert_corr == vert_corr_2) then      
      call da_vertical_transform(grid, 'u_inv', be, grid%xb % vertical_inner_product, vv, vp)
      !call da_write_vp(grid,vv,'vv_afterUvTransf')
   else
      vv % v1(its:ite,jts:jte,kts:kte) = vp % v1(its:ite,jts:jte,kts:kte)
      vv % v2(its:ite,jts:jte,kts:kte) = vp % v2(its:ite,jts:jte,kts:kte)
      vv % v3(its:ite,jts:jte,kts:kte) = vp % v3(its:ite,jts:jte,kts:kte)
      vv % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)
      vv % v5(its:ite,jts:jte,kts:kte) = vp % v5(its:ite,jts:jte,kts:kte)
      if ( cloud_cv_options >= 2 ) then 
        vv % v6(its:ite,jts:jte,kts:kte) = vp % v6(its:ite,jts:jte,kts:kte)
        vv % v7(its:ite,jts:jte,kts:kte) = vp % v7(its:ite,jts:jte,kts:kte)
        vv % v8(its:ite,jts:jte,kts:kte) = vp % v8(its:ite,jts:jte,kts:kte)
        vv % v9(its:ite,jts:jte,kts:kte) = vp % v9(its:ite,jts:jte,kts:kte)
        vv % v10(its:ite,jts:jte,kts:kte) = vp % v10(its:ite,jts:jte,kts:kte)
      end if
      if ( use_cv_w ) vv % v11(its:ite,jts:jte,kts:kte) = vp % v11(its:ite,jts:jte,kts:kte)
      if (be % ne > 0) then
!        vv % alpha(its:ite,jts:jte,kts:kte,1:be%ne) = vp%alpha(its:ite,jts:jte,kts:kte,1:be%ne)
         vv % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:be%ne) =  &
             vp%alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:be%ne)
      end if
   end if

   !----------------------------------------------------------------------
   ! [3.0]: Perform inverse of recursive filter: cv = u_h^{-1} vv
   !----------------------------------------------------------------------

   !if (global) then
   !   call da_transform_vtovv_global(cv_size, xbx, be, cv, vv)
   !else if ( (fg_format == fg_format_wrf_arw_regional .or.   &
   !           fg_format == fg_format_wrf_nmm_regional) .and. &
   !           (.not. cv_options == 3) )then

       call da_transform_vtovv_inv(grid, cv_size, be, cv, vv)

   !end if

   !end if
   
   if (trace_use) call da_trace_exit("da_transform_vtox_inv")

end subroutine da_transform_vtox_inv

