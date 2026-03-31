module prescribed_wv

!-------------------------------------------------------------------------- 
! Purpose:
!
! Reads cloud-related fields, puts the them into the physics buffer for use
! by radiation
! 
!--------------------------------------------------------------------------

  use shr_kind_mod, only : r8 => shr_kind_r8
  use abortutils,   only : endrun
  use spmd_utils,   only : masterproc
  use tracer_data,  only : trfld, trfile
  use cam_logfile,  only : iulog
  use time_manager, only : get_curr_date
  implicit none
  private
  save 

  type(trfld), pointer :: fields(:)
  type(trfile)         :: file

  public :: prescribed_wv_init
  public :: prescribed_wv_adv
  public :: write_prescribed_wv_restart
  public :: read_prescribed_wv_restart
  public :: has_prescribed_wv
  public :: prescribed_wv_register
  public :: init_prescribed_wv_restart
  public :: prescribed_wv_readnl

  logical :: has_prescribed_wv = .true.
!! JGOmod
!  integer          , parameter :: nflds             = 10
!  character(len=16), parameter :: cloud_name(nflds) = (/'DEI_in'   ,'MU_in'  ,'LAMBDAC_in' ,'ICIWP_in' ,'ICLWP_in' ,'DES_in' , &
!                                                        'ICSWP_in' ,'CLD_in' ,'CLDLIQ_in'  ,'CLDICE_in'  /)
!
!  character(len=16)  :: fld_name(nflds)             = (/'DEI_rad'  ,'MU_rad' ,'LAMBDAC_rad','ICIWP_rad','ICLWP_rad','DES_rad', &
!                                                        'ICSWP_rad','CLD_rad','CLDLIQ_rad' ,'CLDICE_rad' /)
  integer          , parameter :: nflds             = 1
  character(len=16), parameter :: cloud_name(nflds) = 'Q_in'

  character(len=16)  :: fld_name(nflds)             = 'Q'
!! JGOmod
  character(len=256) :: filename                    = 'AMIP_CTL.cam.h1.1979-01-01-00000.nc'
  character(len=256) :: filelist                    = ''
  character(len=256) :: datapath                    = '/network/rit/lab/zhoulab_rit/lzhuo/AMIP_CTL/run'
  character(len=32)  :: data_type                   = 'CYCLICAL'
  logical            :: rmv_file                    = .false.
  integer            :: cycle_yr                    = 1979
  integer            :: fixed_ymd                   = 0
  integer            :: fixed_tod                   = 0
  character(len=32)  :: specifier(nflds)            = ''
  real(r8) datatimem,datatimep 
  integer yr, mon, day, ncsec, ymd  ! components of a date
contains

!-------------------------------------------------------------------
!-------------------------------------------------------------------
  subroutine prescribed_wv_register()
    use ppgrid,         only: pver, pcols
    use physics_buffer, only : pbuf_add_field, dtype_r8

    integer :: i,idx

    if (has_prescribed_wv) then
       do i = 1,nflds
          call pbuf_add_field(cloud_name(i),'physpkg',dtype_r8,(/pcols,pver/),idx)
       enddo
    endif

  end subroutine prescribed_wv_register

!-------------------------------------------------------------------
!-------------------------------------------------------------------
  subroutine prescribed_wv_init()

    use tracer_data, only : trcdata_init

    implicit none

    integer :: ndx, istat, i
    
    if ( has_prescribed_wv ) then
       if ( masterproc ) then
          write(iulog,*) 'wv is prescribed in :'//trim(filename)
       endif
    else
       return
    endif

    do i = 1,nflds
       specifier(i) = trim(cloud_name(i))//':'//trim(fld_name(i))
    end do


    allocate(file%in_pbuf(size(specifier)))
    file%in_pbuf(:) = .true.
    file%stepTime   = .true.
    !file%xyzint     = .false.
    call trcdata_init( specifier, filename, filelist, datapath, fields, file, &
                       rmv_file, cycle_yr, fixed_ymd, fixed_tod, data_type)
    call get_curr_date(yr, mon, day, ncsec)
    ymd = yr*10000 + mon*100 + day
    datatimem=file%datatimem
    datatimep=file%datatimep
    write(iulog,*) 'ymd=',ymd,'tod=',ncsec,'datatimem=',datatimem,'datatimep=',datatimep
  end subroutine prescribed_wv_init

!-------------------------------------------------------------------
!-------------------------------------------------------------------
subroutine prescribed_wv_readnl(nlfile)

   use namelist_utils,  only: find_group_name
   use units,           only: getunit, freeunit
   use mpishorthand

   character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

   ! Local variables
   integer :: unitn, ierr
   character(len=*), parameter :: subname = 'prescribed_cloud_readnl'

   character(len=256) :: prescribed_wv_file
   character(len=256) :: prescribed_wv_filelist
   character(len=256) :: prescribed_wv_datapath
   character(len=32)  :: prescribed_wv_type
   logical            :: prescribed_wv_rmfile
   integer            :: prescribed_wv_cycle_yr
   integer            :: prescribed_wv_fixed_ymd
   integer            :: prescribed_wv_fixed_tod

   namelist /prescribed_wv_nl/ &
      prescribed_wv_file,      &
      prescribed_wv_filelist,  &
      prescribed_wv_datapath,  &
      prescribed_wv_type,      &
      prescribed_wv_rmfile,    &
      prescribed_wv_cycle_yr,  &
      prescribed_wv_fixed_ymd, &
      prescribed_wv_fixed_tod      
   !-----------------------------------------------------------------------------

   ! Initialize namelist variables from local module variables.
   prescribed_wv_file     = filename
   prescribed_wv_filelist = filelist
   prescribed_wv_datapath = datapath
   prescribed_wv_type     = data_type
   prescribed_wv_rmfile   = rmv_file
   prescribed_wv_cycle_yr = cycle_yr
   prescribed_wv_fixed_ymd= fixed_ymd
   prescribed_wv_fixed_tod= fixed_tod

   ! Read namelist
   if (masterproc) then
      unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'prescribed_wv_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, prescribed_wv_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(subname // ':: ERROR reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)
   end if

#ifdef SPMD
   ! Broadcast namelist variables
   call mpibcast(prescribed_wv_file,     len(prescribed_wv_file),     mpichar, 0, mpicom)
   call mpibcast(prescribed_wv_filelist, len(prescribed_wv_filelist), mpichar, 0, mpicom)
   call mpibcast(prescribed_wv_datapath, len(prescribed_wv_datapath), mpichar, 0, mpicom)
   call mpibcast(prescribed_wv_type,     len(prescribed_wv_type),     mpichar, 0, mpicom)
   call mpibcast(prescribed_wv_rmfile,   1, mpilog,  0, mpicom)
   call mpibcast(prescribed_wv_cycle_yr, 1, mpiint,  0, mpicom)
   call mpibcast(prescribed_wv_fixed_ymd,1, mpiint,  0, mpicom)
   call mpibcast(prescribed_wv_fixed_tod,1, mpiint,  0, mpicom)
#endif

   ! Update module variables with user settings.
   filename   = prescribed_wv_file
   filelist   = prescribed_wv_filelist
   datapath   = prescribed_wv_datapath
   data_type  = prescribed_wv_type
   rmv_file   = prescribed_wv_rmfile
   cycle_yr   = prescribed_wv_cycle_yr
   fixed_ymd  = prescribed_wv_fixed_ymd
   fixed_tod  = prescribed_wv_fixed_tod

   ! Turn on prescribed wv if user has specified an input dataset.
   if (len_trim(filename) > 0 .or. len_trim(filelist)>1) has_prescribed_wv = .true.

end subroutine prescribed_wv_readnl

!-------------------------------------------------------------------
!-------------------------------------------------------------------
  subroutine prescribed_wv_adv( state, pbuf2d)

    use tracer_data,  only : advance_trcdata
    use physics_types,only : physics_state
    use ppgrid,       only : begchunk, endchunk
    use ppgrid,       only : pcols, pver
    use string_utils, only : to_lower, GLC
    use physconst,    only : mwdry                ! molecular weight dry air ~ kg/kmole
    
    use physics_buffer, only : physics_buffer_desc, pbuf_get_chunk, pbuf_get_field, pbuf_set_field

    implicit none

    type(physics_state), intent(in)    :: state(begchunk:endchunk)                 
    
    type(physics_buffer_desc), pointer :: pbuf2d(:,:)

    if( .not. has_prescribed_wv ) return

    call advance_trcdata( fields, file, state, pbuf2d )
    call get_curr_date(yr, mon, day, ncsec)
    ymd = yr*10000 + mon*100 + day
    datatimem=file%datatimem
    datatimep=file%datatimep
    write(iulog,*) 'ymd=',ymd,'tod=',ncsec,'datatimem=',datatimem,'datatimep=',datatimep
  end subroutine prescribed_wv_adv

!-------------------------------------------------------------------

  subroutine init_prescribed_wv_restart( piofile )
    use pio, only : file_desc_t
    use tracer_data, only : init_trc_restart
    implicit none
    type(file_desc_t),intent(inout) :: pioFile     ! pio File pointer

    call init_trc_restart( 'prescribed_wv', piofile, file )

  end subroutine init_prescribed_wv_restart
!-------------------------------------------------------------------
  subroutine write_prescribed_wv_restart( piofile )
    use tracer_data, only : write_trc_restart
    use pio, only : file_desc_t
    implicit none

    type(file_desc_t) :: piofile

    call write_trc_restart( piofile, file )

  end subroutine write_prescribed_wv_restart

!-------------------------------------------------------------------
  subroutine read_prescribed_wv_restart( pioFile )
    use tracer_data, only : read_trc_restart
    use pio, only : file_desc_t
    implicit none

    type(file_desc_t) :: piofile
    
    call read_trc_restart( 'prescribed_wv', piofile, file )

  end subroutine read_prescribed_wv_restart
!================================================================================================

end module prescribed_wv
