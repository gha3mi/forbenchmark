module forbenchmark_coarray

#if defined(USE_COARRAY)

   use kinds
   use fortime,  only: timer

   implicit none

   private

   public benchmark

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   type :: benchmark
      type(timer),  allocatable           :: time[:]
      character(:), allocatable           :: filename
      character(:), allocatable           :: method
      character(:), allocatable           :: description
      integer                             :: nloops
      integer,  dimension(:), allocatable :: argi
      real(rk), dimension(:), allocatable :: argr
      real(rk), allocatable               :: gflops[:]
      real(rk)                            :: elapsed_time_average
      real(rk)                            :: gflops_total
   contains
      procedure          :: init
      procedure          :: start_benchmark
      procedure          :: stop_benchmark
      procedure, private :: write_benchmark
      procedure          :: finalize
   end type benchmark
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine init(this, title, filename, nloops)
      use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options

      class(benchmark),      intent(inout) :: this
      character(*),          intent(in)    :: title
      character(*),          intent(in)    :: filename
      integer,               intent(in)    :: nloops
      integer                              :: unit_num

      this%filename = trim(filename)
      this%nloops   = nloops

      allocate(this%time[*])
      allocate(this%gflops[*])

      open (newunit = unit_num, file = this%filename)
      write(unit_num,'(a)') '-----------------------------------------------------'
      write(unit_num,'(a)') 'ForBenchmark - https://github.com/gha3mi/forbenchmark'
      write(unit_num,'(a)') '-----------------------------------------------------'
      write(unit_num,'(a)') ''
      write(unit_num,'(a)') trim(title)
      write(unit_num,'(a)') ''
      write(unit_num,'(a,a)') 'compiler_version: ', compiler_version()
      write(unit_num,'(a,a)') 'compiler_options: ', compiler_options()
      write(unit_num,'(a)') ''
      write(unit_num,'(a)') &
      &'       METHOD        |&
      &      TIME(avg)       |&
      &     GFLOPS(tot)      |&
      &     TIME(image)      |&
      &    GFLOPS(image)     |&
      &  NLOOPS  |&
      &   ARGS  '
      close(unit_num)
   end subroutine init
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine start_benchmark(this, method, description, argi, argr)
      use face

      class(benchmark),       intent(inout)        :: this
      character(*),           intent(in)           :: method
      integer,  dimension(:), intent(in)           :: argi
      real(rk), dimension(:), intent(in), optional :: argr
      character(*),           intent(in), optional :: description

      this%description = description
      this%method      = method
      this%argi        = argi
      if (present(argr)) this%argr = argr

      sync all

      if (present(description) .and. this_image() == 1) then
         print'(a,a," ",a,*(g0,1x))',&
            colorize('Meth.: '//this%method, color_fg='green',style='bold_on'),&
            colorize('; Des.: '//this%description, color_fg='green_intense'),&
            '; Args.:',&
            this%argi
      elseif (.not. present(description) .and. this_image() == 1) then
         print'(a," ",*(g0,1x))',&
            colorize('Meth.: '//this%method, color_fg='green',style='bold_on'), this%argi
      end if

      call this%time[this_image()]%timer_start()
   end subroutine start_benchmark
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine stop_benchmark(this, flops)
      use face

      interface
         impure function Fun(argi, argr)
            import rk
            integer,  dimension(:), intent(in)           :: argi
            real(rk), dimension(:), intent(in), optional :: argr
            real(rk)                                     :: Fun
         end function Fun
      end interface

      procedure(Fun) :: flops

      class(benchmark), intent(inout) :: this
      real(rk)                   :: elapsed_time_average
      real(rk)                   :: gflops_total
      integer                    :: i

      call this%time[this_image()]%timer_stop(message=' Elapsed time :',nloops=this%nloops)

      this%gflops[this_image()] = flops(this%argi,this%argr)/this%time[this_image()]%elapsed_time

      print'(a,f6.2,a)', ' Performance  : ', this%gflops[this_image()],' [GFLOPS/image]'

      sync all

      if (this_image() == 1) then
         elapsed_time_average = 0.0_rk
         gflops_total = 0.0_rk
         do i = 1,num_images()
            elapsed_time_average = elapsed_time_average + this%time[i]%elapsed_time
            gflops_total = gflops_total + this%gflops[i]
         end do
         elapsed_time_average = elapsed_time_average/num_images()
         print'(a,f7.3,a)', colorize(' Elapsed time (average) :', color_fg='blue'),&
            elapsed_time_average, ' [s]'
         print'(a,f6.2,a)', colorize(' Performance  (total)   : ', color_fg='cyan'),&
            gflops_total, ' [GFLOPS]'
         print'(a)', ''
      end if
      call co_broadcast(elapsed_time_average, 1)
      call co_broadcast(gflops_total, 1)
      this%elapsed_time_average = elapsed_time_average
      this%gflops_total = gflops_total
      call this%write_benchmark()
   end subroutine stop_benchmark
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine write_benchmark(this)
      class(benchmark), intent(inout) :: this
      integer                         :: nunit
      character(len=75)               :: fmt
      integer                         :: lm

      lm = 20-len_trim(this%method)
      write(fmt,'(a,g0,a)') '(a,',lm,'x,3x,E20.14,3x,E20.14,3x,E20.14,3x,E20.14,3x,g8.0,3x,*(g8.0,3x))'

      open (newunit = nunit, file = this%filename, access = 'append')
      write(nunit,fmt) &
         this%method,&
         this%elapsed_time_average,&
         this%gflops_total,&
         this%time[this_image()]%elapsed_time,&
         this%gflops[this_image()],&
         this%nloops,&
         this%argi
      close(nunit)
   end subroutine write_benchmark
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine finalize(this)
      class(benchmark), intent(inout) :: this
      integer                         :: nunit

      if (allocated(this%argi)) deallocate(this%argi)
      if (allocated(this%argr)) deallocate(this%argr)
      if (allocated(this%time)) deallocate(this%time)
      if (allocated(this%gflops)) deallocate(this%gflops)

      if (this_image() == 1) print'(a)', 'end of benchmark'

      open (newunit = nunit, file = this%filename, access = 'append')
      write(nunit,'(a)') 'end of benchmark'
      close(nunit)

   end subroutine finalize
   !===============================================================================

#endif

end module forbenchmark_coarray
