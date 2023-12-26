module forbenchmark_serial

   use kinds
   use fortime,  only: timer

   implicit none

   private

   public benchmark

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   type :: benchmark
      type(timer)                         :: time
      character(:), allocatable           :: filename
      character(:), allocatable           :: method
      character(:), allocatable           :: description
      integer                             :: nloops
      integer,  dimension(:), allocatable :: argi
      real(rk), dimension(:), allocatable :: argr
      real(rk)                            :: gflops
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
      &         TIME         |&
      &        GFLOPS        |&
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

      if (present(description)) then
         print'(a,a," ",a,*(g0,1x))',&
         colorize('Meth.: '//this%method, color_fg='green',style='bold_on'),&
         colorize('; Des.: '//this%description, color_fg='green_intense'),&
         '; Args.:',&
         this%argi
      else
         print'(a," ",*(g0,1x))',&
         colorize('Meth.: '//this%method, color_fg='green',style='bold_on'), this%argi
      end if

      call this%time%timer_start()
   end subroutine start_benchmark
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine stop_benchmark(this, flops)

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

      call this%time%timer_stop(message=' Elapsed time :',nloops=this%nloops)

      this%gflops = flops(this%argi,this%argr)/this%time%elapsed_time

      print'(a,f6.2,a)', ' Performance  : ', this%gflops,' [GFLOPS]'
      print'(a)', ''

      call this%write_benchmark()
   end subroutine stop_benchmark
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine write_benchmark(this)
      class(benchmark), intent(inout) :: this
      integer                         :: nunit
      character(len=55)               :: fmt
      integer                         :: lm

      lm = 20-len_trim(this%method)
      write(fmt,'(a,g0,a)') '(a,',lm,'x,3x,E20.14,3x,E20.14,3x,g8.0,3x,*(g8.0,3x))'

      open (newunit = nunit, file = this%filename, access = 'append')
      write(nunit,fmt) &
         this%method, this%time%elapsed_time, this%gflops, this%nloops, this%argi
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

      print'(a)', 'end of benchmark'

      open (newunit = nunit, file = this%filename, access = 'append')
      write(nunit,'(a)') 'end of benchmark'
      close(nunit)

   end subroutine finalize
   !===============================================================================

end module forbenchmark_serial
