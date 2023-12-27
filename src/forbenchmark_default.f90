module forbenchmark_default

   use kinds
   use fortime, only: timer

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
   elemental impure subroutine init(this, title, filename, nloops)
      use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options

      class(benchmark), intent(inout)        :: this
      character(*),     intent(in), optional :: title
      character(*),     intent(in), optional :: filename
      integer,          intent(in), optional :: nloops
      integer                                :: nunit
      logical                                :: exist
      integer                                :: iostat

      if (present(filename)) then
         this%filename = trim(filename//'.data')
      else
         this%filename = 'benchmark.data'
      endif
      
      if (present(nloops)) then
         if (nloops <= 0) error stop 'nloops must be greater than zero.'
         this%nloops = nloops
      else
         this%nloops = 10
      end if

      inquire(file=this%filename, exist=exist, iostat=iostat)
      if (iostat /= 0 .or. .not. exist) then
         error stop 'file '//trim(this%filename)//' does not exist or cannot be accessed.'
      end if
      open (newunit = nunit, file = this%filename)
      write(nunit,'(a)') '-----------------------------------------------------'
      write(nunit,'(a)') 'ForBenchmark - https://github.com/gha3mi/forbenchmark'
      write(nunit,'(a)') '-----------------------------------------------------'
      write(nunit,'(a)') ''
      if (present(title)) then
      write(nunit,'(a)') trim(title)
      else
      write(nunit,'(a)') 'ForBenchmark'
      endif
      write(nunit,'(a)') current_date_and_time()
      write(nunit,'(a)') ''
      write(nunit,'(a,a)') 'compiler_version: ', compiler_version()
      write(nunit,'(a,a)') 'compiler_options: ', compiler_options()
      write(nunit,'(a)') ''
      write(nunit,'(a)') ''
      write(nunit,'(a)') &
      &'       METHOD        |&
      &         TIME         |&
      &        GFLOPS        |&
      &  NLOOPS  |&
      &   ARGS  '
      close(nunit)
   end subroutine init
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   impure subroutine start_benchmark(this, method, description, argi, argr)
      use face

      class(benchmark),       intent(inout)        :: this
      character(*),           intent(in)           :: method
      integer,  dimension(:), intent(in), optional :: argi
      real(rk), dimension(:), intent(in), optional :: argr
      character(*),           intent(in), optional :: description

      this%description = description
      this%method      = method

      if (present(argi)) then
         this%argi = argi
      else
         if(.not. allocated(this%argi)) allocate(this%argi(0))
      endif

      if (present(argr)) then
         this%argr = argr
      else
         if(.not. allocated(this%argr)) allocate(this%argr(0))
      endif

      if (present(description) .and. present(argi)) then
         print'(a,a," ",a,*(g0,1x))',&
         colorize('Meth.: '//this%method, color_fg='green',style='bold_on'),&
         colorize('; Des.: '//this%description, color_fg='green_intense'),&
         '; Args.:',&
         this%argi
      elseif (present(description) .and. .not. present(argi)) then
         print'(a,a," ",a)',&
         colorize('Meth.: '//this%method, color_fg='green',style='bold_on'),&
         colorize('; Des.: '//this%description, color_fg='green_intense')
      elseif (.not. present(description) .and. present(argi)) then
         print'(a,a,*(g0,1x))',&
         colorize('Meth.: '//this%method, color_fg='green',style='bold_on'),&
         '; Args.:',&
         this%argi
      else
         print'(a)', colorize('Meth.: '//this%method, color_fg='green',style='bold_on')
      end if

      call this%time%timer_start()
   end subroutine start_benchmark
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   impure subroutine stop_benchmark(this, flops)

      interface
         impure function Fun(argi, argr)
            import rk
            integer,  dimension(:), intent(in), optional :: argi
            real(rk), dimension(:), intent(in), optional :: argr
            real(rk)                                     :: Fun
         end function Fun
      end interface

      procedure(Fun), optional :: flops

      class(benchmark), intent(inout) :: this

      call this%time%timer_stop(message=' Elapsed time :',nloops=this%nloops)

      if (present(flops)) then
         this%gflops = flops(this%argi,this%argr)/this%time%elapsed_time
         print'(a,f6.2,a)', ' Performance  : ', this%gflops,' [GFLOPS]'
      else
         this%gflops = 0.0_rk
      endif
      print'(a)', ''

      call this%write_benchmark()
   end subroutine stop_benchmark
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   impure subroutine write_benchmark(this)
      class(benchmark), intent(inout) :: this
      integer                         :: nunit
      character(len=55)               :: fmt
      integer                         :: lm
      logical                         :: exist
      integer                         :: iostat

      lm = 20-len_trim(this%method)
      write(fmt,'(a,g0,a)') '(a,',lm,'x,3x,E20.14,3x,E20.14,3x,g8.0,3x,*(g8.0,3x))'

      inquire(file=this%filename, exist=exist, iostat=iostat)
      if (iostat /= 0 .or. .not. exist) then
         error stop 'file '//trim(this%filename)//' does not exist or cannot be accessed.'
      end if
      open (newunit = nunit, file = this%filename, access = 'append')
      write(nunit,fmt) &
         this%method, this%time%elapsed_time, this%gflops, this%nloops, this%argi
      close(nunit)
   end subroutine write_benchmark
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine finalize(this)
      class(benchmark), intent(inout) :: this
      integer                         :: nunit
      logical                         :: exist
      integer                         :: iostat

      inquire(file=this%filename, exist=exist, iostat=iostat)
      if (iostat /= 0 .or. .not. exist) then
         error stop 'file '//trim(this%filename)//' does not exist or cannot be accessed.'
      end if
      open (newunit = nunit, file = this%filename, access = 'append')
      write(nunit,'(a)') 'end of benchmark'
      close(nunit)

      if (allocated(this%filename)) deallocate(this%filename)
      if (allocated(this%method)) deallocate(this%method)
      if (allocated(this%description)) deallocate(this%description)
      if (allocated(this%argi)) deallocate(this%argi)
      if (allocated(this%argr)) deallocate(this%argr)

      print'(a)', 'end of benchmark'

   end subroutine finalize
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   impure function current_date_and_time() result(datetime)
      character(21) :: datetime
      character(10) :: date
      character(8)  :: time
      integer       :: values(8)
      character(4)  :: year
      character(2)  :: month, day, hour, minute, second
      
      call date_and_time(values=values)

      write(year,'(i4)')   values(1)
      write(month,'(i2)')  values(2)
      write(day,'(i2)')    values(3)
      write(hour,'(i2)')   values(5)
      write(minute,'(i2)') values(6)
      write(second,'(i2)') values(7)
      date=year//'.'//month//'.'//day
      time=hour//':'//minute//':'//second
      datetime = date//' - '//time
   end function current_date_and_time
   !===============================================================================

end module forbenchmark_default
