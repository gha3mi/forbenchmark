module forbenchmark_default
   !! author: Seyed Ali Ghasemi

   use kinds
   use fortime, only: timer

   implicit none

   private

   public benchmark

   !===============================================================================
   type :: mark
   !! author: Seyed Ali Ghasemi
      character(:), allocatable :: method
      character(:), allocatable :: description
      type(timer)               :: time
      real(rk)                  :: elapsed_time
      real(rk)                  :: speedup
      real(rk)                  :: flops
   contains
      procedure, private :: finalize_mark
   end type mark
   !===============================================================================


   !===============================================================================
   type :: benchmark
   !! author: Seyed Ali Ghasemi
      type(mark),  dimension(:), allocatable :: marks
      character(:),              allocatable :: filename
      integer                                :: nloops
      integer(ik), dimension(:), allocatable :: argi
      real(rk),    dimension(:), allocatable :: argr
      character(:),              allocatable :: timer
      integer                                :: imark
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
   elemental impure subroutine init(this, nmarks, title, filename, nloops, timer)
   !! author: Seyed Ali Ghasemi
      use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options

      class(benchmark), intent(inout)        :: this
      integer,          intent(in)           :: nmarks
      character(*),     intent(in), optional :: title
      character(*),     intent(in), optional :: filename
      integer,          intent(in), optional :: nloops
      character(*),     intent(in), optional :: timer
      integer                                :: nunit
      integer                                :: iostat

      if (nmarks <= 0) error stop 'nmarks must be greater than zero.'

      allocate(this%marks(nmarks))

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

      if (present(timer)) then
         select case (trim(timer))
          case ('wall')
            this%timer = 'wall'
          case ('date_and_time')
            this%timer = 'date_and_time'
          case ('cpu')
            this%timer = 'cpu'
          case ('omp')
#if defined(USE_OMP)
            this%timer = 'omp'
#else
            error stop 'Use -DUSE_OMP to enable OpenMP.'
#endif
          case ('mpi')
#if defined(USE_MPI)
            this%timer = 'mpi'
#else
            error stop 'Use -DUSE_MPI to enable MPI.'
#endif
          case default
            error stop 'timer is not valid. Valid options are: wall, date_and_time, cpu, omp, mpi.'
         end select
      else
         this%timer = 'wall'
      end if

      inquire(file=this%filename, iostat=iostat)
      if (iostat /= 0) then
         error stop 'file '//trim(this%filename)//' cannot be accessed.'
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
      &   SPEEDUP    |&
      &         TIME         |&
      &        GFLOPS        |&
      &  NLOOPS  |&
      &   ARGI  '
      close(nunit)
   end subroutine init
   !===============================================================================


   !===============================================================================
   impure subroutine start_benchmark(this, imark, method, description, argi, argr)
   !! author: Seyed Ali Ghasemi
      use face

      class(benchmark),          intent(inout)        :: this
      integer,                   intent(in)           :: imark
      character(*),              intent(in)           :: method
      integer(ik), dimension(:), intent(in), optional :: argi
      real(rk),    dimension(:), intent(in), optional :: argr
      character(*),              intent(in), optional :: description

      if (imark <= 0 .or. imark > size(this%marks)) error stop 'imark is out of range.'

      this%imark = imark

      this%marks(this%imark)%description = description
      this%marks(this%imark)%method      = method

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
         colorize('Meth.: '//this%marks(this%imark)%method, color_fg='green',style='bold_on'),&
         colorize('; Des.: '//this%marks(this%imark)%description, color_fg='green_intense'),&
         '; Argi.:',&
         this%argi
      elseif (present(description) .and. .not. present(argi)) then
         print'(a,a," ",a)',&
         colorize('Meth.: '//this%marks(this%imark)%method, color_fg='green',style='bold_on'),&
         colorize('; Des.: '//this%marks(this%imark)%description, color_fg='green_intense')
      elseif (.not. present(description) .and. present(argi)) then
         print'(a,a,*(g0,1x))',&
         colorize('Meth.: '//this%marks(this%imark)%method, color_fg='green',style='bold_on'),&
         '; Argi.:',&
         this%argi
      else
         print'(a)', colorize('Meth.: '//this%marks(this%imark)%method, color_fg='green',style='bold_on')
      end if

      select case (trim(this%timer))
       case ('wall')
         call this%marks(this%imark)%time%timer_start()
       case ('date_and_time')
         call this%marks(this%imark)%time%dtimer_start()
       case ('cpu')
         call this%marks(this%imark)%time%ctimer_start()
       case ('omp')
#if defined(USE_OMP)
         call this%marks(this%imark)%time%otimer_start()
#else
         error stop 'Use -DUSE_OMP to enable OpenMP.'
#endif
       case ('mpi')
#if defined(USE_MPI)
         call this%marks(this%imark)%time%mtimer_start()
#else
         error stop 'Use -DUSE_MPI to enable MPI.'
#endif
      end select
   end subroutine start_benchmark
   !===============================================================================


   !===============================================================================
   impure subroutine stop_benchmark(this, flops)
   !! author: Seyed Ali Ghasemi

      interface
         impure function Fun(argi, argr)
            import rk, ik
            integer(ik), dimension(:), intent(in), optional :: argi
            real(rk),    dimension(:), intent(in), optional :: argr
            real(rk)                                        :: Fun
         end function Fun
      end interface

      procedure(Fun), optional :: flops

      class(benchmark), intent(inout) :: this

      select case (trim(this%timer))
       case ('wall')
         call this%marks(this%imark)%time%timer_stop(message=' Elapsed time :',nloops=this%nloops)
         this%marks(this%imark)%elapsed_time = this%marks(this%imark)%time%elapsed_time
       case ('date_and_time')
         call this%marks(this%imark)%time%dtimer_stop(message=' Elapsed time :',nloops=this%nloops)
         this%marks(this%imark)%elapsed_time = this%marks(this%imark)%time%elapsed_dtime
       case ('cpu')
         call this%marks(this%imark)%time%ctimer_stop(message=' Elapsed time :',nloops=this%nloops)
         this%marks(this%imark)%elapsed_time = this%marks(this%imark)%time%cpu_time
       case ('omp')
#if defined(USE_OMP)
         call this%marks(this%imark)%time%otimer_stop(message=' Elapsed time :',nloops=this%nloops)
         this%marks(this%imark)%elapsed_time = this%marks(this%imark)%time%omp_time
#else
         error stop 'Use -DUSE_OMP to enable OpenMP.'
#endif
       case ('mpi')
#if defined(USE_MPI)
         call this%marks(this%imark)%time%mtimer_stop(message=' Elapsed time :',nloops=this%nloops)
         this%marks(this%imark)%elapsed_time = this%marks(this%imark)%time%mpi_time
#else
         error stop 'Use -DUSE_MPI to enable MPI.'
#endif
      end select

      this%marks(this%imark)%speedup = this%marks(this%imark)%elapsed_time/this%marks(1)%elapsed_time

      if (present(flops)) then
         print'(a,f7.3,a)', ' Speedup      :', this%marks(this%imark)%speedup,' [-]'
         this%marks(this%imark)%flops = flops(this%argi,this%argr)/this%marks(this%imark)%elapsed_time
         print'(a,f7.3,a)', ' Performance  :', this%marks(this%imark)%flops,' [GFLOPS]'
      else
         this%marks(this%imark)%flops = 0.0_rk
      endif
      print'(a)', ''


      call this%write_benchmark()
   end subroutine stop_benchmark
   !===============================================================================


   !===============================================================================
   impure subroutine write_benchmark(this)
   !! author: Seyed Ali Ghasemi
      class(benchmark), intent(inout) :: this
      integer                         :: nunit
      character(len=65)               :: fmt
      integer                         :: lm
      logical                         :: exist
      integer                         :: iostat

      inquire(file=this%filename, exist=exist, iostat=iostat)
      if (iostat /= 0 .or. .not. exist) then
         error stop 'file '//trim(this%filename)//' does not exist or cannot be accessed.'
      end if
      open (newunit = nunit, file = this%filename, access = 'append')

      lm = 20-len_trim(this%marks(this%imark)%method)
      write(fmt,'(a,g0,a)') '(a,',lm,'x,3x,F12.6,3x,E20.14,3x,E20.14,3x,g8.0,3x,*(g20.0,3x))'

      write(nunit,fmt) &
         this%marks(this%imark)%method,&
         this%marks(this%imark)%speedup,&
         this%marks(this%imark)%elapsed_time,&
         this%marks(this%imark)%flops,&
         this%nloops,&
         this%argi

      close(nunit)
   end subroutine write_benchmark
   !===============================================================================


   !===============================================================================
   elemental pure subroutine finalize_mark(this)
   !! author: Seyed Ali Ghasemi
      class(mark), intent(inout) :: this

      if (allocated(this%method)) deallocate(this%method)
      if (allocated(this%description)) deallocate(this%description)
   end subroutine finalize_mark
   !===============================================================================


   !===============================================================================
   elemental impure subroutine finalize(this)
   !! author: Seyed Ali Ghasemi
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

      call this%marks(:)%finalize_mark()
      if (allocated(this%filename)) deallocate(this%filename)
      if (allocated(this%argi)) deallocate(this%argi)
      if (allocated(this%argr)) deallocate(this%argr)

      print'(a)', 'end of benchmark'

   end subroutine finalize
   !===============================================================================


   !===============================================================================
   impure function current_date_and_time() result(datetime)
   !! author: Seyed Ali Ghasemi
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
