module forbenchmark_default
   !! author: Seyed Ali Ghasemi
   !! license: BSD 3-Clause License
   !! A Fortran module for benchmarking and performance evaluation for non-coarray codes.
   !!

   use kinds
   use fortime, only: timer

   implicit none

   private

   public benchmark

   !===============================================================================
   type :: mark
      !! author: Seyed Ali Ghasemi
      !! Derived type for each method being benchmarked.
      !!
      character(:), allocatable :: method       !! Name of the method being benchmarked
      character(:), allocatable :: description  !! Description of the method being benchmarked
      type(timer)               :: time         !! Timer object to measure elapsed time
      real(rk)                  :: elapsed_time !! Elapsed time for the benchmark
      real(rk)                  :: speedup      !! Speedup relative to a reference benchmark
      real(rk)                  :: flops        !! Floating-point operations per second
   contains
      procedure, private :: finalize_mark       !! Finalize procedure for mark type
   end type mark
   !===============================================================================


   !===============================================================================
   type :: benchmark
      !! author: Seyed Ali Ghasemi
      !! Derived type for benchmarking and performance evaluation.
      !!
      type(mark),  dimension(:), allocatable :: marks     !! Array of marks to store benchmark data
      character(:),              allocatable :: filename  !! Filename for storing benchmark data
      integer                                :: nloops    !! Number of loops for each benchmark
      integer(ik), dimension(:), allocatable :: argi      !! Integer arguments for benchmarks
      real(rk),    dimension(:), allocatable :: argr      !! Real arguments for benchmarks
      character(:),              allocatable :: timer     !! Timer object for measuring time
      integer                                :: imark     !! Index of current benchmark mark
   contains
      procedure          :: init             !! Initialize the benchmark object
      procedure          :: start_benchmark  !! Start a benchmark
      procedure          :: stop_benchmark   !! Stop a benchmark
      procedure, private :: write_benchmark  !! Write benchmark data to file
      procedure          :: finalize         !! Finalize the benchmark object
   end type benchmark
   !===============================================================================

contains

   !===============================================================================
   elemental impure subroutine init(this, nmarks, title, filename, nloops, timer)
      !! author: Seyed Ali Ghasemi
      !! Initialize the benchmark object.
      !!
      use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options

      class(benchmark), intent(inout)        :: this       !! Benchmark object
      integer,          intent(in)           :: nmarks     !! Number of methods to be benchmarked
      character(*),     intent(in), optional :: title      !! Title of the benchmark
      character(*),     intent(in), optional :: filename   !! Filename for storing benchmark data
      integer,          intent(in), optional :: nloops     !! Number of loops for each benchmark (default: 10)
      character(*),     intent(in), optional :: timer      !! Timer object for measuring time (default: wall). The timer options available are 'wall', 'date_and_time', 'cpu', 'omp', and 'mpi'.
      integer                                :: nunit      !! Unit number for file access
      integer                                :: iostat     !! I/O status
      integer                                :: which_compiler !! Logical variables for compiler detection
      character(:), allocatable              :: compiler   !! Compiler name

      if (nmarks <= 0) error stop 'nmarks must be greater than zero.'

      allocate(this%marks(nmarks))

      compiler =''
      which_compiler = index(compiler_version(), 'Intel(R) Fortran Compiler')
      if (which_compiler /= 0) compiler = '_ifx'
      which_compiler = index(compiler_version(), 'Intel(R) Fortran Intel(R)')
      if (which_compiler /= 0) compiler = '_ifort'
      which_compiler = index(compiler_version(), 'GCC')
      if (which_compiler /= 0) compiler = '_gfortran'
      which_compiler = index(compiler_version(), 'nvfortran')
      if (which_compiler /= 0) compiler = '_nvfortran'

      if (present(filename)) then
         this%filename = trim(filename//compiler//'.data')
      else
         this%filename = 'benchmark'//compiler//'.data'
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
      !! Start a specific benchmark
      !!
      use face

      class(benchmark),          intent(inout)        :: this          !! Benchmark object
      integer,                   intent(in)           :: imark         !! Index of the current method
      character(*),              intent(in)           :: method        !! Name of the method being benchmarked
      integer(ik), dimension(:), intent(in), optional :: argi          !! Integer arguments for the benchmark (optional)
      real(rk),    dimension(:), intent(in), optional :: argr          !! Real arguments for the benchmark (optional)
      character(*),              intent(in), optional :: description   !! Description of the method being benchmarked (optional)

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
      !! Stops the currently active benchmark, calculates performance metrics, and writes the results to the file and terminal.
      !!
      interface
         impure function Fun(argi, argr)
            import rk, ik
            integer(ik), dimension(:), intent(in), optional :: argi
            real(rk),    dimension(:), intent(in), optional :: argr
            real(rk)                                        :: Fun
         end function Fun
      end interface

      procedure(Fun), optional :: flops !! Function to calculate Floating Point Operations Per Second (optional)

      class(benchmark), intent(inout) :: this !! Benchmark object

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

      if (this%marks(this%imark)%elapsed_time <= epsilon(0.0_rk)) error stop 'Elapsed time is too small'

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
      !! Writes the benchmark data to a specified file, including method, speedup, elapsed time, flops, and other details.
      !!
      class(benchmark), intent(inout) :: this   !! Benchmark object
      integer                         :: nunit  !! Unit number for file access
      character(len=65)               :: fmt    !! Format for write
      logical                         :: exist  !! Logical variable for file existence
      integer                         :: iostat !! I/O status
      integer                         :: lm

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
      !! Finalizes the mark object by deallocating allocated memory for method and description.
      !!
      class(mark), intent(inout) :: this !! Mark object to be finalized

      if (allocated(this%method)) deallocate(this%method)
      if (allocated(this%description)) deallocate(this%description)
   end subroutine finalize_mark
   !===============================================================================


   !===============================================================================
   elemental impure subroutine finalize(this)
      !! author: Seyed Ali Ghasemi
      !! Finalizes the benchmark object by deallocating memory and performs necessary cleanup.
      !!
      class(benchmark), intent(inout) :: this   !! Benchmark object to be finalized
      integer                         :: nunit  !! Unit number for file access
      logical                         :: exist  !! Logical variable for file existence
      integer                         :: iostat !! I/O status

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
      !! Retrieves the current date and time and returns it as a string
      !! It utilizes the intrinsic `date_and_time` function to obtain system time information.
      !! A string containing the current date and time in the format "YYYY.MM.DD - HH:MM:SS".
      !!
      character(21) :: datetime  !! Character containing the current date and time
      character(10) :: date      !! Character containing the current date
      character(8)  :: time      !! Character containing the current time
      integer       :: values(8) !! Array containing the current date and time values
      character(4)  :: year      !! Current year
      character(2)  :: month     !! Current month
      character(2)  :: day       !! Current day
      character(2)  :: hour      !! Current hour
      character(2)  :: minute    !! Current minute
      character(2)  :: second    !! Current second

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
