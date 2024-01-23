module forbenchmark_coarray
   !! author: Seyed Ali Ghasemi
   !! license: BSD 3-Clause License
   !! A Fortran module for benchmarking and performance evaluation for coarray codes.
   !!

#if defined(USE_COARRAY)

   use kinds
   use fortime, only: timer

   implicit none

   private

   public benchmark

   !===============================================================================
   type :: mark_co
      !! author: Seyed Ali Ghasemi
      !! Derived type for each method being benchmarked in each image.
      !!
      type(timer) :: time          !! Timer object to measure elapsed time in each image
      real(rk)    :: elapsed_time  !! Elapsed time for the benchmark in each image
      real(rk)    :: flops         !! Floating-point operations per second in each image
   end type mark_co
   !===============================================================================


   !===============================================================================
   type :: mark
      !! author: Seyed Ali Ghasemi
      !! Derived type for each method being benchmarked in all images.
      !!
      character(:), allocatable :: method                !! Name of the method being benchmarked
      character(:), allocatable :: description           !! Description of the method being benchmarked
      real(rk)                  :: elapsed_time_min      !! Minimum elapsed time for the benchmark in all images
      real(rk)                  :: elapsed_time_average  !! Average elapsed time for the benchmark in all images
      real(rk)                  :: elapsed_time_max      !! Maximum elapsed time for the benchmark in all images
      real(rk)                  :: flops_total           !! Total floating-point operations per second in all images
      real(rk)                  :: speedup_max_total     !! Maximum speedup in all images compared to the reference benchmark
   contains
      procedure, private :: finalize_mark !! Finalize procedure for mark type
   end type mark
   !===============================================================================


   !===============================================================================
   type :: benchmark
      !! author: Seyed Ali Ghasemi
      !! Derived type for benchmarking and performance evaluation.
      !!
      type(mark_co), dimension(:), allocatable :: marks_co[:]     !! Array of mark_co type for each method being benchmarked in each image
      type(mark),    dimension(:), allocatable :: marks           !! Array of mark type for each method being benchmarked in all images
      character(:),                allocatable :: filename        !! Filename for storing the benchmark data in all images
      character(:),                allocatable :: filename_image  !! Filename for storing the benchmark data in each image
      integer                                  :: nloops          !! Number of loops for each benchmark
      integer(ik),   dimension(:), allocatable :: argi            !! Integer arguments for benchmarks
      real(rk),      dimension(:), allocatable :: argr            !! Real arguments for benchmarks
      character(:),                allocatable :: timer           !! Timer object for measuring time
      integer                                  :: imark           !! Index of the current benchmark
   contains
      procedure          :: init             !! Initialization the benchmark object
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

      class(benchmark), intent(inout)        :: this      !! Benchmark object
      integer,          intent(in)           :: nmarks    !! Number of methods being benchmarked
      character(*),     intent(in), optional :: title     !! Title of the benchmark
      character(*),     intent(in), optional :: filename  !! Filename for storing the benchmark data
      integer,          intent(in), optional :: nloops    !! Number of loops for each benchmark (default: 10)
      character(*),     intent(in), optional :: timer     !! Timer object for measuring time (default: wall)
      integer                                :: nunit     !! Unit number for file access
      integer                                :: iostat    !! I/O status
      character(10)                          :: im_chr    !! Character representation of the image number
      integer                                :: which_compiler !! Logical variables for compiler detection
      character(:), allocatable              :: compiler   !! Compiler name

      if (nmarks <= 0) error stop 'nmarks must be greater than zero.'

      compiler =''
      which_compiler = index(compiler_version(), 'Intel(R) Fortran Compiler')
      if (which_compiler /= 0) compiler = '_ifx'
      which_compiler = index(compiler_version(), 'Intel(R) Fortran Intel(R)')
      if (which_compiler /= 0) compiler = '_ifort'
      which_compiler = index(compiler_version(), 'GCC')
      if (which_compiler /= 0) compiler = '_gfortran'
      which_compiler = index(compiler_version(), 'nvfortran')
      if (which_compiler /= 0) compiler = '_nvfortran'

      write (im_chr, '(i0)') this_image()
      if (present(filename)) then
         this%filename_image = trim(filename//compiler//'_im'//trim(im_chr)//'.data')
         this%filename = trim(filename//compiler//'_co'//'.data')
      else
         this%filename_image = trim('benchmark'//compiler//'_im'//trim(im_chr)//'.data')
         this%filename = trim('benchmark'//compiler//'_co'//'.data')
      end if

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

      allocate(this%marks_co(nmarks)[*])
      allocate(this%marks(nmarks))

      inquire(file=this%filename_image, iostat=iostat)
      if (iostat /= 0) then
         error stop 'file '//trim(this%filename_image)//' cannot be accessed.'
      end if
      open (newunit = nunit, file = this%filename_image)
      write(nunit,'(a)') '-----------------------------------------------------'
      write(nunit,'(a)') 'ForBenchmark - https://github.com/gha3mi/forbenchmark'
      write(nunit,'(a)') '-----------------------------------------------------'
      write(nunit,'(a)') ''
      if (present(title)) then
         write(nunit,'(a)') trim(title)
      else
         write(nunit,'(a)') 'ForBenchmark'
      end if
      write(nunit,'(a)') current_date_and_time()
      write(nunit,'(a)') ''
      write(nunit,'(a,a)') 'compiler_version: ', compiler_version()
      write(nunit,'(a,a)') 'compiler_options: ', compiler_options()
      write(nunit,'(a,g0,a,g0)') 'image: ',this_image(),' of ',num_images()
      write(nunit,'(a)') ''
      write(nunit,'(a)') &
      &'       METHOD        |&
      &     TIME(image)      |&
      &    GFLOPS(image)     |&
      &  NLOOPS  |&
      &   ARGI  '
      close(nunit)

      if (this_image() == 1) then
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
         end if
         write(nunit,'(a)') current_date_and_time()
         write(nunit,'(a)') ''
         write(nunit,'(a,a)') 'compiler_version: ', compiler_version()
         write(nunit,'(a,a)') 'compiler_options: ', compiler_options()
         write(nunit,'(a,g0)') 'num_image: ', num_images()
         write(nunit,'(a)') ''
         write(nunit,'(a)') &
         &'       METHOD        |&
         & SPEEDUP(max) |&
         &      TIME(max)       |&
         &      TIME(min)       |&
         &      TIME(avg)       |&
         &     GFLOPS(tot)      |&
         &  NLOOPS  |&
         &   ARGI  '
         close(nunit)
      end if

   end subroutine init
   !===============================================================================


   !===============================================================================
   impure subroutine start_benchmark(this, imark, method, description, argi, argr)
      !! author: Seyed Ali Ghasemi
      !! Start a specific benchmark.
      !!
      use face

      class(benchmark),          intent(inout)        :: this         !! Benchmark object
      integer,                   intent(in)           :: imark        !! Index of the current method
      character(*),              intent(in)           :: method       !! Name of the method being benchmarked
      integer(ik), dimension(:), intent(in), optional :: argi         !! Integer arguments for benchmarks (optional)
      real(rk),    dimension(:), intent(in), optional :: argr         !! Real arguments for benchmarks (optional)
      character(*),              intent(in), optional :: description  !! Description of the method being benchmarked (optional)

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

      sync all

      if (present(description) .and. present(argi) .and. this_image() == 1) then
         print'(a,a," ",a,*(g0,1x))',&
            colorize('Meth.: '//this%marks(this%imark)%method, color_fg='green',style='bold_on'),&
            colorize('; Des.: '//this%marks(this%imark)%description, color_fg='green_intense'),&
            '; Argi.:',&
            this%argi
      elseif (.not. present(description) .and. present(argi) .and. this_image() == 1) then
         print'(a," ",a,*(g0,1x))',&
            colorize('Meth.: '//this%marks(this%imark)%method, color_fg='green',style='bold_on'),&
            '; Argi.:',&
            this%argi
      elseif (present(description) .and. .not. present(argi) .and. this_image() == 1) then
         print'(a,a)',&
            colorize('Meth.: '//this%marks(this%imark)%method, color_fg='green',style='bold_on'),&
            colorize('; Des.: '//this%marks(this%imark)%description, color_fg='green_intense')
      elseif (.not. present(description) .and. .not. present(argi) .and. this_image() == 1) then
         print'(a)', colorize('Meth.: '//this%marks(this%imark)%method, color_fg='green',style='bold_on')
      end if

      select case (trim(this%timer))
       case ('wall')
         call this%marks_co(this%imark)%time%timer_start()
       case ('date_and_time')
         call this%marks_co(this%imark)%time%dtimer_start()
       case ('cpu')
         call this%marks_co(this%imark)%time%ctimer_start()
       case ('omp')
#if defined(USE_OMP)
         call this%marks_co(this%imark)%time%ptimer_start()
#else
         error stop 'Use -DUSE_OMP to enable OpenMP.'
#endif
       case ('mpi')
#if defined(USE_MPI)
         call this%marks_co(this%imark)%time%mtimer_start()
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
      use face

      interface
         impure function Fun(argi, argr)
            import rk, ik
            integer(ik), dimension(:), intent(in), optional :: argi
            real(rk),    dimension(:), intent(in), optional :: argr
            real(rk)                                        :: Fun
         end function Fun
      end interface

      procedure(Fun), optional :: flops !! Function to calculate Floating Point Operations Per Second (optional)

      class(benchmark), intent(inout)     :: this                 !! Benchmark object
      real(rk)                            :: elapsed_time_average !! Average elapsed time for the benchmark in all images
      real(rk)                            :: elapsed_time_min     !! Minimum elapsed time for the benchmark in all images
      real(rk)                            :: elapsed_time_max     !! Maximum elapsed time for the benchmark in all images
      real(rk)                            :: flops_total          !! Total floating-point operations per second in all images
      real(rk), dimension(:), allocatable :: elapsed_times        !! Array of elapsed times in all images
      integer                             :: i

      select case (trim(this%timer))
       case ('wall')
         call this%marks_co(this%imark)%time%timer_stop(message=' Elapsed time :',nloops=this%nloops)
         this%marks_co(this%imark)%elapsed_time = this%marks_co(this%imark)%time%elapsed_time
       case ('date_and_time')
         call this%marks_co(this%imark)%time%dtimer_stop(message=' Elapsed time :',nloops=this%nloops)
         this%marks_co(this%imark)%elapsed_time = this%marks_co(this%imark)%time%elapsed_dtime
       case ('cpu')
         call this%marks_co(this%imark)%time%ctimer_stop(message=' Elapsed time :',nloops=this%nloops)
         this%marks_co(this%imark)%elapsed_time = this%marks_co(this%imark)%time%cpu_time
       case ('omp')
#if defined(USE_OMP)
         call this%marks_co(this%imark)%time%otimer_stop(message=' Elapsed time :',nloops=this%nloops)
         this%marks_co(this%imark)%elapsed_time = this%marks_co(this%imark)%time%omp_time
#else
         error stop 'Use -DUSE_OMP to enable OpenMP.'
#endif
       case ('mpi')
#if defined(USE_MPI)
         call this%marks_co(this%imark)%time%mtimer_stop(message=' Elapsed time :',nloops=this%nloops)
         this%marks_co(this%imark)%elapsed_time = this%marks_co(this%imark)%time%mpi_time
#else
         error stop 'Use -DUSE_MPI to enable MPI.'
#endif
      end select

      if (present(flops)) then
         if (this%marks_co(this%imark)%elapsed_time <= epsilon(0.0_rk)) error stop 'Elapsed time is too small.'
         this%marks_co(this%imark)%flops = flops(this%argi,this%argr)/this%marks_co(this%imark)%elapsed_time
         print'(a,f7.3,a)', ' Performance  :', this%marks_co(this%imark)%flops,' [GFLOPS/image]'
      else
         this%marks_co(this%imark)%flops = 0.0_rk
      end if

      sync all

      if (this_image() == 1) then
         allocate(elapsed_times(num_images()))
         do i = 1, num_images()
            elapsed_times(i) = this%marks_co(this%imark)[i]%elapsed_time
         end do
         elapsed_time_max = maxval(elapsed_times)
         elapsed_time_min = minval(elapsed_times)
         elapsed_time_average = sum(elapsed_times)/num_images()

         if (present(flops)) flops_total = 0.0_rk
         do i = 1,num_images()
            if (present(flops)) flops_total = flops_total + this%marks_co(this%imark)[i]%flops
         end do
      end if
      call co_broadcast(elapsed_time_average, 1)
      call co_broadcast(elapsed_time_min, 1)
      call co_broadcast(elapsed_time_max, 1)
      if (present(flops)) call co_broadcast(flops_total, 1)
      this%marks(this%imark)%elapsed_time_average = elapsed_time_average
      this%marks(this%imark)%elapsed_time_min = elapsed_time_min
      this%marks(this%imark)%elapsed_time_max = elapsed_time_max

      if (this%marks(1)%elapsed_time_max <= epsilon(0.0_rk)) error stop 'Maximum elapsed time for the reference benchmark is too small.'
      this%marks(this%imark)%speedup_max_total = this%marks(1)%elapsed_time_max/this%marks(this%imark)%elapsed_time_max


      if (this_image()==1) then
         print'(a,f7.3,a)', colorize(' Elapsed time (max)     :', color_fg='blue'),&
            this%marks(this%imark)%elapsed_time_max, ' [s]'
         print'(a,f7.3,a)', colorize(' Elapsed time (min)     :', color_fg='blue'),&
            this%marks(this%imark)%elapsed_time_min, ' [s]'

         print'(a,f7.3,a)', colorize(' Elapsed time (average) :', color_fg='blue'),&
            this%marks(this%imark)%elapsed_time_average, ' [s]'

         print'(a,f7.3,a)', colorize(' Speedup (max)          :', color_fg='blue'),&
            this%marks(this%imark)%speedup_max_total, ' [-]'
         if (present(flops)) print'(a,f7.3,a)', colorize(' Performance  (total)   :', color_fg='cyan'),&
            flops_total, ' [GFLOPS]'
         print'(a)', ''
      end if


      if (present(flops)) then
         this%marks(this%imark)%flops_total = flops_total
      else
         this%marks(this%imark)%flops_total = 0.0_rk
      end if

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
      character(len=53)               :: fmt1   !! Format for write
      character(len=82)               :: fmt2   !! Format for write
      logical                         :: exist  !! Logical variable for file existence
      integer                         :: iostat !! I/O status
      integer                         :: lm

      lm = 20-len_trim(this%marks(this%imark)%method)
      write(fmt1,'(a,g0,a)')&
         '(a,',lm,'x,3x,E20.14,3x,E20.14,3x,g8.0,3x,*(g8.0,3x))'

      inquire(file=this%filename_image, exist=exist, iostat=iostat)
      if (iostat /= 0 .or. .not. exist) then
         error stop 'file '//trim(this%filename_image)//' does not exist or cannot be accessed.'
      end if
      open (newunit = nunit, file = this%filename_image, access = 'append')
      write(nunit,fmt1) &
         this%marks(this%imark)%method,&
         this%marks_co(this%imark)%time%elapsed_time,&
         this%marks_co(this%imark)%flops,&
         this%nloops,&
         this%argi
      close(nunit)

      if (this_image() == 1) then
         write(fmt2,'(a,g0,a)')&
            '(a,',lm,'x,3x,F12.6,3x,E20.14,3x,E20.14,3x,E20.14,3x,E20.14,3x,g8.0,3x,*(g20.0,3x))'

         inquire(file=this%filename, exist=exist, iostat=iostat)
         if (iostat /= 0 .or. .not. exist) then
            error stop 'file '//trim(this%filename)//' does not exist or cannot be accessed.'
         end if
         open (newunit = nunit, file = this%filename, access = 'append')
         write(nunit,fmt2) &
            this%marks(this%imark)%method,&
            this%marks(this%imark)%speedup_max_total,&
            this%marks(this%imark)%elapsed_time_max,&
            this%marks(this%imark)%elapsed_time_min,&
            this%marks(this%imark)%elapsed_time_average,&
            this%marks(this%imark)%flops_total,&
            this%nloops,&
            this%argi
         close(nunit)
      end if

   end subroutine write_benchmark
   !===============================================================================


   !===============================================================================
   pure elemental subroutine finalize_mark(this)
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

      inquire(file=this%filename_image, exist=exist, iostat=iostat)
      if (iostat /= 0 .or. .not. exist) then
         error stop 'file '//trim(this%filename_image)//' does not exist or cannot be accessed.'
      end if
      open (newunit = nunit, file = this%filename_image, access = 'append')
      write(nunit,'(a)') 'end of benchmark'
      close(nunit)

      if (this_image() == 1) then
         inquire(file=this%filename, exist=exist, iostat=iostat)
         if (iostat /= 0 .or. .not. exist) then
            error stop 'file '//trim(this%filename)//' does not exist or cannot be accessed.'
         end if
         open (newunit = nunit, file = this%filename, access = 'append')
         write(nunit,'(a)') 'end of benchmark'
         close(nunit)
      end if

      if (allocated(this%marks_co)) deallocate(this%marks_co)
      call this%marks%finalize_mark()
      if (allocated(this%marks)) deallocate(this%marks)
      if (allocated(this%filename_image)) deallocate(this%filename_image)
      if (allocated(this%argi)) deallocate(this%argi)
      if (allocated(this%argr)) deallocate(this%argr)

      if (this_image() == 1) print'(a)', 'end of benchmark'

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

#endif

end module forbenchmark_coarray

