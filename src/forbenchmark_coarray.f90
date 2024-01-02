module forbenchmark_coarray
   !! author: Seyed Ali Ghasemi

#if defined(USE_COARRAY)

   use kinds
   use fortime, only: timer

   implicit none

   private

   public benchmark

   !===============================================================================
   type :: mark_co
   !! author: Seyed Ali Ghasemi
      type(timer) :: time
      real(rk)    :: elapsed_time
      real(rk)    :: flops
   end type mark_co
   !===============================================================================


   !===============================================================================
   type :: mark
   !! author: Seyed Ali Ghasemi
      character(:), allocatable :: method
      character(:), allocatable :: description
      real(rk)                  :: elapsed_time_min
      real(rk)                  :: elapsed_time_average
      real(rk)                  :: elapsed_time_max
      real(rk)                  :: flops_total
      real(rk)                  :: speedup_max_total
   contains
      procedure, private :: finalize_mark
   end type mark
   !===============================================================================


   !===============================================================================
   type :: benchmark
   !! author: Seyed Ali Ghasemi
      type(mark_co), dimension(:), allocatable :: marks_co[:]
      type(mark),    dimension(:), allocatable :: marks
      character(:),                allocatable :: filename
      character(:),                allocatable :: filename_image
      integer                                  :: nloops
      integer,       dimension(:), allocatable :: argi
      real(rk),      dimension(:), allocatable :: argr
      character(:),                allocatable :: timer
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
      character(10)                          :: im_chr

      if (nmarks <= 0) error stop 'nmarks must be greater than zero.'

      write (im_chr, '(i0)') this_image()
      if (present(filename)) then
         this%filename_image = trim(filename//'_im'//trim(im_chr)//'.data')
         this%filename = trim(filename//'_co'//'.data')
      else
         this%filename_image = trim('benchmark'//'_im'//trim(im_chr)//'.data')
         this%filename = trim('benchmark'//'_co'//'.data')
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
      use face

      class(benchmark),       intent(inout)        :: this
      integer,                intent(in)           :: imark
      character(*),           intent(in)           :: method
      integer,  dimension(:), intent(in), optional :: argi
      real(rk), dimension(:), intent(in), optional :: argr
      character(*),           intent(in), optional :: description

      if (imark <= 0 .or. imark > size(this%marks)) error stop 'imark is out of range.'

      this%marks(imark)%description = description
      this%marks(imark)%method      = method

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
            colorize('Meth.: '//this%marks(imark)%method, color_fg='green',style='bold_on'),&
            colorize('; Des.: '//this%marks(imark)%description, color_fg='green_intense'),&
            '; Argi.:',&
            this%argi
      elseif (.not. present(description) .and. present(argi) .and. this_image() == 1) then
         print'(a," ",a,*(g0,1x))',&
            colorize('Meth.: '//this%marks(imark)%method, color_fg='green',style='bold_on'),&
            '; Argi.:',&
            this%argi
      elseif (present(description) .and. .not. present(argi) .and. this_image() == 1) then
         print'(a,a)',&
            colorize('Meth.: '//this%marks(imark)%method, color_fg='green',style='bold_on'),&
            colorize('; Des.: '//this%marks(imark)%description, color_fg='green_intense')
      elseif (.not. present(description) .and. .not. present(argi) .and. this_image() == 1) then
         print'(a)', colorize('Meth.: '//this%marks(imark)%method, color_fg='green',style='bold_on')
      end if

      select case (trim(this%timer))
       case ('wall')
         call this%marks_co(imark)%time%timer_start()
       case ('date_and_time')
         call this%marks_co(imark)%time%dtimer_start()
       case ('cpu')
         call this%marks_co(imark)%time%ctimer_start()
       case ('omp')
#if defined(USE_OMP)
         call this%marks_co(imark)%time%ptimer_start()
#else
         error stop 'Use -DUSE_OMP to enable OpenMP.'
#endif
       case ('mpi')
#if defined(USE_MPI)
         call this%marks_co(imark)%time%mtimer_start()
#else
         error stop 'Use -DUSE_MPI to enable MPI.'
#endif
      end select

   end subroutine start_benchmark
   !===============================================================================


   !===============================================================================
   impure subroutine stop_benchmark(this, imark, flops)
   !! author: Seyed Ali Ghasemi
      use face

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
      integer,          intent(in)    :: imark
      real(rk)                        :: elapsed_time_average, elapsed_time_min, elapsed_time_max
      real(rk)                        :: flops_total
      integer                         :: i
      real(rk), dimension(:), allocatable :: elapsed_times

      if (imark <= 0 .or. imark > size(this%marks)) error stop 'imark is out of range.'

      select case (trim(this%timer))
      case ('wall')
         call this%marks_co(imark)%time%timer_stop(message=' Elapsed time :',nloops=this%nloops)
         this%marks_co(imark)%elapsed_time = this%marks_co(imark)%time%elapsed_time
      case ('date_and_time')
        call this%marks_co(imark)%time%dtimer_stop(message=' Elapsed time :',nloops=this%nloops)
        this%marks_co(imark)%elapsed_time = this%marks_co(imark)%time%elapsed_dtime
      case ('cpu')
        call this%marks_co(imark)%time%ctimer_stop(message=' Elapsed time :',nloops=this%nloops)
        this%marks_co(imark)%elapsed_time = this%marks_co(imark)%time%cpu_time
      case ('omp')
#if defined(USE_OMP)
        call this%marks_co(imark)%time%otimer_stop(message=' Elapsed time :',nloops=this%nloops)
        this%marks_co(imark)%elapsed_time = this%marks_co(imark)%time%omp_time
#else
        error stop 'Use -DUSE_OMP to enable OpenMP.'
#endif
      case ('mpi')
#if defined(USE_MPI)
        call this%marks_co(imark)%time%mtimer_stop(message=' Elapsed time :',nloops=this%nloops)
        this%marks_co(imark)%elapsed_time = this%marks_co(imark)%time%mpi_time
#else
        error stop 'Use -DUSE_MPI to enable MPI.'
#endif
     end select

      if (present(flops)) then
         this%marks_co(imark)%flops = flops(this%argi,this%argr)/this%marks_co(imark)%elapsed_time
         print'(a,f7.3,a)', ' Performance  :', this%marks_co(imark)%flops,' [GFLOPS/image]'
      else
         this%marks_co(imark)%flops = 0.0_rk
      end if

      sync all

      if (this_image() == 1) then
         allocate(elapsed_times(num_images()))
         do i = 1, num_images()
            elapsed_times(i) = this%marks_co(imark)[i]%elapsed_time
         end do
         elapsed_time_max = maxval(elapsed_times)
         elapsed_time_min = minval(elapsed_times)
         elapsed_time_average = sum(elapsed_times)/num_images()

         if (present(flops)) flops_total = 0.0_rk
         do i = 1,num_images()
            if (present(flops)) flops_total = flops_total + this%marks_co(imark)[i]%flops
         end do
      end if
      call co_broadcast(elapsed_time_average, 1)
      call co_broadcast(elapsed_time_min, 1)
      call co_broadcast(elapsed_time_max, 1)
      if (present(flops)) call co_broadcast(flops_total, 1)
      this%marks(imark)%elapsed_time_average = elapsed_time_average
      this%marks(imark)%elapsed_time_min = elapsed_time_min
      this%marks(imark)%elapsed_time_max = elapsed_time_max

      this%marks(imark)%speedup_max_total = this%marks(imark)%elapsed_time_max/this%marks(1)%elapsed_time_max


      if (this_image()==1) then
         print'(a,f7.3,a)', colorize(' Elapsed time (max)     :', color_fg='blue'),&
            this%marks(imark)%elapsed_time_max, ' [s]'
         print'(a,f7.3,a)', colorize(' Elapsed time (min)     :', color_fg='blue'),&
            this%marks(imark)%elapsed_time_min, ' [s]'

         print'(a,f7.3,a)', colorize(' Elapsed time (average) :', color_fg='blue'),&
            this%marks(imark)%elapsed_time_average, ' [s]'

         print'(a,f7.3,a)', colorize(' Speedup (max)          :', color_fg='blue'),&
            this%marks(imark)%speedup_max_total, ' [-]'
         if (present(flops)) print'(a,f7.3,a)', colorize(' Performance  (total)   :', color_fg='cyan'),&
            flops_total, ' [GFLOPS]'
         print'(a)', ''
      end if


      if (present(flops)) then
         this%marks(imark)%flops_total = flops_total
      else
         this%marks(imark)%flops_total = 0.0_rk
      end if

      call this%write_benchmark(imark)
   end subroutine stop_benchmark
   !===============================================================================


   !===============================================================================
   impure subroutine write_benchmark(this, imark)
   !! author: Seyed Ali Ghasemi
      class(benchmark), intent(inout) :: this
      integer,          intent(in)    :: imark
      integer                         :: nunit
      character(len=53)              :: fmt1
      character(len=82)              :: fmt2
      integer                         :: lm
      logical                         :: exist
      integer                         :: iostat

      if (imark <= 0 .or. imark > size(this%marks)) error stop 'imark is out of range.'

      lm = 20-len_trim(this%marks(imark)%method)
      write(fmt1,'(a,g0,a)')&
         '(a,',lm,'x,3x,E20.14,3x,E20.14,3x,g8.0,3x,*(g8.0,3x))'

      inquire(file=this%filename_image, exist=exist, iostat=iostat)
      if (iostat /= 0 .or. .not. exist) then
         error stop 'file '//trim(this%filename_image)//' does not exist or cannot be accessed.'
      end if
      open (newunit = nunit, file = this%filename_image, access = 'append')
      write(nunit,fmt1) &
         this%marks(imark)%method,&
         this%marks_co(imark)%time%elapsed_time,&
         this%marks_co(imark)%flops,&
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
         this%marks(imark)%method,&
         this%marks(imark)%speedup_max_total,&
         this%marks(imark)%elapsed_time_max,&
         this%marks(imark)%elapsed_time_min,&
         this%marks(imark)%elapsed_time_average,&
         this%marks(imark)%flops_total,&
         this%nloops,&
         this%argi
      close(nunit)
   end if

   end subroutine write_benchmark
   !===============================================================================


   !===============================================================================
   pure elemental subroutine finalize_mark(this)
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

#endif

end module forbenchmark_coarray

