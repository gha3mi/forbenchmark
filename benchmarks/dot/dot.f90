program benchmark_dot

   use kinds
   use fordot
   use fast_math, only: fprod, fprod_kahan
   use forbenchmark

   implicit none

   type(benchmark)          :: bench
   real(rk), allocatable    :: u(:), v(:)
   real(rk), volatile       :: a
   integer(ik)              :: p
   integer                  :: nl, seed_size, i
   integer, allocatable     :: seed_array(:)
   integer(ik), allocatable :: num_elements(:)

   call random_seed(size = seed_size)
   allocate(seed_array(seed_size))
   seed_array = 123456789

   call bench%init(7,'Benchmark dot_product','benchmarks/dot/results/dot', 5000)

   num_elements = [100_ik, 1000_ik, 10000_ik, 100000_ik, 1000000_ik]

   do i = 1, size(num_elements)
      p = num_elements(i)

      !===============================================================================
      if (allocated(u)) deallocate(u)
      if (allocated(v)) deallocate(v)
      allocate(u(p))
      allocate(v(p))
      call random_seed(put = seed_array)
      call random_number(u)
      call random_number(v)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(1,'dot_product','a = dot_product(u,v)',[p])
      do nl = 1,bench%nloops
         a = dot_product(u,v)
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(2,'m1', "a = dot_product(u,v,'m1')",[p])
      do nl = 1,bench%nloops
         a = dot_product(u,v,'m1')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(3,'m2', "a = dot_product(u,v,'m2')",[p])
      do nl = 1,bench%nloops
         a = dot_product(u,v,'m2')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(4,'m3', "a = dot_product(u,v,'m3')",[p])
      do nl = 1,bench%nloops
         a = dot_product(u,v,'m3')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(5,'m4', "a = dot_product(u,v,'m4')",[p])
      do nl = 1,bench%nloops
         a = dot_product(u,v,'m4')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(6,'chunks', "a = fprod(u,v)",[p])
      do nl = 1,bench%nloops
         a = fprod(u,v)
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(7,'kahan', "a = fprod_kahan(u,v)",[p])
      do nl = 1,bench%nloops
         a = fprod_kahan(u,v)
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================

   end do

   call bench%finalize()

contains

   !===============================================================================
   function cmp_gflops(argi,argr) result(gflops)
      integer(ik), dimension(:), intent(in), optional :: argi
      real(rk),    dimension(:), intent(in), optional :: argr
      real(rk)                                        :: gflops

      gflops = real(argi(1),kind=rk)*1.0e-9_rk
   end function cmp_gflops
   !===============================================================================

end program benchmark_dot
