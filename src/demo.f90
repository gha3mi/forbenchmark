! this is a demo program for forbenchmark.
! can be used for serial or coarray benchmarks.
! use -DUSE_COARRAY to compile for coarray benchmarks.
program demo

   use kinds,        only: rk         ! use -DREAL32 or -DREAL64 to switch between real32 and real64, default is real64
   use forbenchmark, only: benchmark  ! forbenchmark module

   implicit none

   ! benchmark object
   type(benchmark)                       :: bench
   ! define your variables here
   real(rk), dimension(:,:), allocatable :: A, B, C
   integer                               :: nl, p, i, j, k

   ! initialize the benchmark
   ! title is optional
   ! filename is optional. make sure directory exists
   ! nloops is the number of loops to run for each benchmark
   call bench%init(title='MatMul Benchmark', filename='results/demo_matmul', nloops=10)

   ! start the benchmark
   do p = 100,400, 100 ! loop over problem size

      !===============================================================================
      ! allocate and initialize your variables here
      if (allocated(A)) deallocate(A)
      if (allocated(B)) deallocate(B)
      if (allocated(C)) deallocate(C)
      allocate(A(p,p))
      allocate(B(p,p))
      allocate(C(p,p), source=0.0_rk)
      call random_number(A)
      call random_number(B)
      !===============================================================================


      !===============================================================================
      ! start benchmark for method 1
      ! method is a string to identify the method
      ! description is optional
      ! argi is an integer array of arguments, to write in the output file
      ! argr is a real array of arguments, optional
      call bench%start_benchmark(method='matmul_m1', description='intrinsic, C = matmul(A,B)', argi=[p,p,p])
      ! loop over nloops
      do nl = 1, bench%nloops

         ! call your function or subroutine or ...
         ! here is used intrinsic matmul
         C = matmul(A,B)

      end do
      ! stop benchmark for method 1
      ! cmp_gflops is an optional function to compute gflops
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      ! start benchmark for method 2, same as above.
      call bench%start_benchmark('matmul_m2', 'my_matmul, C = matmul(A,B)', [p,p,p])
      do nl = 1, bench%nloops

         ! call your function or subroutine or ...
         ! here is used another matmul
         C = 0.0_rk
         do i = 1,p
            do j = 1,p
               do k = 1,p
                  C(i,j) = C(i,j) + A(i,k)*B(k,j)
               end do
            end do
         end do

      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================

      ! you can add more methods ...

   end do ! end loop over p

   ! finalize the benchmark
   call bench%finalize()

contains

   !===============================================================================
   ! define an optional function to compute gflops
   function cmp_gflops(argi,argr) result(gflops)
      integer,  dimension(:), intent(in)           :: argi
      real(rk), dimension(:), intent(in), optional :: argr
      real(rk)                                     :: gflops

      gflops = real(argi(1),rk) * real(argi(2),rk) * real(argi(3),rk) * 1.0e-9_rk
   end function cmp_gflops
   !===============================================================================

end program demo
