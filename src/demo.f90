! this is a demo program for forbenchmark.
! can be used for serial or coarray benchmarks.
! use -DUSE_COARRAY to compile for coarray benchmarks.
program demo

   use kinds,        only: rk, ik     ! use -DREAL32 or -DREAL64 to switch between real32 and real64, default is real64
                                      ! use -DINT32 or -DINT64 to switch between int32 and int64, default is int32
   use forbenchmark, only: benchmark  ! forbenchmark module

   implicit none

   ! benchmark object
   type(benchmark)                       :: bench
   ! define your variables here
   real(rk), dimension(:,:), allocatable :: A, B, C
   integer(ik)                           :: p
   integer(ik)                           :: nl, i, j, k

   ! initialize the benchmark
   ! nmarks: number of methods to benchmark
   ! title: optional
   ! filename: optional. make sure directory exists
   ! nloops: optional. number of loops for each benchmark. default is 10.
   ! timer: optional. default is 'wall'. other options are 'cpu', 'omp'. 'mpi', 'date_and_time'
   call bench%init(nmarks=2, title='Demo Benchmark', filename='results/demo', nloops=2, timer='wall')

   ! start the benchmark
   do p = 600_ik,900_ik, 100_ik ! loop over problem size

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
      ! imark is an integer to identify the method, 1 is for reference method
      ! method is a string to identify the method
      ! description is optional
      ! argi is an optional integer array of arguments to write in the output file and to compute gflops
      ! the first element in argi is used for x-axis in the plot
      ! argr is an optional real array of arguments to compute gflops
      ! loop over nloops
      call bench%start_benchmark(imark=1, method='m1', description='intrinsic, C = matmul(A,B)', argi=[p,p,p])
      ! loop over nloops
      do nl = 1, bench%nloops

         ! call your function or subroutine or ...
         ! here is used intrinsic matmul
         C = matmul(A,B)

      end do
      ! stop benchmark for method 1
      ! flops is an optional function to compute flops
      call bench%stop_benchmark(flops=cmp_gflops)
      !===============================================================================


      !===============================================================================
      ! start benchmark for method 2, same as above.
      call bench%start_benchmark(2,'m2', 'my_matmul, C = matmul(A,B)', [p,p,p])
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

   ! you can use a python script to plot and export the results as follows:
   ! python results/export.py demo_<compiler>.data
   ! for coarray benchmarks, utilize:
   ! python results/export_co.py demo_co.data
   ! python results/export_im.py demo_im1.data
   ! python results/export_im.py demo_im2.data
   ! ...
   
contains

   !===============================================================================
   ! define an optional function to compute gflops
   function cmp_gflops(argi,argr) result(gflops)
      integer(ik), dimension(:), intent(in), optional :: argi
      real(rk),    dimension(:), intent(in), optional :: argr
      real(rk)                                        :: gflops

      gflops = real(argi(1),rk) * real(argi(2),rk) * real(argi(3),rk) * 1.0e-9_rk
   end function cmp_gflops
   !===============================================================================

end program demo
