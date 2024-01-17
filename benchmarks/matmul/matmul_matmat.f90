program benchmark_matmul_matmat

   use kinds
   use formatmul
   use forbenchmark

   implicit none

   type(benchmark)       :: bench
   real(rk), allocatable :: A(:,:), B(:,:), C(:,:)
   integer(ik)           :: m, n, o, p
   integer               :: nl

   call bench%init(12,'Benchmark matmul','benchmarks/matmul/results/matmul_matmat', 10)

   do p = 250_ik,1500_ik,250_ik

      !===============================================================================
      ! C(m,o) = A(m,n).B(n,o)
      m = p
      n = p
      o = p

      if (allocated(A)) deallocate(A)
      if (allocated(B)) deallocate(B)
      if (allocated(C)) deallocate(C)
      allocate(A(m,n))
      allocate(B(n,o))
      allocate(C(m,o))
      call random_number(A)
      call random_number(B)
      !===============================================================================


      !===============================================================================
      ! Reference
      call bench%start_benchmark(1,'matmul',"C = matmul(A,B)",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B)
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(2,'m1',"C = matmul(A,B,option='m1')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,option='m1')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(3,'m2',"C = matmul(A,B,option='m2')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,option='m2')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(4,'m3',"C = matmul(A,B,option='m3')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,option='m3')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(5,'m4',"C = matmul(A,B,option='m4')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,option='m4')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(6,'m5',"C = matmul(A,B,option='m5')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,option='m5')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(7,'m6',"C = matmul(A,B,option='m6')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,option='m6')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(8,'m7',"C = matmul(A,B,option='m7')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,option='m7')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(9,'m8',"C = matmul(A,B,option='m8')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,option='m8')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(10,'m9',"C = matmul(A,B,option='m9')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,option='m9')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(11,'m12',"C = matmul(A,B,option='m12')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,option='m12')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(12,'m13',"C = matmul(A,B,option='m13')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,option='m13')
      end do
      call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! GFORTRAN DOESNT SUPPORT SHARED !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! call bench%start_benchmark(13,'m10',"C = matmul(A,B,option='m10')",[m*n*o])
      ! do nl = 1,bench%nloops
      !    C = matmul(A,B,option='m10')
      ! end do
      ! call bench%stop_benchmark(cmp_gflops)
      !===============================================================================


      !===============================================================================
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! GFORTRAN DOESNT SUPPORT SHARED !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! call bench%start_benchmark(14,'m11',"C = matmul(A,B,option='m11')",[m*n*o])
      ! do nl = 1,bench%nloops
      !    C = matmul(A,B,option='m11')
      ! end do
      ! call bench%stop_benchmark(cmp_gflops)
      !===============================================================================

   end do

   call bench%finalize()

contains

   !===============================================================================
   function cmp_gflops(argi,argr) result(gflops)
      integer(ik), dimension(:), intent(in), optional :: argi
      real(rk),    dimension(:), intent(in), optional :: argr
      real(rk)                                        :: gflops

      gflops = 2.0_rk*real(argi(1),kind=rk)*1.0e-9_rk
   end function cmp_gflops
   !===============================================================================

end program benchmark_matmul_matmat
