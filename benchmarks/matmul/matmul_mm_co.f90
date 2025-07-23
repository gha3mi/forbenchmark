program benchmark_matmul_mm_coarray

   use kinds, only: rk, ik
   use formatmul, only: matmul
   use forbenchmark, only: benchmark

   implicit none

   type(benchmark)       :: bench
   real(rk), allocatable :: A(:,:), B(:,:), C(:,:)
   integer(ik)           :: m, n, o, p
   integer               :: nl

   call bench%init(11,'Benchmark matmul','benchmarks/matmul/results/matmul_mm', 10)

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
      call bench%start_benchmark(1,'m1',"C = matmul(A,B,coarray=.true.,option='m1')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m1')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(2,'m2',"C = matmul(A,B,coarray=.true.,option='m2')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m2')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(3,'m3',"C = matmul(A,B,coarray=.true.,option='m3')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m3')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(4,'m4',"C = matmul(A,B,coarray=.true.,option='m4')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m4')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(5,'m5',"C = matmul(A,B,coarray=.true.,option='m5')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m5')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(6,'m6',"C = matmul(A,B,coarray=.true.,option='m6')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m6')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(7,'m7',"C = matmul(A,B,coarray=.true.,option='m7')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m7')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(8,'m8',"C = matmul(A,B,coarray=.true.,option='m8')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m8')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(9,'m9',"C = matmul(A,B,coarray=.true.,option='m9')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m9')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(9,'m9',"C = matmul(A,B,coarray=.true.,option='m9')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m9')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(10,'m12',"C = matmul(A,B,coarray=.true.,option='m12')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m12')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(11,'m13',"C = matmul(A,B,coarray=.true.,option='m13')",[m*n*o])
      do nl = 1,bench%nloops
         C = matmul(A,B,coarray=.true.,option='m13')
      end do
      call bench%stop_benchmark()
      !===============================================================================

   end do

   call bench%finalize()

! contains

!    !===============================================================================
!    function cmp_gflops(argi,argr) result(gflops)
!       integer(ik), dimension(:), intent(in), optional :: argi
!       real(rk),    dimension(:), intent(in), optional :: argr
!       real(rk)                                        :: gflops

!       gflops = 2.0_rk*real(argi(1),kind=rk)*1.0e-9_rk
!    end function cmp_gflops
!    !===============================================================================

end program benchmark_matmul_mm_coarray
