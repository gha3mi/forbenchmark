program benchmark_matmul_mv_coarray

   use kinds
   use formatmul
   use forbenchmark

   implicit none

   type(benchmark)       :: bench
   real(rk), allocatable :: A(:,:)
   real(rk), allocatable :: v(:), w(:)
   integer(ik)           :: m, n, p
   integer               :: nl

   call bench%init(6,'Benchmark matmul','benchmarks/matmul/results/matmul_mv', 10)

   do p = 750_ik,2000_ik,250_ik

      !===============================================================================
      ! w(m) = A(m,n).v(n)
      m = p
      n = p

      if (allocated(A)) deallocate(A)
      if (allocated(v)) deallocate(v)
      if (allocated(w)) deallocate(w)
      allocate(A(m,n))
      allocate(v(n))
      allocate(w(m))
      call random_number(A)
      call random_number(v)
      !===============================================================================


      !===============================================================================
      ! Reference
      call bench%start_benchmark(1,'m1',"w = matmul(A,v,coarray=.true.,option='m1')",[m*n])
      do nl = 1,bench%nloops
         w = matmul(A,v,coarray=.true.,option='m1')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(2,'m2',"w = matmul(A,v,coarray=.true.,option='m2')",[m*n])
      do nl = 1,bench%nloops
         w = matmul(A,v,coarray=.true.,option='m2')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(3,'m3',"w = matmul(A,v,coarray=.true.,option='m3')",[m*n])
      do nl = 1,bench%nloops
         w = matmul(A,v,coarray=.true.,option='m3')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(4,'m4',"w = matmul(A,v,coarray=.true.,option='m4')",[m*n])
      do nl = 1,bench%nloops
         w = matmul(A,v,coarray=.true.,option='m4')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(5,'m5',"w = matmul(A,v,coarray=.true.,option='m5')",[m*n])
      do nl = 1,bench%nloops
         w = matmul(A,v,coarray=.true.,option='m5')
      end do
      call bench%stop_benchmark()
      !===============================================================================


      !===============================================================================
      call bench%start_benchmark(6,'m6',"w = matmul(A,v,coarray=.true.,option='m6')",[m*n])
      do nl = 1,bench%nloops
         w = matmul(A,v,coarray=.true.,option='m6')
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

end program benchmark_matmul_mv_coarray
