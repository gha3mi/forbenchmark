program benchmark_matmul_mm

    use kinds, only: rk, ik
    use formatmul, only: fmatmul => matmul ! use fmatmul instead of matmul to avoid overloading for reference implementation
    use forbenchmark, only: benchmark

    implicit none

    type(benchmark)          :: bench
    real(rk),    allocatable :: A(:,:), B(:,:), C(:,:)
    integer(ik)              :: m, n, o, p
    integer                  :: nl, seed_size, i, imark
    integer,     allocatable :: seed_array(:)
    integer(ik), allocatable :: num_elements(:)

    call random_seed(size = seed_size)
    allocate(seed_array(seed_size))
    seed_array = 123456789

    call bench%init(9,'Benchmark matmul','benchmarks/matmul/results/matmul_mm', 100)

    num_elements = [500_ik, 1000_ik, 1500_ik, 2000_ik]

    do i = 1, size(num_elements)
        p = num_elements(i)

        ! C(m,o) = A(m,n).B(n,o)
        m = p
        n = p
        o = p

        call init_matrices(m,n,o,seed_array,A,B,C)

        !===============================================================================
        ! Reference
        call bench%start_benchmark(1,'matmul',"C = matmul(A,B)",[m*n*o])
        do nl = 1,bench%nloops
            C = matmul(A,B)
            call prevent_optimization(C,nl) ! loop-invariant
        end do
        call bench%stop_benchmark()
        !===============================================================================

        call init_matrices(m,n,o,seed_array,A,B,C)

        !===============================================================================
        call bench%start_benchmark(2,'blas',"C = matmul(A,B,option='m2')",[m*n*o])
        do nl = 1,bench%nloops
            C = fmatmul(A,B,option='m2')
            call prevent_optimization(C,nl) ! loop-invariant
        end do
        call bench%stop_benchmark()
        !===============================================================================

        call init_matrices(m,n,o,seed_array,A,B,C)

        !===============================================================================
        call bench%start_benchmark(3,'m1_b32',"C = matmul(A,B,option='m1',nblock=32)",[m*n*o])
        do nl = 1,bench%nloops
            C = fmatmul(A,B,option='m1',nblock=32)
            call prevent_optimization(C,nl) ! loop-invariant
        end do
        call bench%stop_benchmark()
        !===============================================================================

        call init_matrices(m,n,o,seed_array,A,B,C)

        !===============================================================================
        call bench%start_benchmark(4,'m3_b32',"C = matmul(A,B,option='m3',nblock=32)",[m*n*o])
        do nl = 1,bench%nloops
            C = fmatmul(A,B,option='m3',nblock=32)
            call prevent_optimization(C,nl) ! loop-invariant
        end do
        call bench%stop_benchmark()
        !===============================================================================

        call init_matrices(m,n,o,seed_array,A,B,C)

        !===============================================================================
        call bench%start_benchmark(5,'m4_b32',"C = matmul(A,B,option='m4',nblock=32)",[m*n*o])
        do nl = 1,bench%nloops
            C = fmatmul(A,B,option='m4',nblock=32)
            call prevent_optimization(C,nl) ! loop-invariant
        end do
        call bench%stop_benchmark()
        !===============================================================================

        call init_matrices(m,n,o,seed_array,A,B,C)

        !===============================================================================
        call bench%start_benchmark(6,'m5_b32',"C = matmul(A,B,option='m5',nblock=32)",[m*n*o])
        do nl = 1,bench%nloops
            C = fmatmul(A,B,option='m5',nblock=32)
            call prevent_optimization(C,nl) ! loop-invariant
        end do
        call bench%stop_benchmark()
        !===============================================================================

        call init_matrices(m,n,o,seed_array,A,B,C)

        !===============================================================================
        call bench%start_benchmark(7,'m6_b32',"C = matmul(A,B,option='m6',nblock=32)",[m*n*o])
        do nl = 1,bench%nloops
            C = fmatmul(A,B,option='m6',nblock=32)
            call prevent_optimization(C,nl) ! loop-invariant
        end do
        call bench%stop_benchmark()
        !===============================================================================

        call init_matrices(m,n,o,seed_array,A,B,C)

        !===============================================================================
        call bench%start_benchmark(8,'m7_b32',"C = matmul(A,B,option='m7',nblock=32)",[m*n*o])
        do nl = 1,bench%nloops
            C = fmatmul(A,B,option='m7',nblock=32)
            call prevent_optimization(C,nl) ! loop-invariant
        end do
        call bench%stop_benchmark()
        !===============================================================================

        call init_matrices(m,n,o,seed_array,A,B,C)

        !===============================================================================
        call bench%start_benchmark(9,'m8_b32',"C = matmul(A,B,option='m8',nblock=32)",[m*n*o])
        do nl = 1,bench%nloops
            C = fmatmul(A,B,option='m8',nblock=32)
            call prevent_optimization(C,nl) ! loop-invariant
        end do
        call bench%stop_benchmark()
        !===============================================================================

    end do

    call bench%finalize()

contains

    !===============================================================================
    subroutine init_matrices(m,n,o,seed_array,A,B,C)
        integer(ik), intent(in) :: m,n,o
        real(rk), allocatable, intent(inout) :: A(:,:), B(:,:), C(:,:)
        integer, intent(in) :: seed_array(:)

        if (allocated(A)) deallocate(A)
        if (allocated(B)) deallocate(B)
        if (allocated(C)) deallocate(C)
        allocate(A(m,n))
        allocate(B(n,o))
        allocate(C(m,o))
        call random_seed(put = seed_array)
        call random_number(A)
        call random_number(B)
    end subroutine init_matrices
    !===============================================================================


    ! !===============================================================================
    ! function cmp_gflops(argi,argr) result(gflops)
    !     integer(ik), dimension(:), intent(in), optional :: argi
    !     real(rk),    dimension(:), intent(in), optional :: argr
    !     real(rk)                                        :: gflops

    !     gflops = 2.0_rk*real(argi(1),kind=rk)*1.0e-9_rk
    ! end function cmp_gflops
    ! !===============================================================================


    !===============================================================================
    ! to prevent compiler from optimizing (loop-invariant)
    subroutine prevent_optimization(C, nl)
        real(rk), dimension(:,:), intent(in) :: C
        integer, intent(in)  :: nl
        if (abs(C(1,1))<tiny(0.0_rk)) print*, nl, C(1,1)
    end subroutine prevent_optimization
    !===============================================================================

end program benchmark_matmul_mm
