program test_dot

   use kinds
   use fordot
   use fast_math, only: fprod, fprod_kahan
   use forunittest

   implicit none

   real(rk), allocatable :: u(:), v(:)
   real(rk)              :: a, a_ref
   integer               :: m
   type(unit_test)       :: ut

   allocate(u(m),v(m))
   call random_number(u)
   call random_number(v)

   a_ref = dot_product(u,v)

   a = dot_product(u,v, option='m1')
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m1")

   a = dot_product(u,v, option='m2')
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m2")

   a = dot_product(u,v, option='m3')
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m3")

   a = dot_product(u,v, option='m4')
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m4")

   a = fprod(u,v)
   call ut%check(a, a_ref, tol=1e-12_rk, msg="fprod")

   a = fprod_kahan(u,v)
   call ut%check(a, a_ref, tol=1e-12_rk, msg="fprod_kahan")

end program test_dot