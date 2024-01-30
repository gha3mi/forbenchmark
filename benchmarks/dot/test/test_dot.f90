program test_dot

   use kinds
   use fordot, only: fdot_product => dot_product
   use fast_math, only: fprod, fprod_kahan
   use forunittest

   implicit none

   real(rk), allocatable :: u(:), v(:)
   real(rk)              :: a, a_ref
   integer               :: m
   type(unit_test)       :: ut

   m = 300

   allocate(u(m),v(m))
   call random_number(u)
   call random_number(v)

   a_ref = dot_product(u,v)

   a = fdot_product(u,v, option='m1')
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m1")

   a = fdot_product(u,v, option='m2')
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m2")

   a = fdot_product(u,v, option='m3')
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m3")

   a = fdot_product(u,v, option='m4')
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m4")

   a = fprod(u,v)
   call ut%check(a, a_ref, tol=1e-12_rk, msg="fprod")

   a = fprod_kahan(u,v)
   call ut%check(a, a_ref, tol=1e-12_rk, msg="fprod_kahan")

   a = fdot_product(u,v, option='m1',nblock=16)
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m1b")

   a = fdot_product(u,v, option='m2',nblock=16)
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m2b")

   a = fdot_product(u,v, option='m3',nblock=16)
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m3b")

   a = fdot_product(u,v, option='m4',nblock=16)
   call ut%check(a, a_ref, tol=1e-12_rk, msg="dot_product m4b")

end program test_dot