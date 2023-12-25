module forunittest

   use kinds

   implicit none

   private

   public unit_test

   !===============================================================================
   interface unit_test
      module procedure unit_test_r0
      module procedure unit_test_r1
      module procedure unit_test_r2
      module procedure unit_test_compare_r1
      module procedure unit_test_compare_i1
   end interface unit_test
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine unit_test_r0(res, expected, tol, message)
      real(rk), intent(in) :: res, expected
      real(rk), intent(in) :: tol
      character(*), intent(in) :: message
      logical :: condition
      integer :: lm
      character(len=20) :: fmt
      real(rk) :: rel_err

      lm = 46-len_trim(message)
      write(fmt,'(a,g0,a)') '(a,a',lm,',a)'

      if (abs(expected)<tiny(0.0_rk)) then
         rel_err = abs(res-expected)
      else
         rel_err = abs(res-expected)/abs(expected)
      end if

      condition = rel_err < tol

      call print_message(condition , message)
   end subroutine unit_test_r0
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine unit_test_r1(res, expected, tol, message)
      real(rk), intent(in), dimension(:) :: res, expected
      real(rk), intent(in) :: tol
      character(*), intent(in) :: message
      logical :: condition
      integer :: lm
      character(len=20) :: fmt
      real(rk) :: rel_err

      lm = 46-len_trim(message)
      write(fmt,'(a,g0,a)') '(a,a',lm,',a)'

      if (norm2(expected)<tiny(0.0_rk)) then
         rel_err = norm2(res-expected)
      else
         rel_err = norm2(res-expected)/norm2(expected)
      end if

      condition = rel_err < tol

      call print_message(condition , message)
   end subroutine unit_test_r1
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine unit_test_r2(res, expected, tol, message)
      real(rk), intent(in), dimension(:,:) :: res, expected
      real(rk), intent(in) :: tol
      character(*), intent(in) :: message
      logical :: condition
      integer :: lm
      character(len=20) :: fmt
      real(rk) :: rel_err

      lm = 46-len_trim(message)
      write(fmt,'(a,g0,a)') '(a,a',lm,',a)'

      if (norm2(expected)<tiny(0.0_rk)) then
         rel_err = norm2(res-expected)
      else
         rel_err = norm2(res-expected)/norm2(expected)
      end if

      condition = rel_err < tol

      call print_message(condition , message)
   end subroutine unit_test_r2
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine unit_test_compare_r1(u, v, message)
      real(rk), intent(in), dimension(:) :: u, v
      character(*), intent(in) :: message
      logical :: condition
      integer :: lm
      character(len=20) :: fmt
      real(rk) :: rel_err

      lm = 46-len_trim(message)
      write(fmt,'(a,g0,a)') '(a,a',lm,',a)'

      condition = any(u==v)

      call print_message(condition , message)
   end subroutine unit_test_compare_r1
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine unit_test_compare_i1(u, v, message)
      integer, intent(in), dimension(:) :: u, v
      character(*), intent(in) :: message
      logical :: condition
      integer :: lm
      character(len=20) :: fmt
      real(rk) :: rel_err

      lm = 46-len_trim(message)
      write(fmt,'(a,g0,a)') '(a,a',lm,',a)'

      condition = any(u==v)

      call print_message(condition , message)
   end subroutine unit_test_compare_i1
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine print_message(condition, message)
      use face
      logical, intent(in) :: condition
      character(*), intent(in) :: message
      integer  :: lm
      character(len=20) :: fmt

      lm = 55-len_trim(message)
      write(fmt,'(a,g0,a)') '(a,a',lm,',a)'

      if (condition) then
         print(fmt), message, colorize('passed.', color_fg='green')
      else
         print(fmt), message, colorize('failed.', color_fg='red')
      end if
   end subroutine print_message
   !===============================================================================

end module forunittest
