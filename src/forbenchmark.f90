module forbenchmark
   !! author: Seyed Ali Ghasemi
   !! license: BSD 3-Clause License
   !! This module is used to switch between the default and coarray versions of the benchmark
   !!

#if defined(USE_COARRAY)
   use forbenchmark_coarray, only: benchmark
#else
   use forbenchmark_default, only: benchmark
#endif

   implicit none

   private
   public benchmark

end module forbenchmark