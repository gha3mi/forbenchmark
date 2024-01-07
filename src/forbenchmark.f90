module forbenchmark
   !! summary: This module is used to switch between the default and coarray versions of the benchmark
   !! author: Seyed Ali Ghasemi
   !! version: {!VERSION!}
   !! license: {!LICENSE!}

#if defined(USE_COARRAY)
   use forbenchmark_coarray
#else
   use forbenchmark_default
#endif

   public benchmark

end module forbenchmark