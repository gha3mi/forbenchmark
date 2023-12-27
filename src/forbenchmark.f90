module forbenchmark

#if defined(USE_COARRAY)
   use forbenchmark_coarray
#else
   use forbenchmark_default
#endif

   public benchmark

end module forbenchmark