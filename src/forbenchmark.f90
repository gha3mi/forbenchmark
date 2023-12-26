module forbenchmark

#if defined(USE_COARRAY)
   use forbenchmark_coarray
#else
   use forbenchmark_serial
#endif

   public benchmark

end module forbenchmark