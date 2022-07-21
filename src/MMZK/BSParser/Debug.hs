{-# LANGUAGE CPP  #-}

module MMZK.BSParser.Debug where

__UNREACHABLE__ :: a -> a
#ifdef ASSERTS
__UNREACHABLE__ = error "Assertion Error: Unreachable!"
#else
__UNREACHABLE__ = id
#endif
{-# INLINE __UNREACHABLE__ #-}
