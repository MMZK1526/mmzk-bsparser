{-# LANGUAGE CPP  #-}

module MMZK.BSParser.Debug (__ASSERT__, __UNREACHABLE__) where

#define __POS__ (++ (__FILE__ ++ ": line " ++ show (__LINE__ :: Integer)))

__UNREACHABLE__ :: a -> a
#ifdef ASSERTS
__UNREACHABLE__ = error . __POS__ $ "Assertion Error: Unreachable!\n"
#else
__UNREACHABLE__ = id
#endif
{-# INLINE __UNREACHABLE__ #-}

__ASSERT__ :: Bool -> a -> a
#ifdef ASSERTS
__ASSERT__ True  = id
__ASSERT__ False = error . __POS__ $ "Assertion Error: Assertion failed!\n"
#else
__ASSERT__ = const id
#endif
{-# INLINE __ASSERT__ #-}
