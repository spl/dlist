{-# LANGUAGE CPP #-}

#if !defined(__GLASGOW_HASKELL__)
#error "Your compiler is not GHC. Let us know if dlist can be made to work on it."
#endif

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#else
#error "Your GHC does not support Safe Haskell. That's okay!"
#endif

-----------------------------------------------------------------------------

{-|

This module is declared @Safe@ but imports a module declared @Unsafe@.
Therefore, any attempt to compile this module should fail.

We use @#error@ above just to make it fail for all versions of GHC.

-}

module Safe (main) where

-----------------------------------------------------------------------------

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
import Data.DList.Unsafe ()
#endif

main :: IO ()
main = putStrLn "This module should fail to compile."
