{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Text.FastParse where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe
import GHC.Exts
import System.IO.Unsafe

newtype Res# e a
  = Res#
      (#
        (# a, Addr# #)|
        (# Parser e a, Addr# #)|
        (# #)|
        (# e #)
      #)

pattern Ok# :: a -> Addr# -> Res# e a
pattern Ok# a addr = Res# (# (# a, addr #) | | | #)

pattern Partial# :: Parser e a -> Addr# -> Res# e a
pattern Partial# f addr = Res# (# | (# f, addr #) | | #)

pattern Err# :: e -> Res# e a
pattern Err# e = Res# (# | | | (# e #) #)

pattern Fail# :: Res# e a
pattern Fail# = Res# (# | | (##) | #)

{-# COMPLETE Ok#, Partial#, Err#, Fail# #-}

newtype Parser e a = Parser
  {runParser# :: Addr# -> Addr# -> Res# e a}

instance Functor (Parser e) where
  fmap f (Parser g) = Parser $ \eob s ->
    case g eob s of
      Ok# a s' -> let !b = f a in Ok# b s'
      Partial# p s' -> Partial# (fmap f p) s'
      x -> unsafeCoerce# x
  {-# INLINE fmap #-}

instance Applicative (Parser e) where
  pure a = Parser $ \_ s -> Ok# a s
  {-# INLINE pure #-}
  Parser ff <*> Parser fa = Parser $ \eob s ->
    case ff eob s of
      Ok# f s' ->
        case fa eob s' of
          Ok# a s'' -> let !b = f a in Ok# b s''
          Partial# p s'' -> Partial# (fmap f p) s''
          x -> unsafeCoerce# x
      Partial# p s' -> Partial# (p <*> Parser fa) s'
      x -> unsafeCoerce# x
  {-# INLINE (<*>) #-}

instance Monad (Parser e) where
  return = pure
  {-# INLINE return #-}
  Parser fa >>= k = Parser $ \eob s ->
    case fa eob s of
      Ok# a s' -> runParser# (k a) eob s'
      Partial# p s' -> Partial# (p >>= k) s'
      x -> unsafeCoerce# x
  {-# INLINE (>>=) #-}

instance Alternative (Parser e) where
  empty = Parser $ \_ _ -> Fail#
  {-# INLINE empty #-}
  (<|>) :: Parser e a -> Parser e a -> Parser e a
  Parser f <|> Parser g = Parser $ \eob s ->
    case f eob s of
      Fail# -> g eob s
      x -> x
  {-# INLINE (<|>) #-}

instance MonadPlus (Parser e)

data Result e a
  = Ok a {-# UNPACK #-} !ByteString
  | Fail
  | Err e
  | Partial (ByteString -> Result e a)

runParser :: Parser e a -> ByteString -> Result e a
runParser (Parser f) b =
  unsafeDupablePerformIO $
    unsafeUseAsCStringLen b $ \(Ptr buf, I# len) ->
      let end = plusAddr# buf len
       in case f end buf of
            Ok# a s ->
              let offset = minusAddr# s buf
               in pure (Ok a (BS.copy (BS.drop (I# offset) b))) -- always copies since the result is probably much smaller
            Partial# p s ->
              pure
                ( Partial $ \b' ->
                    let offset = minusAddr# s buf
                        b0 = BS.drop (I# offset) b
                     in runParser p (b0 <> b')
                )
            Err# e -> pure (Err e)
            Fail# -> pure Fail
{-# NOINLINE runParser #-}
