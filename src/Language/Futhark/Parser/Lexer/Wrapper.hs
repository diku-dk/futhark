{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Utility definitions used by the lexer.  None of the default Alex
-- "wrappers" are precisely what we need.  The code here is based on
-- the "monad-bytestring" wrapper.  The code here is completely
-- Futhark-agnostic, and perhaps it can even serve as inspiration for
-- other Alex lexer wrappers.
module Language.Futhark.Parser.Lexer.Wrapper
  ( runAlex,
    Alex,
    AlexInput,
    Byte,
    LexerError (..),
    alexSetInput,
    alexGetInput,
    alexGetByte,
    alexGetStartCode,
    alexError,
    alexGetPos,
  )
where

import Control.Applicative (liftA)
import qualified Data.ByteString.Internal as BS (w2c)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import Data.Loc (Loc, Pos (..))
import Data.Word (Word8)

type Byte = Word8

-- | The input type.  Contains:
--
-- 1. current position
--
-- 2. previous char
--
-- 3. current input string
--
-- 4. bytes consumed so far
type AlexInput =
  ( Pos, -- current position,
    Char, -- previous char
    BS.ByteString, -- current input string
    Int64 -- bytes consumed so far
  )

{-# INLINE alexGetByte #-}
alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (p, _, cs, n) =
  case BS.uncons cs of
    Nothing -> Nothing
    Just (b, cs') ->
      let c = BS.w2c b
          p' = alexMove p c
          n' = n + 1
       in p' `seq` cs' `seq` n' `seq` Just (b, (p', c, cs', n'))

tabSize :: Int
tabSize = 8

{-# INLINE alexMove #-}
alexMove :: Pos -> Char -> Pos
alexMove (Pos !f !l !c !a) '\t' = Pos f l (c + tabSize - ((c - 1) `mod` tabSize)) (a + 1)
alexMove (Pos !f !l _ !a) '\n' = Pos f (l + 1) 1 (a + 1)
alexMove (Pos !f !l !c !a) _ = Pos f l (c + 1) (a + 1)

data AlexState = AlexState
  { alex_pos :: !Pos, -- position at current input location
    alex_bpos :: !Int64, -- bytes consumed so far
    alex_inp :: BS.ByteString, -- the current input
    alex_chr :: !Char, -- the character before the input
    alex_scd :: !Int -- the current startcode
  }

runAlex :: Pos -> BS.ByteString -> Alex a -> Either LexerError a
runAlex start_pos input (Alex f) =
  case f
    ( AlexState
        { alex_pos = start_pos,
          alex_bpos = 0,
          alex_inp = input,
          alex_chr = '\n',
          alex_scd = 0
        }
    ) of
    Left msg -> Left msg
    Right (_, a) -> Right a

newtype Alex a = Alex {unAlex :: AlexState -> Either LexerError (AlexState, a)}

data LexerError = LexerError Loc String

instance Show LexerError where
  show (LexerError _ s) = s

instance Functor Alex where
  fmap = liftA

instance Applicative Alex where
  pure a = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
    Left msg -> Left msg
    Right (s', f) -> case unAlex a s' of
      Left msg -> Left msg
      Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k = Alex $ \s -> case unAlex m s of
    Left msg -> Left msg
    Right (s', a) -> unAlex (k a) s'

alexGetInput :: Alex AlexInput
alexGetInput =
  Alex $ \s@AlexState {alex_pos = pos, alex_bpos = bpos, alex_chr = c, alex_inp = inp} ->
    Right (s, (pos, c, inp, bpos))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos, c, inp, bpos) =
  Alex $ \s -> case s
    { alex_pos = pos,
      alex_bpos = bpos,
      alex_chr = c,
      alex_inp = inp
    } of
    state@AlexState {} -> Right (state, ())

alexError :: Loc -> String -> Alex a
alexError loc message = Alex $ const $ Left $ LexerError loc message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState {alex_scd = sc} -> Right (s, sc)

alexGetPos :: Alex Pos
alexGetPos = Alex $ \s -> Right (s, alex_pos s)
