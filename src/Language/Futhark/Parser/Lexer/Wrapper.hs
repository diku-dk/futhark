{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Utility definitions used by the lexer.  None of the default Alex
-- "wrappers" are precisely what we need.  The code here is highly
-- minimalistic.  Lexers should not be complicated!
module Language.Futhark.Parser.Lexer.Wrapper
  ( initialLexerState,
    AlexInput,
    alexInputPrevChar,
    LexerError (..),
    alexGetByte,
    alexGetPos,
  )
where

import Data.ByteString.Internal qualified as BS (w2c)
import Data.ByteString.Lazy qualified as BS
import Data.Int (Int64)
import Data.Loc (Loc, Pos (..))
import Data.Text qualified as T
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

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, prev, _, _) = prev

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

alexGetPos :: AlexInput -> Pos
alexGetPos (pos, _, _, _) = pos

tabSize :: Int
tabSize = 8

{-# INLINE alexMove #-}
alexMove :: Pos -> Char -> Pos
alexMove (Pos !f !l !c !a) '\t' = Pos f l (c + tabSize - ((c - 1) `mod` tabSize)) (a + 1)
alexMove (Pos !f !l _ !a) '\n' = Pos f (l + 1) 1 (a + 1)
alexMove (Pos !f !l !c !a) _ = Pos f l (c + 1) (a + 1)

initialLexerState :: Pos -> BS.ByteString -> AlexInput
initialLexerState start_pos input =
  (start_pos, '\n', input, 0)

data LexerError = LexerError Loc T.Text

instance Show LexerError where
  show (LexerError _ s) = T.unpack s
