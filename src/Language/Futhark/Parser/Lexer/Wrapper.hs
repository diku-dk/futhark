{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Utility definitions used by the lexer.  None of the default Alex
-- "wrappers" are precisely what we need.  The code here is based on
-- the "monad-bytestring" wrapper.
module Language.Futhark.Parser.Lexer.Wrapper
  ( runAlex',
    Alex (..),
    AlexInput,
    AlexPosn (..),
    AlexState (..),
    Byte,
    alexSetInput,
    alexGetInput,
    alexGetByte,
    alexGetStartCode,
    alexMove,
    alexError,
  )
where

import Control.Applicative (liftA)
import qualified Data.ByteString.Internal as BS (w2c)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
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
  ( AlexPosn, -- current position,
    Char, -- previous char
    BS.ByteString, -- current input string
    Int64 -- bytes consumed so far
  )

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (p, _, cs, n) =
  case BS.uncons cs of
    Nothing -> Nothing
    Just (b, cs') ->
      let c = BS.w2c b
          p' = alexMove p c
          n' = n + 1
       in p' `seq` cs' `seq` n' `seq` Just (b, (p', c, cs', n'))

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
  deriving (Eq, Show)

tabSize :: Int
tabSize = 8

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a + 1) l (c + tabSize - ((c -1) `mod` tabSize))
alexMove (AlexPn a l _) '\n' = AlexPn (a + 1) (l + 1) 1
alexMove (AlexPn a l c) _ = AlexPn (a + 1) l (c + 1)

data AlexState = AlexState
  { alex_pos :: !AlexPosn, -- position at current input location
    alex_bpos :: !Int64, -- bytes consumed so far
    alex_inp :: BS.ByteString, -- the current input
    alex_chr :: !Char, -- the character before the input
    alex_scd :: !Int -- the current startcode
  }

runAlex' :: AlexPosn -> BS.ByteString -> Alex a -> Either String a
runAlex' start_pos input__ (Alex f) =
  case f
    ( AlexState
        { alex_pos = start_pos,
          alex_bpos = 0,
          alex_inp = input__,
          alex_chr = '\n',
          alex_scd = 0
        }
    ) of
    Left msg -> Left msg
    Right (_, a) -> Right a

newtype Alex a = Alex {unAlex :: AlexState -> Either String (AlexState, a)}

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

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState {alex_scd = sc} -> Right (s, sc)
