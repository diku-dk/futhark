{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.COpenGL.Boilerplate
  ( generateBoilerplate
  ) where

import Data.FileEmbed
import qualified Data.Map as M
import qualified Language.C.Syntax as C
import qualified Language.C.Quote as C

import Futhark.CodeGen.ImpCode.OpenGL
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.Util (chunk, zEncodeString)

generateBoilerplate :: String -> String -> [String]
                    -> M.Map Name SizeClass
                    -> GC.CompilerM OpenGL () ()
generateBoilerplate opengl_code opengl_prelude shader_names sizes = do
  -- Insert required boilerplate here; see
  -- Futhark.CodeGen.Backends.COpenCL.Boilerplate.  Feel free tot
  -- ignore most of the parameters.
  undefined
