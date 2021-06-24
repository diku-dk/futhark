{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Futhark.CodeGen.Backends.GenericWASM
  (
    runServer,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
    JSEntryPoint (..),
    emccExportNames,
    javascriptWrapper,
    extToString
  )
where

import Data.FileEmbed
import Data.List (intercalate, nub)
import qualified Data.Text as T
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.SequentialC.Boilerplate
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import Futhark.IR.Primitive
import Futhark.IR.SeqMem
import Futhark.MonadFreshNames
import NeatInterpolation (text)

extToString :: Imp.ExternalValue -> String
extToString (Imp.TransparentValue (Imp.ArrayValue vn _ pt s dimSize)) = concat (replicate (length dimSize) "[]") ++ extToString (Imp.TransparentValue (Imp.ScalarValue pt s vn))
extToString (Imp.TransparentValue (Imp.ScalarValue (FloatType Float32) _ _)) = "f32"
extToString (Imp.TransparentValue (Imp.ScalarValue (FloatType Float64) _ _)) = "f64"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int8) Imp.TypeDirect _)) = "i8"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int16) Imp.TypeDirect _)) = "i16"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int32) Imp.TypeDirect _)) = "i32"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int64) Imp.TypeDirect _)) = "i64"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int8) Imp.TypeUnsigned _)) = "u8"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int16) Imp.TypeUnsigned _)) = "u16"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int32) Imp.TypeUnsigned _)) = "u32"
extToString (Imp.TransparentValue (Imp.ScalarValue (IntType Int64) Imp.TypeUnsigned _)) = "u64"
extToString (Imp.TransparentValue (Imp.ScalarValue Bool _ _)) = "bool"
extToString (Imp.OpaqueValue _ _) = "opaq"
extToString _ = "Not Reached"

type EntryPointTyp = String

data JSEntryPoint = JSEntryPoint
  { name :: String,
    parameters :: [EntryPointTyp],
    ret :: [EntryPointTyp]
  }

jsServer :: String
jsServer = $(embedStringFile "rts/javascript/server.js")

jsValues :: String
jsValues = $(embedStringFile "rts/javascript/values.js")

javascriptWrapper :: [JSEntryPoint] -> String
javascriptWrapper entryPoints =
  unlines
    [ jsServer,
      jsValues,
      cwraps,
      cwrapsJSE entryPoints,
      unlines $ map cwrapEntryPoint entryPoints,
      heapFuns,
      classFutharkArray,
      "class FutharkOpaque { constructor(ptr) { this.ptr = ptr; } }",
      classDef,
      constructor entryPoints,
      getEntryPointsFun,
      getErrorFun,
      unlines $ concatMap (\jse -> map toFutharkArray (nub (ret jse ++ parameters jse))) entryPoints,
      unlines $ map jsWrapEntryPoint entryPoints,
      endClassDef
    ]

classFutharkArray :: String
classFutharkArray =
  T.unpack
    [text|
  class FutharkArray {
    constructor(ctx, ptr, type_name, dim, array_type, fshape, fvalues, ffree) {
      this.ctx = ctx;
      this.ptr = ptr;
      this.type_name = type_name;
      this.dim = dim;
      this.array_type = array_type;
      this.fshape = fshape;
      this.fvalues = fvalues;
      this.ffree = ffree;
    }
    futharkType() { return this.type_name; }
    free() { this.ffree(this.ctx, this.ptr); }
    shape() {
      var s = this.fshape(this.ctx, this.ptr);
      return Array.from(viewHeap(s, BigUint64Array, this.dim));
    }
    toTypedArray(dims = this.shape()) {
      var length = Number(dims.reduce((a, b) => a * b));
      var v = this.fvalues(this.ctx, this.ptr);
      return viewHeap(v, this.array_type, length);
    }
    toArray() {
      var dims = this.shape();
      var ta = this.toTypedArray(dims);
      return (function nest(offs, ds) {
        var d0 = Number(ds[0]);
        if (ds.length === 1) {
          return Array.from(ta.subarray(offs, offs + d0));
        } else {
          var d1 = Number(ds[1]);
          return Array.from(Array(d0), (x,i) => nest(offs + i * d1, ds.slice(1)));
        }
      })(0, dims);
    }
  }
  |]

cwraps :: String
cwraps =
  unlines
    [ cwrapFun "futhark_context_config_new" 0,
      cwrapFun "futhark_context_new" 1,
      cwrapFun "futhark_context_sync" 1,
      cwrapFun "futhark_context_get_error" 1
    ]

-- TODO Only wrap functions for first params
cwrapsJSE :: [JSEntryPoint] -> String
cwrapsJSE jses =
  unlines $
    map (\arg -> cwrapFun (gfn "new" arg) (dim arg + 2)) jses'
      ++ map (\arg -> cwrapFun (gfn "free" arg) 2) jses'
      ++ map (\arg -> cwrapFun (gfn "shape" arg) 2) jses'
      ++ map (\arg -> cwrapFun (gfn "values_raw" arg) 2) jses'
  where
    jses' = filter (\t -> dim t > 0) $ nub $ concatMap (\jse -> parameters jse ++ ret jse) jses
    gfn typ str = "futhark_" ++ typ ++ "_" ++ baseType str ++ "_" ++ show (dim str) ++ "d"

emccExportNames :: [JSEntryPoint] -> [String]
emccExportNames jses =
  map (\jse -> "'_futhark_entry_" ++ name jse ++ "'") jses
    ++ map (\arg -> "'" ++ gfn "new" arg ++ "'") jses'
    ++ map (\arg -> "'" ++ gfn "free" arg ++ "'") jses'
    ++ map (\arg -> "'" ++ gfn "shape" arg ++ "'") jses'
    ++ map (\arg -> "'" ++ gfn "values_raw" arg ++ "'") jses'
    ++ map (\arg -> "'" ++ gfn "values" arg ++ "'") jses'
    ++ ["_futhark_context_config_new", "_futhark_context_new", "_futhark_context_sync", "_futhark_context_get_error"]
  where
    jses' = filter (\t -> dim t > 0) $ nub $ concatMap (\jse -> parameters jse ++ ret jse) jses
    gfn typ str = "_futhark_" ++ typ ++ "_" ++ baseType str ++ "_" ++ show (dim str) ++ "d"

cwrapFun :: String -> Int -> String
cwrapFun fname numArgs =
  T.unpack
    [text|
  ${fn} =
  Module.cwrap(
      '${fn}', 'number', [${args}],
    );
  |]
  where
    fn = T.pack fname
    args = T.pack $ intercalate ", " $ replicate numArgs "'number'"

heapFuns :: String
heapFuns =
  T.unpack
    [text|
  function viewHeap(ptr, array_type, length) {
    return new array_type(Module.HEAPU8.buffer, ptr, length);
  }
  function copyToHeap(ta) {
    var bytes = new Uint8Array(ta.buffer, ta.byteOffset, ta.byteLength);
    var ptr = Module._malloc(bytes.length);
    Module.HEAPU8.set(bytes, ptr);
    return ptr;
  }
  |]

classDef :: String
classDef = "class FutharkContext {"

endClassDef :: String
endClassDef = "}"

constructor :: [JSEntryPoint] -> String
constructor jses =
  T.unpack
    [text|
  constructor() {
    this.cfg = futhark_context_config_new();
    this.ctx = futhark_context_new(this.cfg);
    this.entry_points = {
      ${entries}
    };
  }
  |]
  where
    entries = T.pack $ intercalate "," $ map dicEntry jses

getEntryPointsFun :: String
getEntryPointsFun =
  T.unpack
    [text|
  get_entry_points() {
    return this.entry_points;
  }
  |]

getErrorFun :: String
getErrorFun =
  T.unpack
    [text|
  get_error() {
    var error_ptr = futhark_context_get_error(this.ctx);
    var error_msg = '';
    var next_char;
    for (var i = 0; 0 != (next_char = getValue(i + error_ptr, 'i8')); i++) {
      error_msg += String.fromCharCode(next_char);
    }
    return error_msg;
  }
  |]

dicEntry :: JSEntryPoint -> String
dicEntry jse =
  T.unpack
    [text|
  '${ename}' : [${params}, ${rets}]
  |]
  where
    ename = T.pack $ name jse
    params = T.pack $ show $ parameters jse
    rets = T.pack $ show $ ret jse

jsWrapEntryPoint :: JSEntryPoint -> String
jsWrapEntryPoint jse =
  unlines
    [ func_name ++ "(" ++ inparams ++ ") {",
      "  var out = [" ++ outparams ++ "].map(n => Module._malloc(n));",
      "  var to_free = [];",
      "  var do_free = () => { out.forEach(Module._free); to_free.forEach(f => f.free()); };",
      paramsToPtr,
      "  if (futhark_entry_" ++ func_name ++ "(this.ctx, ...out, " ++ ins ++ ") > 0) {",
      "    do_free();",
      "    throw this.get_error();",
      "  }",
      results,
      "  do_free();",
      "  return " ++ res ++ ";",
      "}"
    ]
  where
    func_name = name jse

    alp = [0 .. length (parameters jse) - 1]
    inparams = intercalate ", " ["in" ++ show i | i <- alp]
    ins = intercalate ", " [maybeDerefence ("in" ++ show i) $ parameters jse !! i | i <- alp]
    paramsToPtr = unlines $ filter ("" /=) [arrayPointer ("in" ++ show i) $ parameters jse !! i | i <- alp]

    alr = [0 .. length (ret jse) - 1]
    outparams = intercalate ", " [show $ typeSize $ ret jse !! i | i  <- alr]
    results = unlines [makeResult i $ ret jse !! i | i <- alr]
    res_array = intercalate ", " ["result" ++ show i | i <- alr]
    res = if length (ret jse) == 1 then "result0" else ("[" ++ res_array ++ "]")

maybeDerefence :: String -> String -> String
maybeDerefence arg typ =
  if head typ == '[' || typ == "opaq" then arg ++ ".ptr" else arg

arrayPointer :: String -> String -> String
arrayPointer arg typ =
  if head typ == '['
  then "  if (" ++ arg ++ " instanceof Array) { " ++ reassign ++ "; to_free.push(" ++ arg ++ "); }"
  else ""
  where
    reassign = arg ++ " = this.new_" ++ signature ++ "_from_jsarray(" ++ arg ++ ")"
    signature = baseType typ  ++ "_" ++ show (dim typ) ++ "d"

makeResult :: Int -> String -> String
makeResult i typ =
  "  var result" ++ show i ++ " = " ++
    if head typ == '['
    then "this.new_" ++ signature ++ "_from_ptr(getValue(" ++ res ++ ", 'i32'));"
    else
      if typ == "opaq"
      then "new FutharkOpaque(getValue(" ++ res ++ ", 'i32'));"
      else "viewHeap(" ++ res ++ ", " ++ typeConversion typ ++ ", 1)[0]"
             ++ if typ == "bool" then "!==0;" else ";"
  where
    res = "out[" ++ show i ++ "]"
    signature = baseType typ  ++ "_" ++ show (dim typ) ++ "d"

cwrapEntryPoint :: JSEntryPoint -> String
cwrapEntryPoint jse =
  T.unpack
    [text|
    futhark_entry_${ename} =
      Module.cwrap(
        'futhark_entry_${ename}', 'number', ${args}
      );
   |]
  where
    ename = T.pack $ name jse
    arg_length = length (parameters jse) + length (ret jse)
    args = T.pack $ "['number'" ++ concat (replicate arg_length ", 'number'") ++ "]"

baseType :: String -> String
baseType ('[' : ']' : end) = baseType end
baseType typ = typ

dim :: String -> Int
dim ('[' : ']' : end) = dim end + 1
dim _ = 0

typeConversion :: String -> String
typeConversion typ =
  case typ of
    "i8" -> "Int8Array"
    "i16" -> "Int16Array"
    "i32" -> "Int32Array"
    "i64" -> "BigInt64Array"
    "u8" -> "Uint8Array"
    "u16" -> "Uint16Array"
    "u32" -> "Uint32Array"
    "u64" -> "BigUint64Array"
    "f32" -> "Float32Array"
    "f64" -> "Float64Array"
    "bool" -> "Int8Array"
    "opaq" -> "Int32Array"
    _ -> typ

typeSize :: String -> Integer
typeSize typ =
  case typ of
    "i8" -> 1
    "i16" -> 2
    "i32" -> 4
    "i64" -> 8
    "u8" -> 1
    "u16" -> 2
    "u32" -> 4
    "u64" -> 8
    "f32" -> 4
    "f64" -> 8
    "bool" -> 1
    _ -> 4

toFutharkArray :: String -> String
toFutharkArray str =
  if dim str == 0
  then ""
  else unlines [
    new ++ "(typedArray, " ++ dims ++ ") {",
    -- "  // Possibly do sanity check that dimensions dim0 * dimn == arr.length",
    "  var copy = copyToHeap(typedArray);",
    "  var ptr = " ++ fnew ++ "(this.ctx, copy, " ++ bigint_dims ++ ");",
    "  Module._free(copy);",
    "  return this." ++ new ++ "_from_ptr(ptr);",
    "}",
    new ++ "_from_jsarray(a) {",
    "  return this." ++ new ++ "(" ++ atype ++ ".from(a.flat()), " ++ a_dims ++ ");",
    "}",
    new ++ "_from_ptr(ptr) {",
    "  return new FutharkArray(this.ctx, ptr, "
      ++ intercalate ", " ["'" ++ ftype ++ "'", show d, atype, fshape, fvalues, ffree] ++ ");",
    "}"
  ]
  where
    d = dim str
    ftype = baseType str
    atype = typeConversion ftype
    signature = ftype ++ "_" ++ show d ++ "d"
    new = "new_" ++ signature
    fnew = "futhark_new_" ++ signature
    fshape = "futhark_shape_" ++ signature
    fvalues = "futhark_values_raw_" ++ signature
    ffree = "futhark_free_" ++ signature
    dims = intercalate ", " [ "d" ++ show i | i <- [0..d-1] ]
    bigint_dims = intercalate ", " [ "BigInt(d" ++ show i ++ ")" | i <- [0..d-1] ]
    a_dims = intercalate ", " [ "a" ++ mult i "[0]" ++ ".length" | i <- [0..d-1] ]
    mult i s = concat $ replicate i s

runServer :: String
runServer =
  T.unpack
    [text|
   Module.onRuntimeInitialized = () => {
     var context = new FutharkContext();
     var server = new Server(context);
     server.run();
   }
  |]
