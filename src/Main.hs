-- | L0 Compiler Driver
module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity (Identity)
import System.Environment (getArgs)

import L0.AbSyn
import L0.Parser (parseL0)
import L0.TypeChecker
import L0.Renamer
import L0.Interpreter
-- import L0.CCodeGen

-- To parse and prettyprint an input program located at ../DATA/filename.l0, run
--  
--  $ l0c -p ../DATA/filename.l0
-- To interpret an input program located at ../DATA/filename.l0, run
--  
--  $ l0c -I ../DATA/filename.l0
-- 
--  To compile the same program with optimizations enabled, run
-- 
--  $ l0c -o ../DATA/filename.l0
-- 
--  To compile the same program without optimizations enabled, run
-- 
--  $ l0c -c ../DATA/filename.l0
-- 
--  or simply,
-- 
--  $ l0c ../DATA/filename.l0
-- 
-- Little of the above is implemented.  Your mileage will vary.
--

main :: IO ()
main = do args <- getArgs
          case args of
            ["-p", file] -> prettyprint file
            ["-t", file] -> typecheck (const $ return ()) file
            ["-tp", file] -> typecheck (putStrLn . prettyPrint) file
            ["-r", file] -> rename file
            ["-i", file] -> interpret file
--            ["-c", file] -> compile file
            _ -> error "Usage: <-p|-t|-tp|-r|-i|-c> <file>"

prettyprint :: FilePath -> IO ()
prettyprint file = putStrLn =<< prettyPrint <$> parse file

typecheck :: (Prog Identity -> IO ()) -> FilePath -> IO ()
typecheck next file = do
  prog <- parse file
  case checkProg prog of
    Left e -> error $ show e
    Right prog'  ->
      case checkProg prog' of
        Left e  -> error $ "Error during second type checking phase. This implies a bug in the type checker.\n" ++ show e
        Right _ -> next prog'

rename :: FilePath -> IO ()
rename file = do prog <- renameProg <$> parse file
                 putStrLn $ prettyPrint prog
                 case checkProg prog of
                   Left e -> error $ "Type error after renaming:\n" ++ show e
                   _      -> return ()

interpret :: FilePath -> IO ()
interpret file = do
  prog <- parse file
  case checkProg prog of
    Left err    -> error $ "Typechecking error:\n" ++ show err
    Right prog' -> do
      res <- runProgIO prog'
      case res of Left err -> error $ "Interpreter error:\n" ++ show err
                  Right v  -> putStrLn $ ppValue v -- ++ (prettyPrint prog')
{-
compile :: FilePath -> IO ()
compile file = do
  prog <- parse file
  case checkProg prog of
    Left err    -> error $ "Typechecking error:\n" ++ show err
    Right prog' -> putStr $ compileProg $ renameProg prog'
-}

parse :: FilePath -> IO (Prog Maybe)
parse = return . parseL0 <=< readFile

{-
  fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer ( fn buff => fn n => Nonstdio.buff_input is buff 0 n )

  fun errorMsg s = TextIO.output (TextIO.stdErr,s ^ "\n")

  fun errorMsgAt s (line, column) = errorMsg
    (s ^ "\nLine " ^ makestring line ^ ", column " ^ makestring column ^ ".")

(*
  fun interpret pgm =
    let val absyn_str = AbSynL0.prettyPrint pgm
      val () = print
        ("Program is:\n\n" ^ absyn_str ^ "\n\nInput/Output:\n")
      val resultStr = AbSynL0.pp_exp 0 (Interpret.evalPgm pgm)
    in
      print "\n\nRESULT: ";
      print resultStr;
      print "\n"
    end


  fun applyAll fs x = foldr (fn (f, y) => f y) x fs

  fun compileAux opts pgm outpath =
    let val pgmDecorated = Type.checkProgram pgm
        val opt_pgm = applyAll opts pgmDecorated
        val code = Compiler.compile opt_pgm
        val outfile = TextIO.openOut outpath
    in
      TextIO.output (outfile, Mips.pp_mips_list code);
      TextIO.closeOut outfile
    end

  fun compile arg path =
    let
      val inpath = path ^ ".l0"
      val outpath = path ^ ".asm"
      val lexbuf = createLexerStream (BasicIO.open_in inpath)
    in
      let val pgm = Parser.Prog Lexer.Token lexbuf
      in case arg of
        "-i" => interpret pgm
      | "-o" => compileAux [Optimization.opt_pgm] pgm outpath
      | _ => compileAux [] pgm outpath
      end
      handle
        Parsing.yyexit ob => errorMsg "Parser-exit\n"
      | Parsing.ParseError ob =>
          errorMsgAt "Parsing error" (Lexer.getPos lexbuf)

      | Lexer.LexicalError (mess, pos) =>
          errorMsgAt ("Lexing error: "  ^ mess) pos

      | Interpret.Error (mess, pos) =>
          errorMsgAt ("Interpreter error: " ^ mess) pos

      | SymTab.Duplicate (mess) =>
          errorMsg ("Symbol table error: " ^ mess)

      | Optimization.NotInSymTab (id) =>
          errorMsg ("Optimization error: Id not found in symbol table " ^ id)

      | Optimization.Error (mess, pos) =>
          errorMsgAt ("Optimization error: " ^ mess) pos

      | Compiler.Error (mess, pos) =>
          errorMsgAt ("Compilation error: " ^ mess) pos

      | Type.Error (mess, pos) =>
          errorMsgAt ("Type error: " ^ mess) pos

      | SysErr (s,_) => errorMsg ("Exception: " ^ s)
    end
  val _ =
    let
      val argv = Mosml.argv()
    in
      case argv of
        [_, arg, path] => compile arg path
      | [_, path] => compile "-c" path
      | _ => print "Please supply a path to a L0 program.\n"
    end
end

*)





  fun compile arg path =
    let
      val inpath = path ^ ".l0"
      val outpath = path ^ ".asm"
      val lexbuf = createLexerStream (BasicIO.open_in inpath)
    in
      let val pgm = Parser.Prog Lexer.Token lexbuf
      in case arg of
        "-i" => print("Program is:\n\n" ^ AbSynL0.prettyPrint pgm ^ "\n\n")
      | "-o" => print("Program is:\n\n" ^ AbSynL0.prettyPrint pgm ^ "\n\n")
      | other=> print("Program is:\n\n" ^ AbSynL0.prettyPrint pgm ^ "\n\n")
      end
      handle
        Parsing.yyexit ob => errorMsg "Parser-exit\n"
      | Parsing.ParseError ob =>
          errorMsgAt "Parsing error" (Lexer.getPos lexbuf)

      | Lexer.LexicalError (mess, pos) =>
          errorMsgAt ("Lexing error: "  ^ mess) pos

      | AbSynL0.Error(mess, pos) => 
          errorMsgAt ("AbSynL0 pretty-print error: " ^ mess) pos

      | SymTab.Duplicate (mess) =>
          errorMsg ("Symbol table error: " ^ mess)

      | SysErr (s,_) => errorMsg ("Exception: " ^ s)
    end
  val _ =
    let
      val argv = Mosml.argv()
    in
      case argv of
        [_, arg, path] => compile arg path
      | [_, path] => compile "-c" path
      | _ => print "Please supply a path to a L0 program.\n"
    end
end
-}
