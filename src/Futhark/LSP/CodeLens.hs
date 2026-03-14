{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

module Futhark.LSP.CodeLens (evalLensesFor, execute, resolve) where

import Control.Arrow ((>>>))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AllocationLimitExceeded (AllocationLimitExceeded), handle)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Except (Except, MonadError (throwError), runExceptT)
import Control.Monad.Trans (MonadIO (liftIO), lift)
import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Foldable (toList)
import Data.Function ((&))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Mixed.Rope qualified as R
import Futhark.Compiler.Program (VFS)
import Futhark.Eval (Evaluation (abort), InterpreterConfig (InterpreterConfig), newFutharkiState, runEvalRecordRef, runExpr)
import Futhark.LSP.CommandType qualified as CommandType
import Futhark.LSP.Tool (transformVFS, Execute)
import Futhark.Util (showText)
import Futhark.Util.Pretty (docText, plural, pretty, vcat)
import Language.LSP.Protocol.Message (SMethod (SMethod_WorkspaceApplyEdit))
import Language.LSP.Protocol.Types (ApplyWorkspaceEditParams (..), CodeLens (..), Command (..), ErrorCodes (ErrorCodes_InvalidParams), Position (..), Range (..), TextEdit (..), UInt, Uri, WorkspaceEdit (..), fromNormalizedFilePath, toNormalizedUri, uriToNormalizedFilePath, type (|?) (..))
import Language.LSP.Server (LspT, getVirtualFile, getVirtualFiles, sendRequest)
import Language.LSP.VFS (file_text)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Mem (enableAllocationLimit, setAllocationCounter)
import System.Timeout (timeout)

-- | All the possible lenses
--
-- I tried some trickery with GADTs here for advanced type safety, but I
-- ultimately didn't succeed because I was unable to write/derive instances
newtype LensPayload
  = EvalLensPayload EvalLensData

instance Aeson.ToJSON LensPayload where
  -- it is necessary to make sure that this mirror with @FromJSON@
  toJSON :: LensPayload -> Aeson.Value
  toJSON (EvalLensPayload payload) = payloadWithType "EvalLens" payload
    where
      payloadWithType :: (Aeson.ToJSON a) => Text -> a -> Aeson.Value
      payloadWithType typ load =
        Aeson.Object . KeyMap.fromList $
          [ ("type", Aeson.toJSON typ),
            ("payload", Aeson.toJSON load)
          ]

instance Aeson.FromJSON LensPayload where
  -- it is necessary to make sure that this mirror with @toJSON@
  parseJSON :: Aeson.Value -> Aeson.Parser LensPayload
  parseJSON = Aeson.withObject "LensPayload" $ \object -> do
    (typ :: Text) <- Aeson.parseField object "type"
    case typ of
      "EvalLens" -> EvalLensPayload <$> Aeson.parseField object "payload"
      _ -> fail $ "Unknown Lens Type: " ++ T.unpack typ

-- which document, which line
-- this is valid as long as the document has an eval comment on that line
data EvalLensData = EvalLensData
  { eldTextDocument :: Uri,
    eldLine :: UInt
  }

instance Aeson.ToJSON EvalLensData where
  toJSON :: EvalLensData -> Aeson.Value
  toJSON (EvalLensData textDoc line) = Aeson.toJSON (textDoc, line)

instance Aeson.FromJSON EvalLensData where
  parseJSON :: Aeson.Value -> Aeson.Parser EvalLensData
  parseJSON o = do
    (textDoc, line) <- parseJSON o
    pure $ EvalLensData textDoc line

evalLensPrefix :: T.Text
evalLensPrefix = "-- >>>"

evalLensesFor :: Uri -> LspT () IO (Either Text [CodeLens])
evalLensesFor file_uri = runExceptT $ do
  vfile <-
    lift (getVirtualFile $ toNormalizedUri file_uri) >>= \case
      Nothing -> throwError . T.pack $ "Could not find file: " ++ show file_uri
      Just vfile -> pure vfile

  let commentLines =
        vfile ^. file_text
          & R.lines
          & zip [0 :: UInt ..]
          & mapMaybe filterComment

  pure $
    commentLines
      & map evalLens
  where
    filterComment (i, line) = i <$ T.stripPrefix evalLensPrefix line
    evalLens i =
      CodeLens
        { _command = Nothing,
          _data_ =
            Just . Aeson.toJSON . EvalLensPayload $
              EvalLensData
                { eldTextDocument = file_uri,
                  eldLine = i
                },
          _range =
            Range
              { _start = Position {_line = i, _character = 0},
                _end = Position {_line = i, _character = maxBound}
              }
        }

resolve :: CodeLens -> Except Text CodeLens
resolve (CodeLens _ (Just _) _) =
  throwError "CodeLens is already resolved"
resolve (CodeLens _ _ Nothing) =
  throwError "CodeLens doesn't have data attached"
resolve (CodeLens range Nothing (Just payload)) =
  case Aeson.fromJSON payload :: Aeson.Result LensPayload of
    Aeson.Error msg -> throwError $ T.pack msg
    Aeson.Success _ ->
      pure $
        CodeLens
          { _range = range,
            _data_ = Nothing,
            _command =
              Just
                Command
                  { _arguments = Just [payload],
                    _title = "Evaluate",
                    _command = showText CommandType.CodeLens
                  }
          }

-- | Decode the arguments
execute :: Maybe [Aeson.Value] -> Execute ()
execute (Just [argument]) = case Aeson.fromJSON argument :: Aeson.Result LensPayload of
  Aeson.Success payload -> executeLens payload
  Aeson.Error msg ->
    throwError
      ( "Failed to decode CodeLens command argument: " <> T.pack msg,
        InR ErrorCodes_InvalidParams
      )
execute bad =
  throwError
    ( "Expected exactly one argument for the CodeLens Command, got: "
        <> showText bad,
      InR ErrorCodes_InvalidParams
    )

-- | Dispatch to the correct lens executor
--
-- (currently there's only one)
executeLens :: LensPayload -> Execute ()
executeLens (EvalLensPayload payloadData) = executeEvalLens payloadData

-- | Execute a Evaluation Lens
executeEvalLens :: EvalLensData -> Execute ()
executeEvalLens (EvalLensData docUri line) = do
  -- retrieve the file
  file <-
    lift (getVirtualFile $ toNormalizedUri docUri) >>= \case
      Nothing ->
        throwError
          ( "Could not find document specified in Command: " <> showText docUri,
            InR ErrorCodes_InvalidParams
          )
      Just file -> pure file

  -- check the line
  let lineText = R.toText . R.getLine (fromIntegral line) $ file ^. file_text
  expressionText <- case T.stripPrefix evalLensPrefix lineText of
    Nothing ->
      throwError
        ( "Specified line does not contain an evaluation comment",
          InR ErrorCodes_InvalidParams
        )
    Just expression -> pure expression

  currentVFS <- lift $ transformVFS <$> getVirtualFiles
  result <- liftIO $ performEvaluation currentVFS expressionText

  publishResult result $ file ^. file_text

  pure ()
  where
    publishResult (result, traces) fileRope =
      void . lift $
        sendRequest SMethod_WorkspaceApplyEdit workSpaceEditParams $
          const (pure ())
      where
        findResultLinesEnd i
          -- don't override other eval comments
          | T.isPrefixOf evalLensPrefix commentLine =
              i
          -- replace all output comments
          | T.isPrefixOf "-- " commentLine =
              findResultLinesEnd $ succ i
          -- stop otherwise
          | otherwise = i
          where
            commentLine = R.toText $ R.getLine i fileRope
        insertText =
          -- TODO: configurable retained trace count
          let droppedTraceCount = Seq.length traces - 100
              truncatedTraces =
                Seq.drop droppedTraceCount traces
                  & if droppedTraceCount > 0
                    then
                      ( "Hiding "
                          <> pretty droppedTraceCount
                          <> plural " trace" " traces" droppedTraceCount
                          Seq.<|
                      )
                    else id
              allDocs = truncatedTraces :|> either id id result
              commentLines =
                T.lines
                  >>> map ("-- " <>)
                  >>> T.unlines
           in commentLines . docText . vcat . toList $ allDocs
        insertResultEdit =
          TextEdit
            { _newText = insertText,
              _range =
                Range
                  { _end =
                      Position
                        { _character = 0,
                          -- insert below the evaluation comment
                          _line = line + 1
                        },
                    _start =
                      Position
                        { _line =
                            -- replace the entire comment range
                            -- removes any previous results
                            fromIntegral
                              . findResultLinesEnd
                              . succ
                              . fromIntegral
                              $ line,
                          _character = 0
                        }
                  }
            }
        workSpaceEditParams =
          ApplyWorkspaceEditParams
            { _label = Just "Insert result of code lens evaluation",
              _edit =
                WorkspaceEdit
                  { _documentChanges = Nothing,
                    _changeAnnotations = Nothing,
                    _changes = Just $ M.singleton docUri [insertResultEdit]
                  }
            }

    performEvaluation ::
      VFS ->
      Text ->
      IO (Either (Doc AnsiStyle) (Doc AnsiStyle), Seq (Doc AnsiStyle))
    performEvaluation currentVFS expressionText = do
      resultVar <- newEmptyMVar
      traceRef <- newIORef Seq.empty
      _evaluationTid <- forkIO $ putMVar resultVar =<< evaluationAction traceRef
      (,) <$> takeMVar resultVar <*> readIORef traceRef
      where
        setupLimits = do
          -- I was unable to trigger the timeout with a lower limit at all
          setAllocationCounter 100_000_000_000
          enableAllocationLimit

        evaluationAction ::
          IORef (Seq (Doc AnsiStyle)) ->
          IO (Either (Doc AnsiStyle) (Doc AnsiStyle))
        evaluationAction traceRef = interpret $ do
          -- do not print warnings, no file
          let interpreterConfig = InterpreterConfig False Nothing
          let filePath =
                toNormalizedUri docUri
                  & uriToNormalizedFilePath
                  & fmap fromNormalizedFilePath

          -- load the file the expression is located in
          interpreterState <-
            newFutharkiState interpreterConfig filePath currentVFS
              >>= either abort pure

          liftIO setupLimits

          runExpr interpreterState expressionText
          where
            -- TODO: Configurable allocation limit
            handleOom AllocationLimitExceeded =
              pure (Left "Computation ran out of memory")

            -- TODO: Configurable timeout
            handleTimeout action =
              timeout timeoutMilliseconds action
                & fmap (fromMaybe (Left "Computation ran out of time"))
              where
                timeoutMilliseconds = 15 * 10 ^ (6 :: Int)

            interpret =
              handle handleOom . handleTimeout . runEvalRecordRef traceRef
