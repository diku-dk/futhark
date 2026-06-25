module Language.Futhark.TypeChecker.ModulesTests
  ( tests,
  )
where

import Data.Map qualified as M
import Futhark.Util.Pretty (docString)
import Language.Futhark.Semantic
import Language.Futhark.Syntax
import Language.Futhark.SyntaxTests ()
import Language.Futhark.TypeChecker
import Language.Futhark.TypeChecker.Modules
import Test.Tasty
import Test.Tasty.HUnit

higherOrderModules :: TestTree
higherOrderModules =
  testCase "higher order modules" $
    case matchMTys mty_a mty_b mempty of
      Left err -> assertFailure $ docString $ prettyTypeErrorNoLoc err
      Right m -> m @?= M.fromList [("t_4654", "t_4658"), ("f_4656", "f_4661")]
  where
    mty_a =
      MTy mempty . ModFun $
        FunModType
          { funModTypeAbs =
              M.fromList [("t_4658", Unlifted)],
            funModTypeMod =
              ModEnv $
                mempty
                  { envTypeTable =
                      M.fromList
                        [("t_4658", TypeAbbr Unlifted [] "t_4658")],
                    envNameMap = M.fromList [((Type, "t"), "t_4658")]
                  },
            funModTypeMty =
              MTy
                { mtyAbs = mempty,
                  mtyMod =
                    ModEnv $
                      mempty
                        { envVtable =
                            M.fromList
                              [ ( "f_4661",
                                  BoundV
                                    { boundValTParams = [],
                                      boundValType = "(x_4660: t_4658) -> t_4659.t_4658"
                                    }
                                )
                              ],
                          envNameMap = M.fromList [((Term, "f"), "f_4661")]
                        }
                }
          }
    mty_b =
      MTy mempty . ModFun $
        FunModType
          { funModTypeAbs = M.fromList [("t_4654", Unlifted)],
            funModTypeMod =
              ModEnv $
                mempty
                  { envTypeTable =
                      M.fromList
                        [ ( VName "t" 4654,
                            TypeAbbr Unlifted [] "t_4654"
                          )
                        ],
                    envNameMap =
                      M.fromList [((Type, "t"), "t_4654")]
                  },
            funModTypeMty =
              MTy
                { mtyAbs = mempty,
                  mtyMod =
                    ModEnv $
                      mempty
                        { envVtable =
                            M.fromList
                              [ ( "f_4656",
                                  BoundV
                                    { boundValTParams = [],
                                      boundValType = "V_4655.t_4654 -> V_4655.t_4654"
                                    }
                                )
                              ],
                          envNameMap =
                            M.fromList [((Term, "f"), "f_4656")]
                        }
                }
          }

tests :: TestTree
tests =
  testGroup
    "ModulesTests"
    [ testGroup
        "matchMTys"
        [ higherOrderModules
        ]
    ]
