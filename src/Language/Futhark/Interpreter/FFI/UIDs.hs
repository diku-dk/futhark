module Language.Futhark.Interpreter.FFI.UIDs
  ( EntryUID,
    TypeUID,
    ValueUID,
    UID.uid,
    UIDSource,
    UIDSourceT,
    UID.runUIDSourceT,
    UIDSourceM,
    UID.runUIDSourceM,
    UID.getUID,
    UID.getUIDs,
  )
where

import Futhark.Util.UID qualified as UID

data Entry

data Type

data Value

type EntryUID = UID.UID Entry Word

type TypeUID = UID.UID Type Word

type ValueUID = UID.UID Value Word

type UIDSource = UID.UIDSource Word

type UIDSourceT = UID.UIDSourceT Word

type UIDSourceM = UID.UIDSourceM Word
