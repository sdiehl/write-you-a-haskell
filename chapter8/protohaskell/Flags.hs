module Flags (
  -- * Compiler flags
  Flag(..),
  Flags,

  -- * Setting/Unsetting
  isSet,
  set,
  unset,

  -- * Command line switches
  flagOpts,
  flagFor,
) where

import qualified Data.Set as S
import Control.Monad (msum)
import Data.List (isPrefixOf)

-- Flag set.
type Flags = S.Set Flag

data Flag
  = DumpC
  | DumpLLVM         -- ^ \-ddump-llvm
  | DumpASM          -- ^ \-ddump-asm
  | DumpParsed       -- ^ \-ddump-parsed
  | DumpDesugar      -- ^ \-ddump-desugar
  | DumpInfer        -- ^ \-ddump-infer
  | DumpCore         -- ^ \-ddump-core
  | DumpTypes        -- ^ \-ddump-types
  | DumpKinds        -- ^ \-ddump-types
  | DumpStg          -- ^ \-ddump-stg
  | DumpImp          -- ^ \-ddump-imp
  | DumpRenamer      -- ^ \-ddump-rn
  | DumpToFile       -- ^ \-ddump-to-file
  deriving (Eq, Ord, Show)

-- | Query a flag setting.
isSet :: Flags -> Flag -> Bool
isSet = flip S.member

-- | Insert a flag into the flag set.
set :: Flags -> Flag -> Flags
set = flip S.insert

-- | Remove a flag into the flag set.
unset :: Flags -> Flag -> Flags
unset = flip S.delete

flags :: [(String, Flag)]
flags = [
    ("ddump-parsed"  , DumpParsed)
  , ("ddump-ds"      , DumpDesugar)
  , ("ddump-core"    , DumpCore)
  , ("ddump-infer"   , DumpInfer)
  , ("ddump-types"   , DumpTypes)
  , ("ddump-kinds"   , DumpKinds)
  , ("ddump-stg"     , DumpStg)
  , ("ddump-imp"     , DumpImp)
  , ("ddump-c"       , DumpC)
  , ("ddump-rn"      , DumpRenamer)
  , ("ddump-to-file" , DumpToFile)
  ]

matches :: String -> (String, Flag) -> Maybe Flag
matches s (flagstr, flag)
  | ('-' : flagstr) `isPrefixOf` s = Just flag
  | otherwise = Nothing

-- | Command line switches for flag options
flagOpts :: [String]
flagOpts = fmap fst flags

-- | Lookup the flag from a command line option switch.
flagFor :: String -> Maybe Flags.Flag
flagFor s = msum $ fmap (matches s) flags
