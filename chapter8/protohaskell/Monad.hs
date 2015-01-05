{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad (
  -- * Compiler driver
  CompilerM,
  runCompilerM,

  -- * Compiler state
  CompilerState(..),
  emptyCS,

  -- * Reporting
  Pos,
  Msg,

  -- * Utilities
  inIO,
  ifSet,
) where

import Data.Monoid
import qualified Data.Text.Lazy as L

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except

import qualified Flags
import qualified Frontend as Syn

-------------------------------------------------------------------------------
-- Compiler Monad
-------------------------------------------------------------------------------

type CompilerMonad =
  ExceptT Msg
    (StateT CompilerState IO)

-- | Main compiler driver a monad.
newtype CompilerM a = Compiler { runCompiler :: CompilerMonad a }
  deriving
  ( Functor
  , Applicative
  , Alternative
  , Monad
  , MonadFix
  , MonadPlus
  , MonadIO
  , MonadState CompilerState
  , MonadError Msg
  )

-------------------------------------------------------------------------------
-- Compiler State
-------------------------------------------------------------------------------

data CompilerState = CompilerState
  { _fname    :: Maybe FilePath            -- ^ File path
  , _imports  :: [FilePath]                -- ^ Loaded modules
  , _src      :: Maybe L.Text              -- ^ File source
  , _ast      :: Maybe Syn.Module          -- ^ Frontend AST
  , _flags    :: Flags.Flags               -- ^ Compiler flags

  -- Future Chapters
  -- , _tenv     :: Env.Env                   -- ^ Typing environment
  -- , _kenv     :: Map.Map Name Kind         -- ^ Kind environment
  -- , _cenv     :: ClassEnv.ClassEnv         -- ^ Class environment
  -- , _cast     :: Maybe Core.Module         -- ^ Core AST
  -- , _cprg     :: Maybe String              -- ^ Outputted source
  -- , _venv     :: CoreEval.ValEnv Core.Expr -- ^ Core interpreter environment
  -- , _denv     :: DataEnv.DataEnv           -- ^ Entity dictionary
  -- , _clenv    :: ClassEnv.ClassHier        -- ^ Typeclass hierarchy
  -- , _stg      :: Maybe STG.Module          -- ^ STG module
  -- , _imp      :: Maybe Imp.ImpModule       -- ^ Imp module
  } deriving (Eq, Show)

-- | Initial empty compiler state.
emptyCS :: CompilerState
emptyCS = CompilerState
  { _fname    = Nothing
  , _imports  = mempty
  , _src      = Nothing
  , _ast      = Nothing
  , _flags    = mempty

  -- Future Chapters
  -- , _tenv     = mempty
  -- , _cenv     = mempty
  -- , _kenv     = mempty
  -- , _cast     = Nothing
  -- , _cprg     = Nothing
  -- , _venv     = mempty
  -- , _denv     = mempty
  -- , _clenv    = mempty
  -- , _stg      = Nothing
  -- , _imp      = Nothing
  }

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Position information
type Pos = String

-- | Failure message.
type Msg = String

-- | Run the compiler pipeline.
runCompilerM
  :: CompilerM a
  -> CompilerState
  -> IO (Either Msg a, CompilerState)
runCompilerM = runStateT . runExceptT . runCompiler

-- | Lift IO action into the Compiler IO layer.
inIO :: IO a -> CompilerM a
inIO = Compiler . liftIO

-- | Conditional execute monadic action if a flag is set.
ifSet :: Flags.Flag -> CompilerM a -> CompilerM ()
ifSet flag m = do
  flags <- gets _flags
  when (Flags.isSet flags flag) (void m)
