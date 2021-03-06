
module Cardano.CLI.Shelley.Run
  ( ShelleyClientCmdError
  , renderShelleyClientCmdError
  , runShelleyClientCommand
  , emptyPParamsUpdate
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, left, right,
                   newExceptT)

import           Cardano.Api (ApiError, EpochNo, PParams'(..),
                   ShelleyPParamsUpdate, StrictMaybe (..),
                   Update (..), createShelleyUpdateProposal, hashKey,
                   renderApiError, textShow, writeUpdate)


import           Cardano.Config.Shelley.ColdKeys (KeyError, KeyRole(..), readVerKey,
                   renderKeyError)

import           Control.Monad.Trans.Except (ExceptT)

import           Cardano.CLI.Shelley.Parsers

import           Cardano.CLI.Shelley.Run.Address
import           Cardano.CLI.Shelley.Run.StakeAddress
import           Cardano.CLI.Shelley.Run.Transaction
import           Cardano.CLI.Shelley.Run.Node
import           Cardano.CLI.Shelley.Run.Pool
import           Cardano.CLI.Shelley.Run.Query
                                         -- Block, System, DevOps
import           Cardano.CLI.Shelley.Run.Genesis
import           Cardano.CLI.Shelley.Run.TextView

data ShelleyClientCmdError
  = ShelleyCmdAddressError !ShelleyAddressCmdError
  | ShelleyCmdGenesisError !ShelleyGenesisCmdError
  | ShelleyCmdGovernanceError !ShelleyGovernanceError
  | ShelleyCmdNodeError !ShelleyNodeCmdError
  | ShelleyCmdPoolError !ShelleyPoolCmdError
  | ShelleyCmdStakeAddressError !ShelleyStakeAddressCmdError
  | ShelleyCmdTextViewError !ShelleyTextViewFileError
  | ShelleyCmdTransactionError !ShelleyTxCmdError
  | ShelleyCmdQueryError !ShelleyQueryCmdError
  deriving Show

renderShelleyClientCmdError :: ShelleyClientCmdError -> Text
renderShelleyClientCmdError err =
  case err of
    ShelleyCmdAddressError addrCmdErr -> renderShelleyAddressCmdError addrCmdErr
    ShelleyCmdGenesisError genesisCmdErr -> renderShelleyGenesisCmdError genesisCmdErr
    ShelleyCmdGovernanceError govCmdErr -> renderShelleyGovernanceError govCmdErr
    ShelleyCmdNodeError nodeCmdErr -> renderShelleyNodeCmdError nodeCmdErr
    ShelleyCmdPoolError poolCmdErr -> renderShelleyPoolCmdError poolCmdErr
    ShelleyCmdStakeAddressError stakeAddrCmdErr -> renderShelleyStakeAddressCmdError stakeAddrCmdErr
    ShelleyCmdTextViewError txtViewErr -> renderShelleyTextViewFileError txtViewErr
    ShelleyCmdTransactionError txErr -> renderShelleyTxCmdError txErr
    ShelleyCmdQueryError queryErr -> renderShelleyQueryCmdError queryErr

--
-- CLI shelley command dispatch
--

runShelleyClientCommand :: ShelleyCommand -> ExceptT ShelleyClientCmdError IO ()
runShelleyClientCommand (AddressCmd      cmd) = firstExceptT ShelleyCmdAddressError $ runAddressCmd cmd
runShelleyClientCommand (StakeAddressCmd cmd) = firstExceptT ShelleyCmdStakeAddressError $ runStakeAddressCmd cmd
runShelleyClientCommand (TransactionCmd  cmd) = firstExceptT ShelleyCmdTransactionError $ runTransactionCmd  cmd
runShelleyClientCommand (NodeCmd         cmd) = firstExceptT ShelleyCmdNodeError $ runNodeCmd cmd
runShelleyClientCommand (PoolCmd         cmd) = firstExceptT ShelleyCmdPoolError $ runPoolCmd cmd
runShelleyClientCommand (QueryCmd        cmd) = firstExceptT ShelleyCmdQueryError $ runQueryCmd cmd
runShelleyClientCommand (BlockCmd        cmd) = runBlockCmd cmd
runShelleyClientCommand (SystemCmd       cmd) = runSystemCmd cmd
runShelleyClientCommand (GovernanceCmd   cmd) = runGovernanceCmd cmd
runShelleyClientCommand (GenesisCmd      cmd) = firstExceptT ShelleyCmdGenesisError $ runGenesisCmd cmd
runShelleyClientCommand (TextViewCmd     cmd) = firstExceptT ShelleyCmdTextViewError $ runTextViewCmd cmd



--TODO: if you fill any of these in, move them into their own modules first!

runBlockCmd :: BlockCmd -> ExceptT ShelleyClientCmdError IO ()
runBlockCmd cmd = liftIO $ putStrLn $ "TODO: runBlockCmd: " ++ show cmd

runSystemCmd:: SystemCmd -> ExceptT ShelleyClientCmdError IO ()
runSystemCmd cmd = liftIO $ putStrLn $ "TODO: runSystemCmd: " ++ show cmd

runGovernanceCmd :: GovernanceCmd -> ExceptT ShelleyClientCmdError IO ()
runGovernanceCmd (GovernanceUpdateProposal out eNo genVKeys ppUp) =
  firstExceptT ShelleyCmdGovernanceError $ runGovernanceUpdateProposal out eNo genVKeys ppUp
runGovernanceCmd cmd = liftIO $ putStrLn $ "TODO: runGovernanceCmd: " ++ show cmd

data ShelleyGovernanceError
  = GovernanceWriteUpdateError !FilePath !ApiError
  | GovernanceEmptyUpdateProposal
  | GovernanceReadGenVerKeyError !FilePath !KeyError
  deriving Show

renderShelleyGovernanceError :: ShelleyGovernanceError -> Text
renderShelleyGovernanceError err =
  case err of
    GovernanceReadGenVerKeyError fp keyErr ->
      "Error reading genesis verification key at: " <> textShow fp <> " Error: " <> renderKeyError keyErr
    GovernanceWriteUpdateError fp apiError ->
      "Error writing shelley update proposal at: " <> textShow fp <> " Error: " <> renderApiError apiError
    -- TODO: The equality check is still not working for empty update proposals.
    GovernanceEmptyUpdateProposal ->
      "Empty update proposals are not allowed"

runGovernanceUpdateProposal
  :: OutputFile
  -> EpochNo
  -> [VerificationKeyFile]
  -- ^ Genesis verification keys
  -> ShelleyPParamsUpdate
  -> ExceptT ShelleyGovernanceError IO ()
runGovernanceUpdateProposal (OutputFile upFile) eNo genVerKeyFiles upPprams' = do
    upPprams <- checkForEmptyProposal $! upPprams'
    genVKeys <- mapM
                  (\(VerificationKeyFile fp) -> do
                    gvk <- firstExceptT (GovernanceReadGenVerKeyError fp) $ readVerKey GenesisKey fp
                    pure gvk
                  )
                  genVerKeyFiles
    let genKeyHashes = map hashKey genVKeys
        upProp = ShelleyUpdate $ createShelleyUpdateProposal eNo genKeyHashes upPprams
    firstExceptT (GovernanceWriteUpdateError upFile)  . newExceptT $ writeUpdate upFile upProp
  where
    checkForEmptyProposal :: ShelleyPParamsUpdate -> ExceptT ShelleyGovernanceError IO ShelleyPParamsUpdate
    checkForEmptyProposal sPParams
      | sPParams == emptyPParamsUpdate = left GovernanceEmptyUpdateProposal
      | otherwise = right sPParams

-- TODO: Import from shelley ledger specs
emptyPParamsUpdate :: ShelleyPParamsUpdate
emptyPParamsUpdate =
  PParams
    { _minfeeA = SNothing,
      _minfeeB = SNothing,
      _maxBBSize = SNothing,
      _maxTxSize = SNothing,
      _maxBHSize = SNothing,
      _keyDeposit = SNothing,
      _keyMinRefund = SNothing,
      _keyDecayRate = SNothing,
      _poolDeposit = SNothing,
      _poolMinRefund = SNothing,
      _poolDecayRate = SNothing,
      _eMax = SNothing,
      _nOpt = SNothing,
      _a0 = SNothing,
      _rho = SNothing,
      _tau = SNothing,
      _d = SNothing,
      _extraEntropy = SNothing,
      _protocolVersion = SNothing,
      _minUTxOValue = SNothing
    }
