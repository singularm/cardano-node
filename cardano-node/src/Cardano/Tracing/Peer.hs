{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Cardano.Tracing.Peer
  ( Peer (..)
  , NodeKernelData (..)
  , getCurrentPeers
  , initialNodeKernelData
  , ppPeer
  , setNodeKernel
  , tracePeers
  ) where

import           Cardano.Prelude hiding (atomically)
import           Prelude (String)

import qualified Control.Monad.Class.MonadSTM.Strict as STM

import           Data.Aeson (ToJSON(..), toJSON, Value (..), (.=))
import           Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Text.Printf (printf)

import           Cardano.BM.Data.LogItem (LOContent (..),
                                          PrivacyAnnotation (..),
                                          mkLOMeta)
import           Cardano.BM.Tracing
import           Cardano.BM.Trace (traceNamedObject, appendName)
import           Cardano.BM.Data.Tracer (emptyObject, mkObject)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Mempool.API
                   (MempoolCapacityBytes (..), getCapacity)
import           Ouroboros.Consensus.Node (NodeKernel(..), RunNode, nodeMaxTxSize, remoteAddress)
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as Net
import qualified Ouroboros.Network.Block as Net
import           Ouroboros.Network.Block (unSlotNo)
import qualified Ouroboros.Network.BlockFetch.ClientRegistry as Net
import           Ouroboros.Network.BlockFetch.ClientState (PeerFetchInFlight (..), PeerFetchStatus (..), readFetchClientState)
import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.NodeToNode (RemoteConnectionId) 

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB

data Peer blk =
  Peer
  !RemoteConnectionId
  !(Net.AnchoredFragment (Header blk))
  !(PeerFetchStatus (Header blk))
  !(PeerFetchInFlight (Header blk))
  deriving (Generic)

instance NoUnexpectedThunks (Peer blk) where
    whnfNoUnexpectedThunks _ _ = pure NoUnexpectedThunks

instance NFData (Peer blk) where
    rnf _ = ()

ppPeer :: Peer blk -> Text
ppPeer (Peer cid _af status inflight) =
  Text.pack $ printf "%-15s %-8s %s" (ppCid cid) (ppStatus status) (ppInFlight inflight)

ppCid :: RemoteConnectionId -> String
ppCid = takeWhile (/= ':') . show . remoteAddress

ppInFlight :: PeerFetchInFlight header -> String
ppInFlight f = printf
 "%5s  %3d  %5d  %6d"
 (ppMaxSlotNo $ peerFetchMaxSlotNo f)
 (peerFetchReqsInFlight f)
 (Set.size $ peerFetchBlocksInFlight f)
 (peerFetchBytesInFlight f)

ppMaxSlotNo :: Net.MaxSlotNo -> String
ppMaxSlotNo Net.NoMaxSlotNo = "???"
ppMaxSlotNo (Net.MaxSlotNo x) = show (unSlotNo x)

ppStatus :: PeerFetchStatus header -> String
ppStatus PeerFetchStatusShutdown = "shutdown"
ppStatus PeerFetchStatusAberrant = "aberrant"
ppStatus PeerFetchStatusBusy     = "fetching"
ppStatus PeerFetchStatusReady {} = "ready"


data SMaybe a
  = SNothing
  | SJust !a
  deriving (Foldable, Functor, Generic, NFData, NoUnexpectedThunks, Traversable)

fromSMaybe :: a -> SMaybe a -> a
fromSMaybe x SNothing = x
fromSMaybe _ (SJust x) = x

newtype LVNodeKernel blk = LVNodeKernel
  { getNodeKernel :: NodeKernel IO RemoteConnectionId LocalConnectionId blk }
  deriving (Generic)

instance NoUnexpectedThunks (LVNodeKernel blk) where
    whnfNoUnexpectedThunks _ _ = pure NoUnexpectedThunks

instance NFData (LVNodeKernel blk) where
    rnf _ = ()

data NodeKernelData blk = NodeKernelData
  { nkdMempoolCapacity      :: !Word64
  , nkdMempoolCapacityBytes :: !Word64
  , nkdKernel               :: !(SMaybe (LVNodeKernel blk))
  }

initialNodeKernelData :: NodeKernelData blk
initialNodeKernelData = NodeKernelData 0 0 SNothing

setNodeKernel :: RunNode blk
              => IORef (NodeKernelData blk)
              -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
              -> IO ()
setNodeKernel nodeKernIORef nodeKern = do
  -- This is correct for Byron and Shelley.
  MempoolCapacityBytes mempoolCapacityBytes <- STM.atomically $ getCapacity (getMempool nodeKern)

  currentLedger <- STM.atomically $ ChainDB.getCurrentLedger (getChainDB nodeKern)

  let maxTxSize = nodeMaxTxSize $ ledgerState currentLedger
      actualNodeKernelData = NodeKernelData
        { nkdMempoolCapacity = fromIntegral $ mempoolCapacityBytes `div` maxTxSize
        , nkdMempoolCapacityBytes = fromIntegral mempoolCapacityBytes
        , nkdKernel = SJust (LVNodeKernel nodeKern)
        }
  -- We do it once, so don't need an atomic updating here.
  writeIORef nodeKernIORef actualNodeKernelData

getCurrentPeers
  :: IORef (NodeKernelData blk)
  -> IO [Peer blk]
getCurrentPeers nodeKernIORef = do
  nkd <- readIORef nodeKernIORef
  fromSMaybe mempty <$> sequence (extractPeers . getNodeKernel <$> nkdKernel nkd)
 where
  tuple3pop :: (a, b, c) -> (a, b)
  tuple3pop (a, b, _) = (a, b)

  getCandidates
    :: STM.StrictTVar IO (Map peer (STM.StrictTVar IO (Net.AnchoredFragment (Header blk))))
    -> STM.STM IO (Map peer (Net.AnchoredFragment (Header blk)))
  getCandidates var = STM.readTVar var >>= traverse STM.readTVar

  extractPeers :: NodeKernel IO RemoteConnectionId LocalConnectionId blk
                -> IO [Peer blk]
  extractPeers kernel = do
    peerStates <- fmap tuple3pop <$> (   STM.atomically
                                       . (>>= traverse readFetchClientState)
                                       . Net.readFetchClientsStateVars
                                       . getFetchClientRegistry $ kernel
                                     )
    candidates <- STM.atomically . getCandidates . getNodeCandidates $ kernel

    let peers = flip Map.mapMaybeWithKey candidates $ \cid af ->
                  maybe Nothing
                        (\(status, inflight) -> Just $ Peer cid af status inflight)
                        $ Map.lookup cid peerStates
    pure . Map.elems $ peers

-- | Trace peers list, it will be forwarded to an external process
--   (for example, to RTView service).
tracePeers
  :: Trace IO Text
  -> [Peer blk]
  -> IO ()
tracePeers tr peers = do
  let tr' = appendName "metrics" tr
  let tr'' = appendName "peersFromNodeKernel" tr'
  meta <- mkLOMeta Notice Public
  traceNamedObject tr'' (meta, LogStructured $ toObject MaximalVerbosity peers)

-- | Instances for convertin [Peer blk] to Object.

instance ToObject [Peer blk] where
  toObject MinimalVerbosity _ = emptyObject
  toObject _ [] = emptyObject
  toObject verb xs = mkObject
    [ "kind"  .= String "NodeKernelPeers"
    , "peers" .= toJSON
      (foldl' (\acc x -> toObject verb x : acc) [] xs)
    ]

instance ToObject (Peer blk) where
  toObject _verb (Peer cid _af status inflight) =
    mkObject [ "peerAddress"   .= String (Text.pack . show . remoteAddress $ cid)
             , "peerStatus"    .= String (Text.pack . ppStatus $ status)
             , "peerSlotNo"    .= String (Text.pack . ppMaxSlotNo . peerFetchMaxSlotNo $ inflight)
             , "peerReqsInF"   .= String (show . peerFetchReqsInFlight $ inflight)
             , "peerBlocksInF" .= String (show . Set.size . peerFetchBlocksInFlight $ inflight)
             , "peerBytesInF"  .= String (show . peerFetchBytesInFlight $ inflight)
             ]
