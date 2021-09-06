{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-| Example trace for the uniswap contract
-}
module Uniswap.Trace(
    uniswapTrace
    --
    , setupTokens
    , tokenNames
    , wallets
    ) where

import           Control.Monad                     (forM_, when)
import           Control.Monad.Freer.Error         (throwError)
import qualified Data.Map                          as Map
import qualified Data.Monoid                       as Monoid
import qualified Data.Semigroup                    as Semigroup
import           Ledger
import           Ledger.Ada                        (adaSymbol, adaToken)
import           Ledger.Constraints
import           Ledger.Value                      as Value
import           Plutus.Contract                   hiding (throwError)
import qualified Plutus.Contracts.Currency         as Currency
import           Uniswap.OffChain as OffChain
import           Uniswap.Types    as Types
import           Plutus.Trace.Emulator             (EmulatorRuntimeError (GenericError), EmulatorTrace)
import qualified Plutus.Trace.Emulator             as Emulator
import           Wallet.Emulator.Types             (Wallet (..), walletPubKey)

-- | Set up a liquidity pool and call the "add" endpoint
uniswapTrace :: EmulatorTrace ()
uniswapTrace = do
    cidInit <- Emulator.activateContract (Wallet 1) setupTokens "init"
    _ <- Emulator.waitNSlots 5
    cs <- Emulator.observableState cidInit >>= \case
                Just (Semigroup.Last cur) -> pure (Currency.currencySymbol cur)
                _                         -> throwError $ GenericError "failed to create currency"
    let coins = Map.fromList [(tn, Types.mkCoin cs tn) | tn <- tokenNames]
        ada   = Types.mkCoin adaSymbol adaToken

    cidStart <- Emulator.activateContract (Wallet 1) ownerEndpoint "start"
    _ <- Emulator.waitNSlots 5
    us <- Emulator.observableState cidStart >>= \case
                Monoid.Last (Just (Right v)) -> pure v
                _                            -> throwError $ GenericError "initialisation failed"

    cid2   <- Emulator.activateContractWallet (Wallet 2)  (awaitPromise $ userEndpoints us)
    cid3   <- Emulator.activateContractWallet (Wallet 3)  (awaitPromise $ userEndpoints us)
    cid4   <- Emulator.activateContractWallet (Wallet 4)  (awaitPromise $ userEndpoints us)
    cidArb <- Emulator.activateContractWallet arbitrageur (awaitPromise $ userEndpoints us)
    _ <- Emulator.waitNSlots 5

    let cpAB = OffChain.CreateParams (coins Map.! "A") (coins Map.! "B") 10000 20000
        cpBC = OffChain.CreateParams (coins Map.! "B") (coins Map.! "C") 20000 30000
        cpCD = OffChain.CreateParams (coins Map.! "C") (coins Map.! "D") 30000 40000
        cpDA = OffChain.CreateParams (coins Map.! "D") (coins Map.! "A") 40000 80000

    Emulator.callEndpoint @"create" cid2 cpAB
    _ <- Emulator.waitNSlots 5
    Emulator.callEndpoint @"create" cid2 cpBC
    _ <- Emulator.waitNSlots 5
    Emulator.callEndpoint @"create" cid3 cpCD
    _ <- Emulator.waitNSlots 5
    Emulator.callEndpoint @"create" cid4 cpDA
    _ <- Emulator.waitNSlots 5

    let swp2 = SwapParams2{amount = 3000, path = [coins Map.! "A", coins Map.! "B", coins Map.! "C", coins Map.! "D", coins Map.! "A"]}
    Emulator.callEndpoint @"swapTokensForExactTokens" cidArb swp2

    -- let ap = AddParams{apCoinA = ada, apCoinB = coins Map.! "A", apAmountA = 1000, apAmountB = 5000}
    -- Emulator.callEndpoint @"add" cid2 ap
    _ <- Emulator.waitNSlots 5
    pure ()

-- | Create some sample tokens and distribute them to
--   the emulated wallets
setupTokens :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
setupTokens = do
    ownPK <- pubKeyHash <$> ownPubKey
    cur   <- Currency.mintContract ownPK [(tn, fromIntegral (length wallets) * amount) | tn <- tokenNames]
    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs tn amount | tn <- tokenNames]

    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx

    tell $ Just $ Semigroup.Last cur

  where
    amount = 1000000

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]

arbitrageur :: Wallet
arbitrageur = Wallet 5 

tokenNames :: [TokenName]
tokenNames = ["A", "B", "C", "D"]
