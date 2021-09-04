    {-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Uniswap.OffChain
    ( poolStateCoinFromUniswapCurrency, liquidityCoin
    , CreateParams (..)
    , SwapParams (..)
    , SwapParams2 (..)
    , CloseParams (..)
    , RemoveParams (..)
    , AddParams (..)
    , UniswapUserSchema, UserContractState (..)
    , UniswapOwnerSchema
    , start, create, add, remove, close, swap, pools
    , ownerEndpoint, userEndpoints
    -- , assetSymbol
    -- , assetToken1
    -- , assetToken2
    -- , assetToken3
    -- , toLookup
    -- , allLookups
    ) where

import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.List                        (tails, zipWith, take, drop, mapAccumL)
import           Data.Void                        (Void, absurd)
import           Ledger                           hiding (singleton)

-- import           Control.Monad              hiding (fmap)
-- import           Control.Monad.Freer.Extras as Extras
-- import           Data.Default               (Default (..))
-- import qualified Data.Map                   as Map
-- import           Data.Monoid                (Last (..))
-- import           Data.Text                  (Text)
-- import           Ledger
import           Ledger.Value               (TokenName)
-- import           Ledger.Ada                 as Ada
-- import           Plutus.Contract            as Contract
-- import           Plutus.Trace.Emulator      as Emulator
-- import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
-- import           Prelude                    (IO, Semigroup(..), Show (..))
-- import           Wallet.Emulator.Wallet

import           Ledger.Constraints               as Constraints
import           Ledger.Typed.Scripts             as Scripts
import           Playground.Contract
import           Plutus.Contract
import qualified Plutus.Contracts.Currency        as Currency
import           Uniswap.OnChain (mkUniswapValidator, validateLiquidityMinting)
import           Uniswap.Pool
import           Uniswap.Types
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), dropWhile, flip, unless)
import           Prelude                          as Haskell (Int, Semigroup (..), String, div, mconcat, dropWhile, flip, show,
                                                              (^))
import           Text.Printf                      (printf)


data Uniswapping
instance Scripts.ValidatorTypes Uniswapping where
    type instance RedeemerType Uniswapping = UniswapAction
    type instance DatumType    Uniswapping = UniswapDatum

type UniswapOwnerSchema = Endpoint "start" ()

-- | Schema for the endpoints for users of Uniswap.
type UniswapUserSchema =
        Endpoint "create" CreateParams
        .\/ Endpoint "swap"   SwapParams
        .\/ Endpoint "swap2"  SwapParams2
        .\/ Endpoint "close"  CloseParams
        .\/ Endpoint "remove" RemoveParams
        .\/ Endpoint "add"    AddParams
        .\/ Endpoint "pools"  ()
        .\/ Endpoint "funds"  ()
        .\/ Endpoint "stop"   ()

-- | Type of the Uniswap user contract state.
data UserContractState =
      Pools [((Coin A, Amount A), (Coin B, Amount B))]
    | Funds Value
    | Created
    | Swapped
    | Swapped2
    | Added
    | Removed
    | Closed
    | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)


uniswapTokenName, poolStateTokenName :: TokenName
uniswapTokenName = "Uniswap"
poolStateTokenName = "Pool State"

uniswapInstance :: Uniswap -> Scripts.TypedValidator Uniswapping
uniswapInstance us = Scripts.mkTypedValidator @Uniswapping
    ($$(PlutusTx.compile [|| mkUniswapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode us
        `PlutusTx.applyCode` PlutusTx.liftCode c)
     $$(PlutusTx.compile [|| wrap ||])
  where
    c :: Coin PoolState
    c = poolStateCoin us

    wrap = Scripts.wrapValidator @UniswapDatum @UniswapAction

uniswapScript :: Uniswap -> Validator
uniswapScript = Scripts.validatorScript . uniswapInstance

uniswapAddress :: Uniswap -> Ledger.Address
uniswapAddress = Ledger.scriptAddress . uniswapScript

uniswap :: CurrencySymbol -> Uniswap
uniswap cs = Uniswap $ mkCoin cs uniswapTokenName

liquidityPolicy :: Uniswap -> MintingPolicy
liquidityPolicy us = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \u t -> Scripts.wrapMintingPolicy (validateLiquidityMinting u t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode us
        `PlutusTx.applyCode` PlutusTx.liftCode poolStateTokenName

liquidityCurrency :: Uniswap -> CurrencySymbol
liquidityCurrency = scriptCurrencySymbol . liquidityPolicy

poolStateCoin :: Uniswap -> Coin PoolState
poolStateCoin = flip mkCoin poolStateTokenName . liquidityCurrency

-- | Gets the 'Coin' used to identity liquidity pools.
poolStateCoinFromUniswapCurrency :: CurrencySymbol -- ^ The currency identifying the Uniswap instance.
                                 -> Coin PoolState
poolStateCoinFromUniswapCurrency = poolStateCoin . uniswap

-- | Gets the liquidity token for a given liquidity pool.
liquidityCoin :: CurrencySymbol -- ^ The currency identifying the Uniswap instance.
              -> Coin A         -- ^ One coin in the liquidity pair.
              -> Coin B         -- ^ The other coin in the liquidity pair.
              -> Coin Liquidity
liquidityCoin cs coinA coinB = mkCoin (liquidityCurrency $ uniswap cs) $ lpTicker $ LiquidityPool coinA coinB

-- | Parameters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams = CreateParams
    { cpCoinA   :: Coin A   -- ^ One 'Coin' of the liquidity pair.
    , cpCoinB   :: Coin B   -- ^ The other 'Coin'.
    , cpAmountA :: Amount A -- ^ Amount of liquidity for the first 'Coin'.
    , cpAmountB :: Amount B -- ^ Amount of liquidity for the second 'Coin'.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @swap@-endpoint, which allows swaps between the two different coins in a liquidity pool.
-- One of the provided amounts must be positive, the other must be zero.
data SwapParams = SwapParams
    { spCoinA   :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , spCoinB   :: Coin B         -- ^ The other 'Coin'.
    , spAmountA :: Amount A       -- ^ The amount the first 'Coin' that should be swapped.
    , spAmountB :: Amount B       -- ^ The amount of the second 'Coin' that should be swapped.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data SwapParams2 = SwapParams2
    { 
        amount    :: Amount A
      , path      :: [Coin A]
    -- spCoinA   :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    -- , spCoinB   :: Coin B         -- ^ The other 'Coin'.
    -- , spAmountA :: Amount A       -- ^ The amount the first 'Coin' that should be swapped.
    -- , spAmountB :: Amount B       -- ^ The amount of the second 'Coin' that should be swapped.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams = CloseParams
    { clpCoinA :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , clpCoinB :: Coin B         -- ^ The other 'Coin' of the liquidity pair.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams = RemoveParams
    { rpCoinA :: Coin A           -- ^ One 'Coin' of the liquidity pair.
    , rpCoinB :: Coin B           -- ^ The other 'Coin' of the liquidity pair.
    , rpDiff  :: Amount Liquidity -- ^ The amount of liquidity tokens to burn in exchange for liquidity from the pool.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams = AddParams
    { apCoinA   :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , apCoinB   :: Coin B         -- ^ The other 'Coin' of the liquidity pair.
    , apAmountA :: Amount A       -- ^ The amount of coins of the first kind to add to the pool.
    , apAmountB :: Amount B       -- ^ The amount of coins of the second kind to add to the pool.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Creates a Uniswap "factory". This factory will keep track of the existing liquidity pools and enforce that there will be at most one liquidity pool
-- for any pair of tokens at any given time.
start :: forall w s. Contract w s Text Uniswap
start = do
    pkh <- pubKeyHash <$> ownPubKey
    cs  <- fmap Currency.currencySymbol $
           mapError (pack . show @Currency.CurrencyError) $
           Currency.mintContract pkh [(uniswapTokenName, 1)]
    let c    = mkCoin cs uniswapTokenName
        us   = uniswap cs
        inst = uniswapInstance us
        tx   = mustPayToTheScript (Factory []) $ unitValue c
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx
    void $ waitNSlots 1

    logInfo @String $ printf "started Uniswap %s at address %s" (show us) (show $ uniswapAddress us)
    return us

-- | Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
create :: forall w s. Uniswap -> CreateParams -> Contract w s Text ()
create us CreateParams{..} = do
    when (unCoin cpCoinA == unCoin cpCoinB) $ throwError "coins must be different"
    when (cpAmountA <= 0 || cpAmountB <= 0) $ throwError "amounts must be positive"
    (oref, o, lps) <- findUniswapFactory us
    let liquidity = calculateInitialLiquidity cpAmountA cpAmountB
        lp        = LiquidityPool {lpCoinA = cpCoinA, lpCoinB = cpCoinB}
    let usInst   = uniswapInstance us
        usScript = uniswapScript us
        usDat1   = Factory $ lp : lps
        usDat2   = Pool lp liquidity
        psC      = poolStateCoin us
        lC       = mkCoin (liquidityCurrency us) $ lpTicker lp
        usVal    = unitValue $ usCoin us
        lpVal    = valueOf cpCoinA cpAmountA <> valueOf cpCoinB cpAmountB <> unitValue psC

        lookups  = Constraints.typedValidatorLookups usInst        <>
                   Constraints.otherScript usScript                <>
                   Constraints.mintingPolicy (liquidityPolicy us) <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript usDat1 usVal                                     <>
                   Constraints.mustPayToTheScript usDat2 lpVal                                     <>
                   Constraints.mustMintValue (unitValue psC <> valueOf lC liquidity)              <>
                   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Create lp)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "created liquidity pool: " ++ show lp

-- | Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
close :: forall w s. Uniswap -> CloseParams -> Contract w s Text ()
close us CloseParams{..} = do
    ((oref1, o1, lps), (oref2, o2, lp, liquidity)) <- findUniswapFactoryAndPool us clpCoinA clpCoinB
    pkh                                            <- pubKeyHash <$> ownPubKey
    let usInst   = uniswapInstance us
        usScript = uniswapScript us
        usDat    = Factory $ filter (/= lp) lps
        usC      = usCoin us
        psC      = poolStateCoin us
        lC       = mkCoin (liquidityCurrency us) $ lpTicker lp
        usVal    = unitValue usC
        psVal    = unitValue psC
        lVal     = valueOf lC liquidity
        redeemer = Redeemer $ PlutusTx.toBuiltinData Close

        lookups  = Constraints.typedValidatorLookups usInst        <>
                   Constraints.otherScript usScript                <>
                   Constraints.mintingPolicy (liquidityPolicy us) <>
                   Constraints.ownPubKeyHash pkh                   <>
                   Constraints.unspentOutputs (Map.singleton oref1 o1 <> Map.singleton oref2 o2)

        tx       = Constraints.mustPayToTheScript usDat usVal          <>
                   Constraints.mustMintValue (negate $ psVal <> lVal) <>
                   Constraints.mustSpendScriptOutput oref1 redeemer    <>
                   Constraints.mustSpendScriptOutput oref2 redeemer    <>
                   Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData $ Pool lp liquidity)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "closed liquidity pool: " ++ show lp

-- | Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
remove :: forall w s. Uniswap -> RemoveParams -> Contract w s Text ()
remove us RemoveParams{..} = do
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us rpCoinA rpCoinB
    pkh                           <- pubKeyHash <$> ownPubKey
    when (rpDiff < 1 || rpDiff >= liquidity) $ throwError "removed liquidity must be positive and less than total liquidity"
    let usInst       = uniswapInstance us
        usScript     = uniswapScript us
        dat          = Pool lp $ liquidity - rpDiff
        psC          = poolStateCoin us
        lC           = mkCoin (liquidityCurrency us) $ lpTicker lp
        psVal        = unitValue psC
        lVal         = valueOf lC rpDiff
        inVal        = txOutValue $ txOutTxOut o
        inA          = amountOf inVal rpCoinA
        inB          = amountOf inVal rpCoinB
        (outA, outB) = calculateRemoval inA inB liquidity rpDiff
        val          = psVal <> valueOf rpCoinA outA <> valueOf rpCoinB outB
        redeemer     = Redeemer $ PlutusTx.toBuiltinData Remove

        lookups  = Constraints.typedValidatorLookups usInst          <>
                   Constraints.otherScript usScript                  <>
                   Constraints.mintingPolicy (liquidityPolicy us)   <>
                   Constraints.unspentOutputs (Map.singleton oref o) <>
                   Constraints.ownPubKeyHash pkh

        tx       = Constraints.mustPayToTheScript dat val          <>
                   Constraints.mustMintValue (negate lVal)        <>
                   Constraints.mustSpendScriptOutput oref redeemer

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "removed liquidity from pool: " ++ show lp

-- | Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
add :: forall w s. Uniswap -> AddParams -> Contract w s Text ()
add us AddParams{..} = do
    pkh                           <- pubKeyHash <$> ownPubKey
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us apCoinA apCoinB
    when (apAmountA < 0 || apAmountB < 0) $ throwError "amounts must not be negative"
    let outVal = txOutValue $ txOutTxOut o
        oldA   = amountOf outVal apCoinA
        oldB   = amountOf outVal apCoinB
        newA   = oldA + apAmountA
        newB   = oldB + apAmountB
        delL   = calculateAdditionalLiquidity oldA oldB liquidity apAmountA apAmountB
        inVal  = valueOf apCoinA apAmountA <> valueOf apCoinB apAmountB
    when (delL <= 0) $ throwError "insufficient liquidity"
    logInfo @String $ printf "oldA = %d, oldB = %d, newA = %d, newB = %d, delL = %d" oldA oldB newA newB delL

    let usInst       = uniswapInstance us
        usScript     = uniswapScript us
        dat          = Pool lp $ liquidity + delL
        psC          = poolStateCoin us
        lC           = mkCoin (liquidityCurrency us) $ lpTicker lp
        psVal        = unitValue psC
        lVal         = valueOf lC delL
        val          = psVal <> valueOf apCoinA newA <> valueOf apCoinB newB
        redeemer     = Redeemer $ PlutusTx.toBuiltinData Add

        lookups  = Constraints.typedValidatorLookups usInst             <>
                   Constraints.otherScript usScript                     <>
                   Constraints.mintingPolicy (liquidityPolicy us)       <>
                   Constraints.ownPubKeyHash pkh                        <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript dat val          <>
                   Constraints.mustMintValue lVal                  <>
                   Constraints.mustSpendScriptOutput oref redeemer

    logInfo @String $ printf "val = %s, inVal = %s" (show val) (show inVal)
    logInfo $ show lookups
    logInfo $ show tx

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "added liquidity to pool: " ++ show lp

-- | Uses a liquidity pool two swap one sort of coins in the pool against the other.
swap :: forall w s. Uniswap -> SwapParams -> Contract w s Text ()
swap us SwapParams{..} = do
    unless (spAmountA > 0 && spAmountB == 0 || spAmountA == 0 && spAmountB > 0) $ throwError "exactly one amount must be positive"
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us spCoinA spCoinB
    let outVal = txOutValue $ txOutTxOut o
    let oldA = amountOf outVal spCoinA
        oldB = amountOf outVal spCoinB
    (newA, newB) <- if spAmountA > 0 then do
        let outB = Amount $ findSwapA oldA oldB spAmountA
        when (outB == 0) $ throwError "no payout"
        return (oldA + spAmountA, oldB - outB)
                                     else do
        let outA = Amount $ findSwapB oldA oldB spAmountB
        when (outA == 0) $ throwError "no payout"
        return (oldA - outA, oldB + spAmountB)
    pkh <- pubKeyHash <$> ownPubKey

    logInfo @String $ printf "oldA = %d, oldB = %d, old product = %d, newA = %d, newB = %d, new product = %d" oldA oldB (unAmount oldA * unAmount oldB) newA newB (unAmount newA * unAmount newB)

    let inst    = uniswapInstance us
        val     = valueOf spCoinA newA <> valueOf spCoinB newB <> unitValue (poolStateCoin us)

        lookups = Constraints.typedValidatorLookups inst                 <>
                  Constraints.otherScript (Scripts.validatorScript inst) <>
                  Constraints.unspentOutputs (Map.singleton oref o)      <>
                  Constraints.ownPubKeyHash pkh

        tx      = mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Swap) <>
                  Constraints.mustPayToTheScript (Pool lp liquidity) val

    logInfo $ show tx
    ledgerTx <- submitTxConstraintsWith lookups tx
    logInfo $ show ledgerTx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo $ "swapped with: " ++ show lp

-- allLookups path us inst             = do
--                                          -- res <-forM path $ \[c1, c2] -> toLookup c1 c2 us inst
--                                          let lookups = Haskell.mconcat[x | x <- (map (\[c1, c2] -> toLookup us inst [c1, c2]) path)]
--                                          return lookups
                                         -- [lk | lk <- alk]
-- toLookup :: Uniswap -> Scripts.TypedValidator Uniswapping -> [Coin A] -> ScriptLookups b



-- assetSymbol :: CurrencySymbol
-- assetSymbol = "973816f0528b9a78be74950ba6f07f8ef02b1a32113d12e4e4d3c06b"

-- assetToken1, assetToken2, assetToken3 :: TokenName
-- assetToken1 = "A"
-- assetToken2 = "B"
-- assetToken3 = "C"

                                                        
swap2 :: forall w s. Uniswap -> SwapParams2 -> Contract w s Text ()
swap2 us SwapParams2{..} = do
    -- (oref1, o1, lps) <- findUniswapFactory us
    (tx, lookups) <- case path of
        p:ps                                 -> do
                                                    logInfo @String $ printf "Swapping " ++ show path ++ show inst
                                                    -- (_, (lp1oref, lp1o, lp1, lp1a))         <- findUniswapFactoryAndPool us c1 $ Coin $ unCoin c2
                                                    pkh                                     <- pubKeyHash <$> ownPubKey
                                                    -- let lks = mconcat [lk | lk <- allLookups path2]
                                                    -- let lookups = Haskell.mconcat[x | x <- (map (\[c1, c2] -> toLookup us inst [c1, c2]) path2)]
                                                    -- lookups <- toLookup us inst [c1, c2]
                                                    -- let lookups = [Constraints.typedValidatorLookups inst, Constraints.otherScript (Scripts.validatorScript inst), Constraints.unspentOutputs (Map.singleton lp1oref lp1o), Constraints.ownPubKeyHash pkh]
                                                        -- lks :: ScriptLookups Uniswapping
                                                        -- lks = Haskell.mconcat lookups
                                                    -- let lookups1 = [ Constraints.typedValidatorLookups inst, Constraints.otherScript (Scripts.validatorScript inst)]
                                                        -- lookups2 = [ Constraints.unspentOutputs (Map.singleton lp1oref lp1o), Constraints.ownPubKeyHash pkh]
                                                        -- lookups3 = lookups1:[lookups2]
                                                    -- let lookups = foldr (\[c1, c2] acc -> acc <> (toLookup us inst [c1, c2])) (Constraints.ownPubKeyHash pkh) path2
                                                    lks <- forM path2 $  toLookup us inst
                                                    -- let unwrap a aux = do
                                                    --                        tx <- toTrx a aux
                                                    --                        tx
                                                    (_aux, trxs) <- mapAccumLM (\aux a -> toTrx a aux) amount path2
                                                    let trx = Haskell.mconcat trxs
                                                    -- trx <- toTrx c1 c2 amount
                                                    -- let getTrx [cA, cB] amnt = do
                                                    --     (trx, val) <- toTrx cA cB amnt
                                                    -- x <- toLookup us inst [c1, c2]
                                                    -- y <- toLookup us inst [c2, c3]
                                                    let lookups = Haskell.mconcat $ concat lks -- $ concat [x, y] -- (map (toLookup us inst) path2)-- [toLookup us inst [c1, c2] | [c11, c22] <- [[c1, c2]]]
                                                    -- let lookups = toLookup us inst [c1, c2]

                                                        -- lp1outVal                     = txOutValue $ txOutTxOut lp1o
                                                        -- lp1oldA                       = amountOf lp1outVal c1
                                                        -- lp1oldB                       = amountOf lp1outVal $ Coin $ unCoin c2
                                                        -- -- lp2outVal                     = txOutValue $ txOutTxOut lp2o
                                                        -- -- lp2oldB                       = amountOf lp2outVal c2
                                                        -- -- lp2oldC                       = amountOf lp2outVal $ Coin $ unCoin c3
                                                        -- lp1outB                       = Amount $ findSwapA lp1oldA lp1oldB amount
                                                        -- -- lp2outC                       = Amount $ findSwapA lp2oldB lp2oldC lp1outB
                                                        -- (lp1newA, lp1newB)            = (lp1oldA + amount, lp1oldB - (Amount $ unAmount lp1outB))
                                                        -- -- (lp2newB, lp2newC)            = (lp2oldB + lp1outB, lp2oldC - lp2outC)

                                                        -- val1                          = valueOf c1 lp1newA <> valueOf c2 (Amount $ unAmount lp1newB) <> unitValue (poolStateCoin us)
                                                        -- -- val2                          = valueOf c2 lp2newB <> valueOf c3 (Amount $ unAmount lp2newC) <> unitValue (poolStateCoin us)
                                                        -- trx                           = Constraints.mustSpendScriptOutput lp1oref (Redeemer $ PlutusTx.toBuiltinData Swap)                      <>
                                                        --                                 -- Constraints.mustSpendScriptOutput lp2oref (Redeemer $ PlutusTx.toBuiltinData Swap)                      <>
                                                        --                                 Constraints.mustPayToTheScript (Pool lp1 lp1a) val1
                                                    -- trx     <- toTrx c1 c2 amount  
                                                    -- throwError "problem passing args"
                                                    return (trx, lookups)
                                                    where
                                                        mapAccumLM :: Monad m => (a -> b -> m(a, c)) -> a -> [b] -> m(a, [c])
                                                        mapAccumLM _ a [] = return (a, [])
                                                        mapAccumLM f a (x:xs) = do
                                                          (a', c) <- f a x
                                                          (a'', cs) <- mapAccumLM f a' xs
                                                          return (a'', c:cs)
                                                    -- f :: Monad a => Integer -> [a] -> [[a]]
                                                        -- wrap a aux = do
                                                        --                  let res  = toTrx a aux
                                                        --                  (amt, tx) <- res
                                                        --                  return (amt, tx)
                                                        f n m xs = zipWith const (Data.List.take n <$> tails xs) (drop m xs)


                                                        inst       = uniswapInstance us
                                                        path2      = f 2 1 path

                                                        -- toLookup :: Uniswap -> Scripts.TypedValidator Uniswapping -> [Coin A] -> ScriptLookups Uniswapping
                                                        toLookup us inst [c11, c22]= do (_, (lp1oref, lp1o, lp1, lp1a))         <- findUniswapFactoryAndPool us c11 $ Coin $ unCoin c22
                                                                                        pkh                                     <- pubKeyHash <$> ownPubKey
                                                                                        let lookups1 = [ Constraints.typedValidatorLookups inst, 
                                                                                                         Constraints.otherScript (Scripts.validatorScript inst), 
                                                                                                         Constraints.unspentOutputs (Map.singleton lp1oref lp1o), 
                                                                                                         Constraints.ownPubKeyHash pkh ]
                                                                                        -- let lookups = lookups1:[lookups2]
                                                                                        -- let lks = foldr (\x acc -> foldr (\y acc2 -> y<>acc2) acc x) (Constraints.ownPubKeyHash pkh) lookups
                                                                                        -- Haskell.mconcat[x | x <- lookups]
                                                                                        return lookups1 -- (foldr (\x acc -> acc <> x) (Constraints.ownPubKeyHash pkh) lookups1)

                                                        -- allLookups []             =   Constraints.typedValidatorLookups inst <> Constraints.otherScript (Scripts.validatorScript inst)
                                                        
                                                                                            
                                                                                            -- (_, (lp1oref, lp1o, lp1, lp1a))         <- findUniswapFactoryAndPool us c1 $ Coin $ unCoin c2
                                                                                            -- let lk = (Constraints.unspentOutputs (Map.singleton lp1oref lp1o)) -- mconcat [Constraints.unspentOutputs (Map.singleton lp1oref lp1o) | ]   
                                                                                            -- return lk
                                                                                            -- where
                                                                                                

                                                                                          -- (_, (lp1oref, lp1o, lp1, lp1a))         <- findUniswapFactoryAndPool us c1 $ Coin $ unCoin c2
                                                                                          -- -- let lookup = Constraints.unspentOutputs (Map.singleton lp1oref lp1o)
                                                                                          
                                                    -- path22   = f 2 1 path2
                                                        toTrx [c1, c2] amnt           = do      (_, (lp1oref, lp1o, lp1, lp1a))         <- findUniswapFactoryAndPool us c1 $ Coin $ unCoin c2
                                                                                                -- (_, (lp2oref, lp2o, lp2, lp2a))         <- findUniswapFactoryAndPool us c2 $ Coin $ unCoin c3
                                                                                                let lp1outVal                     = txOutValue $ txOutTxOut lp1o
                                                                                                    lp1oldA                       = amountOf lp1outVal c1
                                                                                                    lp1oldB                       = amountOf lp1outVal $ Coin $ unCoin c2
                                                                                                    -- lp2outVal                     = txOutValue $ txOutTxOut lp2o
                                                                                                    -- lp2oldB                       = amountOf lp2outVal c2
                                                                                                    -- lp2oldC                       = amountOf lp2outVal $ Coin $ unCoin c3
                                                                                                    lp1outB                       = Amount $ findSwapA lp1oldA lp1oldB amnt
                                                                                                    -- lp2outC                       = Amount $ findSwapA lp2oldB lp2oldC lp1outB
                                                                                                    (lp1newA, lp1newB)            = (lp1oldA + amnt, lp1oldB - (Amount $ unAmount lp1outB))
                                                                                                    -- (lp2newB, lp2newC)            = (lp2oldB + lp1outB, lp2oldC - lp2outC)
                    
                                                                                                    val1                          = valueOf c1 lp1newA <> valueOf c2 (Amount $ unAmount lp1newB) <> unitValue (poolStateCoin us)
                                                                                                    -- val2                          = valueOf c2 lp2newB <> valueOf c3 (Amount $ unAmount lp2newC) <> unitValue (poolStateCoin us)
                                                                                                    trx                           = Constraints.mustSpendScriptOutput lp1oref (Redeemer $ PlutusTx.toBuiltinData Swap)                      <>
                                                                                                                                    -- Constraints.mustSpendScriptOutput lp2oref (Redeemer $ PlutusTx.toBuiltinData Swap)                      <>
                                                                                                                                    Constraints.mustPayToTheScript (Pool lp1 lp1a) val1                                                               
                                                                                                          -- Constraints.mustPayToTheScript (Pool lp2 lp2a) val2 
                                                                                                return ((Amount $ unAmount lp1outB), trx)
                                                                                                                                                           -- [val1, val2] = toVal [[c1, c2], [c22, c3]]
       
        _                                    ->     throwError "problem passing args"
                                                       -- return (Nothing, Nothing)
    -- rlookups <- lookups
    logInfo $ show tx
    ledgerTx <- submitTxConstraintsWith lookups tx
    logInfo $ show ledgerTx
    void $ awaitTxConfirmed $ txId ledgerTx
    


-- | Finds all liquidity pools and their liquidity belonging to the Uniswap instance.
-- This merely inspects the blockchain and does not issue any transactions.
pools :: forall w s. Uniswap -> Contract w s Text [((Coin A, Amount A), (Coin B, Amount B))]
pools us = do
    utxos <- utxoAt (uniswapAddress us)
    go $ snd <$> Map.toList utxos
  where
    go :: [TxOutTx] -> Contract w s Text [((Coin A, Amount A), (Coin B, Amount B))]
    go []       = return []
    go (o : os) = do
        let v = txOutValue $ txOutTxOut o
        if isUnity v c
            then do
                d <- getUniswapDatum o
                case d of
                    Factory _ -> go os
                    Pool lp _ -> do
                        let coinA = lpCoinA lp
                            coinB = lpCoinB lp
                            amtA  = amountOf v coinA
                            amtB  = amountOf v coinB
                            s     = ((coinA, amtA), (coinB, amtB))
                        logInfo $ "found pool: " ++ show s
                        ss <- go os
                        return $ s : ss
            else go os
      where
        c :: Coin PoolState
        c = poolStateCoin us

-- | Gets the caller's funds.
funds :: forall w s. Contract w s Text Value
funds = do
    pkh <- pubKeyHash <$> ownPubKey
    os  <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
    return $ PlutusTx.Prelude.mconcat [txOutValue $ txOutTxOut o | o <- os]

getUniswapDatum :: TxOutTx -> Contract w s Text UniswapDatum
getUniswapDatum o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> throwError "datumHash not found"
        Just h -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing -> throwError "datum not found"
            Just (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d  -> return d

findUniswapInstance :: forall a b w s. Uniswap -> Coin b -> (UniswapDatum -> Maybe a) -> Contract w s Text (TxOutRef, TxOutTx, a)
findUniswapInstance us c f = do
    let addr = uniswapAddress us
    logInfo @String $ printf "looking for Uniswap instance at address %s containing coin %s " (show addr) (show c)
    utxos <- utxoAt addr
    go  [x | x@(_, o) <- Map.toList utxos, isUnity (txOutValue $ txOutTxOut o) c]
  where
    go [] = throwError "Uniswap instance not found"
    go ((oref, o) : xs) = do
        d <- getUniswapDatum o
        case f d of
            Nothing -> go xs
            Just a  -> do
                logInfo @String $ printf "found Uniswap instance with datum: %s" (show d)
                return (oref, o, a)

findUniswapFactory :: forall w s. Uniswap -> Contract w s Text (TxOutRef, TxOutTx, [LiquidityPool])
findUniswapFactory us@Uniswap{..} = findUniswapInstance us usCoin $ \case
    Factory lps -> Just lps
    Pool _ _    -> Nothing

findUniswapPool :: forall w s. Uniswap -> LiquidityPool -> Contract w s Text (TxOutRef, TxOutTx, Amount Liquidity)
findUniswapPool us lp = findUniswapInstance us (poolStateCoin us) $ \case
        Pool lp' l
            | lp == lp' -> Just l
        _               -> Nothing

findUniswapFactoryAndPool :: forall w s.
                          Uniswap
                          -> Coin A
                          -> Coin B
                          -> Contract w s Text ( (TxOutRef, TxOutTx, [LiquidityPool])
                                               , (TxOutRef, TxOutTx, LiquidityPool, Amount Liquidity)
                                               )
findUniswapFactoryAndPool us coinA coinB = do
    (oref1, o1, lps) <- findUniswapFactory us
    case [ lp'
         | lp' <- lps
         , lp' == LiquidityPool coinA coinB
         ] of
        [lp] -> do
            (oref2, o2, a) <- findUniswapPool us lp
            return ( (oref1, o1, lps)
                   , (oref2, o2, lp, a)
                   )
        _    -> throwError "liquidity pool not found"

findSwapA :: Amount A -> Amount B -> Amount A -> Integer
findSwapA oldA oldB inA
    | ub' <= 1   = 0
    | otherwise  = go 1 ub'
  where
    cs :: Integer -> Bool
    cs outB = checkSwap oldA oldB (oldA + inA) (oldB - Amount outB)

    ub' :: Integer
    ub' = head $ dropWhile cs [2 ^ i | i <- [0 :: Int ..]]

    go :: Integer -> Integer -> Integer
    go lb ub
        | ub == (lb + 1) = lb
        | otherwise      =
      let
        m = div (ub + lb) 2
      in
        if cs m then go m ub else go lb m

findSwapB :: Amount A -> Amount B -> Amount B -> Integer
findSwapB oldA oldB inB = findSwapA (switch oldB) (switch oldA) (switch inB)
  where
    switch = Amount . unAmount

ownerEndpoint :: Contract (Last (Either Text Uniswap)) EmptySchema ContractError ()
ownerEndpoint = do
    e <- mapError absurd $ runError start
    void $ waitNSlots 1
    tell $ Last $ Just e

-- | Provides the following endpoints for users of a Uniswap instance:
--
--      [@create@]: Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
--      [@swap@]: Uses a liquidity pool two swap one sort of coins in the pool against the other.
--      [@close@]: Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
--      [@remove@]: Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
--      [@add@]: Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
--      [@pools@]: Finds all liquidity pools and their liquidity belonging to the Uniswap instance. This merely inspects the blockchain and does not issue any transactions.
--      [@funds@]: Gets the caller's funds. This merely inspects the blockchain and does not issue any transactions.
--      [@stop@]: Stops the contract.
userEndpoints :: Uniswap -> Promise (Last (Either Text UserContractState)) UniswapUserSchema Void ()
userEndpoints us =
    stop
        `select`
    (void (f (Proxy @"create") (const Created)  create                 `select`
           f (Proxy @"swap")   (const Swapped)  swap                   `select`
           f (Proxy @"swap2")  (const Swapped2) swap2                  `select`
           f (Proxy @"close")  (const Closed)   close                  `select`
           f (Proxy @"remove") (const Removed)  remove                 `select`
           f (Proxy @"add")    (const Added)    add                    `select`
           f (Proxy @"pools")  Pools            (\us' () -> pools us') `select`
           f (Proxy @"funds")  Funds            (\_us () -> funds))
     <> userEndpoints us)
  where
    f :: forall l a p.
         (HasEndpoint l p UniswapUserSchema, FromJSON p)
      => Proxy l
      -> (a -> UserContractState)
      -> (Uniswap -> p -> Contract (Last (Either Text UserContractState)) UniswapUserSchema Text a)
      -> Promise (Last (Either Text UserContractState)) UniswapUserSchema Void ()
    f _ g c = handleEndpoint @l $ \p -> do
        e <- either (pure . Left) (runError . c us) p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a

    stop :: Promise (Last (Either Text UserContractState)) UniswapUserSchema Void ()
    stop = handleEndpoint @"stop" $ \e -> do
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right () -> Right Stopped
