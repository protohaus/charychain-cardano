
--1.Extensions and Imports 
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week10.Charytoken (policy, tokenCurSymbol) where

import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value

--2.Declare Variables
--{-# INLINABLE mytokenName #-}
mytokenName :: TokenName 
mytokenName = "4368617279546f6b656e" --CharyToken


--Minting Policy (Validator that checks under which conditions the policy is executed)
{-# INLINABLE mkPolicy #-}
mkPolicy :: PaymentPubKeyHash -> TokenName -> Integer -> () -> ScriptContext -> Bool
mkPolicy owner tn amt () ctx = traceIfFalse "wrong amount minted" checkMintedAmount -- &&
                               
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    --isSigned :: Bool
    --isSigned = txSignedBy info $ unPaymentPubKeyHash owner

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt')] -> tn' == tn && amt' == amt
        _                -> False

--actual policy and its compilation to Plutus Core 
policy :: PaymentPubKeyHash -> TokenName -> Integer -> Scripts.MintingPolicy
policy owner mytokenName amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \owner' mytokenName amt' -> Scripts.wrapMintingPolicy $ mkPolicy owner' mytokenName amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode owner
    `PlutusTx.applyCode`
    PlutusTx.liftCode mytokenName
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

--Currency Symbol
tokenCurSymbol :: PaymentPubKeyHash -> TokenName -> Integer -> CurrencySymbol
tokenCurSymbol owner tn = scriptCurrencySymbol . policy owner mytokenName
