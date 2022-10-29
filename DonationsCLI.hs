--1.Extensions and Imports
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE DeriveAnyClass               #-}
{-# LANGUAGE DeriveGeneric                #-}
{-# LANGUAGE NoImplicitPrelude            #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE RecordWildCards              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week10.DonationsUR where

import           Control.Monad        hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Map             as Map
import           Data.Tuple           as Tuple
import           Data.Maybe           as Maybe
import           Data.Either          as Either
import           Data.Aeson           (ToJSON, FromJSON)
import           Optics.Fold          as Fold
import           Data.Aeson           (ToJSON, FromJSON, parseJSON) --for redeemer?
import           GHC.Generics         (Generic) --for redeemer? 
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Plutus.Contract      as Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless, length)
import           Ledger               as Plutus
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import           Ledger.Value         as Value
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
--import           Ledger.Address       (PaymentPubKeyHash)
import           Plutus.V1.Ledger.Tx  as Tx
import           Plutus.V1.Ledger.Contexts as Contexts
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema) --ToSchema for redeemer?
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import qualified Prelude              as P
import           Text.Printf          (printf)
import           Plutus.Contract      
import           Plutus.Trace         as Trace
import           Schema               (ToSchema)
import           Wallet.Emulator.Wallet 

import           Week10.Charytoken    as Charytoken 
--2.Declare Variables: Datum that covers all relevant Data Structures: Donors, Projects, Donations, Tokenamount 
data DonationDatum = DonationDatum
    { initiator :: PaymentPubKeyHash
    , donors    :: [PaymentPubKeyHash]
    , projects  :: [(Integer, Integer)]
    , donations :: Integer 
    , ccamount  :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''DonationDatum --this or Lift? 
PlutusTx.makeLift ''DonationDatum

--5. Validator Type daclarations 
data Donating
instance Scripts.ValidatorTypes Donating where 
    type instance DatumType Donating = DonationDatum
    type instance RedeemerType Donating = ()

--4. Validators and other functions

{-# INLINABLE mkDonationValidator #-}
--Valdator that requires a DonationDatum
mkDonationValidator :: DonationDatum -> () -> ScriptContext -> Bool
mkDonationValidator dd () ctx = True

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx 

--6. Compile the Validator: Compiles to PlutusCore
typedDonationValidator :: Scripts.TypedValidator Donating
typedDonationValidator = Scripts.mkTypedValidator @Donating
    $$(PlutusTx.compile [|| mkDonationValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @DonationDatum @()

validator :: Validator
validator = Scripts.validatorScript typedDonationValidator

valHash :: Plutus.ValidatorHash
valHash = Scripts.validatorHash typedDonationValidator

scrAddress :: Plutus.Address
scrAddress = scriptAddress validator
