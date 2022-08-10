--first donate/get donations 

--1.Imports 
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

module MA.Charytoken (policy) where

import           Control.Monad        hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Map             as Map
import           Data.Tuple           as Tuple
import           Data.Maybe           as Maybe
import           Data.Either          as Either
import           Optics.Fold          as Fold
import           System.Random        as R
import           System.IO.Unsafe     as U
import           Data.Aeson           (ToJSON, FromJSON, parseJSON) --for redeemer?
import           GHC.Generics         (Generic) --for redeemer? 
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Plutus.Contract      as Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless, length)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import           Ledger.Value         as Value
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
--import           Ledger.Address       (PaymentPubKeyHash)
import           Plutus.V1.Ledger.Tx  as Tx
import           Plutus.V1.Ledger.Contexts as Contexts
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema) --ToSchema for redeemer?
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String, subtract, maximum, length, toInteger, div)
import           Text.Printf          (printf)
import           Plutus.Trace         as Trace
import           Wallet.Emulator.Wallet 
--2.Declare Variables

mytokenName :: TokenName 
mytokenName = "Chary Token"

--Parameters for the Endpoints 

data MintParams = MintParams
    { mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
--4. Validators and other functions 

--5. Validator Type daclarations 

--6. Compile the Validator 

--Minting Policy 
{-# INLINABLE mkPolicy #-}
mkPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPolicy owner () ctx = txSignedBy (scriptContextTxInfo ctx) $ unPaymentPubKeyHash owner 


policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy owner = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode owner


cc :: PaymentPubKeyHash -> CurrencySymbol
cc = scriptCurrencySymbol . policy


--7. Schema endpoints 
type FreeSchema = Endpoint "mymint" MintParams 

--8. Endpoints logic 

mymint :: MintParams -> Contract w FreeSchema Text ()
mymint mp = do
    owner <- Contract.ownPaymentPubKeyHash
    let val     = Value.singleton (cc owner) (mytokenName) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy owner
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

--9. Endpoints 
endpoints :: Contract () FreeSchema Text ()
endpoints = mymint' >> endpoints
  where
    mymint' = awaitPromise $ endpoint @"mymint" mymint 

--10.Schema Definitions 

mkSchemaDefinitions ''FreeSchema
--11. mkKnownCurrencies 

mkKnownCurrencies []

test::IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    --h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"mymint" h1 $ MintParams 
        { mpAmount = 500 }
    void $ waitUntilSlot 5
    callEndpoint @"mymint" h1 $ MintParams
        { mpAmount    = -200 }
    s <- Trace.waitNSlots 4
    Extras.logInfo $ "reached" ++ show s 
