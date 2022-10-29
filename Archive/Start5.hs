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

module Start4 where

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
minLovelace :: Integer 
minLovelace = 2000000

mytokenName :: TokenName 
mytokenName = "Chary Token"

--3.Data Type declarations 
data DonationDatum = DonationDatum
    { initiator :: PaymentPubKeyHash
    , donors    :: [PaymentPubKeyHash]
    , projects  :: [(Integer, Integer)]
    , donations :: Integer 
    , ccamount  :: Integer
    } deriving Show --projects als Liste und dann from list/to list für operationen`??

PlutusTx.unstableMakeIsData ''DonationDatum --this or Lift?  
--lif to make to plutus.core, try to get type and injecting haskell code to make it comparable with plutus core 
--PlutusTx.makeLift ''DonationDatum


--Parameters for the Endpoints 
data DonateParams1 = DonateParams1
    { dAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema) --when which deriving?

data DonateParams2 = DonateParams2
    { gAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data InitiatorParams = InitiatorParams
    { dprojects    :: ![(Integer, Integer)] 
    , tamount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data VotingParams = VotingParams
    { dvote :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data LotteryParams = LotteryParams
    { dlottery :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data MintParams = MintParams
    { mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
--4. Validators and other functions 

{-# INLINABLE firstValidator #-}
-- This should validate if and only if the two Booleans in the redeemer are equal!
firstValidator :: DonationDatum -> () -> ScriptContext -> Bool
firstValidator (DonationDatum i d p m c) () ctx = True --traceIfFalse "only initiator can get donations" 
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx 

--always need these validators? 
validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

{-# INLINABLE myDatum #-}
myDatum :: Maybe Datum -> Maybe DonationDatum
myDatum md = do 
    Datum d <- md 
    PlutusTx.fromBuiltinData d

--5. Validator Type daclarations 

data Typed
instance Scripts.ValidatorTypes Typed where 
    type instance DatumType Typed = DonationDatum
    type instance RedeemerType Typed = ()

--6. Compile the Validator 

typedValidator :: Scripts.TypedValidator Typed 
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| firstValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @DonationDatum @()

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
type StartSchema =
            Endpoint "initiate" InitiatorParams
        .\/ Endpoint "donate" DonateParams1
        .\/ Endpoint "getdonation" DonateParams2
        .\/ Endpoint "vote" VotingParams
        .\/ Endpoint "dolottery" LotteryParams
        .\/ Endpoint "mymint" MintParams 

--8. Endpoints logic 
initiate :: AsContractError e => InitiatorParams -> Contract w s e ()
initiate i = do 
    pkh <- ownPaymentPubKeyHash
    let owner = DonationDatum
                { initiator = pkh
                , projects = dprojects i
                , donations = 0
                , donors = []
                , ccamount = tamount i 
                }
        thiscs = cc pkh 
        v = (Value.singleton thiscs mytokenName (tamount i)) <> Ada.lovelaceValueOf minLovelace
        tx = Constraints.mustPayToTheScript owner $ v
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo $ "initiated Donations by" <> show pkh 
    Contract.logInfo $ "this currencysymbol" <> show thiscs

mymint :: MintParams -> Contract w StartSchema Text ()
mymint mp = do
    owner <- Contract.ownPaymentPubKeyHash
    let val     = Value.singleton (cc owner) (mytokenName) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy owner
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

donate :: AsContractError e => DonateParams1 -> Contract w s e ()
donate d = do
    utxos <- utxosAt scrAddress
    dpkh <- ownPaymentPubKeyHash
    let thispkh = getInitiator utxos
    let thisdonation = getDonations utxos
        alldonation = thisdonation + (dAmount d) 
    let thisdonors = updateDonors utxos dpkh
    let thisprojects = getProjects utxos 
    let thistokens = getCCamount utxos
        minustoken = thistokens - 1
    let thiscs = cc thispkh
    let don = DonationDatum { initiator = thispkh
                            , projects = thisprojects
                            , donors = thisdonors
                            , donations = alldonation
                            , ccamount = minustoken }
    let oref = Tuple.fst <$> Map.toList utxos
        orefs = oref !! 0
        lookups = Constraints.typedValidatorLookups typedValidator <>
                  Constraints.otherScript validator <>
                  Constraints.unspentOutputs utxos   
        v =       (Value.singleton thiscs mytokenName minustoken) <> (Ada.lovelaceValueOf $ alldonation) <> Ada.lovelaceValueOf minLovelace
        v2 =      Value.singleton thiscs mytokenName 1 <> Ada.lovelaceValueOf minLovelace
        cons =    Constraints.mustPayToTheScript don $ v
        cons3 =   Constraints.mustPayToPubKey dpkh $ v2
        cons2 =   Constraints.mustSpendScriptOutput orefs unitRedeemer
        tx =      cons <> cons2 <> cons3 
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo $ "locking" <> show (alldonation)
    Contract.logInfo $ "current Donors" <> show (thisdonors)

dolottery :: forall w s e. AsContractError e => LotteryParams -> Contract w s e ()
dolottery l = do
    utxos <- utxosAt scrAddress
    pkh <- ownPaymentPubKeyHash
    let thispkh = getInitiator utxos
    let thisdonation = getDonations utxos
    let thisdonors = getDonors utxos 
    let thisprojects = getProjects utxos 
    let thistokens = getCCamount utxos
    let thiscs = cc thispkh
    let lotterywinner = thisdonors !! (dlottery l)
    let lotteryamount1 = thisdonation `div` 100
        lotteryamount2 = lotteryamount1 * 10
        newdonations = thisdonation - lotteryamount2
    if pkh /= thispkh 
        then Contract.logInfo $ "not initiator" <> show thispkh
        else do
            if Map.null utxos
                then Contract.logInfo @String $ "no lottery available"
                else do 
                    let don = DonationDatum { initiator = thispkh
                                            , projects = thisprojects
                                            , donors = thisdonors
                                            , donations = newdonations
                                            , ccamount = thistokens }
                    let oref = Tuple.fst <$> Map.toList utxos
                        orefs = oref !! 0
                        lookups = Constraints.typedValidatorLookups typedValidator <>
                                  Constraints.otherScript validator <>
                                  Constraints.unspentOutputs utxos   
                        v =  (Value.singleton thiscs mytokenName thistokens) <> (Ada.lovelaceValueOf $ newdonations) <> Ada.lovelaceValueOf minLovelace
                        v2 = Ada.lovelaceValueOf lotteryamount2 <> Ada.lovelaceValueOf minLovelace
                        cons3 = Constraints.mustPayToPubKey lotterywinner $ v2
                        cons = Constraints.mustPayToTheScript don $ v
                        cons2 = Constraints.mustSpendScriptOutput orefs unitRedeemer
                        tx = cons <> cons3 <> cons2
                    ledgerTx <- submitTxConstraintsWith lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    Contract.logInfo $ "donors" <> show (thisdonors)
                    Contract.logInfo $ "lottery winner" <> show (lotterywinner) 
                    Contract.logInfo $ "lotteryamount" <> show (lotteryamount2) 
                    Contract.logInfo $ "previous Donations" <> show (thisdonation) 

vote :: AsContractError e => VotingParams -> Contract w s e () 
vote v = do 
    pkh <- ownPaymentPubKeyHash
    utxos <- utxosAt scrAddress
    let thispkh = getInitiator utxos
    let thisdonation = getDonations utxos
    let thisdonors = getDonors utxos 
    let thisprojects = getProjects utxos 
    let thistokens = getCCamount utxos
        minustoken = thistokens + 1
    let thiscs = cc thispkh
    let projectid = dvote v
    if pkh `elem` thisdonors == False 
        then Contract.logInfo $ "not part of donors" <> show (pkh)
        else do 
            let listtomap = Map.fromList thisprojects
            if Map.member projectid listtomap == False
                then Contract.logInfo $ "not valid project" <> show (projectid)
                else do 
                    let currentvalue = listtomap ! projectid
                        updatedvalue = currentvalue + 1
                        updatedprojects = Map.insert projectid updatedvalue listtomap
                        updatedprojects2 = Map.toList updatedprojects
                        don = DonationDatum { initiator = thispkh
                                            , projects = updatedprojects2
                                            , donors = thisdonors
                                            , donations = thisdonation
                                            , ccamount = minustoken}
                    let oref = Tuple.fst <$> Map.toList utxos
                        orefs = oref !! 0
                        va = (Value.singleton thiscs mytokenName minustoken) <> (Ada.lovelaceValueOf $ thisdonation) <> Ada.lovelaceValueOf minLovelace
                        lookups = Constraints.typedValidatorLookups typedValidator <>
                                  Constraints.otherScript validator <>
                                  Constraints.unspentOutputs utxos  
                        cons = Constraints.mustPayToTheScript don $ va --was mit minlovelace?
                        cons2 = Constraints.mustSpendScriptOutput orefs unitRedeemer
                        tx = cons <> cons2
                    ledgerTx <- submitTxConstraintsWith lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    Contract.logInfo $ "updated vote for projectid" <> show (projectid)
                    Contract.logInfo $ "updated vote" <> show (updatedprojects2)

getdonation :: forall w s e. AsContractError e => DonateParams2 -> Contract w s e () 
getdonation gd = do 
    pkh <- ownPaymentPubKeyHash
    utxos <- utxosAt scrAddress
    let thispkh = getInitiator utxos
    let thisdonation = getDonations utxos
    let thisdonors = getDonors utxos 
    let thisprojects = getProjects utxos 
    let thistokens = getCCamount utxos
    let thiscs = cc thispkh
    if pkh /= thispkh 
        then Contract.logInfo $ "not initiator" <> show thispkh
        else do
            if Map.null utxos
                then Contract.logInfo @String $ "no gifts available"
                else do 
                    let winnerprojects = getVotingWinner thisprojects
                    let citxos = Tuple.fst <$> Map.toList utxos
                    let citxo = citxos !! 0
                    let remainingdonations = thisdonation - (gAmount gd)
                        vcontract = (Value.singleton thiscs mytokenName thistokens) <> (Ada.lovelaceValueOf remainingdonations) <> Ada.lovelaceValueOf minLovelace
                        vpkh = Ada.lovelaceValueOf $ gAmount gd --must be smaller than all donations
                        don = DonationDatum { initiator = thispkh
                                            , projects = thisprojects
                                            , donors = thisdonors
                                            , donations = remainingdonations
                                            , ccamount = thistokens}
                        lookups = Constraints.unspentOutputs utxos    <> 
                                  Constraints.otherScript validator    <>
                                  Constraints.typedValidatorLookups typedValidator
                        cons2   = Constraints.mustPayToPubKey pkh $ vpkh
                        cons3   = Constraints.mustPayToTheScript don $ vcontract
                        cons    = Constraints.mustSpendScriptOutput citxo unitRedeemer
                        tx      = cons2 <> cons3 <> cons  
                    ledgerTx <- submitTxConstraintsWith lookups tx  
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    Contract.logInfo $ "collected donations" <> show (gAmount gd) 
                    Contract.logInfo $ "winner projectids" <> show (winnerprojects) 
                    


--Helperfunctions:
getInitiator :: Map TxOutRef ChainIndexTxOut -> PaymentPubKeyHash
getInitiator i = 
    let cpkh = Tuple.snd <$> Map.toList i
        cpkh2 = cpkh !! 0
        pkhdatum = myDatum $ PlutusTx.Prelude.either (const Nothing) Just $ _ciTxOutDatum cpkh2
        thispkh = initiator (Maybe.fromJust pkhdatum)
    in 
        thispkh

getDonations :: Map TxOutRef ChainIndexTxOut -> Integer
getDonations i = 
    let cpkh = Tuple.snd <$> Map.toList i
        cpkh2 = cpkh !! 0
        pkhdatum = myDatum $ PlutusTx.Prelude.either (const Nothing) Just $ _ciTxOutDatum cpkh2
        thisd = donations (Maybe.fromJust pkhdatum)
    in 
        thisd

getCCamount :: Map TxOutRef ChainIndexTxOut -> Integer
getCCamount i = 
    let cpkh = Tuple.snd <$> Map.toList i
        cpkh2 = cpkh !! 0
        ccdatum = myDatum $ PlutusTx.Prelude.either (const Nothing) Just $ _ciTxOutDatum cpkh2
        thisd = ccamount (Maybe.fromJust ccdatum)
    in 
        thisd

updateDonors :: Map TxOutRef ChainIndexTxOut -> PaymentPubKeyHash -> [PaymentPubKeyHash]
updateDonors i d = 
    let cpkh = Tuple.snd <$> Map.toList i
        cpkh2 = cpkh !! 0
        pkhdatum = myDatum $ PlutusTx.Prelude.either (const Nothing) Just $ _ciTxOutDatum cpkh2
        thisd = donors (Maybe.fromJust pkhdatum) 
        thisd2 = d : thisd 
    in 
        thisd2

getDonors :: Map TxOutRef ChainIndexTxOut -> [PaymentPubKeyHash]
getDonors i = 
    let cpkh = Tuple.snd <$> Map.toList i
        cpkh2 = cpkh !! 0
        pkhdatum = myDatum $ PlutusTx.Prelude.either (const Nothing) Just $ _ciTxOutDatum cpkh2
        thisd = donors (Maybe.fromJust pkhdatum) 
    in
        thisd

getProjects ::  Map TxOutRef ChainIndexTxOut -> [(Integer, Integer)]
getProjects p =
    let cpkh = Tuple.snd <$> Map.toList p
        cpkh2 = cpkh !! 0
        pdatum = myDatum $ PlutusTx.Prelude.either (const Nothing) Just $ _ciTxOutDatum cpkh2
        thisp = projects (Maybe.fromJust pdatum) 
    in
        thisp
    
getVotingWinner :: [(Integer, Integer)] -> [(Integer, Integer)]
getVotingWinner w =
    let winnermap = Map.fromList w
        votes = Map.elems winnermap
        maxvotes = maximum votes
        winnerids = Map.filter (== maxvotes) winnermap
        winneridlist = Map.toList winnerids
    in
        winneridlist    


--9. Endpoints 
endpoints :: Contract () StartSchema Text ()
endpoints = awaitPromise (initiate' `select` donate' `select` getdonation' `select` vote' `select` dolottery' `select` mymint') >> endpoints
  where
    initiate' = endpoint @"initiate" initiate
    donate' = endpoint @"donate" donate
    getdonation' = endpoint @"getdonation" getdonation
    vote' = endpoint @"vote" vote 
    dolottery' = endpoint @"dolottery" $ dolottery
    mymint' = endpoint @"mymint" mymint 

--10.Schema Definitions 

mkSchemaDefinitions ''StartSchema
--11. mkKnownCurrencies 

mkKnownCurrencies []


test4 :: IO ()
test4 = Trace.runEmulatorTraceIO myTrace 

myTrace :: EmulatorTrace ()
myTrace = do 
    h1 <- activateContractWallet (knownWallet 1) endpoints 
    h2 <- activateContractWallet (knownWallet 2) endpoints 
    h3 <- activateContractWallet (knownWallet 3) endpoints 
    h4 <- activateContractWallet (knownWallet 4) endpoints 
    callEndpoint @"mymint" h1 $ MintParams
        { mpAmount = 500 }
    void $ waitUntilSlot 5
    callEndpoint @"initiate" h1 $ InitiatorParams
        { dprojects = [(1,0),(2,0),(3,0),(4,0)]
        , tamount = 500 }
    void $ waitUntilSlot 10
    callEndpoint @"donate" h2 $ DonateParams1
        { dAmount = 6000000}
    void $ waitUntilSlot 15
    callEndpoint @"donate" h1 $ DonateParams1
        { dAmount = 5000000 }
    void $ waitUntilSlot 20
    callEndpoint @"donate" h3 $ DonateParams1
        { dAmount = 3000000 }
    void $ waitUntilSlot 25
    callEndpoint @"donate" h4 $ DonateParams1
        { dAmount = 2000000 }
    void $ waitUntilSlot 30
    callEndpoint @"vote" h2 $ VotingParams
        { dvote = 1 } 
    void $ waitUntilSlot 35
    callEndpoint @"vote" h1 $ VotingParams
        { dvote = 1 } 
    void $ waitUntilSlot 40
    callEndpoint @"vote" h3 $ VotingParams
        { dvote = 2 } 
    void $ waitUntilSlot 45
    callEndpoint @"vote" h4 $ VotingParams
        { dvote = 1 } 
    void $ waitUntilSlot 50
    callEndpoint @"dolottery" h1 $ LotteryParams
        { dlottery = 2 }
    void $ waitUntilSlot 55
    callEndpoint @"getdonation" h1 $ DonateParams2
        { gAmount = 10000000} 
    s <- Trace.waitNSlots 4
    Extras.logInfo $ "reached" ++ show s 
    
test5 :: IO ()
test5 = Trace.runEmulatorTraceIO myTrace2 

myTrace2 :: EmulatorTrace ()
myTrace2 = do  
    h1 <- activateContractWallet (knownWallet 1) endpoints 
    h2 <- activateContractWallet (knownWallet 2) endpoints 
    callEndpoint @"mymint" h1 $ MintParams
        { mpAmount = 500 }
    void $ waitUntilSlot 5
    callEndpoint @"initiate" h1 $ InitiatorParams
        { dprojects = [(1,0),(2,0),(3,0),(4,0)]
        , tamount = 500 }
    void $ waitUntilSlot 10
    callEndpoint @"donate" h2 $ DonateParams1
        { dAmount = 6000000}
    void $ waitUntilSlot 15
    callEndpoint @"donate" h1 $ DonateParams1
        { dAmount = 5000000 }
    s <- Trace.waitNSlots 4
    Extras.logInfo $ "reached" ++ show s 
