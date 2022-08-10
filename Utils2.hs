{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

module Week10.Utils
    ( tryReadAddress, unsafeReadAddress
    , tryReadWalletId, unsafeReadWalletId
    , unsafeReadTxOutRef
    , writeJSON, writeUnit, writeUnit2
    --, contractActivationArgs
    , getCredentials, unsafePaymentPubKeyHash, unsafeStakePubKeyHash
    , cidToString
    , writeMintingPolicy
    , unsafeTokenNameToHex
    , writeDonationRedeemer, writeCloseRedeemer, writeLotteryRedeemer, writeVotingRedeemer
    , writeDonationDatumJson
    , writeDonationsValidator
    , writeDonations2Validator, writeDonationDatum2Json
    ) where

import           Cardano.Api                 as API
import           Cardano.Api.Shelley         (Address (..), PlutusScript (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Codec.Serialise             (serialise)
import           Data.Aeson                  (decode, encode)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.String                 (IsString (..))
import           Data.Text                   (pack)
--import           Plutus.PAB.Webserver.Types  (ContractActivationArgs (..))
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import           Plutus.V1.Ledger.Value      (TokenName (..))
import           PlutusTx                    (Data (..))
import qualified PlutusTx
import           PlutusTx.Builtins           (toBuiltin)
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))
import qualified Ledger                      as Plutus
import           Wallet.Emulator.Wallet      (WalletId (..), Wallet (..))
import           Wallet.Types                (ContractInstanceId (..))

import           Week10.JustDonation       as MyContract
import           Week10.DonationsUR        as NoRedeemer

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

unsafeReadTxOutRef :: String -> Plutus.TxOutRef
unsafeReadTxOutRef s =
  let
    (x, _ : y) = span (/= '#') s
  in
    Plutus.TxOutRef
        { Plutus.txOutRefId  = fromString x
        , Plutus.txOutRefIdx = read y
        }

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

writeUnit2 :: FilePath -> IO ()
writeUnit2 outFilePath = writeJSON outFilePath ()

unsafePaymentPubKeyHash :: Plutus.Address -> Plutus.PaymentPubKeyHash
unsafePaymentPubKeyHash addr = maybe (error $ "script address " ++ show addr ++ " does not contain a payment key") fst $ getCredentials addr

unsafeStakePubKeyHash :: Plutus.Address -> Plutus.StakePubKeyHash
unsafeStakePubKeyHash addr = case getCredentials addr of
    Nothing           -> error $ "unexpected script address " ++ show addr
    Just (_, Nothing) -> error $ "addres " ++ show addr ++ " contains no stake component"
    Just (_, Just x)  -> x

--Helperfuntctions Minting

writeMintingPolicy :: FilePath -> Plutus.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getMintingPolicy


unsafeTokenNameToHex :: TokenName -> String
unsafeTokenNameToHex = BS8.unpack . serialiseToRawBytesHex . fromJust . deserialiseFromRawBytes AsAssetName . getByteString . unTokenName
  where
    getByteString (BuiltinByteString bs) = bs

--helperfunctionsRedeemer/Datum

writeDonationRedeemer :: FilePath -> String -> IO ()
writeDonationRedeemer outFilePath possibleDonor = writeJSON outFilePath r
    where r = Donation {
              MyContract.dDonor       = Plutus.PaymentPubKeyHash (fromString possibleDonor)}

writeVotingRedeemer :: FilePath -> String -> IO ()
writeVotingRedeemer outFilePath possibleVoter = writeJSON outFilePath r
    where r = Vote {
              MyContract.vVoter       = Plutus.PaymentPubKeyHash (fromString possibleVoter)}

writeLotteryRedeemer :: FilePath -> String -> IO ()
writeLotteryRedeemer outFilePath possibleWinner = writeJSON outFilePath r
    where r = Lottery {
              MyContract.dWinner       = Plutus.PaymentPubKeyHash (fromString possibleWinner)}

writeCloseRedeemer :: FilePath -> IO ()
writeCloseRedeemer outFilePath = writeJSON outFilePath ()

writeDonationDatumJson :: FilePath -> String -> IO ()
writeDonationDatumJson outFilePath thisDonor = writeJSON outFilePath d
    where d = MyContract.DonationDatum { MyContract.donors    = [Plutus.PaymentPubKeyHash (fromString thisDonor)]
                            , MyContract.donations = 2000000
                            , MyContract.initiator = Plutus.PaymentPubKeyHash (fromString "bda599fa1106e6c44a0019193aec1ced39880beceeb7980cdc85b0b7")
                            , MyContract.projects  = [(2,0),(4,0),(6,0),(8,0)]
                            , MyContract.ccamount = 50
                            }

writeDonationDatum2Json :: FilePath -> String -> IO ()
writeDonationDatum2Json outFilePath thisDonor = writeJSON outFilePath d
    where d = NoRedeemer.DonationDatum { NoRedeemer.donors    = [(Plutus.PaymentPubKeyHash (fromString "1dbb97effefdc1ad8899667ad1c27e9924a442b4e5c5b02d573f3097")),(Plutus.PaymentPubKeyHash (fromString thisDonor))]
                            , NoRedeemer.donations = 4000000
                            , NoRedeemer.initiator = Plutus.PaymentPubKeyHash (fromString "1dbb97effefdc1ad8899667ad1c27e9924a442b4e5c5b02d573f3097")
                            , NoRedeemer.projects  = [(2,1),(4,0),(6,0),(8,0)]
                            , NoRedeemer.ccamount = 50
                            }

--helperfunctions Validatorwrite

writeValidator :: FilePath -> Plutus.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.unValidatorScript

writeDonationsValidator :: IO (Either (FileError ()) ())
writeDonationsValidator = writeValidator "./donations.plutus" $ MyContract.validator

writeDonations2Validator :: IO (Either (FileError ()) ())
writeDonations2Validator = writeValidator "./donationsUR.plutus" $ NoRedeemer.validator
