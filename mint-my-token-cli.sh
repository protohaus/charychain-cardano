#transaction preparation and creation for minting with own minting file

ppFile=protocol-parameters.json
cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file $ppFile

policyFile=mypolicy.plutus
#cabal exec token-policy $policyFile $oref $amt $tn

unsignedFile=tx.unsigned
signedFile=tx.signed
pid=$(cardano-cli transaction policyid --script-file $policyFile)
tnHex=4368617279546f6b656e
addr=$(cat $addrFile)
v="$amt $pid.$tnHex"

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 1097911063 \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$addr + 1500000 lovelace + $v" \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file unitredeemer.json \
    --change-address $addr \
    --protocol-params-file $ppFile \
    --out-file $unsignedFile \

cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file $skeyFile \
    --testnet-magic 1097911063 \
    --out-file $signedFile

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file $signedFile
