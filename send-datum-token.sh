#transaction preparation and execution to send a Datum with fungible token to a script address
cardano-cli query protocol-parameters \
--testnet-magic 1097911063 \
--out-file protocol-parameters.json

addrsender=addr_test1vz76tx06zyrwd3z2qqv3jwhvrnknnzqtanht0xqvmjzmpdcuhc02g
amt=100
txin1=9a0e6acd81714057b399870b41a762167f8364a146da5310ede6f470841bda85#0
txin2=9cdf7b159a0858c50be69cd60e116cfe10c80ce674d8ca53bbced7e34e792093#1
tnHex=4368617279546f6b656e

policyFile=donations.plutus
pid=$(cardano-cli transaction policyid --script-file $policyFile)

//build script file -> helperfunction writeDonationsValidator 
cardano-cli address build-script --script-file donations.plutus --testnet-magic 1097911063 --out-file donations1.addr

//build datum .json file -> helperfunction writeDonationDatum1Json
initiatedatum=initiatorDatum.json
//no redeemer required as sending to script without consuming any utxo 

//txout for sender 
txout1="addr_test1vqwmh9l0lm7urtvgn9n845wz06vjffzzknjutvpd2ulnp9cs7kxks + 3000000 lovelace + 200 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"
//txout for script address 
txout2="addr_test1wrt6rrg0e89xmxln838uvawcm6djw7u0fpezw9rfvdcdelg9aflru + 10000000 lovelace + 50 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1097911063 \
  --change-address $addrsender \
  --tx-in $txin1 \
  --tx-in $txin2 \
  --tx-out "$txout1" \
  --tx-out "$txout2" \
  --tx-out-datum-hash-file $initiatedatum \
  --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 02.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed
