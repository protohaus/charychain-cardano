#Redeem Datum file with transaction and send new Datum file
cardano-cli query protocol-parameters \
--testnet-magic 1097911063 \
--out-file protocol-parameters.json

ppFile=protocol-parameters.json
cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file $ppFile

addrsender=addr_test1vqwmh9l0lm7urtvgn9n845wz06vjffzzknjutvpd2ulnp9cs7kxks //second address 
amt=1
amtdonation=2000000
txin1=9a0e6acd81714057b399870b41a762167f8364a146da5310ede6f470841bda85#1 //fromaddr
txin2=f445e63fd78196e58718f8b715ef1bd14da5bf91577d04c59f55b3b7c48f3df2#2 //fromscript
tnHex=4368617279546f6b656e
addrereciever=addr_test1wpzg70pnd6k85mqjz4nasulhggv396k9wq629w6n928vcrcszcd3w //scriptfile

//txindatum -> initiatorDatum
txindatum=initiatorDatum.json

policyFile=donations.plutus
pid=$(cardano-cli transaction policyid --script-file $policyFile)

//get paymentpubkeyhash from addr
cardano-cli address key-hash --payment-verification-key-file 02.vkey --out-file 02.pkh

//build datum .json file -> helperfunction writeDonationDatum1Json
donateDatum=donateDatum.json

//build redeemer.json file -> helperfunction writeDonationredeemer 
donateRedeemer=donateRedeemer.json

//txout for sender 
txout1="addr_test1vqwmh9l0lm7urtvgn9n845wz06vjffzzknjutvpd2ulnp9cs7kxks + 3000000 lovelace + 0 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"
//txout for script address 
txout2="addr_test1wrt6rrg0e89xmxln838uvawcm6djw7u0fpezw9rfvdcdelg9aflru + 3000000 lovelace + 50 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"


cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1097911063 \
  --change-address $addrsender \
  --tx-in $txin2 \
  --tx-in-script-file $policyFile \
  --tx-in-datum-file $txindatum \
  --tx-in-redeemer-file $donateRedeemer \
  --tx-in-collateral f445e63fd78196e58718f8b715ef1bd14da5bf91577d04c59f55b3b7c48f3df2#0 \
  --tx-out "$txout1" \
  --tx-out "$txout2" \
  --protocol-params-file $ppFile \
  --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 02.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed

txin4=f445e63fd78196e58718f8b715ef1bd14da5bf91577d04c59f55b3b7c48f3df2#0 
txin5=f445e63fd78196e58718f8b715ef1bd14da5bf91577d04c59f55b3b7c48f3df2#1

txout4="addr_test1vqwmh9l0lm7urtvgn9n845wz06vjffzzknjutvpd2ulnp9cs7kxks + 3000000 lovelace + 190 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"
txout5="addr_test1wrt6rrg0e89xmxln838uvawcm6djw7u0fpezw9rfvdcdelg9aflru + 10000000 lovelace + 10 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"

donationdatum=noRedeemerDatum2.json

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1097911063 \
  --change-address $addrsender \
  --tx-in $txin4 \
  --tx-in $txin5 \
  --tx-out "$txout4" \
  --tx-out "$txout5" \
  --tx-out-datum-hash-file $donationdatum \
  --out-file tx.body

\\from address01 redeem and get 1 ct -> send new datum 
addrsender=addr_test1vz76tx06zyrwd3z2qqv3jwhvrnknnzqtanht0xqvmjzmpdcuhc02g
txin1=62977954f8d8cb2cd1a7dc5eaa1bb690b5e6fef8fc050540e443080521a13067#2
policyFile=donationsUR.plutus
txindatum=noRedeemerDatum2.json
donateRedeemer=unitredeemer.json
txout1="addr_test1vz76tx06zyrwd3z2qqv3jwhvrnknnzqtanht0xqvmjzmpdcuhc02g + 2000000 lovelace + 1 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"
txout2="addr_test1wrt6rrg0e89xmxln838uvawcm6djw7u0fpezw9rfvdcdelg9aflru + 6000000 lovelace + 9 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1097911063 \
  --change-address $addrsender \
  --tx-in $txin1 \
  --tx-in-script-file $policyFile \
  --tx-in-datum-file $txindatum \
  --tx-in-redeemer-file $donateRedeemer \
  --tx-in-collateral 34830b5313035a407c6a3b9d3e1921a8d7985546ed1259d9c59c23a0633fa099#0 \
  --tx-out "$txout1" \
  --tx-out "$txout2" \
  --protocol-params-file $ppFile \
  --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 01.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed

txin4=998ca23ea68540d1a5e7351ea97220f18b919f1baeacfb68e5a91d3da05802fa#1 
txin5=c7ab3b5187ef711b2a15eb63215b5d26012afcd7fb091a5f54e5ca058e9b3d05#1

txout4="addr_test1vz76tx06zyrwd3z2qqv3jwhvrnknnzqtanht0xqvmjzmpdcuhc02g + 3000000 lovelace + 40 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"
txout5="addr_test1wrt6rrg0e89xmxln838uvawcm6djw7u0fpezw9rfvdcdelg9aflru + 10000000 lovelace + 10 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"

donationdatum=noRedeemerDatum3.json

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1097911063 \
  --change-address $addrsender \
  --tx-in $txin4 \
  --tx-in $txin5 \
  --tx-out "$txout4" \
  --tx-out "$txout5" \
  --tx-out-datum-hash-file $donationdatum \
  --out-file tx.body
