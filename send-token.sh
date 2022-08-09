//sending 200CT to second address 

cardano-cli query protocol-parameters \
--testnet-magic 1097911063 \
--out-file protocol-parameters.json

//transaction native asset

addrsender=addr_test1vz76tx06zyrwd3z2qqv3jwhvrnknnzqtanht0xqvmjzmpdcuhc02g
addrereciever=addr_test1vqwmh9l0lm7urtvgn9n845wz06vjffzzknjutvpd2ulnp9cs7kxks
amt=200
txin1=2886e90c36816d8583ca596f31f5e430cd6b5f26d3104b02ba5c9a8377040164#1
txin2=2886e90c36816d8583ca596f31f5e430cd6b5f26d3104b02ba5c9a8377040164#0
tnHex=4368617279546f6b656e

ppFile=protocol-parameters.json
cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file $ppFile

policyFile=mypolicy.plutus
pid=$(cardano-cli transaction policyid --script-file $policyFile)

v="$amt $pid.$tnHex"

txout=$addrereciever + $v
txout1="addr_test1vqwmh9l0lm7urtvgn9n845wz06vjffzzknjutvpd2ulnp9cs7kxks + 2000000 lovelace + 300 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"
txout2="addr_test1vz76tx06zyrwd3z2qqv3jwhvrnknnzqtanht0xqvmjzmpdcuhc02g + 983767424 lovelace + 200 1224e5210e86ce3a4c3c49467e065cb129e3b54c0ed57dc3a452e411.4368617279546f6b656e"


cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1097911063 \
  --change-address $addrsender \
  --tx-in $txin3 \
  --tx-out "$txout3" \
  --tx-out "$txout4" \
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


\\send lovelace 
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1097911063 \
  --change-address addr_test1vqwmh9l0lm7urtvgn9n845wz06vjffzzknjutvpd2ulnp9cs7kxks \
  --tx-in $txin3 \
  --tx-out "$txout3" \
  --protocol-params-file $ppFile \
  --out-file tx.body


txin3=c7ab3b5187ef711b2a15eb63215b5d26012afcd7fb091a5f54e5ca058e9b3d05#0
txout3="addr_test1wrt6rrg0e89xmxln838uvawcm6djw7u0fpezw9rfvdcdelg9aflru + 10000000 lovelace"
