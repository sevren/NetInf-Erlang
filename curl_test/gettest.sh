#!/bin/sh

urlget='http://localhost:9999/netinfproto/get'


echo ==================================================================
echo GET request
echo ==================================================================
curl -i --data "URI=ni:///sha-256;d748c318b3f2e63s7163693738b6b80bcf7fb4003f51d390c8367c8d32705df81&msgid=1345&ext=" $urlget


