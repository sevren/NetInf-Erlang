#!/bin/sh

serverurl='http://localhost:9999/netinfproto/publish'
urlget='http://localhost:9999/netinfproto/get'

echo ==================================================================
echo PUBLISH request with an html file
echo ==================================================================

curl -X POST -H "Content-Type: multipart/form-data; boundary=------WebKitFormBoundaryjTwy4nYi2Aj6shCW" --data-binary @test.txt  $serverurl

echo ==================================================================
echo GET request
echo ==================================================================
curl -i --data "URI=ni:///sha-256;d748c318b3f2e63s7163693738b6b80bcf7fb4003f51d390c8367c8d32705df81&msgid=1337&ext=" $urlget



