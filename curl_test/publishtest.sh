#!/bin/sh

serverurl='http://localhost:9999/netinfproto/publish'
urlget='http://localhost:9999/netinfproto/get'
urlsearch='http://localhost:9999/netinfproto/search'

echo""
echo ==================================================================
echo PUBLISH request with an html file
echo ==================================================================

curl -X POST -H "Content-Type: multipart/form-data; boundary=------WebKitFormBoundaryjTwy4nYi2Aj6shCW" --data-binary @test.txt  $serverurl

