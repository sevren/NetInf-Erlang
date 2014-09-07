#!/bin/sh

serverurl='http://localhost:9999/netinfproto/publish'
urlget='http://localhost:9999/netinfproto/get'
urlsearch='http://localhost:9999/netinfproto/search'

echo""
echo ==================================================================
echo SEARCH request
echo ==================================================================
curl -i --data "msgid=1260&tokens=banana&ext=&TTL=1" $urlsearch



