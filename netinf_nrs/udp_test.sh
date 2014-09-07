#!/bin/bash

# Copyright: Ericsson, Uppsala University 2013
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
# 
#      http://www.apache.org/licenses/LICENSE-2.0
# 
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#  
#  Uppsala University
# 
#  Project CS course, Fall 2012
# 
#  Projekt DV/Project CS, is a course in which the students develop software for
#  distributed systems. The aim of the course is to give insights into how a big
#  project is run (from planning to realization), how to construct a complex
#  distributed system and to give hands-on experience on modern construction
#  principles and programming methods.

# ERNI udp testing script.
# This script was created to test the udp functionality of the NRS.
# Tests include:  UDP GET and UDP SEARCH
# Based off of the netinf protocol draft file draft-kutscher-icnrg-netinf-proto-00 (Expiry: April 7th, 2013)
# No gaurentee to comply with future drafts.



#setup global variables
serverurl='http://localhost:9999/netinfproto/publish'
urlget='http://localhost:9999/netinfproto/get'
urlsearch='http://localhost:9999/netinfproto/search'

#check if curl is installed
function checkcurl() 
{
    dpkg -s "curl" >/dev/null 2>&1 && {
        echo -e "curl is installed."
    } || {
        echo -e "curl is not installed. - installing now please wait ..."
	sudo apt-get -y install curl
    }
}


function startnrs()
{
    make all_no_test
    if [ $1 == "list" ]
	then
	bash -c "erl -pa ebin deps/*/ebin -config configs/list -eval 'netinf_nrs:start().'  -sname udp_test -detached" 
	else
	bash -c "erl -pa ebin deps/*/ebin -config configs/riak -eval 'netinf_nrs:start().'  -sname udp_test -detached" 
    fi
    
}

function publish()
{
    echo -e " =================================================================="
    echo -e " PUBLISH request with an html file to Server udp_test"
    echo -e " =================================================================="
    
    curl -X POST -H "Content-Type: multipart/form-data; boundary=------WebKitFormBoundaryjTwy4nYi2Aj6shCW" --data-binary @curludp/test.txt  $serverurl

}

function get()
{
    echo -e "=================================================================="
    echo -e "GET request"
    echo -e "=================================================================="
    curl -i --data "URI=ni:///sha-256;d748c318b3f2e63s7163693738b6b80bcf7fb4003f51d390c8367c8d32705df81&msgid=1345&ext=" $urlget
}

function search()
{
    echo -e "=================================================================="
    echo -e "SEARCH request"
    echo -e "=================================================================="
    curl -i --data "msgid=1260&tokens=banana&ext=&TTL=1" $urlsearch   
}


function udplist() 
{
    echo -e "checking for curl"
    checkcurl
    echo -e "starting the nrs in the background"
    startnrs "list"
    sleep 2
    echo -e "publishing to our NRS"
    publish
    echo -e "\nPlease make sure the other host has an NRS running"
    echo -e "run this script agian on the other computer and press 3"
}

function udplistget() 
{
    echo -e "checking for curl"
    checkcurl
    echo -e "starting the nrs in the background"
    startnrs "list"
    sleep 2
    echo -e "sending http get to our NRS\n"
    get
    echo -e "should see Http 203 below"
}

function udpriak() 
{
    echo -e "checking for curl"
    checkcurl
    echo -e "starting the nrs in the background"
    startnrs "riak"
    sleep 2
    echo -e "publishing to our NRS"
    publish
    echo -e "\nPlease make sure the other host has an NRS running"
    echo -e "run this script agian on the other computer and press 4"
    
}

function udpriakget() 
{
    echo -e "checking for curl"
    checkcurl
    echo -e "starting the nrs in the background"
    startnrs "riak"
    sleep 2
    echo -e "sending http get to our NRS\n"
    get
    echo -e "should see Http 203 below"
    
}

function menu() 
{
    while [ "$OPTION" != "6" ]
    do
	echo -e "==================ERNI Udp testing script========================="
	echo -e "Please select from the following options"
	echo -e "1. Start Netinf Nrs UDP test with list database"
	echo -e "2. Start Netinf Nrs UDP test with riak database"
	echo -e "3. Start Netinf Nrs UDP GET test with list"
	echo -e "4. Start Netinf Nrs UDP GET test with riak"
	echo -e "5. Quit"
	read -r -p "Type a number to choose and option: " OPTION
	case $OPTION in
	    1)
		echo -e "starting udp test PUBLISH & netinf_nrs with list database"
		udplist
		read
		pkill -f "beam.smp.*-sname udp_test"
		exit 0
		;;
	    2)
		echo -e "starting udp test PUBLISH & netinf_nrs with riak database"
		udpriak
		read
		pkill -f "beam.smp.*-sname udp_test"
		exit 0
		;;
	    3)
		echo -e "starting udp test GET & netinf_nrs with list database"
		udplistget
		read
		pkill -f "beam.smp.*-sname udp_test"
		exit 0;
		;;
	     4)
		echo -e "starting udp test GET & netinf_nrs with riak database" 
		udpriakget
		pkill -f "beam.smp.*-sname udp_test"
		exit 0;
		;;
		
	    5)
		echo -e "exiting script"
		exit 0
		;;
	esac
    done
}
menu
