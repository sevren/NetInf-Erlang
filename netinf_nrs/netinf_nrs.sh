#!/bin/bash

# ERNI Setup/start up script
# This script will setup and install all the required dependencies for the current stable version of the erlang
# Netinf System. 
#
# The script requires sudo for some parts of the script such as installing  unzip, make, libssl, riak, erlang, google-chrome, rebar 
# however starting the netinf nrs should NOT require sudo.
# Everything needed to setup the system correctly has been included below and has been tested to work on ubuntu 12.0.4 LTS
# ERNI Group can not gaurentee that the following links will still work after the project completion date: January 17th, 2013
# Should there be any problems using this script, please refer to the detailed instructions in the provided README file. 




# check if google chrome is installed
function checkchrome() {
dpkg -s "google-chrome-stable" >/dev/null 2>&1 && {
        echo "google-chrome web browser is installed."
    } || {
        echo "google-chrome-stable is not installed. - installing now please wait ..."
	wget https://dl.google.com/linux/direct/google-chrome-stable_current_i386.deb
	sudo dpkg -i google-chrome-stable_current_i386.deb
	rm google-chrome-stable_current_i386.deb
    }
}

# check if rebar is present in usr/bin
function checkrebar(){
    if [ -f /usr/bin/rebar ]
	then
	echo "Rebar is installed properly"
	else
	echo "Rebar not found, will install it to /usr/bin"
	wget https://github.com/basho/rebar/archive/master.zip
	unzip master.zip
	cd  rebar-master
	./bootstrap
	sudo cp rebar /usr/bin
	cd ..
	sudo rm -rf rebar-master
	rm master.zip	
    fi
}

# check  if erlang is installed
function checkerlang()
{
   dpkg -s "esl-erlang" >/dev/null 2>&1 && {
	echo "Erlang is installed. check for right version.."
	EVERSION=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell) 
	REQVER="\"R15B03\""
	if [[ $EVERSION == $REQVER* ]] 
	then
	    echo "Erlang is the correct version - R15B03"
	else
	    echo "wrong version is installed.. Installing new Version"
	    sudo apt-get -y remove erlang-*
	    wget https://elearning.erlang-solutions.com/couchdb//rbingen_adapter//package_R15B03_precise32_1354121173/esl-erlang_15.b.3-1~ubuntu~precise_i386.deb
	    sudo dpkg -i esl-erlang_15.b.3-1~ubuntu~precise_i386.deb
	    rm esl-erlang_15.b.3-1~ubuntu~precise_i386.deb
	fi
   } || {
	echo "Erlang is not installed. - installing now please wait ..."
	wget https://elearning.erlang-solutions.com/couchdb//rbingen_adapter//package_R15B03_precise32_1354121173/esl-erlang_15.b.3-1~ubuntu~precise_i386.deb
	sudo dpkg -i esl-erlang_15.b.3-1~ubuntu~precise_i386.deb
	rm esl-erlang_15.b.3-1~ubuntu~precise_i386.deb
   }
 
}

#check if unzip is installed
function checkunzip() {
dpkg -s "unzip" >/dev/null 2>&1 && {
        echo "unzip is installed."
    } || {
        echo "unzip is not installed. - installing now please wait ..."
	sudo apt-get -y install unzip
    }
}

#check if g++ is installed
function checkgcompiler() {
dpkg -s "g++" >/dev/null 2>&1 && {
        echo "g++ is installed."
    } || {
        echo "g++ is not installed. - installing now please wait ..."
	sudo apt-get -y install g++
    }
}

#check if make is installed
function checkmake() {
dpkg -s "make" >/dev/null 2>&1 && {
        echo "make is installed."
    } || {
        echo "make is not installed. - installing now please wait ..."
	sudo apt-get -y install build-essential
    }
}


# check if libssl0.9.8 is installed
function checklibssl() {
dpkg -s "libssl0.9.8" >/dev/null 2>&1 && {
        echo "libssl0.9.8 is installed."
    } || {
        echo "libssl0.9.8 is not installed. - installing now please wait ..."
	sudo apt-get -y install libssl0.9.8
    }
}

# check for riak_1.2.1-1_i386.deb install
function checkriak() {
dpkg -s "riak" >/dev/null 2>&1 && {
	echo "riak_1.2.1-1_i386.deb is installed."
   } || {
	echo "riak_1.2.1-1_i386.deb is not installed. - installing now please wait ..."
	wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/CURRENT/ubuntu/lucid/riak_1.2.1-1_i386.deb
	sudo dpkg -i riak_1.2.1-1_i386.deb
	}
}

# set riak's app.config search functionality to true.
function riaksearchtrue() {
sudo sed -in '/%% To enable Search functionality set this \x27true\x27./{n;h;g;};s#{enabled, false}#{enabled, true}#;' /etc/riak/app.config
}

# set riak's app.cofig search functionality to false.  
function riaksearchfalse {
sudo sed -in '/%% To enable Search functionality set this \x27true\x27./{n;h;g;};s#{enabled, true}#{enabled, false}#;' /etc/riak/app.config
}

function riaksetup() {
    echo "check lib ssl is installed"
    checklibssl
    echo "check riak is installed"
    checkriak
    echo "starting riak"
    riak start
    echo "setting search functionality to true in the /etc/riak/app.config"
    riaksearchtrue
    search-cmd install netinf_bucket
}


function start_netinfnrs()
{
    make all_no_test;
    erl -pa ebin deps/*/ebin -config configs/list -s netinf_nrs -eval "io:format(\"NetInf NRS is running ... ~n\")."
}

function start_netinfnrs_riak()
{
    riaksetup
    echo "building all required components"
    make all_no_test;
    echo "starting netinf_nrs with riak database.."
    erl -pa ebin deps/*/ebin -config configs/riak -s netinf_nrs -eval "io:format(\"NetInf NRS is running ... ~n\")." 
}

#checking whole system - assumes bare laptop
function checksystem()
{ 
   checkunzip
   checkmake
   checkgcompiler
   checkrebar
   checkerlang
   checkchrome 
}


function menu() {

    while [ "$OPTION" != "5" ]
    do
	echo "==================ERNI Netinf_nrs script========================="
	echo "Please select from the following options"
	echo "1. Start Netinf Nrs with the default list"
	echo "2. Start Netinf Nrs with riak"
	echo "3. Install and setup riak only"
	echo "4. Start Ericsson Demo"
	echo "5. Quit"
	read -r -p "Type a number to choose and option: " OPTION
	case $OPTION in
	    1)
		echo "starting netinf_nrs with default list"
		start_netinfnrs
		exit 0
		;;
	    2)
		echo "starting netinf_nrs with riak"
		start_netinfnrs_riak
		exit 0
		;;
	    3)
		echo "setting up riak"
		riaksetup
		exit 0
		;;
	    4)
		echo "starting Ericsson Demo - Please start google chrome and go to localhost:8079"
		checksystem
		start_netinfnrs
		exit 0
		;;
		
	    5)
		echo "exiting script"
		exit 0
		;;
	esac
    done
}
menu
