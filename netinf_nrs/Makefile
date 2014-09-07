########################## MAKEFILE ##############################

BIN_SEARCH_PATHS = -pa ebin deps/*/ebin
MUTE_LOGGER = -kernel error_logger silent
CONFIG_DIR = configs/


all: set_env_folders compile makec eunit
all_no_test: set_env_folders compile makec

eunit: 
	rebar eunit skip_deps=true

integration_test: compile
	erl $(BIN_SEARCH_PATHS) -config $(CONFIG_DIR)list -eval "eunit:test(nn_integration_test)."
integration_test_riak: compile
	erl $(BIN_SEARCH_PATHS) -config $(CONFIG_DIR)riak -eval "eunit:test(nn_integration_test)."

makec:
	make -C deps/json

set_env_folders: 
	rm -rf logs
	rm -rf files
	mkdir files
	mkdir logs
	mkdir logs/old 


compile: #logs logs/old rebar.config src/
	if test -d deps/; then rebar  compile; else rebar  get-deps compile; fi

compile_deps: rebar.config
	rebar clean get-deps compile

start_script_riak: all_no_test
	erl $(BIN_SEARCH_PATHS) -config $(CONFIG_DIR)riak -s netinf_nrs -eval "io:format(\"NetInf NRS is running ... ~n\")."
start_script: all_no_test
	erl $(BIN_SEARCH_PATHS) ebin deps/*/ebin -config $(CONFIG_DIR)list -s netinf_nrs -eval "io:format(\"NetInf NRS is running ... ~n\")."

clean: set_env_folders
	- rm -rf ./ebin
	- rm -rf erl_crash.dump


