-module(ebc).
-behaviour(application).

-export([start/0, start/2, stop/1]).

%% contains the initilisation routines - will be used by the application behaviour
start(_Type, _StartArgs) ->
	{ok, self()}.

stop(_State) ->
	ok.

%%a wrapper to start the application
start() ->
	application:start(ebc),
	ok.

% mnesia:start(), peer_handler:start_link(), tx_handler:start_link(), block_handler:start_link(), ebc_node_srv:start_link(), wallet_handler:start_link(), tx_handler:addTxCallback(wallet_processor, fun(X) -> wallet_handler:tx_callback(X) end), wallet_handler:add_wallet_identifier("muwPYaFuNysH5qZVdGZ9BZuboVFU5asw9C"), ebc_node_srv:connectPeer("192.168.2.3", 18333, ebc_callback_mgr).
