-module(ebc).
-behaviour(application).

-export([start/0, start/2, stop/1, s/0]).

%% contains the initilisation routines - will be used by the application behaviour
start(_Type, _StartArgs) ->
	{ok, self()}.

stop(_State) ->
	ok.

%%a wrapper to start the application
start() ->
	application:start(ebc),
	ok.

s() ->
 mnesia:start(),
 peer_handler:start_link(),
 tx_handler:start_link(),
 block_handler:start_link(),
 ebc_node_srv:start_link(),
 wallet_handler:start_link(),
 tx_handler:addTxCallback(wallet_processor, fun(X) -> wallet_handler:tx_callback(X) end),
 %block_handler:addBlockCallback(block_test, fun(X) -> io:format("This is a test callback on Block~p~n", [X]) end),
 wallet_handler:add_wallet_identifier("muwPYaFuNysH5qZVdGZ9BZuboVFU5asw9C"),
 ebc_node_srv:connectPeer("192.168.2.3", 18333, ebc_callback_mgr),

 ebc_node_srv:getBlock("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943").
