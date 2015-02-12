%% @author David Ellefsen <davidellefsen@gmail.com> 
%%
%% @doc ebc_tx_relay is an application example for the bitcoin network.
%% This creates a simple node that will relay transactions between connected nodes. 
%% There is no extra process that is performed on the transactions.
%%
%% Copyright 2015 David Ellefsen
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ebc_tx_relay).
-behaviour(application).

-include("ebc_node.hrl").

-export([start/0, start/2, stop/1, s/0]).

%% contains the initialisation routines - will be used by the application behaviour
start(_Type, _StartArgs) ->
	{ok, self()}.

stop(_State) ->
	ok.

%%a wrapper to start the application
start() ->
	application:start(ebc_tx_relay),
	ok.


%%
%% Move this to somewhere else
%%
updateTxBlock({Hash, _Number, X}) when is_record(X, block) ->
	tx_handler:updateTransactions(X#block.transactions, Hash).

s() ->
 mnesia:start(),
 peer_handler:start_link(),
 tx_handler:start_link(),
 block_handler:start_link(),
 ebc_node_srv:start_link(),
 tx_handler:addTxCallback(add_tx, fun(X) -> tx_handler:addTx(X) end),
 block_handler:addBlockCallback(update_tx, fun(X) -> updateTxBlock(X) end),
 ebc_node_srv:connectPeer("192.168.2.3", 18333, ebc_callback_mgr).

