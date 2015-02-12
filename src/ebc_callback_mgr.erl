%% @author David Ellefsen <davidellefsen@gmail.com> 
%%
%% @doc ebc_callback_mgr stores a collection of callbacks that are executed for
%% different bitcoin messages that are received from the network
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


-module(ebc_callback_mgr).
-include("ebc_node.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([addr/1, inv/2, tx/1, get_tx/1, block/1, block_header/1]).

addr(Addr) ->
	peer_handler:addPeer(Addr).

inv(#inv_vect{type = ?INV_BLOCK} = IV, Socket) ->
	block_handler:fetchBlock(Socket, block_handler:checkBlock(IV#inv_vect.hash), IV#inv_vect.hash);	
inv(#inv_vect{type = ?INV_TX} = IV, Socket) ->
	tx_handler:checkFetchTx(Socket, IV#inv_vect.hash);
	%tx_handler:fetchTx(Socket, tx_handler:checkTx(IV#inv_vect.hash), IV#inv_vect.hash);
inv(#inv_vect{type = Type}, _Socket) ->
	io:format("UNKNOWN TYPE: ~p~n", [Type]),
	unknown.
%%
%% Management function when a tx has been returned by a get tx request
%%
tx(Tx) ->
	?DGB("Tx Callback~n", []),
	tx_handler:processTx(Tx).

get_tx(Tx) ->
	?DGB("Get Tx Callback~n", []),
	tx_handler:getTx(Tx).

%%
%% Management function when a block has been returned by a get data request
%%	
block(Block) ->
	{Hash, Number} = block_handler:addBlock(Block),
	block_handler:processBlockCallbacks({Hash, Number, Block}).

%%
%% Add a collection of block headers
%%
block_header(BlockHeader) ->
	HeaderList = block_handler:addHeader(BlockHeader),
	block_handler:processBlockHeaderCallbacks(HeaderList).

%% ====================================================================
%% Internal functions
%% ====================================================================


