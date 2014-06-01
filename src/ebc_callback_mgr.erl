%% @author david
%% @doc @todo Add description to ebc_callback_mgr.


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
	tx_handler:fetchTx(Socket, tx_handler:checkTx(IV#inv_vect.hash), IV#inv_vect.hash);
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


