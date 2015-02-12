%% @author David Ellefsen <davidellefsen@gmail.com> 
%%
%% @doc ebc_node_srv is the module that handles the management of connect nodes
%% in the bitcoin P2P network. Functions such as rebuilding the block chain or
%% transaction history are provided in this module
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

-module(ebc_node_srv).
-behaviour(gen_server).

-include("ebc_node.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, connectPeer/3]).
-export([getAddr/0, sendPing/0, getBlocks/1, getBlock/1, getHeaders/1, getMaxBlocksSeen/0, updateLastBlockSeen/2]).
-export([processHeadersList/2, rebuildChain/1, rescanChain/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

connectPeer(Host, Port, Callback) ->
	gen_server:call(?MODULE, {connectToPeer, Host, Port, Callback}).

getAddr() ->
	gen_server:call(?MODULE, {sendGetAddr}).

sendPing() ->
	gen_server:call(?MODULE, {sendPing}).

getBlocks(StartHash) when is_list(StartHash) ->
	getBlocks(ebc_util:reverseBinary(ebc_util:hexToBinary(StartHash)));
getBlocks(StartHash) when is_binary(StartHash)->
	gen_server:call(?MODULE, {getBlocks, StartHash}).

getBlock(BlockHash) when is_list(BlockHash) ->
	getBlock(ebc_util:reverseBinary(ebc_util:hexToBinary(BlockHash)));
getBlock(BlockHash) when is_binary(BlockHash) ->
	gen_server:call(?MODULE, {getBlock, BlockHash}).

fetchBlocks(HeaderList) ->
	InvVect = [#inv_vect{type = ?INV_BLOCK, hash = ebc_util:reverseBinary(ebc_util:hexToBinary(BlockHash))} || {BlockHash, _BlockNumber} <- HeaderList],
	gen_server:call(?MODULE, {fetchBlocks, InvVect}).

getHeaders(StartHash) when is_list(StartHash) ->
	getHeaders(ebc_util:reverseBinary(ebc_util:hexToBinary(StartHash)));
getHeaders(StartHash) when is_binary(StartHash) ->
	gen_server:call(?MODULE, {getHeaders, StartHash}).

getMaxBlocksSeen() ->
	gen_server:call(?MODULE, {getMaxSeenBlock}).

updateLastBlockSeen(BlockHash, BlockNumber) ->
	gen_server:call(?MODULE, {updateLastBlockSeen, BlockHash, BlockNumber}).

%%
%% rebuild the hash chain - don't download blocks
%%
rebuildChain(GenesisHash) ->
	?DGB("Rebuilding the Hash Chain~n", []),
	block_handler:addHeaderCallback(chain_rebuild, fun(X) -> ebc_node_srv:processHeadersList(X, false) end),
	getHeaders(GenesisHash).

%%
%% rescan the hash chain - download the blocks
%%
rescanChain(GenesisHash) ->
	?DGB("Rescanning the Hash Chain~n", []),
	block_handler:addHeaderCallback(chain_rebuild, fun(X) -> ebc_node_srv:processHeadersList(X, true) end),
	getHeaders(GenesisHash).

%%% Callbacks
requestMoreHeaders(LastSeenHash, Start, End) when Start =< End ->
	?DGB("Requesting More Items~n", []),
	getHeaders(LastSeenHash);	%get more headers
requestMoreHeaders(_LastSeenHash, _Start, _End) ->
	?DGB("Nothing More to request~n", []),
	block_handler:removeHeaderCallback(chain_rebuild),
	ok.				%nothing more to get

%% Process the header list without downloading the blocks as we go
processHeaders(HeadersList) ->
 	?DGB("Headers List Callback.~n", [HeadersList]),
	MaxItem = getMaxBlocksSeen(), %get the max seen block from all of the peers
	{LastSeenHash, LastHeight} = hd(lists:reverse(HeadersList)),  %the headers list will be a chain of items, get headers will return a list of items
	?DGB(" Last Seen Hash {~p,~p}~n", [LastSeenHash, LastHeight]),
	requestMoreHeaders(LastSeenHash, LastHeight, MaxItem),
	ok.

% Process the header list with the option of downloading the blocks as we go.
processHeadersList(HeadersList, false) ->
	processHeaders(HeadersList);

processHeadersList(HeadersList, true) ->
	processHeaders(HeadersList),
	fetchBlocks(HeadersList).

%% Behavioural functions 
%% ====================================================================
-record(state, {
		connectedPeers = [],
		lastBlockSeen = {nohash, -1}
}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.

%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({connectToPeer, Host, Port, Callback}, _From, State) ->
	Resp = ebc_node:init(Host, Port, Callback),
	NewState = State#state{
		connectedPeers = [Resp | State#state.connectedPeers]
	},
	{reply, ok, NewState};

handle_call({sendGetAddr}, _From, #state{connectedPeers = Peers} = State) ->
	F = fun(#ebc_client_state{socket = Socket}) ->
		ebc_node:sendCommand(Socket, getaddr, [])
	end,
	lists:map(F, Peers),
	{reply, ok, State};

handle_call({sendPing}, _From, #state{connectedPeers = Peers} = State) ->
	F = fun(#ebc_client_state{socket = Socket}) ->
		ebc_node:sendCommand(Socket, ping, ebc_util:epoch())
	end,
	lists:map(F, Peers),
	{reply, ok, State};

handle_call({getBlocks, StartHash}, _From, #state{connectedPeers = Peers} = State) ->
	F = fun(#ebc_client_state{socket = Socket}) ->
		ebc_node:sendCommand(Socket, getblocks, StartHash)
	end,
	lists:map(F, Peers),
	{reply, ok, State};

handle_call({fetchBlocks, InvVect}, _From, #state{connectedPeers = Peers} = State) ->
	F = fun(#ebc_client_state{socket = Socket}) ->
		block_handler:fetchBlocks(Socket, InvVect)		
	end,
	lists:map(F, Peers),
	{reply, ok, State};

handle_call({getBlock, BlockHash}, _From, #state{connectedPeers = Peers} = State) ->
	F = fun(#ebc_client_state{socket = Socket}) ->
		block_handler:fetchBlock(Socket, block_handler:checkBlock(BlockHash), BlockHash)
	end,
	lists:map(F, Peers),
	{reply, ok, State};

handle_call({getHeaders, StartHash}, _From, #state{connectedPeers = Peers} = State) ->
	F = fun(#ebc_client_state{socket = Socket}) ->
		ebc_node:sendCommand(Socket, getheaders, StartHash)
	end,
	lists:map(F, Peers),
	{reply, ok, State};

handle_call({getMaxSeenBlock}, _From, #state{connectedPeers = Peers} = State) ->
	HeightList = [Version#version.start_height || #ebc_client_state{version = Version} <- Peers],
	Reply = lists:max(HeightList),
	{reply, Reply, State};

handle_call({updateLastBlockSeen, BlockHash, BlockNumber}, _From, State) ->
	{_LastHash, LastBlockNumber} = State#state.lastBlockSeen,
	NewState = case BlockNumber > LastBlockNumber of
		true -> State#state{
				lastBlockSeen = {BlockHash, BlockNumber}
			};
		_ -> State
	end,
	{reply, ok, NewState};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
	terminateAllPeers(State#state.connectedPeers),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
terminateAllPeers([]) ->
	ok;
terminateAllPeers([Peer | PeerList]) ->
	ebc_node:close(Peer),
	terminateAllPeers(PeerList).
