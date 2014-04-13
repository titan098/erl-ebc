%% @author david
%% @doc @todo Add description to ebc_node_srv.


-module(ebc_node_srv).
-behaviour(gen_server).

-include("ebc_node.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, connectPeer/3]).
-export([getAddr/0, sendPing/0, getBlocks/1, getBlock/1, getHeaders/1, getMaxBlocksSeen/0]).
-export([processHeadersList/1, rebuildChain/1]).

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

getBlocks(StartHash) ->
	gen_server:call(?MODULE, {getBlocks, StartHash}).

getBlock(BlockHash) when is_list(BlockHash) ->
	getBlock(ebc_util:reverseBinary(ebc_util:hexToBinary(BlockHash)));
getBlock(BlockHash) when is_binary(BlockHash) ->
	gen_server:call(?MODULE, {getBlock, BlockHash}).

getHeaders(StartHash) when is_list(StartHash) ->
	getHeaders(ebc_util:reverseBinary(ebc_util:hexToBinary(StartHash)));
getHeaders(StartHash) when is_binary(StartHash) ->
	gen_server:call(?MODULE, {getHeaders, StartHash}).

getMaxBlocksSeen() ->
	gen_server:call(?MODULE, {getMaxSeenBlock}).

%%
%% rebuild the hash chain
%%
rebuildChain(GenesisHash) ->
	?DGB("Rebuilding the Hash Chain~n", []),
	block_handler:addHeaderCallback(chain_rebuild, fun(X) -> ebc_node_srv:processHeadersList(X) end),
	getHeaders(GenesisHash).

%%% Callbacks
requestMoreHeaders(LastSeenHash, Start, End) when Start =< End ->
	?DGB("Requesting More Items~n", []),
	getHeaders(LastSeenHash);	%get more headers
requestMoreHeaders(_LastSeenHash, _Start, _End) ->
	?DGB("Nothing More to request~n", []),
	block_handler:removeHeaderCallback(chain_rebuild),
	ok.				%nothing more to get

processHeadersList(HeadersList) ->
	?DGB("Headers List Callback~n", []),
	MaxItem = getMaxBlocksSeen(), %get the max seen block from all of the peers
	{LastSeenHash, LastHeight} = hd(lists:reverse(HeadersList)),  %the headers list will be a chain of items, get headers will return a list of items
	?DGB(" Last Seen Hash {~p,~p}~n", [LastSeenHash, LastHeight]),
	requestMoreHeaders(LastSeenHash, LastHeight, MaxItem),
	ok.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {
		connectedPeers = []
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
