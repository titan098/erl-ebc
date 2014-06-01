%% @author david
%% @doc @todo Add description to block_handler.


-module(block_handler).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, processBlockHeaderCallbacks/1, addHeader/1, addBlock/1, checkBlock/1, getBlockHash/1, fetchBlock/3]).
-export([addHeaderCallback/2, getHeaderCallbacks/0, addBlockCallback/2, getBlockCallbacks/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("ebc_node.hrl").

-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

%%
%% Process the a list of block headers, the list should be of the form
%% [{"BlockHash", BlockNumber}...]
%%
processBlockHeaderCallbacks(BlockHeaderList) ->
	{ok, Callbacks} = getHeaderCallbacks(),

	F = fun({CallbackID, Callback}) ->
		?DGB("Block Header Callback: ~p~n", [CallbackID]),
		spawn(fun() -> Callback(BlockHeaderList) end),	%start it its own process so it doesn't break anything
		{ok, CallbackID}
	end,

	lists:map(F, Callbacks).

%%
%% Process a block that has come in, can be passed off to the wallet_handler to
%% map transactions to blocks - if required.
%%
processBlockCallbacks({Hash, Number, Block}) ->
	{ok, Callbacks} = getBlockCallbacks(),

	F = fun({CallbackID, Callback}) ->
		?DGB("Block Callback: ~p~n", [CallbackID]),
		spawn(fun() -> Callback(Block) end),	%start it its own process so it doesn't break anything
		{ok, CallbackID}
	end,

	lists:map(F, Callbacks).

%%
%% Add a collection of block headers to the DB, this will be a List of headers
%%
addHeader(Header) when is_list(Header) ->
	{ok, HeaderList} = gen_server:call(?MODULE, {addHeaders, Header}),
	HeaderList;
addHeader(Header) when is_record(Header, block_header) ->
	gen_server:call(?MODULE, {addHeader, Header}).

addBlock(Block) when is_record(Block, block) ->
	gen_server:call(?MODULE, {addBlock, Block}).

checkBlock(Block) ->
	gen_server:call(?MODULE, {checkBlock, Block}).

getBlockHash(Number) when is_integer(Number) ->
	gen_server:call(?MODULE, {getBlockHash, Number}).

fetchBlock(Socket, Status, Hash) ->
	io:format("Fetch Block: ~p~n", [ebc_util:binaryToHex(ebc_util:reverseBinary(Hash))]),
	case Status of
		wanted_block ->
			io:format("Block is wanted~n"), 
			ebc_node:sendCommand(Socket, getdata, [#inv_vect{type = ?INV_BLOCK, hash = Hash}]);
		_ -> ok
	end.

addHeaderCallback(CallbackID, Callback) ->
	gen_server:call(?MODULE, {addHeaderCallback, CallbackID, Callback}).

removeHeaderCallback(CallbackID) ->
	gen_server:call(?MODULE, {removeHeaderCallback, CallbackID}).

getHeaderCallbacks() ->
	gen_server:call(?MODULE, {getHeaderCallbacks}).

addBlockCallback(CallbackID, Callback) ->
	gen_server:call(?MODULE, {addBlockCallback, CallbackID, Callback}).

getBlockCallbacks() ->
	gen_server:call(?MODULE, {getBlockCallbacks}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {
	block_callbacks = [],
	header_callbacks = []
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
	openMnesiaTable(),
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
handle_call({addBlock, Block}, _From, State) ->
	io:format("Adding Block: ~p~n", [ebc_util:binaryToHex(ebc_util:reverseBinary(Block#block.hash))]),
	addBlockHeader(Block#block.header),
	{reply, ok, State};

handle_call({addHeader, Header}, _From, State) ->
	io:format("Adding Block Header: ~p~n", [ebc_util:binaryToHex(ebc_util:reverseBinary(Header#block_header.hash))]),
	addBlockHeader(Header),
	{reply, ok, State};

handle_call({addHeaders, HeaderList}, _From, State) ->
	Reply = addBlockHeaderList(HeaderList),
	{reply, {ok, Reply}, State};

handle_call({getBlockHash, Number}, _From, State) ->
	Reply = getBlockForNumber(Number),
	{reply, Reply, State};

handle_call({checkBlock, Block}, _From, State) ->
	io:format("Checking Block: ~p~n", [ebc_util:binaryToHex(ebc_util:reverseBinary(Block))]),
	Reply = checkBlockFromTable(Block),
	{reply, Reply, State};

handle_call({getHeaderCallbacks}, _From, State) ->
	{reply, {ok, State#state.header_callbacks}, State};

handle_call({addHeaderCallback, CallbackID, Callback}, _From, State) ->
	io:format("Adding Block Header Callback: ~p~n", [CallbackID]),
	NewCallbackList = [{CallbackID, Callback} | State#state.header_callbacks],
	{reply, ok, State#state{header_callbacks = NewCallbackList}};

handle_call({removeHeaderCallback, CallbackID}, _From, State) ->
	io:format("Removing Block Header Callback: ~p~n", [CallbackID]),
	NewCallbackList = case lists:keyfind(CallbackID, 1, State#state.header_callbacks) of
			{CallbackID, Callback} -> lists:delete({CallbackID, Callback}, State#state.header_callbacks);
			_ -> State#state.header_callbacks
	end,
	{reply, ok, State#state{header_callbacks = NewCallbackList}};

handle_call({getBlockCallbacks}, _From, State) ->
	{reply, {ok, State#state.block_callbacks}, State};

handle_call({addBlockCallback, CallbackID, Callback}, _From, State) ->
	io:format("Adding Block Callback: ~p~n", [CallbackID]),
	NewCallbackList = [{CallbackID, Callback} | State#state.block_callbacks],
	{reply, ok, State#state{block_callbacks = NewCallbackList}};

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

openMnesiaTable() ->
	mnesia:create_table(block, [{ram_copies, [node()]},
		{record_name, block},
		{type, set},
		{attributes, record_info(fields, block)}]),
	mnesia:create_table(block_header, [{ram_copies, [node()]},
		{record_name, block_header},
		{type, set},
		{index, number},
		{attributes, record_info(fields, block_header)}]).

%addTxToTable(Tx) when is_record(Tx, tx) ->
%	mnesia:transaction(fun() -> mnesia:write(Tx) end).

%%
%% Add block to table
%%
addBlockHeaderToTable(BlockHeader) when is_record(BlockHeader, block_header) ->
	F = fun() ->
		mnesia:write(BlockHeader)
	end,
	
	case mnesia:transaction(F) of
		{atomic, Result} -> {ebc_util:binaryToHex(ebc_util:reverseBinary(BlockHeader#block_header.hash)), BlockHeader#block_header.number};
		_ -> {error, write_error}
	end.	

%%
%% Add a collection of block headers to the table, recording the block number
%%
addBlockHeaderList([]) -> [];
addBlockHeaderList([#block_header{} = Header | MoreBlockHeaders]) ->
	[addBlockHeader(Header) | addBlockHeaderList(MoreBlockHeaders)].
%%
%% Add block header to the header table, recording the block number as required
%%
addBlockHeader(BlockHeader) when is_record(BlockHeader, block_header) ->
	case getBlockNumber(BlockHeader#block_header.hashPrevBlock) of
		{ok, -2} ->
			?DGB(" Block Number: unknown~n", []),
			addBlockHeaderToTable(BlockHeader#block_header{
				number = -2 
			});
		{ok, PreviousNumber} ->
			?DGB(" Block Number: ~p ~n", [PreviousNumber+1]),
			addBlockHeaderToTable(BlockHeader#block_header{
				number = PreviousNumber+1
			});
		{unknown, _} ->
			?DGB(" Block Number: unknown~n", []),
			addBlockHeaderToTable(BlockHeader#block_header{
				number = -2 
			})
	end.

%%
%% Return the block hash for the passed number
%%
getBlockForNumber(Number) when is_integer(Number) or (Number =:= unknown) ->
	F = fun() ->
		qlc:eval(qlc:q([{ebc_util:binaryToHex(ebc_util:reverseBinary(X#block_header.hash)), X#block_header.number} || X <- mnesia:table(block_header), X#block_header.number =:= Number]))
	end,

	case mnesia:transaction(F) of
		{atomic, [Result]} -> {ok, Result};
		{atomic, Result} -> {ok, Result};
		_ -> {error, unknown_block}
	end.

%%
%% Get the number of block for the previous hash
%%
getBlockNumber("0000000000000000000000000000000000000000000000000000000000000000") ->
	{ok, -1};
getBlockNumber(<<0:256>>) ->
	{ok, -1};
getBlockNumber(BlockHash) when is_list(BlockHash) ->
	getBlockNumber(ebc_util:hexToBinary(BlockHash));
getBlockNumber(BlockHash) when is_binary(BlockHash) ->
	case getBlockHeader(BlockHash) of
		{ok, #block_header{number = Number}} -> {ok, Number};
		Error -> {unknown, -2} 
	end.	

%%
%% Get the block headers from the table
%%
getBlockHeader(BlockHash) when is_list(BlockHash) ->
	getBlockHeader(ebc_util:reverseBinary(ebc_util:hexToBinary(BlockHash)));
getBlockHeader(BlockHash) when is_binary(BlockHash) ->
	Result = mnesia:transaction(fun() -> mnesia:read(block_header, BlockHash) end),
	case Result of
		{atomic, [HeaderResult]} -> {ok, HeaderResult};
		_ -> {error, unknown_hash}
	end.


%%
%% Check to see if we have a copy of the block
%%
checkBlockFromTable(BlockHash) when is_list(BlockHash) ->
	checkBlockFromTable(ebc_util:hexToBinary(BlockHash));
checkBlockFromTable(BlockHash) when is_binary(BlockHash) ->
	{atomic, Result} = mnesia:transaction(fun() -> mnesia:read(block, BlockHash) end),
	case Result of
		[] -> wanted_block;
		_ -> known_block
	end.
