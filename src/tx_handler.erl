%% @author david
%% @doc @todo Add description to peer_handler.


-module(tx_handler).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, processTx/1, addTx/1, checkTx/1]).

-include("ebc_node.hrl").

-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

processTx(Tx) when is_record(Tx, tx) ->
	{ok, Callbacks} = getTxCallbacks(),

	F = fun({CallbackID, Callback}) ->
		?DGB("Tx Callback: ~p~n", [CallbackID]),
		spawn(fun() -> Callback(Tx) end),	%start it its own process so it doesn't break anything
		{ok, CallbackID}
	end,

	lists:map(F, Callbacks).	

addTxCallback(CallbackID, Callback) ->
	gen_server:call(?MODULE, {addCallback, CallbackID, Callback}).

getTxCallbacks() ->
	gen_server:call(?MODULE, {getCallbacks}).

addTx(Tx) when is_record(Tx, tx) ->
	gen_server:call(?MODULE, {addTx, Tx}).

updateTransactions(TxList, BlockHash) ->
	gen_server:call(?MODULE, {updateTx, TxList, BlockHash}).

getTx(Tx) ->
	%TODO: Make it execute a retrieval callback to the wallet
	wallet_handler:get_wallet_transaction(Tx).

checkFetchTx(Socket, Hash) ->
	gen_server:call(?MODULE, {checkFetchTx, Socket, Hash}).

checkTx(Tx) ->
	gen_server:call(?MODULE, {checkTx, Tx}).

fetchTx(Socket, Status, Hash) ->
	case Status of
		wanted_tx ->
			ebc_node:sendCommand(Socket, getdata, [#inv_vect{type = ?INV_TX, hash = Hash}]);
		_ -> ok
	end.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {
	callbacks = []
}).

-record(tx_store, {
	hash = undefined,
	status = unconfirmed,
	firstseen = undefined,
	blocktime = undefined,
	tx = <<>>
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
handle_call({getCallbacks}, _From, State) ->
	{reply, {ok, State#state.callbacks}, State};

handle_call({addCallback, CallbackID, Callback}, _From, State) ->
	io:format("Adding Tx Callback: ~p~n", [CallbackID]),
	NewCallbackList = [{CallbackID, Callback} | State#state.callbacks],
	{reply, ok, State#state{callbacks = NewCallbackList}};

handle_call({addTx, Tx}, _From, State) ->
	?DGB("Adding Tx: ~p~n", [ebc_util:binaryToHex(Tx#tx.hash)]),
	addTxToTable(Tx),
	{reply, ok, State};

handle_call({updateTx, TxList, BlockHash}, _From, State) ->
	?DGB("Updating Transactions: ~n", []),
	updateTxListStatus(TxList, BlockHash),
	{reply, ok, State};

handle_call({checkTx, Tx}, _From, State) ->
	?DGB("Checking Tx: ~p~n", [ebc_util:binaryToHex(ebc_util:reverseBinary(Tx))]),
	Reply = checkTxFromTable(Tx),
	?DGB("Checking Tx: ~p ~p~n", [ebc_util:binaryToHex(ebc_util:reverseBinary(Tx)), Reply]),
	{reply, Reply, State};

handle_call({checkFetchTx, Socket, Hash}, _From, State) ->
	?DGB("Checking Tx: ~p~n", [ebc_util:binaryToHex(ebc_util:reverseBinary(Hash))]),
	Reply = checkTxFromTable(Hash),
	?DGB("Checking Tx: ~p ~p~n", [ebc_util:binaryToHex(ebc_util:reverseBinary(Hash)), Reply]),
	fetchTx(Socket, Reply, Hash),

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
	mnesia:create_table(tx_store, [{ram_copies, [node()]},
		{record_name, tx_store},
		{type, set},
		{attributes, record_info(fields, tx_store)}]).

%%
%% Add/update a transaction from the database
%%
addTxToTable(Tx) when is_record(Tx, tx) ->
	mnesia:transaction(
		fun() -> mnesia:write(#tx_store{
				hash = ebc_util:binaryToHex(Tx#tx.hash),
				firstseen = ebc_util:epoch(),
				tx = Tx#tx.payload
			})
		end);

addTxToTable(Tx) when is_record(Tx, tx_store) ->
	mnesia:transaction(fun() -> mnesia:write(Tx) end).

%%
%% Get a TX from the table - the Transaction should be the hex-string in big-endian form
%%
getTxFromTable(TxHash) ->
	{atomic, Result} = mnesia:transaction(fun() -> mnesia:read(tx_store, TxHash) end),
	case Result of
		[] -> unknown_tx;
		[Tx] -> Tx
	end.

%%
%% Update the status of a transaction in the table to reflect a transaction that is confirmed
%%
updateTxListStatus([], _BlockHash) ->
	ok;
updateTxListStatus([Tx|TxMore], BlockHash) ->
	TxHash = ebc_util:binaryToHex(Tx#tx.hash),
	?DGB("Updating Tx: ~p~n", [TxHash]),
	updateTxStatus(TxHash, BlockHash),
	updateTxListStatus(TxMore, BlockHash).
	

updateTxStatus(TxHash, BlockHash) ->
	Transaction = getTxFromTable(TxHash),
	case Transaction of
		unknown_tx -> {error, unknown_transaction};
		Result ->
			addTxToTable(Result#tx_store{
				status = BlockHash,
				blocktime = ebc_util:epoch()
			}),
			{ok, updated}
	end.

%%
%% Check to see if a transaction has been observed before - prevent requesting an already seen transaction
%%
checkTxFromTable(TxHash) ->
%% the transaction in the inv packet is in little endian, and we want it in big endian
%% so we need to reverse the binary.
	?DGB("Checking: ~p~n", [ebc_util:binaryToHex(ebc_util:reverseBinary(TxHash))]),
	{atomic, Result} = mnesia:transaction(fun() -> mnesia:read(tx_store, ebc_util:binaryToHex(ebc_util:reverseBinary(TxHash))) end),
	case Result of
		[] -> wanted_tx;
		_ -> known_tx
	end.


printTxOutHash([]) -> ok;
printTxOutHash([#tx{hash = Hash, tx_out = TxOut} | MoreTx]) ->
	?DGB("TxID: ~p~n", [string:to_lower(cryptopp:hex_dump(Hash))]),
	%?DGB(" OutAddresses: ~p~n", [ebc_util:removeDups(decodeTxOutAddr(TxOut, 0))]),
	printTxOutHash(MoreTx).
