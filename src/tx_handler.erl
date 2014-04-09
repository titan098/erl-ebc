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

getTx(Tx) ->
	%TODO: Make it execute a retrieval callback to the wallet
	wallet_handler:get_wallet_transaction(Tx).

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
	io:format("Adding Tx: ~p~n", [ebc_util:binaryToHex(Tx#tx.hash)]),
	addTxToTable(Tx),
	{reply, ok, State};

handle_call({checkTx, Tx}, _From, State) ->
	io:format("Checking Tx: ~p~n", [ebc_util:binaryToHex(Tx)]),
	Reply = checkTxFromTable(Tx),
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
	mnesia:create_table(tx, [{ram_copies, [node()]},
		{record_name, tx},
		{type, set},
		{attributes, record_info(fields, tx)}]).

addTxToTable(Tx) when is_record(Tx, tx) ->
	mnesia:transaction(fun() -> mnesia:write(Tx) end).

checkTxFromTable(Tx) ->
%% the transaction in the inv packet is in little endian, and we want it in big endian
%% so we need to reverse the binary.
	{atomic, Result} = mnesia:transaction(fun() -> mnesia:read(tx, ebc_util:reverseBinary(Tx)) end),
	case Result of
		[] -> wanted_tx;
		_ -> known_tx
	end.



printTxOutHash([]) -> ok;
printTxOutHash([#tx{hash = Hash, tx_out = TxOut} | MoreTx]) ->
	?DGB("TxID: ~p~n", [string:to_lower(cryptopp:hex_dump(Hash))]),
	%?DGB(" OutAddresses: ~p~n", [ebc_util:removeDups(decodeTxOutAddr(TxOut, 0))]),
	printTxOutHash(MoreTx).
