%% @author david
%% @doc @todo Add description to peer_handler.


-module(wallet_handler).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, add_wallet_identifier/1, check_interest/1, add_wallet_transaction/4]).

-include("ebc_node.hrl").

-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

add_wallet_identifier(Identifier) ->
	gen_server:call(?MODULE, {add_wallet_identifier, Identifier}).

check_interest(Identifier) ->
	gen_server:call(?MODULE, {check_interest, Identifier}).

add_wallet_transaction(Identifier, Type, Index, Tx) ->
	gen_server:call(?MODULE, {add_wallet_transaction, Identifier, Type, Index, Tx}).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

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
handle_call({add_wallet_identifier, Identifier}, _From, State) ->
	Reply = addWalletIdentifierToTable(#wallet_identifier{
		identifier = Identifier,
		timestamp = ebc_util:epoch()
	}),
	{reply, Reply, State};

handle_call({check_interest, Identifier}, _From, State) ->
	Reply = checkInterest(Identifier),
	{reply, Reply, State};

handle_call({add_wallet_transaction, Identifier, Type, Index, Tx}, _From, State) ->
	Reply = addWalletTransaction(Identifier, Type, Index, Tx),
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
	mnesia:create_table(wallet_identifier, [{ram_copies, [node()]},
		{record_name, wallet_identifier},
		{type, set},
		{attributes, record_info(fields, wallet_identifier)}]),
	mnesia:create_table(wallet_transaction, [{ram_copies, [node()]},
		{record_name, wallet_transaction},
		{type, set},
		{attributes, record_info(fields, wallet_transaction)}]).

addWalletIdentifierToTable(WalletIdentifer) when is_record(WalletIdentifer, wallet_identifier) ->
	{atomic, Result} = mnesia:transaction(fun() -> mnesia:write(WalletIdentifer) end),
	Result.

addWalletTransactionToTable(WalletTransaction) when is_record(WalletTransaction, wallet_transaction) ->
	{atomic, Result} = mnesia:transaction(fun() -> mnesia:write(WalletTransaction) end),
	Result.

getWalletTransaction(TxID) ->
	F = fun() -> mnesia:read(TxID, wallet_transaction) end,
	Result = mnesia:transaction(F),
	case Result of
		{atomic, [Transaction]} -> Transaction;
		_ -> undefined
	end.

checkInterest(Identifier) ->
	F = fun() -> mnesia:read(Identifier, wallet_identifier) end,
	Result = mnesia:transaction(F),
	case Result of
		{atomic, [Identifier]} -> {Identifier, true};
		_ -> {Identifier, false}
	end.

checkTransactionInterest(Identifier) ->
	F = fun() -> mnesia:read(Identifier, wallet_transaction) end,
	Result = mnesia:transaction(F),
	case Result of
		{atomic, [Identifier]} -> {Identifier, true};
		_ -> {Identifier, false}
	end.
	
%%handle an outbound transaction (something that came from a TxIn)
addWalletTransaction(Identifier, out, Index, #tx{} = Tx) ->
	TxIn = lists:nth(Tx#tx.tx_in, Index+1),

	case getWalletTransaction(TxIn#tx_in.previous_output) of
		undefined -> 
			{error, unknown_output_transaction_for_input};
		Transaction ->
			OutputWalletTransaction = Transaction#wallet_transaction{
				spent = true,
				spentby = Tx#tx.hash
			},		
			WalletTransaction = #wallet_transaction{
				txid = Tx#tx.hash,
				address = Transaction#wallet_transaction.address,
				type = out,
				amount = Transaction#wallet_transaction.amount,
				index = Index,
				block = undefined,
				status = unconfirmed,
				tx = Tx
			},
			addWalletTransactionToTable(OutputWalletTransaction),
			addWalletTransactionToTable(WalletTransaction)
	end;

%%handle and inbound transaction (something that came from a TXOut)
addWalletTransaction(Identifier, in, Index, #tx{} = Tx) ->
	{Identifier, Amount} = ebc_node:transactionOutAmount(Tx, Index),
	WalletTransaction = #wallet_transaction{
		txid = Tx#tx.hash,
		address = Identifier,
		type = in,	%in transactions are those comming into the wallet (listed in a TxOut)
		amount = Amount,
		index= Index,
		block = undefined, %block this transaction is listed in
		status = unconfirmed, %this is an unconfirmed transaction
		spent = false,
		tx = Tx
	},

	addWalletTransactionToTable(WalletTransaction).