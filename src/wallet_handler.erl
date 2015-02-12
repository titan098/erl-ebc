%% @author David Ellefsen <davidellefsen@gmail.com> 
%%
%% @doc wallet_handler is a handler for wallets on the bitcoin network
%% currently only this will support watching transactions on the network.
%% wallets will register their interest to receive transaction information
%% from the network.
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

-module(wallet_handler).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, add_wallet_identifier/1, check_interest/1, add_wallet_transaction/4, tx_callback/1]).

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

add_wallet_identifier(Identifier) ->
	gen_server:call(?MODULE, {add_wallet_identifier, Identifier}).

check_interest(Identifier) ->
	gen_server:call(?MODULE, {check_interest, Identifier}).

check_transaction(TxID, TxIndex) ->
	gen_server:call(?MODULE, {check_transaction, TxID, TxIndex}).

get_wallet_transactions(Identifier) ->
	gen_server:call(?MODULE, {get_wallet_transactions, Identifier}).

add_wallet_transaction(Identifier, Type, Index, Tx) ->
	gen_server:call(?MODULE, {add_wallet_transaction, Identifier, Type, Index, Tx}).

get_wallet_transaction(TxID) ->
	gen_server:call(?MODULE, {get_wallet_transaction, TxID}).

%this is an incoming transaction from a transaction (not from a block)
addIncomingTransaction(Identifier, Index, Tx, tx) ->
	?DGB("Wallet: Adding Incoming Transaction ~p~n", [cryptopp:hex_dump(Tx#tx.hash)]),
	add_wallet_transaction(Identifier, in, Index, Tx).	

%this is an outgoing transaction from a transaction (not from a block)
addOutgoingTransaction(Identifier, Index, Tx, tx) ->
	?DGB("Wallet: Adding Outgoing Transaction ~p~n", [cryptopp:hex_dump(Tx#tx.hash)]),
	add_wallet_transaction(Identifier, out, Index, Tx).	

%Process the incoming block transaction and update the block number where approeprate
processIncoming(TxOutList, Tx, block, BlockNumber) ->
	InterestList = [{check_interest(Identifier), Amount, Index} || {Identifier, Amount, Index} <- TxOutList],
	FilteredInterestList = lists:filter(fun({{_Identifier, Interest}, _Amount, _Index}) -> Interest =:= true end, InterestList),
	lists:map(fun({{Identifier, _Status}, _Amount, Index}) -> addIncomingTransaction(Identifier, Index, Tx, tx) end, FilteredInterestList),
	lists:map(fun({{Identifier, _Status}, _Amount, Index}) -> updateTransactionBlock(Identifier, BlockNumber) end, FilteredInterestList).

%Process the outgoing block transaction and update the block number where approeprate
processOutgoing(TxInList, Tx, block, BlockNumber) ->
	%check to see if there is interest in the list we have interest in
	InterestList = [{check_interest(Identifier), PrevHash, Index} || {Identifier, PrevHash, Index} <- TxInList],
	FilteredInterestList = lists:filter(fun({{_Identifier, Interest}, _PrevHash, _Index}) -> Interest =:= true end, InterestList),
	lists:map(fun({{Identifier, _Status}, _PrevHash, Index}) -> addOutgoingTransaction(Identifier, Index, Tx, tx) end, FilteredInterestList),
	lists:map(fun({{Identifier, _Status}, _Amount, Index}) -> updateTransactionBlock(Identifier, BlockNumber) end, FilteredInterestList).

processIncoming(TxOutList, Tx, tx) ->
	%Check to see which of the accounts in the lists we have interest in
	InterestList = [{check_interest(Identifier), Amount, Index} || {Identifier, Amount, Index} <- TxOutList],
	FilteredInterestList = lists:filter(fun({{_Identifier, Interest}, _Amount, _Index}) -> Interest =:= true end, InterestList),
	lists:map(fun({{Identifier, _Status}, _Amount, Index}) -> addIncomingTransaction(Identifier, Index, Tx, tx) end, FilteredInterestList).

processOutgoing(TxInList, Tx, tx) ->
	%check to see if there is interest in the list we have interest in
	InterestList = [{check_interest(Identifier), PrevHash, Index} || {Identifier, PrevHash, Index} <- TxInList],
	FilteredInterestList = lists:filter(fun({{_Identifier, Interest}, _PrevHash, _Index}) -> Interest =:= true end, InterestList),
	lists:map(fun({{Identifier, _Status}, _PrevHash, Index}) -> addOutgoingTransaction(Identifier, Index, Tx, tx) end, FilteredInterestList).

tx_callback(Tx) when is_record(Tx, tx) ->
	?DGB("Wallet Tx Callback~n", []),
	TxOutList = ebc_node:decodeTxOutAddr(Tx#tx.tx_out, 0),
	TxInList = ebc_node:decodeTxInAddr(Tx#tx.tx_in, 0),
	processIncoming(TxOutList, Tx, tx),
	processOutgoing(TxInList, Tx, tx).

processBlockTransactions([], _TxCount, _BlockNumber) ->
	ok;
processBlockTransactions([Tx|MoreTransactions], TxCount, BlockNumber) ->
	TxOutList = ebc_node:decodeTxOutAddr(Tx#tx.tx_out, 0),
	TxInList = ebc_node:decodeTxInAddr(Tx#tx.tx_in, 0),
	processIncoming(TxOutList, Tx, block, BlockNumber),
	processOutgoing(TxInList, Tx, block, BlockNumber),
	processBlockTransactions(MoreTransactions, TxCount-1, BlockNumber).

block_callback({Hash, Number, Block}) when is_record(Block, block) ->
	?DGB("Block Callback~n", []),
	TxCount = Block#block.trans,
	TxList = Block#block.transactions,
	processBlockTransactions(TxList, TxCount, Number).
	%Process Transaction List
	

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

handle_call({get_wallet_transaction, TxID, TxIndex}, _From, State) ->
	Reply = getWalletTransaction(TxID, TxIndex),
	{reply, Reply, State};

handle_call({get_wallet_transactions, Identifier}, _From, State) ->
	Reply = getWalletTransactions(Identifier),
	{reply, Reply, State};


handle_call({check_interest, Identifier}, _From, State) ->
	Reply = checkInterest(Identifier),
	{reply, Reply, State};

handle_call({check_transaction, TxID, TxIndex}, _From, State) ->
	Reply = checkTransactionInterest(TxID, TxIndex),
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

getWalletTransaction(TxID, TxIndex) ->
	F = fun() -> mnesia:read(wallet_transaction, {TxID, TxIndex}) end,
	Result = mnesia:transaction(F),
	case Result of
		{atomic, [#wallet_transaction{txid = {TxID, TxIndex}} = Transaction]} -> Transaction;
		_ -> undefined
	end.

getWalletTransactions(Identifier) ->
	F = fun() ->
		qlc:eval(qlc:q([X || X <- mnesia:table(wallet_transaction), X#wallet_transaction.address =:= Identifier]))
	end,

	case mnesia:transaction(F) of
		{atomic, [Result]} -> {ok, Result};
		{atomic, Result} -> {ok, Result};
		_ -> {error, unknown_identifier}
	end.

checkInterest(Identifier) ->
	F = fun() -> mnesia:read(wallet_identifier, Identifier) end,
	Result = mnesia:transaction(F),
	case Result of
		{atomic, [#wallet_identifier{identifier = Identifier}]} -> {Identifier, true};
		_ -> {Identifier, false}
	end.

checkTransactionInterest(Identifier, Index) ->
	case getWalletTransaction(Identifier, Index) of
		undefined -> {Identifier, false};
		_ -> {Identifier, true}
	end.

%update the transaction block number for a passed transaction
doUpdateTransactionBlockNumber([], _BlockNumber) -> ok;
doUpdateTransactionBlockNumber([Transaction|MoreTransaction], BlockNumber) ->
	addWalletTransactionToTable(Transaction#wallet_transaction{
					block = BlockNumber
				}),
	doUpdateTransactionBlockNumber(MoreTransaction, BlockNumber).

%identify the blocks in the wallet and update their block number with the passed block number
updateTransactionBlock(Identifier, BlockNumber) ->
	F = fun() ->
		qlc:eval(qlc:q([X || X <- mnesia:table(wallet_transact), element(1, X#wallet_transaction.txid) =:= Identifier]))
	end,

	case mnesia:transaction(F) of
		{atomic, Result} ->
			doUpdateTransactionBlockNumber(Result, BlockNumber);
		_ -> ok
	end.

%%handle an outbound transaction (something that came from a TxIn)
addWalletTransaction(_Identifier, out, Index, #tx{} = Tx) ->
	TxIn = lists:nth(Index+1, Tx#tx.tx_in),

	%the transaction is in the wrong format, it must be reversed
	PrevTx = ebc_util:reverseBinary(TxIn#tx_in.previous_output),
	case getWalletTransaction(PrevTx, TxIn#tx_in.previous_index) of
		undefined -> 
			%?DGB("OUT: unknown output for input PrevOut:~p~n~n", [{PrevTx, TxIn#tx_in.previous_index}]),
			{error, unknown_output_transaction_for_input};
		Transaction ->
			%?DGB("OUT: Known output for input TX:~p~n", [Transaction]),
			OutputWalletTransaction = Transaction#wallet_transaction{
				spent = true,
				spentby = Tx#tx.hash
			},		
			WalletTransaction = #wallet_transaction{
				txid = {Tx#tx.hash, Index},
				address = Transaction#wallet_transaction.address,
				type = out,
				amount = Transaction#wallet_transaction.amount,
				index = Index,
				block = undefined,
				status = unconfirmed,
				spent = undefined,
				tx = Tx
			},
			addWalletTransactionToTable(OutputWalletTransaction),
			addWalletTransactionToTable(WalletTransaction)
	end;

%%handle and inbound transaction (something that came from a TXOut)
addWalletTransaction(Identifier, in, Index, #tx{} = Tx) ->
	{Identifier, Amount} = ebc_node:transactionOutAmount(Tx, Index),
	WalletTransaction = #wallet_transaction{
		txid = {Tx#tx.hash, Index},
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
