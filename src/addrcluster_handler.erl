%% @author David Ellefsen <davidellefsen@gmail.com> 
%%
%% @doc addrcluster_handler is a auxiliary module that can be used to cluster addresses
%% that have been pulled from a collection of bitcoin block files. The addresses are stored
%% and manipulated in a mnesia database then arranged into clusters. Could be used to group
%% addresses that are owned by the same entity together.
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

-module(addrcluster_handler).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).

-record(addrcluster, {
	address,
	cluster
}).

-record(blocklog, {
	blockhash,
	firstseen	
}).

-record(tx, {
	hash,
	incluster,
	outcluster,
	value
}).

%the transaction log with the addresses replaced with cluster numbers
-record(txcluster, {
	incluster,
	outcluster,
	value
}).
	
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

addAddressGroup([]) ->
	ok;

addAddressGroup(Addresses) ->
	gen_server:call(?MODULE, {addAddressGroup, Addresses}, infinity).

addTransaction(InAddress, OutAddress, Value, Hash, OutIndex) ->
	gen_server:call(?MODULE, {addTransaction, InAddress, OutAddress, Value, Hash, OutIndex}).

addBlockLogEntry(BlockHash) ->
	gen_server:call(?MODULE, {addBlockLogEntry, BlockHash}).

removeBlockLogEntry(BlockHash) ->
	gen_server:call(?MODULE, {removeBlockLogEntry, BlockHash}).

containBlockLogEntry(BlockHash) ->
	gen_server:call(?MODULE, {containsBlockLogEntry, BlockHash}).

getAddressFromTransaction({TxHash, OutIndex}) ->
	case mnesia:dirty_read(tx, {TxHash, OutIndex}) of
		[#tx{outcluster = X}] -> X;
		[] -> undefined
	end.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {
	currentMax = 0,
	timestart = 0,
	blockCount = 0,
	currentTx = 0
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
    {ok, #state{currentMax = getStartCluster(), timestart = now()}}.

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
handle_call({addAddressGroup, Addresses}, _From, #state{currentMax = CurrentMax} = State) ->
	%io:format("Adding AddressGroup: ~p~n", [Addresses]),
	{ClusterNum, NewCurrentMax} = addAddressGroup(Addresses, CurrentMax),
	{reply, ClusterNum, State#state{currentMax = NewCurrentMax}};

handle_call({addTransaction, InAddress, OutAddress, Value, Hash, OutIndex}, _From, State) ->
	%io:format("Adding Transaction: ~p -> ~p [~p]~n", [InAddress, OutAddress, Value]),
	addOutTransaction(InAddress, OutAddress, Value, Hash, OutIndex),
	{reply, ok, State};

handle_call({addBlockLogEntry, BlockHash}, _From, #state{timestart = TimeStart, blockCount = BlockCount} = State) ->
	Reply = addBlockHash(BlockHash),
	{NewTimeStart, NewBlockCount} = showBlockStats(TimeStart, now(), BlockCount),
	{reply, Reply, State#state{timestart = NewTimeStart, blockCount = NewBlockCount+1}};

handle_call({removeBlockLogEntry, BlockHash}, _From, State) ->
	Reply = removeBlockHash(BlockHash),
	{reply, Reply, State};

handle_call({containsBlockLogEntry, BlockHash}, _From, State) ->
	Reply = containsBlockHash(BlockHash),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
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
handle_cast(_Msg, State) ->
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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

openMnesiaTable() ->
	mnesia:create_table(addrcluster, [{disc_copies, [node()]},
		{record_name, addrcluster},
		{type, set},
		{index, [cluster]},
		{attributes, record_info(fields, addrcluster)}]),
	mnesia:create_table(blocklog, [{disc_copies, [node()]},
		{record_name, blocklog},
		{type, set},
		{attributes, record_info(fields, blocklog)}]),
	mnesia:create_table(tx, [{disc_copies, [node()]},
		{record_name, tx},
		{type, set},
		{attributes, record_info(fields, tx)}]),
	ok = waitMnesiaReady([addrcluster, blocklog, tx], 20000, 20).

%delete and recreate the txcluster table.
emptyClusterTxCluster() ->
	mnesia:delete_table(txcluster),
	mnesia:create_table(txcluster, [{disc_copies, [node()]},
		{record_name, txcluster},
		{type, bag},
		{attributes, record_info(fields, txcluster)}]).
	
%wait for the mnesia tables to be ready before we start - this will take forever with a large index set.
waitMnesiaReady(Tables, _Timeout, 0) ->
	io:format("Giving up waiting for tables.~n"),
	throw({timeout, Tables});
waitMnesiaReady(Tables, Timeout, Count) ->
	io:format("Waiting for Mnesia Tables...~n"),
	case mnesia:wait_for_tables(Tables, Timeout) of
		{timeout, RemainingTables} ->
			io:format("Still waiting for: ~p~n", [RemainingTables]),
			waitMnesiaReady(Tables, Timeout, Count-1);
		ok ->
			io:format("Done!~n"),
			ok
	end.

%Add a new grouping to the mnesia database
addAddressGroup([], CurrentMax) ->
	{undefined, CurrentMax};
addAddressGroup(Addresses, CurrentMax) ->
	case getExistingClusters(Addresses) of
		[] ->   %This is a unique cluster
			writeAddresses(Addresses, CurrentMax),
			{CurrentMax, CurrentMax+1};
		A ->	%this cluster is not unique a merge should happen
			ClusterNum = doMerge(getMergeGroups(A)),
			writeAddresses(Addresses, ClusterNum),
			{ClusterNum, CurrentMax}
	end.

%rebuild the tx table with cluster numbers. This uses dirty operations, only
% use when the the mnesia is not be used.
replaceTxAddressesWithClusters() ->
	emptyClusterTxCluster(),
	readExistingTx(mnesia:dirty_first(tx)).

readExistingTx('$end_of_table') -> ok;
readExistingTx(Key) ->
	TxList = mnesia:dirty_read(tx, Key),
	replaceTxAddressesWithClusters(TxList),
	readExistingTx(mnesia:dirty_next(tx, Key)).

replaceTxAddressesWithClusters(TxList) ->
	TxListMod = lists:map(
			fun(#tx{incluster = InAddr, outcluster = OutAddr, value = Value}) ->
				#txcluster{incluster = getAddressCluster(InAddr),
					   outcluster = getAddressCluster(OutAddr),
					   value = Value}
			end, TxList),
	writeTxClusters(TxListMod).

%get the cluster number for an address, if it doesn't exist. Write it to the end of the table
getAddressCluster(Address) ->
	case mnesia:dirty_read(addrcluster, Address) of
		[] -> 
			C = getStartCluster()+1,
			writeAddress(Address, C),
			C;
		[#addrcluster{cluster = Cluster}] ->
			Cluster
	end.

%write the modded clusters out to the database
writeTxClusters(TxListMod) ->
	F = fun() ->
		writeTxClustersMnesia(TxListMod)
	end,
	mnesia:transaction(F).

%include the mnesia write transaction a function for a transaction
writeTxClustersMnesia([]) -> ok;
writeTxClustersMnesia([TxCluster | TxClusterMore]) ->
	mnesia:write(TxCluster),
	writeTxClustersMnesia(TxClusterMore).

%add a transaction to the transaction table
addOutTransaction(InAddress, OutAddress, Value, Hash, OutIndex) ->
	%add the inaddress and outaddress to the db
	%{InCluster, NewCurrentMax1} = addAddressGroup([InAddress], CurrentMax),
	%{OutCluster, NewCurrentMax2} = addAddressGroup([OutAddress], NewCurrentMax1),
	writeTransaction(InAddress, OutAddress, Value, Hash, OutIndex).

%writeTransactions([]) -> ok;
%writeTransactions([#tx{incluster = InC, outcluster = OutC, value = V} | TxS]) ->
%	writeTransaction(InC, OutC, V),
%	writeTransactions(TxS).

writeTransaction(InCluster, OutCluster, Value, Hash, OutIndex) ->
	%F = fun() ->
	%	mnesia:write(#tx{
	%		incluster = InCluster,
	%		outcluster = OutCluster,
	%		value = Value
	%	})
	%end,
	%{atomic, Result} = mnesia:transaction(F),
	%Result.
	mnesia:dirty_write(#tx{
			hash = {Hash, OutIndex},
			incluster = InCluster,
			outcluster = OutCluster,
			value = Value
	}).

deleteTransactions([]) -> ok;
deleteTransactions(Tx) when is_list(Tx) ->
	F = fun() ->
		deleteTransactionMnesia(Tx)
	end,	
	{atomic, Result} = mnesia:transaction(F),
	Result.

deleteTransactionMnesia([]) -> ok;
deleteTransactionMnesia([#tx{incluster = InC, outcluster = OutC, value = V} | TxS]) ->
	mnesia:delete_object(#tx{
		incluster = InC,
		outcluster = OutC,
		value =V 
	}),
	deleteTransactionMnesia(TxS).

%delete an individual transaction.
deleteTransaction(InCluster, OutCluster, Value) ->
	F = fun() ->
		mnesia:delete_object(#tx{
			incluster = InCluster,
			outcluster = OutCluster,
			value = Value
		})
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

%Get a list of existing clusters for the passed address list
getExistingClusters(Addresses) ->
	F = fun() ->
		qlc:eval(qlc:q([#addrcluster{address = Addr, cluster = Cluster} 
			|| #addrcluster{address = Addr, cluster = Cluster} <- mnesia:table(addrcluster), 
			B <- Addresses,
			Addr =:= B]))
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

%get start cluster from the DB on opening. only needs to be done once
getStartCluster() ->
	mnesia:table_info(addrcluster, size) + 1.

%get a list of clusters that should be merged.
% the resulting list will be sorted with the first element being the group where the remaining clusters
% should be merged into.
getMergeGroups(Addresses) ->
	Q = lists:sort(ebc_util:removeDups([Cluster || #addrcluster{cluster = Cluster} <- Addresses])),
	Q.

% Auxillary function to manage the merging of the groups
doMerge([]) -> {error, no_groups};
doMerge([X | Xs]) ->
	doMergeCluster(X, Xs),
	X.

% Merge a list of clusters into a specified cluster group
doMergeCluster(_NewCluster, []) -> ok;	
doMergeCluster(NewCluster, [ClusterGroup | ClusterGroups]) ->
	mergeExistingClusters(NewCluster, ClusterGroup),
	doMergeCluster(NewCluster, ClusterGroups).

% Merge the transactions
%mergeTransactions(NewCluster, OldCluster) ->
%	F1 = fun() ->
%		qlc:eval(qlc:q([#tx{incluster = InC, outcluster = OutC, value = V}
%			|| #tx{incluster = InC, outcluster = OutC, value = V} <- mnesia:table(tx),
%			InC =:= OldCluster]))
%	end,
%	F2 = fun() ->
%		qlc:eval(qlc:q([#tx{incluster = InC, outcluster = OutC, value = V}
%			|| #tx{incluster = InC, outcluster = OutC, value = V} <- mnesia:table(tx),
%			OutC =:= OldCluster]))
%	end,
%	{atomic, Result1} = mnesia:transaction(F1),
%	{atomic, Result2} = mnesia:transaction(F2),
%
%	MResult1 = [#tx{incluster = NewCluster, outcluster = OutC, value = V}
%			|| #tx{outcluster = OutC, value = V} <- Result1],
%	MResult2 = [#tx{outcluster = NewCluster, incluster = InC, value = V}
%			|| #tx{incluster = InC, value = V} <- Result2],
%
%	TxList = ebc_util:removeDups(Result1 ++ Result2),
%	ModTxList = ebc_util:removeDups(MResult1 ++ MResult2),
%
%	deleteTransactions(TxList),
%	writeTransactions(ModTxList).

% Perform the merge in the database
mergeExistingClusters(NewCluster, OldCluster) ->
	io:format("[~p] Merging Cluster ~p->~p~n", [self(), OldCluster, NewCluster]),
	F = fun() ->
		qlc:eval(qlc:q([Addr
			|| #addrcluster{address = Addr, cluster = Cluster} <- mnesia:table(addrcluster),
			Cluster =:= OldCluster]))
	end,
	{atomic, Result} = mnesia:transaction(F),
	writeAddresses(Result, NewCluster).
	%mergeTransactions(NewCluster, OldCluster).

%write a list of addresses and clusters to the database
writeAddresses([], _Cluster) -> ok;
writeAddresses(Addresses, Cluster) when is_list(Addresses) ->
	F = fun() -> writeAddressMnesia(Addresses, Cluster) end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

%perform a write to the database
writeAddressMnesia([], _Cluster) -> ok;
writeAddressMnesia([Address | MoreAddresses], Cluster) ->
	mnesia:write(#addrcluster{
		address = Address,
		cluster = Cluster
	}),
	writeAddressMnesia(MoreAddresses, Cluster).


writeAddress(Address, Cluster) ->
	F = fun() ->
		mnesia:write(#addrcluster{
			address = Address,
			cluster = Cluster
		})
	end,
	mnesia:activity(transaction, F).

addBlockHash(BlockHash) ->
	F = fun() ->
		mnesia:write(#blocklog{
			blockhash = BlockHash,
			firstseen = ebc_util:epoch()
		})
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

removeBlockHash(BlockHash) ->
	case containsBlockHash(BlockHash) of
		true ->
			F = fun() ->
				mnesia:delete(
					blocklog,
					BlockHash)
			end,
			{atomic, Result} = mnesia:transaction(F),
			Result;
		false ->
			ok
	end.		

containsBlockHash(BlockHash) ->
	F = fun() ->
		case mnesia:read({blocklog, BlockHash}) of
			[] -> false;
			_X -> true
		end
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

showBlockStats(StartTime, Now, BlockCount) ->
	showBlockStats(timer:now_diff(Now, StartTime), BlockCount).

showBlockStats(TimeDiff, BlockCount) when TimeDiff >= 60000000 ->
	BlocksPerSec = BlockCount/(TimeDiff/1000000),
	io:format("Blocks per Second: ~p~n", [BlocksPerSec]),
	{now(), 0};
showBlockStats(TimeDiff, BlockCount) ->
	{TimeDiff, BlockCount}.

%% Functions to output the DOT file. starting at a given cluster
getOriginatingTx(ClusterNumber) ->
	F = fun() ->
		qlc:eval(qlc:q([Tx
			|| #txcluster{incluster = InC} = Tx <- mnesia:table(txcluster), InC =:= ClusterNumber]))
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

getReceivingTx(ClusterNumber) ->
	F = fun() ->
		qlc:eval(qlc:q([Tx
			|| #txcluster{outcluster = OutC} = Tx <- mnesia:table(txcluster), OutC =:= ClusterNumber]))
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.
	

outputGraphForTxClusterList([]) ->
	ok;
outputGraphForTxClusterList([#txcluster{incluster = InC, outcluster = OutC} | MoreTx]) ->
	io:format(" ~p -> ~p;~n", [InC, OutC]),
	outputGraphForTxClusterList(MoreTx).
			
%outputGraphForCluster(ClusterNumber) ->
	
