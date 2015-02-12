%% @author David Ellefsen <davidellefsen@gmail.com> 
%%
%% @doc ebc_block_file is an auxiliary module that can process and parse blocks from
%% bitcoin block files. This module is used in conjunction with addrcluster_handler.
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

-module(ebc_block_file).

%-define(DGB(Str, Args), io:format(Str, Args)).
-define(DGB(Str, Args), ok).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile([export_all]).

-record(tx_block_in, {
			previous_output = undefined,
			previous_index = undefined,
			script_length = undefined,
			signature_script = undefined,
			sequence = undefined
		}).

-record(tx_block_out, {
			value = undefined,
			pk_script_length = undefined,
			pk_script = undefined
		}).

-record(msg_payload_tx, {
			hash = undefined,
			version = undefined,
			tx_in_count = undefined,
			tx_in = undefined,
			tx_out_count = undefined,
			tx_out = undefined,
			lock_time = undefined
	   }).

-record(block_header, {
		hash = undefined,
		version = undefined,
		hashPrevBlock = undefined,
		hashMerkleRoot = undefined,
		time = undefined,
		bits = undefined,
		nonce = undefined
	}).

-record(block, {
		magic = undefined, % 4 bytes
		length = undefined, % 4 bytes
		header = undefined, % 80 bytes
		trans = undefined, % 1-9 varint
		transactions = undefined
	}).

processBlockDir(Dir) ->
	{ok, FileList} = file:list_dir(Dir),
	lists:map(fun (X) -> Dir ++ "/" ++ X end, lists:filter(fun("blk" ++ _R) -> true; (_) -> false end, FileList)).

processEachFileWithCallback([], _C, _Cargs) -> ok;
processEachFileWithCallback([File|Rest], C, Cargs) ->
	parseFileWithCallback(File, C, Cargs),
	processEachFileWithCallback(Rest, C, Cargs).

parseFile(Filename) ->
	{ok, File} = file:open(Filename, [read, binary]),
 	Blocks = parseBlockFromFile(File),
	file:close(File),
	Blocks.

parseFileWithCallback(Filename, C, Cargs) ->
	{ok, File} = file:open(Filename, [read, binary]),
	io:format("[~p] Processing: ~p~n", [self(), Filename]),
	parseBlocksFromFileWithCallback(File, C, Cargs),
	file:close(File),
	ok.

%% Processing Callbacks %%
writeOutNonceCallback(Block, NonceFile) ->
	Nonce = <<(Block#block.header#block_header.nonce):32/little>>,
	UNonce = binary:decode_unsigned(Nonce, little),
	file:write(NonceFile, binary:encode_unsigned(erl_popcnt:popcnt(UNonce), little)),
	[NonceFile].

printNonceCallback(Block) ->
	Nonce = <<(Block#block.header#block_header.nonce):32/little>>,
	?DGB("Nonce: ~p~n", [binary:decode_unsigned(Nonce)]),
	[].

printNonceFileCallback(Block, OutFile) ->
	Nonce = <<(Block#block.header#block_header.nonce):32/little>>,
	file:write(OutFile, io_lib:format("Nonce ~p~n", [binary:decode_unsigned(Nonce, little)])),
	[OutFile].

evaluateScriptTxIn(SigScript, PrevOutTxKey) ->
	case script:evaluate(SigScript, []) of
		[_PubKey] -> {address, addrcluster_handler:getAddressFromTransaction(PrevOutTxKey)};
		[PubKey, _Signature] -> {pubkey, PubKey};
		_ -> <<>>
	end.

decodeAddressFromTxInScript(SigScript, PrevOutTxKey) ->
	case evaluateScriptTxIn(SigScript, PrevOutTxKey) of
		<<>> -> ignored;
		{pubkey, PubKey} -> ebc_util:getBitcoinAddress(0, PubKey);
		{address, Addr} -> Addr;
		_ -> ignored
	end.

decodeAddressFromTxOutScript(PKScript) ->
	Script = script:decodeScript(PKScript, []),
	Hash160s = lists:filter(fun(X) when is_binary(X) -> size(X) =:= 20; (_X) -> false end, Script),
	PayToScript = lists:filter(fun(X) when is_binary(X) -> size(X) > 20; (_X) -> false end, Script),
	%what about pay to script?
	lists:flatten(
		lists:map(fun(X) -> ebc_util:getBitcoinAddressFromHash160(0, X) end, Hash160s) ++
		lists:map(fun(X) -> ebc_util:getBitcoinAddress(0, X) end, PayToScript)).

decodeTxInAddr([]) -> [];
decodeTxInAddr([#tx_block_in{signature_script = SigScript, previous_output = PrevHash, previous_index = PrevIndex} | MoreTxIn]) when PrevIndex =/= 16#ffffffff ->
	?DGB("  PreviousOut: ~p~n", [string:to_lower(cryptopp:hex_dump(ebc_util:reverseBinary(PrevHash)))]),
	[decodeAddressFromTxInScript(SigScript, {string:to_lower(cryptopp:hex_dump(ebc_util:reverseBinary(PrevHash))), PrevIndex}) | decodeTxInAddr(MoreTxIn)];
decodeTxInAddr([#tx_block_in{signature_script = _SigScript, previous_index = PrevIndex} | MoreTxIn]) when PrevIndex =:= 16#ffffffff ->
	["0000000000000000000000000000000000" | decodeTxInAddr(MoreTxIn)].

decodeTxOutAddr([], _Index) -> [];
decodeTxOutAddr([#tx_block_out{pk_script = PKScript, value = Value} | MoreTxOut], Index) ->
	Addr = decodeAddressFromTxOutScript(PKScript),
	[{Addr, Value, Index} | decodeTxOutAddr(MoreTxOut, Index+1)].

printTxHash([]) -> ok;
printTxHash([#msg_payload_tx{hash = Hash, tx_in = TxIn} | MoreTx]) ->
	?DGB("TxID: ~p~n", [string:to_lower(cryptopp:hex_dump(Hash))]),
	?DGB(" Addresses: ~p~n", [ebc_util:removeDups(decodeTxInAddr(TxIn))]),
	printTxHash(MoreTx).

printTxOutHash([]) -> ok;
printTxOutHash([#msg_payload_tx{hash = Hash, tx_out = TxOut} | MoreTx]) ->
	?DGB("TxID: ~p~n", [string:to_lower(cryptopp:hex_dump(Hash))]),
	?DGB(" OutAddresses: ~p~n", [ebc_util:removeDups(decodeTxOutAddr(TxOut, 0))]),
	printTxOutHash(MoreTx).

printTransactionHashCallback(Block) ->
	Tx = Block#block.trans,
	Transactions = Block#block.transactions,
	Hash = string:to_lower(cryptopp:hex_dump(Block#block.header#block_header.hash)),
	?DGB("TxCount: ~p    Block Hash: ~p~n", [Tx, Hash]),
	printTxHash(Transactions),
	[].

printTransactionOutHashCallback(Block) ->
	Tx = Block#block.trans,
	Transactions = Block#block.transactions,
	Hash = string:to_lower(cryptopp:hex_dump(Block#block.header#block_header.hash)),
	?DGB("TxCount: ~p    Block Hash: ~p~n", [Tx, Hash]),
	printTxOutHash(Transactions),
	[].

addOutTransactions(_InAddress, [], _Hash) -> ok;
addOutTransactions(InAddress, [{OutAddress, Value, Index} | MoreOutAddresses], Hash) ->
	addrcluster_handler:addTransaction(InAddress, OutAddress, Value, Hash, Index),
	addOutTransactions(InAddress, MoreOutAddresses, Hash).

clusterTxIn([]) -> ok;
clusterTxIn([#msg_payload_tx{hash = Hash, tx_in = TxIn, tx_out = TxOut} | MoreTx]) ->
	?DGB("TxID: ~p~n", [string:to_lower(cryptopp:hex_dump(Hash))]),
	InAddresses = ebc_util:removeDups(decodeTxInAddr(TxIn)),
	OutAddresses = ebc_util:removeDups(decodeTxOutAddr(TxOut,0)),

	addrcluster_handler:addAddressGroup(InAddresses),
	addOutTransactions(hd(InAddresses), OutAddresses, string:to_lower(cryptopp:hex_dump(Hash))),
	
	clusterTxIn(MoreTx).

clusterCallback(Block) ->
	BlockHash = string:to_lower(cryptopp:hex_dump(Block#block.header#block_header.hash)),
	Tx = Block#block.trans,
	?DGB("[~p] TxCount: ~p    Block Hash: ~p~n", [self(), Tx, BlockHash]),

	case addrcluster_handler:containsBlockHash(BlockHash) of
		false ->
			Transactions = Block#block.transactions,
			clusterTxIn(Transactions),
			addrcluster_handler:addBlockHash(BlockHash),
			[];
		true ->
			[]
	end.


%% Output Functions %%
printNonceFile(BlkDir, OutFileName) ->
	{ok, OutFile} = file:open(OutFileName, [write, binary]),
	FileList = processBlockDir(BlkDir),
	processEachFileWithCallback(FileList, fun printNonceFileCallback/2, [OutFile]),
	file:close(OutFile),
	ok.

writeNonceFile(BlkDir, OutFileName) ->
	{ok, OutFile} = file:open(OutFileName, [write, binary]),
	FileList = processBlockDir(BlkDir),
	processEachFileWithCallback(FileList, fun writeOutNonceCallback/2, [OutFile]),
	file:close(OutFile),
	ok.

printNonce(BlkDir) ->
	FileList = processBlockDir(BlkDir),
	processEachFileWithCallback(FileList, fun printNonceCallback/1, []),
	ok.

printTransactionsIn(BlkDir) ->
	FileList = processBlockDir(BlkDir),
	processEachFileWithCallback(FileList, fun printTransactionHashCallback/1, []),
	ok.

printTransactions(BlkDir) ->
	FileList = processBlockDir(BlkDir),
	processEachFileWithCallback(FileList, fun printTransactionOutHashCallback/1, []),
	ok.

getClusterFunction(FileList) ->
	fun() ->
		?MODULE:processEachFileWithCallback(FileList, fun ?MODULE:clusterCallback/1, [])
	end.

clusterWithProcesses(BlkDir) ->
	init_mnesia(),
	addrcluster_handler:start_link(),
	process_handler:start_link(),

	FileList = processBlockDir(BlkDir),
	{FileListPart1, FileListPart2} = lists:split(length(FileList) div 2, FileList),

	io:format("Part1: ~p~n", [FileListPart2]),
	%function to be executed
	F1 = getClusterFunction(FileListPart1),
	F2 = getClusterFunction(FileListPart2),
	process_handler:startProcess(F1),
	process_handler:startProcess(F2).

cluster(BlkDir) ->
	init_mnesia(),
	addrcluster_handler:start_link(),

	FileList = processBlockDir(BlkDir),
	processEachFileWithCallback(FileList, fun clusterCallback/1, []),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

parseBlockFromFile(File) ->
	{ok, <<Header:32/little>>} = file:read(File, 4),
	case Header of
		16#D9B4BEF9 ->
			{ok, <<BlockSize:32/little>>} = file:read(File, 4),
			{ok, BlockPayload} = file:read(File, BlockSize),
			<<HeaderPayload:80/binary, BlockPayloadRest/binary>> = BlockPayload,
			<<Version:32/little, HashPrevBlock:32/binary, HashMerkleRoot:32/binary, Time:32/little, Bits:32/little, Nonce:32/little>> = HeaderPayload,
			{TransCount, TransactionsPayload} = decodeVarInt(BlockPayloadRest),
			[#block {
				magic = 16#D9B4BEF9,
				length = BlockSize,
				header = #block_header{
					hash = ebc_util:reverseBinary(cryptopp:sha256(cryptopp:sha256(HeaderPayload))),
					version = Version,
					hashPrevBlock = HashPrevBlock,
					hashMerkleRoot = HashMerkleRoot,
					time = Time,
					bits = Bits,
					nonce = Nonce
				},
				trans = TransCount,
				transactions = decodeTransactions(TransactionsPayload, TransCount)
			} | parseBlockFromFile(File)];
		H ->
			{ok, BytePos} = file:position(File, cur),
			io:format("I encounted ~p at ~p~n", [H, BytePos]),
			[]
	end.

parseBlocksFromFileWithCallback(File, C, Cargs) ->
	F = file:read(File, 4),
	case F of
		{ok,  <<16#D9B4BEF9:32/little>>} ->
			{ok, <<BlockSize:32/little>>} = file:read(File, 4),
			{ok, BlockPayload} = file:read(File, BlockSize),
			<<HeaderPayload:80/binary, BlockPayloadRest/binary>> = BlockPayload,
			<<Version:32/little, HashPrevBlock:32/binary, HashMerkleRoot:32/binary, Time:32/little, Bits:32/little, Nonce:32/little>> = HeaderPayload,
			{TransCount, TransactionsPayload} = decodeVarInt(BlockPayloadRest),
			Block = #block {
				magic = 16#D9B4BEF9,
				length = BlockSize,
				header = #block_header{
					hash = ebc_util:reverseBinary(cryptopp:sha256(cryptopp:sha256(HeaderPayload))),
					version = Version,
					hashPrevBlock = HashPrevBlock,
					hashMerkleRoot = HashMerkleRoot,
					time = Time,
					bits = Bits,
					nonce = Nonce
				},
				trans = TransCount,
				transactions = decodeTransactions(TransactionsPayload, TransCount)
			},
			NewCargs = erlang:apply(C, [Block | Cargs]),
			parseBlocksFromFileWithCallback(File, C, NewCargs);
		_H ->
			%{ok, BytePos} = file:position(File, cur),
			%io:format("I encounted ~p at ~p~n", [H, BytePos]),
			ok
	end.


decodeVarInt(<<16#FD, Size:16/little, Payload/binary>>) ->
	{Size, Payload};
decodeVarInt(<<16#FE, Size:32/little, Payload/binary>>) ->
	{Size, Payload};													   
decodeVarInt(<<16#FF, Size:64/little, Payload/binary>>) ->
	{Size, Payload};
decodeVarInt(<<Size:8, Payload/binary>>) ->
	{Size, Payload}.

decodeTxIn(0, Payload, Trans) -> {ok, Payload, Trans};
decodeTxIn(Count, Payload, Trans) ->
	<<PreviousOutput:32/binary, TxOutIndex:32/little, Payload1/binary>> = Payload,
	{ScriptLength, Payload2} = decodeVarInt(Payload1),
	<<Script:ScriptLength/binary, Sequence:32/little, Rest/binary>> = Payload2,
	decodeTxIn(Count-1, Rest,
		Trans ++ [#tx_block_in{
			previous_output = PreviousOutput,
			previous_index = TxOutIndex,
			script_length = ScriptLength,
			signature_script = Script,
			sequence = Sequence
		}]).

decodeTxOut(0, Payload, Trans) -> {ok, Payload, Trans};
decodeTxOut(Count, Payload, Trans) ->
	<<Value:64/little, Payload1/binary>> = Payload,
	{PkScriptLength, Payload2} = decodeVarInt(Payload1),
	<<PkScript:PkScriptLength/binary, Rest/binary>> = Payload2,
	decodeTxOut(Count-1, Rest,
		Trans ++ [#tx_block_out{
		 value = Value,
		 pk_script_length = PkScriptLength,
		 pk_script = PkScript
	 }]).

decodeTransactions(_TransactionPayload, 0) -> [];
decodeTransactions(TransactionPayload, TransCount) ->
	{Tx, NewTransPayload} = decodePayload(<<"tx">>, TransactionPayload),
	[Tx | decodeTransactions(NewTransPayload, TransCount-1)].
	
decodePayload(<<"tx">>, Payload) ->
	<<Version:32/little, Payload1/binary>> = Payload,
	{TxInCount, Payload2} = decodeVarInt(Payload1),
	{ok, Payload3, TxIn} = decodeTxIn(TxInCount, Payload2, []),
	{TxOutCount, Payload4} = decodeVarInt(Payload3),
	{ok, Payload5, TxOut} = decodeTxOut(TxOutCount, Payload4, []),	
	<<LockTime:32/little, Rest/binary>> = Payload5,
	HeaderSize = size(Payload) - size(Rest),
	<<HeaderPayload:HeaderSize/binary, _MorePayload/binary>> = Payload,

	{#msg_payload_tx{
		hash = ebc_util:reverseBinary(cryptopp:sha256(cryptopp:sha256(HeaderPayload))),
		version = Version,
		tx_in_count = TxInCount,
		tx_in = TxIn,
		tx_out_count = TxOutCount,
		tx_out = TxOut,
		lock_time = LockTime
	}, Rest}.

init_mnesia() ->
	application:load(mnesia),
	%ok = application:set_env(mnesia, dir, Directory),
	mnesia:create_schema([node()]),
	mnesia:start().

