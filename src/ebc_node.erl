%% @author David Ellefsen <davidellefsen@gmail.com> 
%%
%% @doc ebc_node is the management module that can be used to send and receive
%% messages that are passed by nodes in the bitcoin network.
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

-module(ebc_node).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile([export_all]).

-include("ebc_node.hrl").


-record(msg_payload_inv, {
			count = undefined,
			inventory = undefined			  
		}).

-record(msg_payload_getdata, {
			count = undefined,
			inventory = undefined			  
		}).

-record(msg_payload_notfound, {
			count = undefined,
			inventory = undefined			  
		}).

-record(msg_payload_addr, {
			count = undefined,
			addr_list = undefined
		}).

-record(msg_header, {
			magic = undefined,
			command = undefined,
			length = undefined,
			checksum = undefined,
			payload = undefined
		}).

init(Address, Port, Callback) ->
	case create_socket(Address, Port) of
		{peer, Socket} ->
			case initPeerHandshake({peer, Socket}) of
				{ok, Version} ->
					Resp = #ebc_client_state{
						  address = Address,
						  port = Port,
						  socket = Socket,
						  connected = ebc_util:epoch(),
						  lastseen = ebc_util:epoch(),
						  version = Version#msg_header.payload,
						  sendPid = undefined,
						  recvPid = spawn(?MODULE, recvLoop, [Socket, Callback]),
						  callback = Callback
					},
					sendCommand(Socket, getaddr, []),
					Resp;
				Error -> Error
			end;
		E -> E
	end.

close(#ebc_client_state{socket = Socket} = State) ->
	gen_tcp:close(Socket),
	{ok, State#ebc_client_state{socket = undefined}}.

create_socket(Host, Port) when is_integer(Port) ->
	case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}, {recbuf, 10485760}]) of
		{ok, Socket}-> {peer, Socket};
		_ -> {error, cannot_connect_to_host}
	end.

initPeerHandshake({peer, Socket}) ->
	%send the version message to the peer
	{ok, LocalPort} = inet:port(Socket),
	VersionMessage = versionMessage({"192.168.2.100", LocalPort}, {"192.168.2.3", 18333}),
	io:format("Sending Version~n"),
	case gen_tcp:send(Socket, VersionMessage) of
		ok ->
			case gen_tcp:recv(Socket, 0) of 
				{ok, Packet} ->
					%Process the response from the peer, it should contain a version and verack packet
					DecodedMessages = processPayload(Packet, []),
					initPeerHandshakeS2(Socket, DecodedMessages);
					
				_ -> {error, handshake_incomplete}
			end;
		_ -> {error, handshake_incomplete}
	end.

%second stage of the handshack, recieve a verack
initPeerHandshakeS2(Socket, [#msg_header{command= <<"version">> }] = Payload) ->
	io:format("Version recieved, waiting for verack~n"),
	case gen_tcp:recv(Socket, 0) of 
				{ok, Packet} ->
					%Process the response from the peer, it should contain a version and verack packet
					DecodedMessages = processPayload(Packet, []),
					initPeerHandshakeS2(Socket, Payload ++ DecodedMessages);
					
				_ -> {error, handshake_incomplete}
	end;
initPeerHandshakeS2(Socket, [#msg_header{command= <<"version">> } = Version, #msg_header{command= <<"verack">> } = _Verack | _Other]) ->
	io:format("Version and verack recieved in the correct order, reply with my verack message~n"),
	VerackMessage = verackMessage(),
	case gen_tcp:send(Socket, VerackMessage) of
		ok -> 
			io:format("Verack sent~n"),
			{ok, Version}; %TODO: Do something with other packets
		_ -> {error, verack_not_recieved}
	end.
	
	
%% ====================================================================
%% Internal functions
%% ====================================================================

recvLoop(Socket, Callback) ->
	case getPayload(Socket) of
		{ok, Packet} ->
			doCallback(Socket, Callback, processPayload(Packet,[]));
		_ -> ok
	end,
	recvLoop(Socket, Callback).

doCallback(_Socket, _Callback, []) ->
	ok;
doCallback(Socket, Callback, [#msg_header{command = <<"addr">>, payload=AddrPayload} | MorePayload]) ->
	%io:format("Recieved Addr~n"),
	doFunCallback(Socket, Callback, AddrPayload#msg_payload_addr.addr_list),
	doCallback(Socket, Callback, MorePayload);
doCallback(Socket, Callback, [#msg_header{command = <<"inv">>, payload=InvPayload} | MorePayload]) ->
	%io:format("Received Inv~n"),
	doFunCallback(Socket, Callback, InvPayload#msg_payload_inv.inventory),
	doCallback(Socket, Callback, MorePayload);
doCallback(Socket, Callback, [#msg_header{command = <<"tx">>, payload=TxPayload} | MorePayload]) ->
	%io:format("Received Tx payload~n"),
	doFunCallback(Socket, Callback, TxPayload),
	doCallback(Socket, Callback, MorePayload);
doCallback(Socket, Callback, [#msg_header{command = <<"block">>, payload=BlockPayload} | MorePayload]) ->
	%io:format("Received Block~n"),
	doFunCallback(Socket, Callback, BlockPayload),
	doCallback(Socket, Callback, MorePayload);
doCallback(Socket, Callback, [#msg_header{command = <<"ping">>, payload=Nonce} | MorePayload]) ->
	%io:format("Received Ping~n"),
	sendCommand(Socket, ping, binary:decode_unsigned(Nonce)),
	doCallback(Socket, Callback, MorePayload);
doCallback(Socket, Callback, [#msg_header{command = <<"pong">>, payload=Nonce} | MorePayload]) ->
	io:format("Received Pong: ~p~n", [Nonce]),
	%TODO: do something with the pong
	doCallback(Socket, Callback, MorePayload);
doCallback(Socket, Callback, [#msg_header{command = <<"headers">>, payload=HeadersPayload} | MorePayload]) ->
	io:format("Received Headers~n"),
	doFunCallback(Socket, Callback, HeadersPayload),
	doCallback(Socket, Callback, MorePayload);
doCallback(Socket, Callback, [Msg | MorePayload]) ->
	io:format("Recieved: ~p~n", [Msg]),
	doCallback(Socket, Callback, MorePayload).

doFunCallback(_Socket, _F, []) ->
	ok;
doFunCallback(Socket, F, [#net_addr{} = NetAddr | MoreAddr]) ->
	F:addr(NetAddr),
	doFunCallback(Socket, F, MoreAddr);
doFunCallback(Socket, F, [#inv_vect{} = IV | MoreIV]) ->
	F:inv(IV, Socket),
	doFunCallback(Socket, F, MoreIV);
doFunCallback(_Socket, F, #tx{} = Tx) ->
	F:tx(Tx),
	ok;
doFunCallback(_Socket, F, #block{} = Block) ->
	F:block(Block),
	ok;
doFunCallback(_Socket, F, [#block_header{} | _MoreHeader] = BlockHeaders) ->
	F:block_header(BlockHeaders);
doFunCallback(_Socket, _F, _Payload) ->
	?DGB("I DID NOTHING WITH THIS CALLBACK??~n", []),
	ok. %do nothing 

calculateChecksum([]) -> calculateChecksum(<<>>);	
calculateChecksum(Payload) ->
	<<Checksum:32, _Rest/binary>> = cryptopp:sha256(cryptopp:sha256(Payload)),
	Checksum.

getCommand(Str) ->
	Padding = 96 - (string:len(Str)*8),
	BinCommand = list_to_binary(Str),
	<<BinCommand/binary, 0:Padding>>.

varString([]) -> <<0>>;
varString(Str) ->
	Len = string:len(Str),
	{_Bytes,Size} = if 
			   Len < 16#FD -> {1,<<Len:8/little>>};
			   Len =< 16#FFFF -> {3, <<16#FD/little, Len:16/little>>};
			   Len =< 16#FFFFFFFF -> {5, <<16#FE/little, Len:32/little>>};
			   Len > 16#FFFFFFFF -> {9, <<16#FF/little, Len:64/little>>};
			   true -> {0, <<0>>}
		   end,
	B1 = list_to_binary(Str),
	<<Size/binary, B1/binary>>.

netAddress(ipv4, Address, Port) ->
	{ok, {O1,O2,O3,O4}} = inet:parse_ipv4_address(Address),
	Services = 0,
	<<Services:64/little, 16#FFFF:96, O1:8, O2:8, O3:8, O4:8, Port:16>>.

createHeader(Command, PayloadSize, Checksum) ->
	ProcessedCommand = getCommand(Command),
	<<?TESTNET_MAGIC:32/little, ProcessedCommand/binary, PayloadSize:32/little, Checksum:32>>.

verackMessage() ->
	createHeader("verack", 0, calculateChecksum("")).

versionMessage({MyAddress, MyPort}, {PeerAddress, PeerPort}) ->
	Version = 70001,
	Services = 0,
	Timestamp = ebc_util:epoch(),
	PeerNetAddress = netAddress(ipv4, PeerAddress, PeerPort),
	MyNetAddress = netAddress(ipv4, MyAddress, MyPort),
	Nonce = random:uniform(16#FFFFFFFFFFFFFFFF),
	VersionString = varString("/EBitCoin/"),
	StartingHeight = 0,
	Relay = 1,
	Payload = <<Version:32/little, Services:64/little, Timestamp:64/little, PeerNetAddress/binary, MyNetAddress/binary, Nonce:64/little, VersionString/binary, StartingHeight:32/little, Relay>>,
	
	Header = createHeader("version", size(Payload), calculateChecksum(Payload)),
	<<Header/binary, Payload/binary>>.

decodeVarString(Size, Payload) ->
	<<Content:Size/little-binary, RestPayload/binary>> = Payload,
	{Content, RestPayload}.

decodeVarString(<<16#FD, Size:16/little, Payload/binary>>) ->
	decodeVarString(Size, Payload);
decodeVarString(<<16#FE, Size:32/little, Payload/binary>>) ->
	decodeVarString(Size, Payload);													   
decodeVarString(<<16#FF, Size:64/little, Payload/binary>>) ->
	decodeVarString(Size, Payload);
decodeVarString(<<Size:8, Payload/binary>>) ->
	decodeVarString(Size, Payload).

decodeVarInt(<<16#FD, Size:16/little, Payload/binary>>) ->
	{Size, Payload};
decodeVarInt(<<16#FE, Size:32/little, Payload/binary>>) ->
	{Size, Payload};													   
decodeVarInt(<<16#FF, Size:64/little, Payload/binary>>) ->
	{Size, Payload};
decodeVarInt(<<Size:8, Payload/binary>>) ->
	{Size, Payload}.

encodeVarInt(Num) when Num < 16#FD ->
	<<Num:8>>;
encodeVarInt(Num) when Num =< 16#FFFF ->
	<<16#FD:8, Num:16/little>>;
encodeVarInt(Num) when Num =< 16#FFFFFFFF ->
	<<16#FE:8, Num:32/little>>;
encodeVarInt(Num) when Num > 16#FFFFFFFF ->
	<<16#FF:8, Num:64/little>>.

decodeNetAddress(ipv4, Payload) ->
	<<Services:64/little, _IPv6Padding:96, O1:8, O2:8, O3:8, O4:8, Port:16>> = Payload,
	#net_addr{
			  services = Services,
			  ip = {O1, O2, O3, O4},
			  port = Port
			  }.

decodeFullNetAddress(ipv4, Payload) ->
	<<Time:32/little, Services:64/little, _IPv6Padding:96, O1:8, O2:8, O3:8, O4:8, Port:16>> = Payload,
	#net_addr{
			  time = Time,
			  services = Services,
			  ip = {O1, O2, O3, O4},
			  port = Port
			  }.

decodeMultipleNetAddresses(0, _Payload) -> [];
decodeMultipleNetAddresses(Count, <<NetAddress:30/binary, Payload/binary>>) ->
	[decodeFullNetAddress(ipv4, NetAddress) | decodeMultipleNetAddresses(Count-1, Payload)].

decodeInventory(0, _Payload) -> [];
decodeInventory(Count, <<Type:32/little, Hash:32/binary, Rest/binary>>) ->
	[#inv_vect{type = Type, hash = Hash} | decodeInventory(Count-1, Rest)].

encodeInventoryVect(#inv_vect{type = Type, hash = Hash}) ->
	<<Type:32/little, Hash:32/binary>>.

encodeInventoryVects([], Payload, Count) -> 
	EncCount = encodeVarInt(Count),
	<<EncCount/binary, Payload/binary>>;
encodeInventoryVects([#inv_vect{} = IV | MoreIV], Payload, Count) ->
	MyPayload = encodeInventoryVect(IV),
	encodeInventoryVects(MoreIV, <<Payload/binary, MyPayload/binary>>, Count+1).

encodeInventory(IVs) when is_list(IVs) ->
	encodeInventoryVects(IVs, <<>>, 0);
encodeInventory(#inv_vect{} = IV) ->
	Count = encodeVarInt(1),
	Payload = encodeInventoryVect(IV),
	<<Count/binary, Payload/binary>>.

decodeTxIn(0, Payload, Trans) -> {ok, Payload, Trans};
decodeTxIn(Count, Payload, Trans) ->
	<<PreviousOutput:32/binary, TxOutIndex:32/little, Payload1/binary>> = Payload,
	{ScriptLength, Payload2} = decodeVarInt(Payload1),
	<<Script:ScriptLength/binary, Sequence:32/little, Rest/binary>> = Payload2,
	decodeTxIn(Count-1, Rest,
		Trans ++ [#tx_in{
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
		Trans ++ [#tx_out{
		 value = Value,
		 pk_script_length = PkScriptLength,
		 pk_script = PkScript
	 }]).

decodeTransactions(_TransactionPayload, 0) -> [];
decodeTransactions(TransactionPayload, TransCount) ->
	{Tx, NewTransPayload} = decodeTxPayload(<<"tx">>, TransactionPayload),
	[Tx | decodeTransactions(NewTransPayload, TransCount-1)].

decodeTxPayload(<<"tx">>, Payload) ->
	<<Version:32/little, Payload1/binary>> = Payload,
	{TxInCount, Payload2} = decodeVarInt(Payload1),
	{ok, Payload3, TxIn} = decodeTxIn(TxInCount, Payload2, []),
	{TxOutCount, Payload4} = decodeVarInt(Payload3),
	{ok, Payload5, TxOut} = decodeTxOut(TxOutCount, Payload4, []),	
	<<LockTime:32/little, Rest/binary>> = Payload5,
	HeaderSize = size(Payload) - size(Rest),
	<<HeaderPayload:HeaderSize/binary, _MorePayload/binary>> = Payload,

	{#tx{
		hash = ebc_util:reverseBinary(cryptopp:sha256(cryptopp:sha256(HeaderPayload))),
		version = Version,
		tx_in_count = TxInCount,
		tx_in = TxIn,
		tx_out_count = TxOutCount,
		tx_out = TxOut,
		lock_time = LockTime,
		payload = <<>> %todo - include the raw payload here
	}, Rest}.

decodeHeaders(_HeadersPayload, 0) -> [];
decodeHeaders(HeadersPayload, HeaderCount) ->
	<<HeaderPayload:80/binary, _TxCount:8, RestPayload/binary>> = HeadersPayload,
	<<Version:32/little, HashPrevBlock:32/binary, HashMerkleRoot:32/binary, Time:32/little, Bits:32/little, Nonce:32/little, _Rest/binary>> = HeaderPayload,
	[#block_header{
		hash = cryptopp:sha256(cryptopp:sha256(HeaderPayload)),
		version = Version,
		hashPrevBlock = HashPrevBlock,
		hashMerkleRoot = HashMerkleRoot,
		time = Time,
		bits = Bits,
		nonce = Nonce
	} | decodeHeaders(RestPayload, HeaderCount-1)].

decodePayload(<<"block">>, Payload) ->
	<<HeaderPayload:80/binary, BlockPayloadRest/binary>> = Payload,
	<<Version:32/little, HashPrevBlock:32/binary, HashMerkleRoot:32/binary, Time:32/little, Bits:32/little, Nonce:32/little>> = HeaderPayload,
	{TransCount, TransactionsPayload} = decodeVarInt(BlockPayloadRest),
	HeaderHash = cryptopp:sha256(cryptopp:sha256(HeaderPayload)),
	#block {
		hash = HeaderHash,
		header = #block_header{
			hash = HeaderHash,
			version = Version,
			hashPrevBlock = HashPrevBlock,
			hashMerkleRoot = HashMerkleRoot,
			time = Time,
			bits = Bits,
			nonce = Nonce
		},
		trans = TransCount,
		transactions = decodeTransactions(TransactionsPayload, TransCount)
	};

decodePayload(<<"ping">>, Payload) ->
	<<Nonce:64/little>> = Payload,
	Nonce;

decodePayload(<<"pong">>, Payload) ->
	<<Nonce:64/little>> = Payload,
	Nonce;

decodePayload(<<"headers">>, Payload) ->
	{Count, Payload2} = decodeVarInt(Payload),
	decodeHeaders(Payload2, Count);

decodePayload(<<"tx">>, Payload) ->
	<<Version:32/little, Payload1/binary>> = Payload,
	{TxInCount, Payload2} = decodeVarInt(Payload1),
	{ok, Payload3, TxIn} = decodeTxIn(TxInCount, Payload2, []),
	{TxOutCount, Payload4} = decodeVarInt(Payload3),
	{ok, Payload5, TxOut} = decodeTxOut(TxOutCount, Payload4, []),	
	<<LockTime:32/little, Rest/binary>> = Payload5,
	HeaderSize = size(Payload) - size(Rest),
	<<HeaderPayload:HeaderSize/binary, _MorePayload/binary>> = Payload,

	#tx{
		hash = ebc_util:reverseBinary(cryptopp:sha256(cryptopp:sha256(HeaderPayload))),
		version = Version,
		tx_in_count = TxInCount,
		tx_in = TxIn,
		tx_out_count = TxOutCount,
		tx_out = TxOut,
		lock_time = LockTime,
		payload = Payload
	};

decodePayload(<<"inv">>, Payload) ->
	{Count, MorePayload} = decodeVarInt(Payload),
	#msg_payload_inv{
		 count = Count,
		 inventory = decodeInventory(Count, MorePayload)
	 };

decodePayload(<<"getdata">>, Payload) ->
	{Count, MorePayload} = decodeVarInt(Payload),
	#msg_payload_getdata{
		 count = Count,
		 inventory = decodeInventory(Count, MorePayload)
	 };

decodePayload(<<"notfound">>, Payload) ->
	{Count, MorePayload} = decodeVarInt(Payload),
	#msg_payload_notfound{
		 count = Count,
		 inventory = decodeInventory(Count, MorePayload)
	 };

decodePayload(<<"addr">>, Payload) ->
	{Count, MorePayload} = decodeVarInt(Payload),
	#msg_payload_addr{
		  count = Count,
		  addr_list = decodeMultipleNetAddresses(Count, MorePayload)
	};

decodePayload(<<"version">>, Payload) ->
	%get the main content and then decode the varstr
	<<Version:32/little, Services:64/little, Timestamp:64/little, AddrRecv:26/little-binary, AddrFrom:26/little-binary, Nonce:64/little, RestPayload/binary>> = Payload,
	{UserAgent, MorePayload} = decodeVarString(RestPayload),
	<<StartHeight:32/little, _RRest/binary>> = MorePayload,
	#version{
		 version = Version,
		 services = Services,
		 timestamp = Timestamp,
		 addr_recv = decodeNetAddress(ipv4, AddrRecv),
		 addr_from = decodeNetAddress(ipv4, AddrFrom),
		 nonce = Nonce,
		 user_agent = UserAgent,
		 start_height = StartHeight
	};

decodePayload(<<"verack">>, _Payload) ->
	[];

decodePayload(_, _Payload) ->
	[].

decodeCommand(<<"version", _Rest/binary>>) ->
	<<"version">>;
decodeCommand(<<"verack", _Rest/binary>>) ->
	<<"verack">>;
decodeCommand(<<"addr", _Rest/binary>>) ->
	<<"addr">>;
decodeCommand(<<"block", _Rest/binary>>) ->
	<<"block">>;
decodeCommand(<<"tx", _Rest/binary>>) ->
	<<"tx">>;
decodeCommand(<<"inv", _Rest/binary>>) ->
	<<"inv">>;
decodeCommand(<<"getdata", _Rest/binary>>) ->
	<<"getdata">>;
decodeCommand(<<"notfound", _Rest/binary>>) ->
	<<"notfound">>;
decodeCommand(<<"ping", _Rest/binary>>) ->
	<<"ping">>;
decodeCommand(<<"pong", _Rest/binary>>) ->
	<<"pong">>;
decodeCommand(<<"headers", _Rest/binary>>) ->
	<<"headers">>;
decodeCommand(_) ->
	<<"unknown">>.

retrieveRemainderPayload(Socket) ->
	%io:format("Receiving More Packet Data~n"),
	case gen_tcp:recv(Socket, 0, 2000) of
		{ok, Packet} ->
			MorePayload = retrieveRemainderPayload(Socket),
			<<Packet/binary, MorePayload/binary>>;
		{error, timeout} -> <<>>
	end.
	
processRemainderofPayload(Socket, (<<?TESTNET_MAGIC:32/little, Command:12/little-binary, _RestPayload/binary>>) = Payload) ->
	io:format("TESTNET Packet Header detected: ~p~n", [binary_to_list(decodeCommand(Command))]),
	MorePayload = retrieveRemainderPayload(Socket),
	{ok, <<Payload/binary, MorePayload/binary>>};
processRemainderofPayload(Socket, (<<?MAIN_MAGIC:32/little, Command:12/little-binary, _RestPayload/binary>>) = Payload) ->
	io:format("MAIN Packet Header detected: ~p~n", [binary_to_list(decodeCommand(Command))]),
	MorePayload = retrieveRemainderPayload(Socket),
	{ok, <<Payload/binary, MorePayload/binary>>};
processRemainderofPayload(_Socket, Payload) ->
	io:format("Packet did not match version~n"),
	{ok, Payload}.

getPayload(Socket) ->
	%io:format("I have been asked to recieve a payload~n"),
	case gen_tcp:recv(Socket, 0, 5000) of
		{ok, Packet} ->
			processRemainderofPayload(Socket, Packet);
		_ -> ok
	end.

getRecordFromPayload(Payload) ->
	 <<Magic:32/little, Command:12/little-binary, PayloadLength:32/little, Checksum:32, RestPayload/binary>> = Payload,
	 <<MyPayload:PayloadLength/binary, MoreContent/binary>> = RestPayload,
	 DecCommand = decodeCommand(Command),
	 {#msg_header{
				 magic = Magic,
				 command = DecCommand,
				 length = PayloadLength,
				 checksum = Checksum,
				 payload = decodePayload(DecCommand, MyPayload)
				 }, MoreContent}.

processPayload(<<>>, X) -> X;
processPayload(Payload, X) ->
	{Packet, MorePayload} = getRecordFromPayload(Payload),
	processPayload(MorePayload, X ++ [Packet]).

%************* Transaction Functions ***********%
evaluateScriptTxIn(SigScript, PrevOutTxKey) ->
	case script:evaluate(SigScript, []) of
		[_PubKey] -> {address, tx_handler:getTx(PrevOutTxKey)}; %pay to publickey
		[PubKey, _Signature] -> {pubkey, PubKey}; %standard transaction
		_ -> <<>>
	end.

decodeAddressFromTxOutScript(PKScript) ->
	Script = script:decodeScript(PKScript, []),
	Hash160s = lists:filter(fun(X) when is_binary(X) -> size(X) =:= 20; (_X) -> false end, Script),
	PayToScript = lists:filter(fun(X) when is_binary(X) -> size(X) > 20; (_X) -> false end, Script),
	%what about pay to script?
	lists:flatten(
		lists:map(fun(X) -> ebc_util:getBitcoinAddressFromHash160(?TESTNET_PREFIX, X) end, Hash160s) ++
		lists:map(fun(X) -> ebc_util:getBitcoinAddress(?TESTNET_PREFIX, X) end, PayToScript)).

decodeAddressFromTxInScript(SigScript, PrevOutTxKey) ->
	case evaluateScriptTxIn(SigScript, PrevOutTxKey) of
		<<>> -> ignored;
		{pubkey, PubKey} -> ebc_util:getBitcoinAddress(?TESTNET_PREFIX, PubKey);
		{address, Addr} -> Addr; %pay to public key
		_ -> ignored
	end.

transactionOutAmount(Tx, Index) ->
	TxOut = lists:nth(Index+1, Tx#tx.tx_out),
	case TxOut of
		#tx_out{value = Amount, pk_script = PKScript} ->
			Identifier = decodeAddressFromTxOutScript(PKScript),
			{Identifier, Amount};
		_ -> {error, no_input}
	end.

decodeTxInAddr([], _Index) -> [];
decodeTxInAddr([#tx_in{signature_script = SigScript, previous_output = PrevHash, previous_index = PrevIndex} | MoreTxIn], Index) when PrevIndex =/= 16#ffffffff ->
	%?DGB("  PreviousOut: ~p~n", [{decodeAddressFromTxInScript(SigScript, PrevHash), PrevHash, PrevIndex}]),
	[{decodeAddressFromTxInScript(SigScript, PrevHash), PrevHash, Index} | decodeTxInAddr(MoreTxIn, Index+1)];
decodeTxInAddr([#tx_in{signature_script = _SigScript, previous_index = PrevIndex} | MoreTxIn], Index) when PrevIndex =:= 16#ffffffff ->
	[{"0000000000000000000000000000000000", <<0:256>>, Index} | decodeTxInAddr(MoreTxIn, Index+1)].

decodeTxOutAddr([], _Index) -> [];
decodeTxOutAddr([#tx_out{pk_script = PKScript, value = Value} | MoreTxOut], Index) ->
	Addr = decodeAddressFromTxOutScript(PKScript),
	[{Addr, Value, Index} | decodeTxOutAddr(MoreTxOut, Index+1)].
	
%************* Encoding Functions **************%

encodePayload(#tx_out{value = Value, pk_script_length = ScriptLength, pk_script = Script} = _TxOut) ->
	Vi_ScriptLength = encodeVarInt(ScriptLength),
	<<Value:64/little, Vi_ScriptLength/binary, Script/binary>>;

encodePayload(#tx_in{previous_output = PreOut, previous_index = PreIndex, script_length = ScriptLength, signature_script = Signature, sequence = Sequence} = _TxIn) ->
	Vi_ScriptLength = encodeVarInt(ScriptLength),
	<<PreOut/binary, PreIndex:32/little, Vi_ScriptLength/binary, Signature/binary, Sequence:32/little>>;

encodePayload(#tx{version = Version, tx_in_count = TxInCount, tx_in = TxIn, tx_out_count = TxOutCount, tx_out = TxOut, lock_time = LockTime} = _Tx) ->
	Vi_TxInCount = encodeVarInt(TxInCount),
	Vi_TxOutCount = encodeVarInt(TxOutCount),
	TxInBin = encodePayloadArray(TxIn),
	TxOutBin = encodePayloadArray(TxOut),
	<<Version:32/little, Vi_TxInCount/binary, TxInBin/binary, Vi_TxOutCount/binary, TxOutBin/binary, LockTime:32/little>>.

encodePayloadArray([]) -> <<>>;
encodePayloadArray([Payload | MorePayload]) when is_tuple(Payload) ->
	PayloadBin = encodePayload(Payload),
	MorePayloadBin = encodePayload(MorePayload),
	<<PayloadBin/binary, MorePayloadBin/binary>>.

%************* Sending Commands ****************%
sendCommand(Socket, getaddr, []) ->
	gen_tcp:send(Socket, constuctAddrMessage());
sendCommand(Socket, getdata, Inventory) when is_list(Inventory) ->
	gen_tcp:send(Socket, constructGetDataMessage(Inventory));
sendCommand(Socket, pong, Nonce) ->
	gen_tcp:send(Socket, constructPingMessage("pong", Nonce));
sendCommand(Socket, ping, Nonce) ->
	gen_tcp:send(Socket, constructPingMessage("ping", Nonce));	%%when pinging send a random 64-bit nonce
sendCommand(Socket, getblocks, StartBlockHash) ->
	gen_tcp:send(Socket, constructGetBlocksMessage(StartBlockHash));
sendCommand(Socket, getheaders, StartBlockHash) ->
	gen_tcp:send(Socket, constructGetHeadersMessage(StartBlockHash)).


constructGetBlocksMessage(StartBlockHash) ->
	HashLocatorEntries = encodeVarInt(1),
	Payload = <<70001:32/little, HashLocatorEntries/binary, StartBlockHash/binary, 0:256/little>>,
	Header = createHeader("getblocks", byte_size(Payload), calculateChecksum(Payload)),
	<<Header/binary, Payload/binary>>.

constructGetHeadersMessage(StartBlockHash) ->
	HashLocatorEntries = encodeVarInt(1),
	Payload = <<70001:32/little, HashLocatorEntries/binary, StartBlockHash/binary, 0:256/little>>,
	Header = createHeader("getheaders", byte_size(Payload), calculateChecksum(Payload)),
	<<Header/binary, Payload/binary>>.

constuctAddrMessage() ->
	createHeader("getaddr", 0, calculateChecksum("")).

constructGetDataMessage(Inventory) ->
	Payload = encodeInventory(Inventory),
	Header = createHeader("getdata", byte_size(Payload), calculateChecksum(Payload)),
	<<Header/binary, Payload/binary>>.

constructPingMessage(Type, Nonce) ->
	Payload = <<Nonce:64/little>>,
	Header = createHeader(Type, byte_size(Payload), calculateChecksum(Payload)),
	<<Header/binary, Payload/binary>>.
	
