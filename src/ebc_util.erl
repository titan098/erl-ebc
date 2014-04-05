%% @author david
%% @doc @todo Add description to ebc_util.

-module(ebc_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([epoch/0, binaryToHex/1, removeDups/1, reverseBinary/1]).
-export([hash160/1, sha256/1]).
-export([convertToBase58/1, getBitcoinAddress/2, getBitcoinAddressFromHash160/2]).

epoch() ->
	UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
	LocalDateTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	LocalDateTime - UnixEpoch.

binaryToHex(X) ->
	A = binary_to_list(X),
	lists:flatten(lists:map(fun hex_char/1, A)).

hex_char(X) ->
	lists:flatten(io_lib:format("~2.16.0b", [X])).

removeDups([]) -> [];
removeDups([X | Xs]) ->
	[X | removeDups(lists:filter(fun(Y) -> Y =/= X end, Xs))].

%% Perform a hash160 on some passed data
hash160(Data) ->
	cryptopp:ripemd160(cryptopp:sha256(Data)).

sha256(Data) ->
	cryptopp:sha256(Data).

%%% Auxillary Functions
reverseBinary(B) when is_binary(B) ->
	S = size(B)*8,
	<<X:S/integer-little>> = B,
	<<X:S/integer-big>>.

%% this will convert the incoming data into the equivillant Base58 format
%% this representation include a 4-byte checksum generated from the first
%% four bytes of the double-sha256 hash of the data that is appended to the stream.
convertToBase58(Data) ->
	<<Checksum:32, _Rest/binary>> = sha256(sha256(Data)),
	CheckedData = <<Data/binary, Checksum:32/big>>,
	base58:binary_to_base58(CheckedData).		

%% Get a bitcoin address from the passed public key
getBitcoinAddress(Version, PubKey) ->
	Kpub1 = hash160(PubKey),
	Kpub2 = <<Version, Kpub1/binary>>,
	convertToBase58(Kpub2).

getBitcoinAddressFromHash160(Version, Hash160) ->
	Kpub = <<Version, Hash160/binary>>,
	convertToBase58(Kpub).

%% ====================================================================
%% Internal functions
%% ====================================================================


