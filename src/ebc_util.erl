%% @author David Ellefsen <davidellefsen@gmail.com> 
%%
%% @doc ebc_util is a simple set of utility functions that is used
%% in other parts of the library
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

-module(ebc_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([epoch/0, binaryToHex/1, hexToBinary/1, removeDups/1, reverseBinary/1]).
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

hexToBinary(S) ->
  hexToBinary(S, []).
hexToBinary([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexToBinary([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexToBinary(T, [V | Acc]).

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


