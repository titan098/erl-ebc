%% @author David Ellefsen <davidellefsen@gmail.com> 
%%
%% @doc script is a parser for Bitcoin scripts. Bitcoin scripts are written
%% in a Forth-type language and are used to facilitate the verification of
%% signatures. There are some functions missing in this module, but the basic
%% functionality is sufficient for my needs.
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

-module(script).
-export([]).

-compile([export_all, debug_info]).

%%% CONSTANTS
-define(OP_FALSE,	16#00).
-define(OP_PUSHDATA1,	16#4c).
-define(OP_PUSHDATA2,	16#4d).
-define(OP_PUSHDATA4,	16#4e).
-define(OP_1NEGATE,	16#4f).
-define(OP_TRUE,	16#51).
-define(OP_2,		16#52).
-define(OP_3,		16#53).
-define(OP_4,		16#54).
-define(OP_5,		16#55).
-define(OP_6,		16#56).
-define(OP_7,		16#57).
-define(OP_8,		16#58).
-define(OP_9,		16#59).
-define(OP_10,		16#5a).
-define(OP_11,		16#5b).
-define(OP_12,		16#5c).
-define(OP_13,		16#5d).
-define(OP_14,		16#5e).
-define(OP_15,		16#5f).
-define(OP_16,		16#60).

%%% FLOW CONTROL
-define(OP_NOP,		16#61).
-define(OP_IF,		16#63).
-define(OP_NOTIF,	16#64).
-define(OP_ELSE,	16#67).
-define(OP_ENDIF,	16#68).
-define(OP_VERIFY,	16#69).
-define(OP_RETURN,	16#6a).

%%% STACK
-define(OP_TOALTSTACK,	16#6b).
-define(OP_FROMALTSTACK,16#6c).
-define(OP_IFDUP,	16#73).
-define(OP_DEPTH,	16#74).
-define(OP_DROP,	16#75).
-define(OP_DUP,		16#76).
-define(OP_NIP,		16#77).
-define(OP_OVER,	16#78).
-define(OP_PICK,	16#79).
-define(OP_ROLL,	16#7a).
-define(OP_ROT,		16#7b).
-define(OP_SWAP,	16#7c).
-define(OP_TUCK,	16#7d).
-define(OP_2DROP,	16#6d).
-define(OP_2DUP,	16#6e).
-define(OP_3DUP,	16#6f).
-define(OP_2OVER,	16#70).
-define(OP_2ROT,	16#71).
-define(OP_2SWAP,	16#72).

%%% SPLICE
-define(OP_CAT,		16#7e).
-define(OP_SUBSTR,	16#7f).
-define(OP_LEFT,	16#80).
-define(OP_RIGHT,	16#81).
-define(OP_SIZE,	16#82).

%%% BITWISE LOGIC
-define(OP_INVERT,	16#83).
-define(OP_AND,		16#84).
-define(OP_OR,		16#85).
-define(OP_XOR,		16#86).
-define(OP_EQUAL,	16#87).
-define(OP_EQUALVERIFY,	16#88).

%%% ARITHMETIC
-define(OP_1ADD,		16#8b).
-define(OP_1SUB,		16#8c).
-define(OP_2MUL,		16#8d).
-define(OP_2DIV,		16#8e).
-define(OP_NEGATE,		16#8f).
-define(OP_ABS,			16#90).
-define(OP_NOT,			16#91).
-define(OP_0NOTEQUAL,		16#92).
-define(OP_ADD,			16#93).
-define(OP_SUB,			16#94).
-define(OP_MUL,			16#95).
-define(OP_DIV,			16#96).
-define(OP_MOD,			16#97).
-define(OP_LSHIFT,		16#98).
-define(OP_RSHIFT,		16#99).
-define(OP_BOOLAND,		16#9a).
-define(OP_BOOLOR,		16#9b).
-define(OP_NUMEQUAL,		16#9c).
-define(OP_NUMEQUALVERIFY,	16#9d).
-define(OP_NUMNOTEQUAL,		16#9e).
-define(OP_LESSTHAN,		16#9f).
-define(OP_GREATERTHAN,		16#a0).
-define(OP_LESSTHANOREQUAL,	16#a1).
-define(OP_GREATERTHANOREQUAL,	16#a2).
-define(OP_MIN,			16#a3).
-define(OP_MAX,			16#a4).
-define(OP_WITHIN,		16#a5).

%%% CRYPTO
-define(OP_RIPEMD160,		16#a6).
-define(OP_SHA1,		16#a7).
-define(OP_SHA256,		16#a8).
-define(OP_HASH160,		16#a9).
-define(OP_HASH256,		16#aa).
-define(OP_CODESEPARATOR,	16#ab).
-define(OP_CHECKSIG,		16#ac).
-define(OP_CHECKSIGVERIFY,	16#ad).
-define(OP_CHECKMULTISIG,	16#ae).
-define(OP_CHECKMULTISIGVERIFY,	16#af).

%%% PSEUDO-WORDS
-define(OP_PUBKEYHASH,		16#fd).
-define(OP_PUBKEY,		16#fe).
-define(OP_INVALIDOPCODE,	16#ff).

%%% RESERVED WORDS
-define(OP_RESERVED,	16#50).
-define(OP_VER,		16#62).
-define(OP_VERIF,	16#65).
-define(OP_VERNOTIF,	16#66).
-define(OP_RESERVED1,	16#89).
-define(OP_RESERVED2,	16#8a).
-define(OP_NOP1,	16#b0).
-define(OP_NOP2,	16#b1).
-define(OP_NOP3,	16#b2).
-define(OP_NOP4,	16#b3).
-define(OP_NOP5,	16#b4).
-define(OP_NOP6,	16#b5).
-define(OP_NOP7,	16#b6).
-define(OP_NOP8,	16#b7).
-define(OP_NOP9,	16#b8).
-define(OP_NOP10,	16#b9).

%%% CONSTANTS
evaluate(<<?OP_FALSE, RestScript/binary>>, Stack) -> evaluate(RestScript, [0 | Stack]);
evaluate(<<?OP_PUSHDATA1, ToPush, RestScript/binary>>, Stack) -> 
	extractBytesThenEvaluate(ToPush, RestScript, Stack);	
evaluate(<<?OP_PUSHDATA2, ToPush:16, RestScript/binary>>, Stack) -> 
	extractBytesThenEvaluate(ToPush, RestScript, Stack);
evaluate(<<?OP_PUSHDATA4, ToPush:32, RestScript/binary>>, Stack) -> 
	extractBytesThenEvaluate(ToPush, RestScript, Stack);
evaluate(<<?OP_1NEGATE, RestScript/binary>>, Stack) -> evaluate(RestScript, [-1 | Stack]);
evaluate(<<?OP_TRUE, RestScript/binary>>, Stack) -> evaluate(RestScript, [1 | Stack]);
evaluate(<<?OP_2, RestScript/binary>>, Stack) -> evaluate(RestScript, [2 | Stack]);
evaluate(<<?OP_3, RestScript/binary>>, Stack) -> evaluate(RestScript, [3 | Stack]);
evaluate(<<?OP_4, RestScript/binary>>, Stack) -> evaluate(RestScript, [4 | Stack]);
evaluate(<<?OP_5, RestScript/binary>>, Stack) -> evaluate(RestScript, [5 | Stack]);
evaluate(<<?OP_6, RestScript/binary>>, Stack) -> evaluate(RestScript, [6 | Stack]);
evaluate(<<?OP_7, RestScript/binary>>, Stack) -> evaluate(RestScript, [7 | Stack]);
evaluate(<<?OP_8, RestScript/binary>>, Stack) -> evaluate(RestScript, [8 | Stack]);
evaluate(<<?OP_9, RestScript/binary>>, Stack) -> evaluate(RestScript, [9 | Stack]);
evaluate(<<?OP_10, RestScript/binary>>, Stack) -> evaluate(RestScript, [10 | Stack]);
evaluate(<<?OP_11, RestScript/binary>>, Stack) -> evaluate(RestScript, [11 | Stack]);
evaluate(<<?OP_12, RestScript/binary>>, Stack) -> evaluate(RestScript, [12 | Stack]);
evaluate(<<?OP_13, RestScript/binary>>, Stack) -> evaluate(RestScript, [13 | Stack]);
evaluate(<<?OP_14, RestScript/binary>>, Stack) -> evaluate(RestScript, [14 | Stack]);
evaluate(<<?OP_15, RestScript/binary>>, Stack) -> evaluate(RestScript, [15 | Stack]);
evaluate(<<?OP_16, RestScript/binary>>, Stack) -> evaluate(RestScript, [16 | Stack]);

%%% FLOW CONTROL
evaluate(<<?OP_NOP, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);
evaluate(<<?OP_IF, RestScript/binary>>, Stack) -> evaluate(RestScript, [16 | Stack]); %%
evaluate(<<?OP_NOTIF, RestScript/binary>>, Stack) -> evaluate(RestScript, [16 | Stack]); %%
evaluate(<<?OP_ELSE, RestScript/binary>>, Stack) -> evaluate(RestScript, [16 | Stack]); %%
evaluate(<<?OP_ENDIF, RestScript/binary>>, Stack) -> evaluate(RestScript, [16 | Stack]); %%
evaluate(<<?OP_VERIFY, RestScript/binary>>, [?OP_TRUE | Stack] ) -> evaluate(RestScript, Stack);
evaluate(<<?OP_VERIFY, _RestScript/binary>>, [_ | _Stack] ) -> false;
evaluate(<<?OP_RETURN, _RestScript/binary>>, _Stack) -> false;

%%% STACK
evaluate(<<?OP_TOALTSTACK, _RestScript/binary>>, _Stack) -> false; %%for now don't worry about the alt stack,
evaluate(<<?OP_FROMALTSTACK, _RestScript/binary>>, _Stack) -> false; %% it is probably not going to be used in scripts
evaluate(<<?OP_IFDUP, RestScript/binary>>, [?OP_FALSE | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_IFDUP, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [Val, Val | Stack]);
evaluate(<<?OP_DEPTH, RestScript/binary>>, Stack) -> evaluate(RestScript, [length(Stack) | Stack]);
evaluate(<<?OP_DROP, RestScript/binary>>, [_Val | Stack]) -> evaluate(RestScript, Stack);
evaluate(<<?OP_DUP, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [Val, Val | Stack]);
evaluate(<<?OP_NIP, RestScript/binary>>, [_Val1, Val2 | Stack]) -> evaluate(RestScript, [Val2 | Stack]);
evaluate(<<?OP_OVER, RestScript/binary>>, [Val1, Val2 | Stack]) -> evaluate(RestScript, [Val1, Val2, Val1 | Stack]);
evaluate(<<?OP_PICK, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack); %%
evaluate(<<?OP_ROLL, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack); %%
evaluate(<<?OP_ROT, RestScript/binary>>, [Val1, Val2, Val3 | Stack]) -> evaluate(RestScript, [Val2, Val3, Val1 | Stack]);
evaluate(<<?OP_SWAP, RestScript/binary>>, [Val1, Val2 | Stack]) -> evaluate(RestScript, [Val2, Val1 | Stack]);
evaluate(<<?OP_TUCK, RestScript/binary>>, [Val1, Val2 | Stack]) -> evaluate(RestScript, [Val2, Val1, Val2 | Stack]);
evaluate(<<?OP_2DROP, RestScript/binary>>, [_Val1, _Val2 | Stack]) -> evaluate(RestScript, Stack);
evaluate(<<?OP_2DUP, RestScript/binary>>, [Val1, Val2 | Stack]) -> evaluate(RestScript, [Val1, Val2, Val1, Val2 | Stack]);
evaluate(<<?OP_3DUP, RestScript/binary>>, [Val1, Val2, Val3 | Stack]) -> evaluate(RestScript, [Val1, Val2, Val3, Val1, Val2, Val3 | Stack]);
evaluate(<<?OP_2OVER, RestScript/binary>>, [Val1, Val2, Val3, Val4 | Stack]) -> evaluate(RestScript, [Val1, Val2, Val3, Val4, Val1, Val2 | Stack]);
evaluate(<<?OP_2ROT, RestScript/binary>>, [Val1, Val2, Val3, Val4, Val5, Val6 | Stack]) -> evaluate(RestScript, [Val3, Val4, Val5, Val6, Val1, Val2 | Stack]);
evaluate(<<?OP_2SWAP, RestScript/binary>>, [Val1, Val2, Val3, Val4 | Stack]) -> evaluate(RestScript, [Val3, Val4, Val1, Val2 | Stack]);

%%% SPLICE
evaluate(<<?OP_CAT, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_SUBSTR, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_LEFT, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_RIGHT, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_SIZE, RestScript/binary>>, [Val1 | Stack]) -> evaluate(RestScript, [length(Val1) | Stack]);

%%% BITWISE LOGIC
evaluate(<<?OP_INVERT, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_AND, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_OR, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_XOR, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_EQUAL, RestScript/binary>>, [Val1, Val1 | Stack]) -> evaluate(RestScript, [?OP_TRUE | Stack]);
evaluate(<<?OP_EQUAL, RestScript/binary>>, [_Val1, _Val2 | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_EQUALVERIFY, RestScript/binary>>, Stack) -> evaluate(<<?OP_EQUAL, ?OP_VERIFY, RestScript/binary>>, Stack);

%%% Arithmetic
evaluate(<<?OP_1ADD, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [Val+1 | Stack]);
evaluate(<<?OP_1SUB, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [Val-1 | Stack]);
evaluate(<<?OP_2MUL, _RestScript/binary>>, _Script) -> false;
evaluate(<<?OP_2DIV, _RestScript/binary>>, _Script) -> false;
evaluate(<<?OP_NEGATE, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [-Val | Stack]);
evaluate(<<?OP_ABS, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [abs(Val) | Stack]);
evaluate(<<?OP_NOT, RestScript/binary>>, [Val | Stack]) when Val == ?OP_FALSE orelse Val == ?OP_TRUE -> evaluate(RestScript, [abs(1-Val) | Stack]);
evaluate(<<?OP_NOT, RestScript/binary>>, [_Val | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_0NOTEQUAL, RestScript/binary>>, [?OP_FALSE | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_0NOTEQUAL, RestScript/binary>>, [_Val | Stack]) -> evaluate(RestScript, [?OP_TRUE | Stack]);
evaluate(<<?OP_ADD, RestScript/binary>>, [Val1, Val2 | Stack]) -> evaluate(RestScript, [Val1 + Val2 | Stack]);
evaluate(<<?OP_SUB, RestScript/binary>>, [Val1, Val2 | Stack]) -> evaluate(RestScript, [Val2 - Val1 | Stack]);
evaluate(<<?OP_MUL, _RestScript/binary>>, _Script) -> false;
evaluate(<<?OP_DIV, _RestScript/binary>>, _Script) -> false;
evaluate(<<?OP_MOD, _RestScript/binary>>, _Script) -> false;
evaluate(<<?OP_LSHIFT, _RestScript/binary>>, _Script) -> false;
evaluate(<<?OP_RSHIFT, _RestScript/binary>>, _Script) -> false;
evaluate(<<?OP_BOOLAND, RestScript/binary>>, [Val1, Val2 | Stack]) when Val1 /= 0 andalso Val2 /= 0 -> evaluate(RestScript, [?OP_TRUE | Stack]);
evaluate(<<?OP_BOOLAND, RestScript/binary>>, [_Val1, _Val2 | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_BOOLOR, RestScript/binary>>, [Val1, Val2 | Stack]) when Val1 /= 0 orelse Val2 /= 0 -> evaluate(RestScript, [?OP_TRUE | Stack]);
evaluate(<<?OP_BOOLOR, RestScript/binary>>, [_Val1, _Val2 | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_NUMEQUAL, RestScript/binary>>, [Val, Val | Stack]) -> evaluate(RestScript, [?OP_TRUE | Stack]);
evaluate(<<?OP_NUMEQUAL, RestScript/binary>>, [_Val1, _Val2 | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_NUMEQUALVERIFY, RestScript/binary>>, Stack) -> evaluate(<<?OP_NUMEQUAL, ?OP_VERIFY, RestScript/binary>>, Stack);
evaluate(<<?OP_NUMNOTEQUAL, RestScript/binary>>, [Val, Val | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_NUMNOTEQUAL, RestScript/binary>>, [_Val1, _Val2 | Stack]) -> evaluate(RestScript, [?OP_TRUE | Stack]);
evaluate(<<?OP_LESSTHAN, RestScript/binary>>, [Val1, Val2 | Stack]) when Val1 < Val2 -> evaluate(RestScript, [?OP_TRUE | Stack]);
evaluate(<<?OP_LESSTHAN, RestScript/binary>>, [_Val1, _Val2 | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_GREATERTHAN, RestScript/binary>>, [Val1, Val2 | Stack]) when Val1 > Val2 -> evaluate(RestScript, [?OP_TRUE | Stack]);
evaluate(<<?OP_GREATERTHAN, RestScript/binary>>, [_Val1, _Val2 | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_LESSTHANOREQUAL, RestScript/binary>>, [Val1, Val2 | Stack]) when Val1 =< Val2 -> evaluate(RestScript, [?OP_TRUE | Stack]);
evaluate(<<?OP_LESSTHANOREQUAL, RestScript/binary>>, [_Val1, _Val2 | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_GREATERTHANOREQUAL, RestScript/binary>>, [Val1, Val2 | Stack]) when Val1 >= Val2 -> evaluate(RestScript, [?OP_TRUE | Stack]);
evaluate(<<?OP_GREATERTHANOREQUAL, RestScript/binary>>, [_Val1, _Val2 | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);
evaluate(<<?OP_MIN, RestScript/binary>>, [Val1, Val2 | Stack]) when Val1 >= Val2 -> evaluate(RestScript, [Val1 | Stack]);
evaluate(<<?OP_MIN, RestScript/binary>>, [_Val1, Val2 | Stack]) -> evaluate(RestScript, [Val2 | Stack]);
evaluate(<<?OP_MAX, RestScript/binary>>, [Val1, Val2 | Stack]) when Val1 >=  Val2 -> evaluate(RestScript, [Val1 | Stack]);
evaluate(<<?OP_MAX, RestScript/binary>>, [_Val1, Val2 | Stack]) -> evaluate(RestScript, [Val2 | Stack]);
evaluate(<<?OP_WITHIN, RestScript/binary>>, [Val, Min, Max | Stack]) when Val >= Min andalso Val =< Max -> evaluate(RestScript, [?OP_TRUE | Stack]);
evaluate(<<?OP_WITHIN, RestScript/binary>>, [_Val, _Min, _Max | Stack]) -> evaluate(RestScript, [?OP_FALSE | Stack]);

%%% CRYPTO
evaluate(<<?OP_RIPEMD160, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [cryptopp:ripemd160(Val) | Stack]);
evaluate(<<?OP_SHA1, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [cryptopp:sha1(Val) | Stack]);
evaluate(<<?OP_SHA256, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [cryptopp:sha256(Val) | Stack]);
evaluate(<<?OP_HASH160, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [cryptopp:ripemd160(cryptopp:sha256(Val)) | Stack]);
evaluate(<<?OP_HASH256, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [cryptopp:sha256(cryptopp:sha256(Val)) | Stack]);
evaluate(<<?OP_CODESEPARATOR, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);
evaluate(<<?OP_CHECKSIG, RestScript/binary>>, [Sig, Pubkey | Stack]) -> evaluate(RestScript, Stack);
evaluate(<<?OP_CHECKSIGVERIFY, RestScript/binary>>, Stack) -> evaluate(<<?OP_CHECKSIG, ?OP_VERIFY, RestScript/binary>>, Stack);
evaluate(<<?OP_CHECKMULTISIG, RestScript/binary>>, [Val | Stack]) -> evaluate(RestScript, [Val, Val | Stack]);
evaluate(<<?OP_CHECKMULTISIGVERIFY, RestScript/binary>>, Stack) -> evaluate(<<?OP_CHECKMULTISIG, ?OP_VERIFY, RestScript/binary>>, Stack);

%%% PSEUDO-WORDS
evaluate(<<?OP_PUBKEYHASH, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_PUBKEY, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_INVALIDOPCODE, _RestScript/binary>>, _Stack) -> false;

%%% RESERVED
evaluate(<<?OP_RESERVED, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_VER, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_VERIF, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_VERNOTIF, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_RESERVED1, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_RESERVED2, _RestScript/binary>>, _Stack) -> false;
evaluate(<<?OP_NOP1, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);
evaluate(<<?OP_NOP2, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);
evaluate(<<?OP_NOP3, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);
evaluate(<<?OP_NOP4, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);
evaluate(<<?OP_NOP5, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);
evaluate(<<?OP_NOP6, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);
evaluate(<<?OP_NOP7, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);
evaluate(<<?OP_NOP8, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);
evaluate(<<?OP_NOP9, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);
evaluate(<<?OP_NOP10, RestScript/binary>>, Stack) -> evaluate(RestScript, Stack);

%%% Data Extraction
evaluate(<<Op, RestBinary/binary>>, Stack) when Op >= 1 andalso Op =< 75 ->
	extractBytesThenEvaluate(Op, RestBinary, Stack);

%%% if invalid return false
evaluate(<<>>, Stack) -> Stack;
evaluate(_, _) -> false.

%%decode the script and create a list that represents the stack.
decodeScript(<<>>, Stack) -> Stack;
decodeScript(<<Op, RestBinary/binary>>, Stack) when Op >= 16#1 andalso Op =< 16#4B ->
	case extractBytes(Op, RestBinary, <<>>) of
		{Script, Bytes} -> decodeScript(Script, [Bytes | Stack]);
		false -> false
	end;
decodeScript(<<Op, RestBinary/binary>>, Stack) ->
	decodeScript(RestBinary, [Op | Stack]).

%%% AUXILLARY FUNCTONS %%%
extractBytesThenEvaluate(NumBytes, Binary, Stack) ->
	case extractBytes(NumBytes, Binary, <<>>) of
		{Script, Bytes} -> evaluate(Script, [Bytes | Stack]);
		false -> false
	end.	

extractBytes(0, Binary, Res) -> {Binary, Res};
extractBytes(Op, <<Byte, Binary/binary>>, Res) -> extractBytes(Op-1, Binary, <<Res/binary, Byte>>);
extractBytes(_, _, _) -> false.
