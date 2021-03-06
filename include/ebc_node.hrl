-record(ebc_client_state, {
			address = undefined,
			port = undefined,
			socket = undefined,
			connected = undefined,
			lastseen = undefined,
			version = undefined,
			sendPid = undefined,
			recvPid = undefined,
			callback = undefined
		}).

-record(net_addr, {
			ip = undefined,
			time = undefined,
			services = undefined,
			port = undefined
		}).

-record(inv_vect, {
			type = undefined,
			hash = undefined
		}).

-record(outpoint, {
			hash = undefined,
			index = undefined
		}).

-record(tx_in, {
			previous_output = undefined,
			previous_index = undefined,
			script_length = undefined,
			signature_script = undefined,
			sequence = undefined
		}).

-record(tx_out, {
			value = undefined,
			pk_script_length = undefined,
			pk_script = undefined
		}).

-record(tx, {
			hash = undefined,
			version = undefined,
			tx_in_count = undefined,
			tx_in = undefined,
			tx_out_count = undefined,
			tx_out = undefined,
			lock_time = undefined,
			payload = <<>> % the binary form of the transaction - not generally included
	   }).

-record(version, {
			version = undefined,
			services = undefined,
			timestamp = undefined,
			addr_recv = undefined,
			addr_from = undefined,
			nonce = undefined,
			user_agent = undefined,
			start_height = undefined,
			relay = undefined
		}).


-record(block_header, {
		hash = <<>>,
		number = -1,
		version = undefined,
		hashPrevBlock = undefined,
		hashMerkleRoot = undefined,
		time = undefined,
		bits = undefined,
		nonce = undefined
	}).

-record(block, {
		hash = undefined,
		header = undefined, % 80 bytes
		trans = undefined, % 1-9 varint
		transactions = undefined
	}).

-record(wallet_identifier, {
		identifier = undefined,	%the address of the wallets we want to observe
		timestamp = undefined
	}).

-record(wallet_transaction, {
		txid = undefined,	%the transaction id
		address = undefined,	%the address that interacted with this wallet
		type = undefined,	%in or out
		amount = undefined,	%amount transacted
		index = undefined,	%the index of the input/output
		block = undefined,	%the block this transaction was included in
		status = unconfirmed,	%the status of this transaction unconfirmed/confirmed
		spent = false,		%is this a spent transaction
		spentby = undefined,	%the TxID of the transaction that spent this input
		tx = undefined		%the raw transaction
	}).


-define(INV_TX, 1).
-define(INV_BLOCK, 2).

-define(TESTNET_MAGIC, 16#0709110B).
-define(MAIN_MAGIC, 16#D9B4BEF9).

-define(TESTNET_PREFIX, 16#6f).
-define(MAINNET_PREFIX, 16#0).

-define(TESTNET_GENESIS, "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943").
-define(TESTNET_GENESIS_BIN, <<67,73,127,215,248,38,149,113,8,244,163,15,217,206,195,174,186,121,151,32,132,233,14,173,1,234,51,9,0,0,0,0>>).

-define(DGB(Str, Args), io:format(Str, Args)).
%-define(DGB(Str, Args), ok).
