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
			lock_time = undefined
	   }).

-record(block_header, {
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


-define(INV_TX, 1).
-define(INV_BLOCK, 2).
