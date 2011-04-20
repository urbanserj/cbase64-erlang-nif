cbase64-erlang-nif
------------------

A Base64 encoder/decoder as a NIF.

Performance
-----------

	Erlang R14B02 (erts-5.8.3) [source] [smp:2:2] [rq:2] [async-threads:0] [kernel-poll:false]
	
	Eshell V5.8.3  (abort with ^G)
	1> Data = crypto:rand_bytes(16 * 1024 * 1024).
	<<135,140,81,236,211,149,90,162,57,147,181,94,130,84,1,39,
	  24,157,131,251,98,10,7,70,250,207,116,73,69,...>>
	2> {_, DataB64} = timer:tc(base64, encode, [Data]).
	{3406972,
	 <<"h4xR7NOVWqI5k7VeglQBJxidg/tiCgdG+s90SUVKKMXG8fj7hWgB9T6i4V+o1/F+Ru1Cfdac/VvoKwW33Ot9DPnojNoEqlCQHIKQBi9XjV4v"...>>}
	3> {_, DataB64} = timer:tc(cbase64, encode, [Data]).
	{117399,
	 <<"h4xR7NOVWqI5k7VeglQBJxidg/tiCgdG+s90SUVKKMXG8fj7hWgB9T6i4V+o1/F+Ru1Cfdac/VvoKwW33Ot9DPnojNoEqlCQHIKQBi9XjV4v"...>>}
	4> {_, Data} = timer:tc(base64, decode, [DataB64]). 
	{3492609,
	 <<135,140,81,236,211,149,90,162,57,147,181,94,130,84,1,
	   39,24,157,131,251,98,10,7,70,250,207,116,...>>}
	5> {_, Data} = timer:tc(cbase64, decode, [DataB64]).
	{98955,
	 <<135,140,81,236,211,149,90,162,57,147,181,94,130,84,1,
	   39,24,157,131,251,98,10,7,70,250,207,116,...>>}
	6> 
