cbase64-erlang-nif
------------------

A Base64 encoder/decoder as a NIF, but without responsiveness degradetion of the VM.

Performance
-----------

```
Erlang R16B01 (erts-5.10.2) [source] [async-threads:10] [kernel-poll:false]

Eshell V5.10.2  (abort with ^G)
1> Data = crypto:rand_bytes(16 * 1024 * 1024).
<<83,237,34,171,184,22,127,108,161,173,188,103,220,147,60,
  52,17,251,147,202,96,39,163,99,35,205,226,82,8,...>>
2> {_, DataB64} = timer:tc(base64, encode, [Data]).
{3308268,
 <<"U+0iq7gWf2yhrbxn3JM8NBH7k8pgJ6NjI83iUgi4mzojAGVx7EFK46MyRlbF3pEBj3qMNLq9yhdTi10HSQNXSSoASESQ9OxukASsX8uM2fW+"...>>}
3> {_, DataB64} = timer:tc(cbase64, encode, [Data]).
{140611,
 <<"U+0iq7gWf2yhrbxn3JM8NBH7k8pgJ6NjI83iUgi4mzojAGVx7EFK46MyRlbF3pEBj3qMNLq9yhdTi10HSQNXSSoASESQ9OxukASsX8uM2fW+"...>>}
4> {_, Data} = timer:tc(base64, decode, [DataB64]).
{3819640,
 <<83,237,34,171,184,22,127,108,161,173,188,103,220,147,
   60,52,17,251,147,202,96,39,163,99,35,205,226,...>>}
5> {_, Data} = timer:tc(cbase64, decode, [DataB64]).
{107382,
 <<83,237,34,171,184,22,127,108,161,173,188,103,220,147,
   60,52,17,251,147,202,96,39,163,99,35,205,226,...>>}
6>
```
