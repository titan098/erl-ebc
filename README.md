erl-ebc
==========

Version: 0.0.1

erl-ebc is an Erlang-based Bitcoin client. The client can interact with the main Bitcoin network (MainNET and TestNET).

Building
--------

Building the application requires dependencies to be downloaded and compiled. This has been tested on Linux and MacOSX. ``rebar`` will handle the download and compilation of the dependencies.

To build a fresh copy of ``rebar``:

```
./makerebar.sh
```

The build erl-cryptopp:

```
./rebar get-deps
./rebar compile
```

Caveats
-------

When compiling on Linux you will need to compile CryptoPP using a ``-fPIC`` flag. This will enable the generation of position independent code. This can be achieved by modifying the makefile for CryptoPP to uncomment the line ``CXXFLAGS+=-fPIC``. A ``make clean`` might be required in the CryptoPP directory to force a rebuild.

CryptoPP can be found in the ``deps`` directory once ``./rebar get-deps`` has been executed.

License
-------

This application is licensed under an [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0.html)

    Copyright 2015 David Ellefsen 

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.



