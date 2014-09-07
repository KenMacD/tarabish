Tarabish
========

## Requirements ################################################################
 * [Erlang](http://www.erlang.org/) R17
 * [Rebar](https://github.com/rebar/rebar) 2.5
 * [Python](http://www.python.org/) 2.7

Once Rebar is installed from the server directory run:

    $ rebar get-deps
    $ rebar compile
    $ make test

## Source ######################################################################
### Repository #################################################################
Source if available on GitHub:

    $ git clone git://github.com/KenMacD/tarabish.git

### Layout #####################################################################

    .
    ├── api        Thrift API for any servers/clients (no longer used)
    ├── client-py
    │   ├── qt     Qt4 main client
    │   └── py     Python test client and bot
    └── server     Erlang Tarabish Server
        ├── src
        └── test

### Building ###################################################################

The server:

    $ cd server
    $ make


## Running #####################################################################

Copy or link the Dart generated files in to the server directory:

    $ cd server
    $ ln -s ../client/tarabishdart/build ./docroot

To start the server run `make start` from the server directory. It will listen
on port 42745 for client connections.

## Legal #######################################################################

Licensed under an MIT/Expat license. See COPYING

Card images from http://www.jfitz.com/cards/

Suits image from http://en.wikipedia.org/wiki/File:French_suits.svg
