Tarabish
========

## Requirements ################################################################
 * [Erlang](http://www.erlang.org/) R17
 * [Rebar](https://github.com/rebar/rebar) 2.5
 * [Python](http://www.python.org/) 2.7

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

The client:

    $ cd client/tarabishdart
    $ pub get
    $ pub build

The server:

    $ cd server
    $ rebar get-deps
    $ rebar compile
    $ make
    $ ln -s ../client/tarabishdart/build/web/ ./docroot/

## Running #####################################################################

To start the server run `make start` from the server directory. It will listen
on port 42745 for client connections.

## Legal #######################################################################

Licensed under an MIT/Expat license. See COPYING

Card images from http://www.jfitz.com/cards/

Suits image from http://en.wikipedia.org/wiki/File:French_suits.svg
