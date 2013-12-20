Tarabish
========

## Requirements ################################################################
 * [Erlang](http://www.erlang.org/) R14
 * [Rebar](https://github.com/rebar/rebar) 2.0
 * [Python](http://www.python.org/) 2.6

Once Rebar is installed from the server directory run:

    $ rebar compile

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

To start the server run `make start` from the server directory. It will listen
on port 42745 for client connections.

The client can be started in the DartEditor, running index.html.

## Legal #######################################################################

Licensed under an MIT/Expat license. See COPYING

Card images from http://www.jfitz.com/cards/

Suits image from http://en.wikipedia.org/wiki/File:French_suits.svg
