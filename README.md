Tarabish
========

## Requirements ################################################################
 * [Apache Thrift](http://thrift.apache.org/) 0.6.0
 * [Erlang](http://www.erlang.org/) R14
 * [Python](http://www.python.org/) 2.6

## Source ######################################################################
### Repository #################################################################
Source if available on GitHub:

    $ git clone git://github.com/KenMacD/tarabish.git

### Layout #####################################################################

    .
    ├── api        Thrift API for any servers/clients
    ├── client-py  Simple python client, mainly for testing
    └── server     Erlang Tarabish Server
        ├── src
        └── test

### Building ###################################################################

The API must be build first:

    $ cd api
    $ make

Then the server can be build:

    $ cd server
    $ make

## Running #####################################################################

To start the server run `make start` from the server directory. It will listen
on port 42745 and 42746 for client connections.

A bot exists in the client-py directory that can be made to connect. It joins
the first empty seat and plays any card permitted. Currently a bot has to be
started from the base directory with `./client-py/bot.py`. Start 4 to get a
game going.

## Legal #######################################################################

Licensed under an MIT/Expat license. See COPYING
