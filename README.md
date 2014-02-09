# What is this?

A multiplayer, real-time, tic-tac-toe web app  powered by [Erlang](http://www.erlang.org/) and [cowboy](https://github.com/extend/cowboy).

# How to build and run

## Requirements

- Erlang/OTP R16B3
- rebar 2.2.0

## Build instructions

Execute the following

    rebar get-deps
    rebar compile

## Run instructions

- Execute


        ./start.sh

- Point your browser to <http://localhost:8080>
- Click "Create game"
- Visit the link in the address bar using another tab/computer

# TODO

- Makefile
- Clean up code
- Dialyze
- Create behaviour for game session
