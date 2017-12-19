#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable
%%%-------------------------------------------------------------------
%%% @author Adrianx Lau <adrianx.lau@gmail.com>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% Created : 12. 十月 2017 下午2:52
%%%-------------------------------------------------------------------
-module(start).
-author("Adrianx Lau <adrianx.lau@gmail.com>").

%% API
-export([main/1]).

main(_)->
	code:add_path("ebin"),
	Paths = filelib:wildcard("./deps/*/ebin"),
	RunPath = code:get_path(),
	lists:foreach(
		fun(Path) ->
			case lists:member(Path, RunPath) of
				false -> true = code:add_path(Path);
				true ->
					ignor
			end
		end, Paths),
	Cmd = "erl -pa ebin -s stockquery -config stockquery.config",
	run(Cmd).

run(CmdLine)->
	io:format("cmd:~s~n",[CmdLine]),
	os:cmd(CmdLine).