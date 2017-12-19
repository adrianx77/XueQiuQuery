%%%-------------------------------------------------------------------
%%% @author Adrianx Lau <adrianx.lau@gmail.com>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% Created : 19. 十二月 2017 下午1:29
%%%-------------------------------------------------------------------
-module(txt_util).
-author("Adrianx Lau <adrianx.lau@gmail.com>").
-include_lib("kernel/include/file.hrl").
%% API
-export([split_name_code/2]).

split_name_code(File,TermOutFun)->
	case file:read_file(File) of
		{ok, Binary} -> spilte(TermOutFun,Binary,[]);
		{error, Reason}->io:format("read file error:~p~n",[Reason])
	end.
spilte(TermOutFun,<<>>,_)->
	TermOutFun(over);
spilte(TermOutFun,<<"(",B1,B2,B3,B4,B5,B6,")",Binary/binary>>,LastName) ->
	Code = binary_to_list(<<B1,B2,B3,B4,B5,B6>>),
	TermOutFun({LastName,Code}),
	spilte(TermOutFun,Binary,[]);
spilte(TermOutFun,<<"\n",Binary/binary>>,LastName) ->
	spilte(TermOutFun,Binary,LastName);
spilte(TermOutFun,<<C,Binary/binary>>,LastName) ->
	LastName2 = LastName ++[C],
	spilte(TermOutFun,Binary,LastName2).