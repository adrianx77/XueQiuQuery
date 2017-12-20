%%%-------------------------------------------------------------------
%%% @author Adrianx Lau <adrianx.lau@gmail.com>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% Created : 19. 十二月 2017 下午12:24
%%%-------------------------------------------------------------------
-module(query_process).
-author("Adrianx Lau <adrianx.lau@gmail.com>").

-behaviour(gen_server).

%% API
-export([start_link/2]).
-include("stock_query_define.hrl").

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {index::integer(),mod::integer(),curpos::integer(),write::term()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Index::integer(),MOD::integer()) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Index,MOD) ->
	gen_server:start_link(?MODULE, [Index,MOD], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([Index,MOD]) ->
	io:format("Index:~p~n",[Index]),
	{ok,OutFile} = file:open("Index_"++integer_to_list(Index)++".txt",[write]),
	self()! 'START_PULL',
	{ok, #state{index = Index,mod=MOD,curpos=Index,write=OutFile}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
	State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info({http,{_RequestId, Result}},#state{write = OutFile}=State)->
	{OName,OCode,OArea} = get('curRequest'),
	case Result of
		{_, _Headers, Body}->
			JsObj = jsx:decode(Body,[return_maps]),
			case maps:keys(JsObj) of
				[Key]->
					#{Key:=Content} = JsObj,
					#{<<"name">>:=Name ,<<"code">> := Code, <<"eps">>:=EPS , <<"close">>:= Close ,<<"open">> :=Open} = Content,
					io:format("successed:~s~n",[Code]),
					case EPS of
						<<>>-> io:fwrite(OutFile,"~s (~s) EPS:0 safe_price:0 close:~s open:~s~n",[Name,Code,Close,Open]);
						_->io:fwrite(OutFile,"~s (~s) EPS:~s safe_price:~p close:~s open:~s~n",[Name,Code,EPS,binary_to_float(EPS)*11.49425287,Close,Open])
					end;
				_->
					io:format("Error get ~s ~s ~s~n",[OName,OCode,OArea]),
					ignore_error
			end;
		_->
			io:format("Error get ~s ~s ~s~n",[OName,OCode,OArea])
	end,
	timer:send_after(50,'START_PULL'),
	{noreply, State};
handle_info('START_PULL',#state{curpos = CurPos,mod = MOD,write = OutFile} = State) ->
	case query_manager:get_name_code(CurPos) of
		{}-> file:close(OutFile);
		{_,_Pos,Name,Code,Area}->
			query_info(Name,Code,Area)
	end,
	{noreply, State#state{curpos = CurPos + MOD}};
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
	Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
query_info(Name,Code,Area)->
	Url = "http://xueqiu.com/v4/stock/quote.json?code=" ++Area++Code,
	Cookie = "xq_a_token=95b69ccb71a54ebf3d7060a84a72b45015fead7f; xq_a_token.sig=r7RhUAkpd9FiBmPDlOV3F-V8LFo; xq_r_token=6589f21e3e52d21c4d3de00d3135b3920fa8a52f; xq_r_token.sig=Ho5fiQYDNIITpBXlltLZVADXRSI; u=831513731324295; device_id=0547c0f6b8a976d1a9d750c9746c515a; s=fd1bqf2682; __utmc=1; __utmt=1; Hm_lvt_1db88642e346389874251b5a1eded6e3=1513731327,1513731386; Hm_lpvt_1db88642e346389874251b5a1eded6e3=1513731386; __utma=1.1932201061.1513731344.1513731344.1513731386.2; __utmz=1.1513731386.2.2.utmcsr=baidu|utmccn=(organic)|utmcmd=organic; __utmb=1.1.10.1513731386",
	Headers = [
		{"User-Agent","Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36"},
		{"Cookie",Cookie}, %%Cookie需要你自己去你的浏览器查看开发者工具，看Request的Header，复制出来就可以了
%%		{"Accept-Encoding","gzip, deflate"}, 如果加上这个需要你自己来gzip，还是算了的好
		{"Accept","text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8"},
		{"Accept-Language","zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7"}
	],
	put('curRequest',{Name,Code,Area}),
	httpc:request(get, {Url,Headers}, [], [{full_result, true},{sync, false}]).