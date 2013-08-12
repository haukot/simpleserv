-module(my_serv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	Opts = [{ip, {127,0,0,1}}],
	case gen_tcp:listen(7654, Opts) of
    {ok, Listen_socket} ->
        gen_server:cast(?SERVER, loop),
        {ok, Socket;
    {error, Reason} ->
        {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(loop, Socket) -> 
	{ok, Conn} = gen_tcp:accept(Socket),
	gen_server:cast(?SERVER, {send, Conn, "Hello Erlang!"}),
	gen_server:cast(?SERVER, loop),
	{noreply, Socket};
handle_cast({send, Conn, Message}}, Socket) -> 
	gen_tcp:send(Conn, Message),
	gen_tcp:close(Conn),
	{noreply, Socket};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, ListSock, Ref, Error) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State = {Socket}) ->
	gen_tcp:close(Socket)
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

