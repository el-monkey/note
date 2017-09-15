%%%-------------------------------------------------------------------
%% @doc note top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(note_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    NoteServer = {
        note_server, 
        {note_server, start_link, []},
        permanent, 
        1000, 
        worker,
        [note_server]
    },
    Children = [NoteServer],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
