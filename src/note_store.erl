-module(note_store).

-include("note.hrl").

-export([init/0]).
-export([find_note_using_uuid/1]).
-export([fetch_notes/0]).
-export([save_note/1]).
-export([delete_note/1]).

-define(TABLE, note).

%% @doc initialise the database
%% @end
init() -> 
    create_tables().

%% @doc find a note using the supplied uuid
%% @end
find_note_using_uuid(Uuid) -> 
    % {ok, Note}
    % {error, not_found}
    Fun = fun() ->
        mnesia:read({?TABLE, Uuid}) 
    end,
    case mnesia:transaction(Fun) of
        {atomic, [#note{} = Note | _Tail]} -> {ok, Note};
        {atomic, []}                       -> {error, not_found};
        {aborted, Reason}                  -> {error, Reason} 
    end.

%% @doc fetch all note records
%% @end
fetch_notes() -> 
    % {ok, Notes}
    % {error, Msg}
    Fun = fun() ->
        mnesia:foldl(fun(X, Acc) -> [X|Acc] end, [], ?TABLE) 
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> {ok, Result};
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc save the Note
%% @end
save_note(#note{} = Note) -> 
    % {ok, Note}
    % {error, Msg}
    Fun = fun() -> 
        mnesia:write(Note)
    end,
    case mnesia:transaction(Fun) of 
        {atomic,  Result} -> {ok, Note};
        {aborted, Reason} -> {error, Reason}
    end. 

%% @doc delete the note
%% @end
delete_note(#note{uuid = Uuid} = Note) -> 
    Fun  = fun() -> 
        mnesia:delete({?TABLE, Uuid})
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok}      -> ok;
        {aborted, Reason} -> {error, Reason}
    end.


%% PRIVATE FUNCTIONS

create_tables() -> 
    case mnesia:create_table(?TABLE, [{attributes, record_info(fields, note)}]) of
        {atomic, ok}      -> ok;
        {aborted, Reason} -> io:format("~p", [Reason]), {aborted, Reason};
        Other -> io:format("~p", [Other])
    end.
