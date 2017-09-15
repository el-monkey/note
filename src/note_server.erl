-module(note_server).
-behaviour(gen_server).

-include("note.hrl").
-define(SERVER, ?MODULE).

%% state record
-record(state, {}).
%% callbacks

-export([start_link/0]).
-export([create_note/0, create_note/1]).
-export([find_note_using_uuid/1]).
-export([fetch_notes/0]).
-export([save_note/1]).
-export([delete_note/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% PUBLIC API

%% @doc start link
%% @end
start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc create a new note record
%% @end
create_note() -> 
    {ok, #note{
            uuid          = erlang:list_to_bitstring(uuid:to_string(uuid:uuid4())),  
            created_date  = calendar:universal_time(), 
            modified_date = calendar:universal_time()}}.

%% @doc create a new note record with the specified text
%% @end
create_note(Text) -> 
    case note_server:create_note() of
        {ok, Note} -> {ok,  Note#note{text = Text}};
        _Other                                     -> {error, not_created}
    end.

%% @doc find the note identified by the supplied uuid
%% @end
find_note_using_uuid(Uuid) -> 
    case gen_server:call(?SERVER, {find_note_using_uuid, Uuid}) of
        {ok, #note{} = Note} -> {ok, Note};
        {error, not_found}   -> {error, not_found}
    end.
    
%% @doc fetch all of the notes in the application
%% @end
fetch_notes() -> 
    case gen_server:call(?SERVER, {fetch_notes}) of
        {ok, Notes} when erlang:is_list(Notes) -> {ok, Notes};
        {error, Msg}                           -> {error, Msg}
    end. 

%% @doc save the note
%% @end
save_note(#note{} = Note) -> 
    ModifiedNote = Note#note{modified_date = calendar:universal_time()},
    case gen_server:call(?SERVER, {save_note, ModifiedNote}) of
        {ok, SavedNote} when erlang:is_record(SavedNote, note) -> {ok, SavedNote};
        {error, Msg} -> {error, Msg}
    end.

%% @doc delete the supplied note (this will perform a hard delete)
%% @end
delete_note(#note{} = Note) -> 
    case gen_server:call(?SERVER, {delete_note, Note}) of
        ok           -> ok;
        {error, Msg} -> {error, Msg}
    end.


%% GEN SERVER 

%% Function: init
%% Purpose: initialise the server
init([]) -> 
    {ok, #state{}}.

%% @doc handle_call
%% @end
handle_call({find_note_using_uuid, Uuid}, _From, State) ->
    case note_store:find_note_using_uuid(Uuid) of
        {ok, Note}         -> {reply, {ok, Note}, State};
        {error, not_found} -> {reply, {error, not_found}, State}
    end;
handle_call({fetch_notes}, _From, State) -> 
    case note_store:fetch_notes() of
        {ok, Notes}  -> {reply, {ok, Notes}, State};
        {error, Msg} -> {reply, {error, Msg}, State}
    end;
handle_call({save_note, Note}, _From, State) ->
    case note_store:save_note(Note) of
        {ok, SavedNote} -> {reply, {ok, SavedNote}, State};
        {error, Msg}    -> {reply, {error, Msg}, State}
    end;
handle_call({delete_note, Note}, _From, State) -> 
    case note_store:delete_note(Note) of
        ok                -> {reply, ok, State};
        {error, Msg}      -> {reply, {error, Msg}, State}
    end.

% handle_call(_Msg, _From, State) -> 
%     {reply, undefined, State}.

%% @doc handle_cast
%% @end
handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(_Msg, State) -> 
    {noreply, State}.

terminate(_Reason, _State) -> 
    ok. % return value isnt important

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.
