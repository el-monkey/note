-module(note).
-export([create_note/0, 
         create_note/1]).
-export([find_note_using_uuid/1]).
-export([fetch_notes/0]).
-export([save_note/1]).
-export([delete_note/1]).
-export([uuid/1, created_date/1, modified_date/1, text/1]).

-include("note.hrl").

%% @doc create a new blank note
%% @end
create_note() ->
    {ok, Note} = note_server:create_note(),
    {ok, Note}.

%% @doc create a note with the supplied Text
%% @end
create_note(Text) when erlang:is_bitstring(Text) -> 
    case note_server:create_note(Text) of
        {ok, Note} -> {ok, Note};
        _Other     -> {error, not_created}
    end.

%% @doc find the note identified by the supplied uuid value
%% @end
find_note_using_uuid(Uuid) -> 
    case note_server:find_note_using_uuid(Uuid) of
        {ok, Note}         -> {ok, Note};
        {error, not_found} -> {error, not_found}
    end.

%% @doc fetch the notes currently in the store
%% @end
fetch_notes() -> 
    case note_server:fetch_notes() of
        {ok, Notes}  -> {ok, Notes};
        {error, Msg} -> {error, Msg}
    end.

%% @doc save the note
%% @end
save_note(Note) when erlang:is_record(Note, note) ->
    case note_server:save_note(Note) of
        {ok, SavedNote}    -> {ok, SavedNote};
        {error, not_found} -> {error, not_found}
    end. 

%% @doc delete the supplied note
%% @end
delete_note(Note) when erlang:is_record(Note, note) ->
    case note_server:delete_note(Note) of
        ok           -> ok;
        {error, Msg} -> {error, Msg}
    end. 

%% @doc accessor function for retrieving the uuid
%% @end
uuid(#note{uuid = Uuid} = Note) when erlang:is_record(Note, note) andalso Uuid =/= undefined -> Uuid; 
uuid(_) -> undefined.

%% @doc accessor function for retrieving the created_date
%% @end
created_date(#note{created_date = CreatedDate} = Note) when erlang:is_record(Note, note) andalso CreatedDate =/= undefined -> CreatedDate;
created_date(_) -> undefined.

%% @doc accessor function for retrieving the modified_date
%% @end
modified_date(#note{modified_date = ModifiedDate} = Note) when erlang:is_record(Note, note) andalso ModifiedDate =/= undefined -> ModifiedDate;
modified_date(_) -> undefined.

%% @doc accessor function for retrieving the text 
%% @end
text(#note{text = Text} = Note) when erlang:is_record(Note, note) andalso Text =/= undefined -> Text;
text(_) -> undefined.
        

