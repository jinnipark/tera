%%% -------------------------------------------------------------------
%%% Author : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Convert a proplist into a record.
%%% ?props_to_record(PropList, RecordSymbol) -> Record
%%%
%%% Created : Feb 26, 2012
%%% -------------------------------------------------------------------
-author("Sungjin Park <jinni.park@gmail.com>").

-ifndef(PROPS_TO_RECORD).
-define(PROPS_TO_RECORD, 0).

%% @doc Convert a proplist into a record.
%% @spec ?props_to_record(Props=[{atom(), term()}], Record=atom()) -> record().
-define(props_to_record(Props, Record),
        ?props_to_record(Props, Record, #Record{})()).

-define(props_to_record(Props, Record, Default),
        fun() ->
          Fields = record_info(fields, Record),
          [Record | Defaults] = tuple_to_list(Default),
          List = [proplists:get_value(F, Props, D) || {F, D} <- lists:zip(Fields, Defaults)],
          list_to_tuple([Record | List])
        end
).

-endif.
