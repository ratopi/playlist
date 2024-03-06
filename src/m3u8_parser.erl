%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2019 11:30
%%% Reworked : 04. Mar 2024 22:56
%%%-------------------------------------------------------------------
-module(m3u8_parser).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([parse/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec(parse(Bin :: binary()) -> [{term(), map()}]).
parse(Bin) when is_binary(Bin) ->
	parse(Bin, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse(Bin, Acc) ->
	case read_line(Bin, <<>>) of
		{ok, Line, Rest} ->

			case Line of
				<<"">> ->
					parse(Rest, Acc);

				<<"#EXT", _/binary>> ->
					Entry = parse_ext_line(Line),
					parse(Rest, [Entry | Acc]);

				_ ->
					parse(Rest, [Line | Acc])
			end;

		eof ->
			lists:reverse(Acc)
	end.

% ---

parse_ext_line(Line = <<"#EXT", _/binary>>) ->
	case get_type(Line) of
		{type_only, Type} ->
			Type;
		{Type, Rest} ->
			{Type, parse_csv(Type, Rest, #{})}
	end.

% ---

get_type(Line) ->
	case get_type(Line, <<>>) of
		{Type, <<>>} ->
			{type_only, transform_type(Type)};
		{Type, Rest} ->
			{transform_type(Type), Rest}
	end.


get_type(<<>>, Type) ->
	{Type, <<>>};

get_type(<<$:, Rest/binary>>, Type) ->
	{Type, Rest};

get_type(<<Letter, Rest/binary>>, Type) ->
	get_type(Rest, <<Type/binary, Letter>>).

% ---

parse_csv(_Type, <<>>, Map) ->
	Map;

parse_csv(Type, Bin, Map) ->
	case parse_csv_string(standard, $=, Bin, <<>>) of
		{ok, Key, <<>>} ->
			case maps:size(Map) of
				0 -> Key;
				_ -> maps:put(Key, undefined, Map)
			end;
		{ok, Key, <<Rest1/binary>>} ->
			{ok, Value, <<Rest2/binary>>} = parse_csv_string(standard, $,, Rest1, <<>>),
			{K, V} = transform_entry(Type, Key, Value),
			parse_csv(Type, Rest2, maps:put(K, V, Map))
	end.

% ---

parse_csv_string(standard, Delimiter, <<$", Rest/binary>>, <<>>) ->
	parse_csv_string(in_string, Delimiter, Rest, <<>>);

parse_csv_string(in_string, Delimiter, <<$", Rest/binary>>, Value) ->
	parse_csv_string(standard, Delimiter, Rest, Value);

parse_csv_string(standard, _Delimiter, <<>>, Value) ->
	{ok, Value, <<>>};

parse_csv_string(standard, Delimiter, <<Delimiter, Rest/binary>>, Value) ->
	{ok, Value, Rest};

parse_csv_string(Mode, Delimiter, <<Letter, Rest/binary>>, Value) ->
	parse_csv_string(Mode, Delimiter, Rest, <<Value/binary, Letter>>).

% ---

read_line(<<>>, <<>>) ->
	eof;

read_line(<<>>, Line) ->
	{ok, Line, <<>>};

read_line(<<"\n", Rest/binary>>, Line) ->
	{ok, Line, Rest};

read_line(<<"\r", Rest/binary>>, Line) ->
	{ok, Line, Rest};

read_line(<<Letter, Rest/binary>>, Line) when Letter >= 32, Letter =/= 127 ->
	read_line(Rest, <<Line/binary, Letter>>).

% ---

transform_type(<<"#EXTINF">>) -> 'extinf';
transform_type(<<"#EXTM3U">>) -> 'extm3u';
transform_type(<<"#EXT-X-INDEPENDENT-SEGMENTS">>) -> 'ext-x-independent-segments';
transform_type(<<"#EXT-X-MEDIA">>) -> 'ext-x-media';
transform_type(<<"#EXT-X-MEDIA-SEQUENCE">>) -> 'ext-x-media-sequence';
transform_type(<<"#EXT-X-PROGRAM-DATE-TIME">>) -> 'ext-x-program-date-time';
transform_type(<<"#EXT-X-STREAM-INF">>) -> 'ext-x-stream-inf';
transform_type(<<"#EXT-X-TARGETDURATION">>) -> 'ext-x-targetduration';
transform_type(<<"#EXT-X-VERSION">>) -> 'ext-x-version';
transform_type(Type) when is_binary(Type) -> Type.


transform_entry('ext-x-stream-inf', <<"AUDIO">>, V) -> {audio, V};
transform_entry('ext-x-stream-inf', <<"AVERAGE-BANDWIDTH">>, V) -> {'average-bandwidth', binary_to_integer(V)};
transform_entry('ext-x-stream-inf', <<"BANDWIDTH">>, V) -> {bandwidth, binary_to_integer(V)};
transform_entry('ext-x-stream-inf', <<"CODECS">>, V) -> {codecs, csv_list(V)};
transform_entry('ext-x-stream-inf', <<"FRAME-RATE">>, V) -> {'frame-rate', binary_to_float(V)};
transform_entry('ext-x-stream-inf', <<"RESOLUTION">>, V) -> {resolution, resolution_parser(V)};
transform_entry('ext-x-stream-inf', <<"SUBTITLES">>, V) -> {subtitles, V};

transform_entry('ext-x-media', <<"AUTOSELECT">>, V) -> {autoselect, yes_no(V)};
transform_entry('ext-x-media', <<"DEFAULT">>, V) -> {default, yes_no(V)};
transform_entry('ext-x-media', <<"FORCED">>, V) -> {forced, yes_no(V)};
transform_entry('ext-x-media', <<"GROUP-ID">>, V) -> {'group-id', V};
transform_entry('ext-x-media', <<"LANGUAGE">>, V) -> {language, V};
transform_entry('ext-x-media', <<"NAME">>, V) -> {name, V};
transform_entry('ext-x-media', <<"TYPE">>, V) -> {type, media_type(V)};
transform_entry('ext-x-media', <<"URI">>, V) -> {uri, V};

transform_entry(_, K, V) when is_binary(K), is_binary(V) -> {K, V}.



yes_no(<<"YES">>) -> yes;
yes_no(<<"NO">>) -> no;
yes_no(V) -> V.


media_type(<<"AUDIO">>) -> audio;
media_type(<<"SUBTITLES">>) -> subtitles;
media_type(V) -> V.


csv_list(V) -> string:split(V, <<",">>, all).


resolution_parser(V) ->
	lists:map(
		fun binary_to_integer/1,
		string:split(V, <<"x">>, all)
	).
