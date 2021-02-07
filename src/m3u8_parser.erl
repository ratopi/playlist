%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2019 11:30
%%%-------------------------------------------------------------------
-module(m3u8_parser).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([parse/1]).

parse(Bin) ->
	{ok, Line, Rest} = read_line(Bin),
	case Line of
		<<"#EXTM3U">> ->
			parse(#{}, Rest, []);
		_ ->
			{error, missing_header}
	end.


parse(Map, Bin, Acc) ->
	case read_line(Bin) of
		eof ->
			% TODO : Check if Map is empty !
			{ok, lists:reverse(Acc)};
		{ok, Line = <<$#, _/binary>>, Rest} ->
			{ok, Directive, Parameters} = parse_directive(Line),
			NewMap = maps:put(Directive, Parameters, Map),
			parse(NewMap, Rest, Acc);
		{ok, URL, Rest} ->
			NewMap = maps:put(url, URL, Map),
			parse(#{}, Rest, [NewMap | Acc])
	end.


% ---

parse_directive(Line) ->
	{ok, Name, Parameter} = parse_directive(start, Line).


parse_directive(start, <<"#EXT-X-STREAM-INF:", Rest/binary>>) ->
	{ok, Name, ParameterString} = parse_directive({parameter, 'ext-x-stream-inf', <<>>}, Rest),
	{ok, Name, convert_parameter(Name, ParameterString)};

parse_directive(start, <<"#EXT-X-VERSION:", Bin/binary>>) ->
	{ok, 'ext-x-version', Bin};

parse_directive(start, <<"#EXT-X-TARGETDURATION:", Bin/binary>>) ->
	{N, <<>>} = string:to_integer(Bin),
	{ok, 'ext-x-targetduration', N};

parse_directive(start, <<"#EXT-X-MEDIA-SEQUENCE:", Bin/binary>>) ->
	{N, <<>>} = string:to_integer(Bin),
	{ok, 'ext-x-media-sequence', N};

parse_directive(start, <<"#EXT-X-PROGRAM-DATE-TIME:", Bin/binary>>) ->
	{ok, 'ext-x-program-date-time', Bin};

parse_directive(start, <<"#EXTINF:", Bin/binary>>) ->
	{N, _} = string:to_float(Bin),
	{ok, 'extinf', #{runtime => N}};

parse_directive(start, <<$#, Rest/binary>>) ->
	parse_directive({name, <<"">>}, Rest);

parse_directive({name, Name}, <<>>) ->
	{ok, Name, nil};

parse_directive({name, Name}, <<":", Rest/binary>>) ->
	parse_directive({parameter, Name, <<"">>}, Rest);

parse_directive({name, Name}, <<Letter, Rest/binary>>) when Letter >= 32, Letter =/= 127 ->
	parse_directive({name, <<Name/binary, Letter>>}, Rest);

parse_directive({parameter, Name, Parameter}, <<>>) ->
	{ok, Name, Parameter};

parse_directive({parameter, Name, Parameter}, <<Letter, Rest/binary>>) when Letter >= 32, Letter =/= 127 ->
	parse_directive({parameter, Name, <<Parameter/binary, Letter>>}, Rest).


% ---

convert_parameter(Type = 'ext-x-stream-inf', ParameterString) ->
	maps:from_list(
		lists:map(
			fun(E = {_, _}) ->
				map_key(Type, E)
			end,
			maps:to_list(
				parse_csv(ParameterString)
			)
		)
	);

convert_parameter(_, P) ->
	P.

% ---

map_key('ext-x-stream-inf', {<<"CODECS">>, Value}) ->
	{'codecs', string:split(Value, <<",">>)};
map_key('ext-x-stream-inf', {<<"BANDWIDTH">>, Value}) ->
	{'bandwidth', n_only(string:to_integer(Value))};
map_key('ext-x-stream-inf', {<<"AVERAGE-BANDWIDTH">>, Value}) ->
	{'average-bandwidth', n_only(string:to_integer(Value))};
map_key('ext-x-stream-inf', {<<"FRAME-RATE">>, Value}) ->
	{'frame-rate', n_only(string:to_float(Value))};
map_key('ext-x-stream-inf', {<<"RESOLUTION">>, Value}) ->
	[A, B] = string:split(Value, <<"x">>),
	W = n_only(string:to_integer(A)),
	H = n_only(string:to_integer(B)),
	{'resolution', [W, H]};
% map_key('ext-x-stream-inf', {<<"PROGRAM-ID">>, Value}) ->
%    {'program-id', Value};
map_key(_, X) -> X.

% ---

n_only({N, <<>>}) -> N;
n_only(X) -> {error, {n_only, X}}.

% ---

parse_csv(Bin) ->
	parse_csv(Bin, #{}).


parse_csv(<<>>, Map) ->
	Map;

parse_csv(Bin, Map) ->
	{ok, Key, <<Rest1/binary>>} = parse_get_string(standard, $=, Bin, <<>>),
	{ok, Value, <<Rest2/binary>>} = parse_get_string(standard, $,, Rest1, <<>>),
	parse_csv(
		Rest2,
		maps:put(Key, Value, Map)
	).

% ---

parse_get_string(standard, Delimiter, <<$", Rest/binary>>, <<>>) ->
	parse_get_string(in_string, Delimiter, Rest, <<>>);

parse_get_string(in_string, Delimiter, <<$", Rest/binary>>, Value) ->
	parse_get_string(standard, Delimiter, Rest, Value);

parse_get_string(standard, _Delimiter, <<>>, Value) ->
	{ok, Value, <<>>};

parse_get_string(standard, Delimiter, <<Delimiter, Rest/binary>>, Value) ->
	{ok, Value, Rest};

parse_get_string(Mode, Delimiter, <<Letter, Rest/binary>>, Value) ->
	parse_get_string(Mode, Delimiter, Rest, <<Value/binary, Letter>>).

% ---

read_line(<<>>) ->
	eof;

read_line(Bin) ->
	case read_line(Bin, <<>>) of
		{ok, <<>>, Rest} ->
			read_line(Rest);
		R ->
			R
	end.



read_line(<<>>, Line) ->
	{ok, Line, <<>>};

read_line(<<"\n", Rest/binary>>, Line) ->
	{ok, Line, Rest};

read_line(<<"\r", Rest/binary>>, Line) ->
	{ok, Line, Rest};

read_line(<<Letter, Rest/binary>>, Line) when Letter >= 32, Letter =/= 127 ->
	read_line(Rest, <<Line/binary, Letter>>).
