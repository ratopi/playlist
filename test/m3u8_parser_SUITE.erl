%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2019 13:11
%%%-------------------------------------------------------------------
-module(m3u8_parser_SUITE).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-include_lib("common_test/include/ct.hrl").

%% API
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([test1/1]).

all() -> [
	test1
].

% ---

init_per_suite(Config) ->
	Config.


end_per_suite(_Config) ->
	ok.

init_per_testcase(_, Config) ->
	Config.


end_per_testcase(_, _Config) ->
	ok.

% ---

test1(_Config) ->
	{Content, Expected} = master_file('3sat'),
	{ok, Expected} = m3u8_parser:parse(Content).


% ---

master_file('3sat') ->
	{
		<<"#EXTM3U


#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=692095,AVERAGE-BANDWIDTH=578095,FRAME-RATE=25.000,RESOLUTION=480x272
https://zdfhls18-i.akamaihd.net/hls/live/744751/dach/1/1.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=692095,AVERAGE-BANDWIDTH=578095,FRAME-RATE=25.000,RESOLUTION=480x272
https://zdfhls18-i.akamaihd.net/hls/live/744751-b/dach/1/1.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=1088617,AVERAGE-BANDWIDTH=884617,FRAME-RATE=25.000,RESOLUTION=640x360
https://zdfhls18-i.akamaihd.net/hls/live/744751/dach/2/2.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=1088617,AVERAGE-BANDWIDTH=884617,FRAME-RATE=25.000,RESOLUTION=640x360
https://zdfhls18-i.akamaihd.net/hls/live/744751-b/dach/2/2.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=2040269,AVERAGE-BANDWIDTH=1620269,FRAME-RATE=25.000,RESOLUTION=852x480
https://zdfhls18-i.akamaihd.net/hls/live/744751/dach/3/3.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=2040269,AVERAGE-BANDWIDTH=1620269,FRAME-RATE=25.000,RESOLUTION=852x480
https://zdfhls18-i.akamaihd.net/hls/live/744751-b/dach/3/3.m3u8

">>,

		[
			#{
				'ext-x-stream-inf' =>
				#{
					'average-bandwidth' => 578095,
					bandwidth => 692095,
					codecs => [<<"avc1.4d401f">>, <<"mp4a.40.2">>],
					'frame-rate' => 25.0,
					resolution => [480, 272]
				},
				url => <<"https://zdfhls18-i.akamaihd.net/hls/live/744751/dach/1/1.m3u8">>
			},
			#{
				'ext-x-stream-inf' =>
				#{'average-bandwidth' => 578095, bandwidth => 692095,
					codecs => [<<"avc1.4d401f">>, <<"mp4a.40.2">>],
					'frame-rate' => 25.0,
					resolution => [480, 272]},
				url => <<"https://zdfhls18-i.akamaihd.net/hls/live/744751-b/dach/1/1.m3u8">>
			},
			#{
				'ext-x-stream-inf' =>
				#{
					'average-bandwidth' => 884617, bandwidth => 1088617,
					codecs => [<<"avc1.4d401f">>, <<"mp4a.40.2">>],
					'frame-rate' => 25.0,
					resolution => [640, 360]
				},
				url => <<"https://zdfhls18-i.akamaihd.net/hls/live/744751/dach/2/2.m3u8">>
			},
			#{
				'ext-x-stream-inf' =>
				#{
					'average-bandwidth' => 884617, bandwidth => 1088617,
					codecs => [<<"avc1.4d401f">>, <<"mp4a.40.2">>],
					'frame-rate' => 25.0,
					resolution => [640, 360]
				},
				url => <<"https://zdfhls18-i.akamaihd.net/hls/live/744751-b/dach/2/2.m3u8">>},
			#{'ext-x-stream-inf' =>
			#{'average-bandwidth' => 1620269, bandwidth => 2040269,
				codecs => [<<"avc1.4d401f">>, <<"mp4a.40.2">>],
				'frame-rate' => 25.0,
				resolution => [852, 480]},
				url =>
				<<"https://zdfhls18-i.akamaihd.net/hls/live/744751/dach/3/3.m3u8">>},
			#{'ext-x-stream-inf' =>
			#{'average-bandwidth' => 1620269, bandwidth => 2040269,
				codecs => [<<"avc1.4d401f">>, <<"mp4a.40.2">>],
				'frame-rate' => 25.0,
				resolution => [852, 480]},
				url =>
				<<"https://zdfhls18-i.akamaihd.net/hls/live/744751-b/dach/3/3.m3u8">>}]
	}.
