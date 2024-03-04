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
	};

master_file('ARD') ->
	{
		<<"#EXTM3U
#EXT-X-VERSION:4
#EXT-X-INDEPENDENT-SEGMENTS
#EXT-X-STREAM-INF:BANDWIDTH=1672000,AVERAGE-BANDWIDTH=1460800,VIDEO-RANGE=SDR,CODECS=\"avc1.4d401f,mp4a.40.2\",RESOLUTION=640x360,FRAME-RATE=50.000,AUDIO=\"program_audio\",SUBTITLES=\"subs\",HDCP-LEVEL=NONE
master_640p_1200.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=6520800,AVERAGE-BANDWIDTH=5640800,VIDEO-RANGE=SDR,CODECS=\"avc1.64002a,mp4a.40.2\",RESOLUTION=1920x1080,FRAME-RATE=50.000,AUDIO=\"program_audio\",SUBTITLES=\"subs\",HDCP-LEVEL=NONE
master_1920p_5000.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=4224000,AVERAGE-BANDWIDTH=3660800,VIDEO-RANGE=SDR,CODECS=\"avc1.640020,mp4a.40.2\",RESOLUTION=1280x720,FRAME-RATE=50.000,AUDIO=\"program_audio\",SUBTITLES=\"subs\",HDCP-LEVEL=NONE
master_1280p_3200.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=2182400,AVERAGE-BANDWIDTH=1900800,VIDEO-RANGE=SDR,CODECS=\"avc1.4d401f,mp4a.40.2\",RESOLUTION=960x540,FRAME-RATE=50.000,AUDIO=\"program_audio\",SUBTITLES=\"subs\",HDCP-LEVEL=NONE
master_960p_1600.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=1034000,AVERAGE-BANDWIDTH=910800,VIDEO-RANGE=SDR,CODECS=\"avc1.4d401e,mp4a.40.2\",RESOLUTION=480x270,FRAME-RATE=50.000,AUDIO=\"program_audio\",SUBTITLES=\"subs\",HDCP-LEVEL=NONE
master_480p_700.m3u8
#EXT-X-MEDIA:TYPE=AUDIO,LANGUAGE=\"de\",NAME=\"Deutsch\",AUTOSELECT=YES,DEFAULT=YES,GROUP-ID=\"program_audio\",URI=\"master_audio1_128.m3u8\"
#EXT-X-MEDIA:TYPE=AUDIO,LANGUAGE=\"klare sprache\",NAME=\"Klare Sprache\",AUTOSELECT=NO,DEFAULT=NO,GROUP-ID=\"program_audio\",URI=\"master_audio2_KS_128.m3u8\"
#EXT-X-MEDIA:TYPE=SUBTITLES,NAME=\"Untertitel\",DEFAULT=YES,AUTOSELECT=YES,FORCED=NO,LANGUAGE=\"de\",GROUP-ID=\"subs\",URI=\"master_subs_webvtt.m3u8\"
">>,
		[]
	};

master_file('ARTE.DE') ->
	{
		<<"#EXTM3U
#EXT-X-VERSION:3
#EXT-X-INDEPENDENT-SEGMENTS
#EXT-X-STREAM-INF:BANDWIDTH=3379200,AVERAGE-BANDWIDTH=3256000,CODECS=\"avc1.4d401f,mp4a.40.2\",RESOLUTION=1280x720,FRAME-RATE=25.000
https://artesimulcast.akamaized.net/hls/live/2030993/artelive_de/master_v720.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=3379200,AVERAGE-BANDWIDTH=3256000,CODECS=\"avc1.4d401f,mp4a.40.2\",RESOLUTION=1280x720,FRAME-RATE=25.000
https://artesimulcast.akamaized.net/hls/live/2030993-b/artelive_de/master_v720.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=1856800,AVERAGE-BANDWIDTH=1790800,CODECS=\"avc1.4d401f,mp4a.40.2\",RESOLUTION=960x540,FRAME-RATE=25.000
https://artesimulcast.akamaized.net/hls/live/2030993/artelive_de/master_v540.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=1856800,AVERAGE-BANDWIDTH=1790800,CODECS=\"avc1.4d401f,mp4a.40.2\",RESOLUTION=960x540,FRAME-RATE=25.000
https://artesimulcast.akamaized.net/hls/live/2030993-b/artelive_de/master_v540.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=1056000,AVERAGE-BANDWIDTH=1020800,CODECS=\"avc1.77.30,mp4a.40.2\",RESOLUTION=640x360,FRAME-RATE=25.000
https://artesimulcast.akamaized.net/hls/live/2030993/artelive_de/master_v360.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=1056000,AVERAGE-BANDWIDTH=1020800,CODECS=\"avc1.77.30,mp4a.40.2\",RESOLUTION=640x360,FRAME-RATE=25.000
https://artesimulcast.akamaized.net/hls/live/2030993-b/artelive_de/master_v360.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=655600,AVERAGE-BANDWIDTH=635800,CODECS=\"avc1.4d4015,mp4a.40.2\",RESOLUTION=426x240,FRAME-RATE=25.000
https://artesimulcast.akamaized.net/hls/live/2030993/artelive_de/master_v240.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=655600,AVERAGE-BANDWIDTH=635800,CODECS=\"avc1.4d4015,mp4a.40.2\",RESOLUTION=426x240,FRAME-RATE=25.000
https://artesimulcast.akamaized.net/hls/live/2030993-b/artelive_de/master_v240.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=334400,AVERAGE-BANDWIDTH=325600,CODECS=\"avc1.42c00c,mp4a.40.2\",RESOLUTION=320x180,FRAME-RATE=25.000
https://artesimulcast.akamaized.net/hls/live/2030993/artelive_de/master_v180.m3u8
#EXT-X-STREAM-INF:BANDWIDTH=334400,AVERAGE-BANDWIDTH=325600,CODECS=\"avc1.42c00c,mp4a.40.2\",RESOLUTION=320x180,FRAME-RATE=25.000
https://artesimulcast.akamaized.net/hls/live/2030993-b/artelive_de/master_v180.m3u8
">>,
		[]
	};

master_file('3sat_neu') ->

	{
		<<"#EXTM3U

#EXT-X-INDEPENDENT-SEGMENTS

#EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"A1.1+A2.1+A3.1-3211583617\",NAME=\"TV Ton\",LANGUAGE=\"deu\",DEFAULT=YES,URI=\"https://zdf-hls-18.akamaized.net/hls/live/2016501/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/5/5.m3u8\"
#EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"A1.1+A2.1+A3.1-3211583617\",NAME=\"Klare Sprache / Originalton\",LANGUAGE=\"mul\",URI=\"https://zdf-hls-18.akamaized.net/hls/live/2016501/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/6/6.m3u8\"
#EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"A1.1+A2.1+A3.1-3211583617\",NAME=\"Audio-Deskription\",LANGUAGE=\"deu\",URI=\"https://zdf-hls-18.akamaized.net/hls/live/2016501/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/7/7.m3u8\"

#EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"A1.1+A2.1+A3.1-1566965899\",NAME=\"TV Ton\",LANGUAGE=\"deu\",DEFAULT=YES,URI=\"https://zdf-hls-18.akamaized.net/hls/live/2016501-b/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/5/5.m3u8\"
#EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"A1.1+A2.1+A3.1-1566965899\",NAME=\"Klare Sprache / Originalton\",LANGUAGE=\"mul\",URI=\"https://zdf-hls-18.akamaized.net/hls/live/2016501-b/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/6/6.m3u8\"
#EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"A1.1+A2.1+A3.1-1566965899\",NAME=\"Audio-Deskription\",LANGUAGE=\"deu\",URI=\"https://zdf-hls-18.akamaized.net/hls/live/2016501-b/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/7/7.m3u8\"

#EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID=\"T1-3211583617\",NAME=\"Untertitel deutsch\",CODECS=\"wvtt\",LANGUAGE=\"deu\",DEFAULT=YES,AUTOSELECT=YES,URI=\"https://zdf-hls-18.akamaized.net/hls/live/2016501/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/8/8.m3u8\"

#EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID=\"T1-1566965899\",NAME=\"Untertitel deutsch\",CODECS=\"wvtt\",LANGUAGE=\"deu\",DEFAULT=YES,AUTOSELECT=YES,URI=\"https://zdf-hls-18.akamaized.net/hls/live/2016501-b/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/8/8.m3u8\"

#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=671111,AVERAGE-BANDWIDTH=581111,AUDIO=\"A1.1+A2.1+A3.1-3211583617\",SUBTITLES=\"T1-3211583617\",FRAME-RATE=25.000,RESOLUTION=480x270
https://zdf-hls-18.akamaized.net/hls/live/2016501/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/1/1.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=671111,AVERAGE-BANDWIDTH=581111,AUDIO=\"A1.1+A2.1+A3.1-1566965899\",SUBTITLES=\"T1-1566965899\",FRAME-RATE=25.000,RESOLUTION=480x270
https://zdf-hls-18.akamaized.net/hls/live/2016501-b/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/1/1.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=1173371,AVERAGE-BANDWIDTH=969371,AUDIO=\"A1.1+A2.1+A3.1-3211583617\",SUBTITLES=\"T1-3211583617\",FRAME-RATE=25.000,RESOLUTION=640x360
https://zdf-hls-18.akamaized.net/hls/live/2016501/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/2/2.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=1173371,AVERAGE-BANDWIDTH=969371,AUDIO=\"A1.1+A2.1+A3.1-1566965899\",SUBTITLES=\"T1-1566965899\",FRAME-RATE=25.000,RESOLUTION=640x360
https://zdf-hls-18.akamaized.net/hls/live/2016501-b/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/2/2.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=2257198,AVERAGE-BANDWIDTH=1807198,AUDIO=\"A1.1+A2.1+A3.1-3211583617\",SUBTITLES=\"T1-3211583617\",FRAME-RATE=25.000,RESOLUTION=960x540
https://zdf-hls-18.akamaized.net/hls/live/2016501/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/3/3.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.4d401f,mp4a.40.2\",BANDWIDTH=2257198,AVERAGE-BANDWIDTH=1807198,AUDIO=\"A1.1+A2.1+A3.1-1566965899\",SUBTITLES=\"T1-1566965899\",FRAME-RATE=25.000,RESOLUTION=960x540
https://zdf-hls-18.akamaized.net/hls/live/2016501-b/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/3/3.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.640028,mp4a.40.2\",BANDWIDTH=4504154,AVERAGE-BANDWIDTH=3544154,AUDIO=\"A1.1+A2.1+A3.1-3211583617\",SUBTITLES=\"T1-3211583617\",FRAME-RATE=50.000,RESOLUTION=1280x720
https://zdf-hls-18.akamaized.net/hls/live/2016501/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/4/4.m3u8
#EXT-X-STREAM-INF:CODECS=\"avc1.640028,mp4a.40.2\",BANDWIDTH=4504154,AVERAGE-BANDWIDTH=3544154,AUDIO=\"A1.1+A2.1+A3.1-1566965899\",SUBTITLES=\"T1-1566965899\",FRAME-RATE=50.000,RESOLUTION=1280x720
https://zdf-hls-18.akamaized.net/hls/live/2016501-b/dach/4c5a14a9d03ced57f8e34f50c0bfebd2/4/4.m3u8
">>,
		[]
	}.
