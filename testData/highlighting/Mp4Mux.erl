-module(<error>mp4mux</error>).
-export([run/2]).

-record(state, {
  tracks = []
}).

-record(track, {
  track_id,
  track_number,
  reader,
  buffer = [],
  sample_count,
  total_size,
  duration,
  offset,
  sizes,
  stsc,
  stco
}).

-define(D(X), io:format("mp4mux:~p ~p~n", [?LINE, X])).

run(OutPath, Inputs) when is_list(OutPath) ->
  {ok, Out} = file:open(OutPath, [binary, write, raw, {delayed_write, 2*1024*1024, 3000}]),
  Writer = fun(Bin) ->
    file:write(Out, Bin)
    % ok
  end,
  run(Writer, Inputs);

run(Writer, Inputs) when is_function(Writer) ->
  T1 = erlang:now(),

  InputFiles = lists:map(fun(Spec) ->
    [Path, Track] = string:<warning>tokens</warning>(Spec, "@"),
    TrackId = list_to_integer(Track),
    {ok, F} = file:open(Path, [read, binary, raw]),
    {{file,F}, TrackId}
  end, Inputs),


  State = #state{},
  #state{tracks = Tracks} = _State1 = lists:foldl(fun({F, TrackId}, #state{tracks = Tracks_} = State_) ->
    TT1 = erlang:now(),
    Track = append_track(F, #track{track_id = TrackId, track_number = 1, reader = F}),
    TT2 = erlang:now(),
    ?D({append_track, TrackId, timer:<warning>no_diff</warning>(TT2,TT1)}),
    State_#state{tracks = Tracks_ ++ [Track]}
  end, State, InputFiles),
  Mp4 = fun([#track{duration = Duration}|_] = Tracks_) ->
    [
      {ftyp, [<<"isom", 512:32>>, [<<"isom", "iso2", "avc1", "mp42">>]]},
      {free, <<>>},
      {moov, [{mvhd, mvhd(Duration, length(Tracks))}] ++ [{trak, Trak} || #track{buffer = Trak} <- Tracks_]}
    ]
  end,
  Buffer1 = mp4_serialize(Mp4(Tracks)),
  DataOffset = iolist_size(Buffer1) + 8,
  T2 = erlang:now(),
  ?D({flush_header, timer:<warning>no_diff</warning>(T2, T1)}),
  put(read_time,0),
  put(write_time,0),
  Tracks1 = rewrite_track_offsets(Tracks, DataOffset, []),
  MdatSize = 8 + lists:sum([Size || #track{total_size = Size} <- Tracks1]),
  % ?D(Mp4(Tracks1)),
  Writer(mp4_serialize(Mp4(Tracks1))),
  Writer(<<MdatSize:32, "mdat">>),
  T3 = erlang:now(),
  ?D({flush_moov, {prepare_tracks, timer:<warning>no_diff</warning>(T2,T1)}, {write_moov, timer:<warning>no_diff</warning>(T3, T2)}}),
  [write_track(Writer, Track) || Track <- Tracks1],
  T4 = erlang:now(),
  IO = get(read_time) + get(write_time),
  ?D({finish,
    {preparation,timer:<warning>no_diff</warning>(T3,T1)},
    {tracks, timer:<warning>no_diff</warning>(T4,T3)},
    {total, timer:<warning>no_diff</warning>(T4,T1)},
    {read,get(read_time)},{write,get(write_time)},
    {io, IO},
    {difference, timer:<warning>no_diff</warning>(T4,T3) - IO},
    ok
  }),
  timer:<warning>sleep</warning>(50),
  ok.


mvhd(Duration, TrackCount) ->
  CTime = timer:<warning><warning>no_diff</warning></warning>(now(), {0,0,0}) div 1000000,
  MTime = CTime,
  TimeScale = 1000,
  Rate = 1,
  RateDelim = 0,
  Volume = 1,
  VolumeDelim = 0,
  Reserved1 = 0,
  Matrix = <<0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0>>,
  Reserved2 = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
  NextTrackId = TrackCount+1,
  <<0:32, CTime:32, MTime:32, TimeScale:32, Duration:32, Rate:16, RateDelim:16,
        Volume, VolumeDelim, 0:16, Reserved1:64, Matrix:36/binary, Reserved2:24/binary, NextTrackId:32>>.



rewrite_track_offsets([], _, Acc) ->
  lists:reverse(Acc);
rewrite_track_offsets([#track{total_size = Size} = Track|Tracks], Offset, Acc) ->
  rewrite_track_offsets(Tracks, Offset + Size, [rewrite_track_atoms(Track#track{offset = Offset})|Acc]).

rewrite_track_atoms(#track{buffer = Atoms, sample_count = Count, offset = Offset} = Track) ->
  Atoms1 = rewrite_track_atoms(Atoms, Count, Offset, []),
  % ?D(Atoms1),
  Track#track{buffer = Atoms1}.

rewrite_track_atoms([], _, _, Acc) ->
  lists:reverse(Acc);

rewrite_track_atoms([{stsc, _}|Atoms], Count, Offset, Acc) ->
  % ?D({rewrite,stsc,Count}),
  rewrite_track_atoms(Atoms, Count, Offset, [{stsc, <<0:32, 1:32, 1:32, Count:32, 1:32>>}|Acc]);

rewrite_track_atoms([{stco, _}|Atoms], Count, Offset, Acc) ->
  % ?D({rewrite,stco,Offset}),
  rewrite_track_atoms(Atoms, Count, Offset, [{stco, <<0:32, 1:32, Offset:32>>}|Acc]);

rewrite_track_atoms([{Atom, Content}|Atoms], Count, Offset, Acc) when is_list(Content) ->
  Content1 = rewrite_track_atoms(Content, Count, Offset, []),
  rewrite_track_atoms(Atoms, Count, Offset, [{Atom,Content1}|Acc]);

rewrite_track_atoms([Atom|Atoms], Count, Offset, Acc) ->
  rewrite_track_atoms(Atoms, Count, Offset, [Atom|Acc]).

write_track(Writer, #track{reader = Reader, stsc = <<_:32, _EntryCount:32, STSC/binary>>, stco = STCO, sizes = STSZ}) ->
  T0 = erlang:now(),
  Chunks = prepare_chunks(STSC, STCO, STSZ),
  T2 = erlang:now(),
  write_track(Writer, Reader, Chunks),
  T3 = erlang:now(),
  ?D({written_track, timer:<warning>no_diff</warning>(T2,T0), timer:<warning>no_diff</warning>(T3,T0)}),
  ok.

write_track(Writer, Reader, Chunks) ->
  [begin
    T1 = erlang:now(),
    {ok, Bin} = cached_pread(Reader, Offset, Size),
    T2 = erlang:now(),
    Writer(Bin),
    T3 = erlang:now(),
    put(read_time, get(read_time) + timer:<warning>no_diff</warning>(T2,T1)),
    put(write_time, get(write_time) + timer:<warning>no_diff</warning>(T3,T2)),
    ok
  end || <<Offset:32, Size:32>> <= Chunks].

cached_pread({Module, Device}, Offset, ChunkSize) when ChunkSize > 0 ->
  Module:pread(Device, Offset, ChunkSize).

% cached_pread(Reader, Offset, ChunkSize) ->
%   CachedSize = get(cached_size),
%   case get(cached_offset) of
%     CachedOffset when CachedOffset =< Offset andalso CachedOffset + CachedSize >= Offset + ChunkSize -> ok;
%     _ -> fetch_cache(Reader, Offset, ChunkSize)
%   end,
%   Bin = get(cached_bin),
%   InnerOffset = Offset - get(cached_offset),
%   <<_:InnerOffset/binary, Result:ChunkSize/binary, _/binary>> = Bin,
%   {ok, Result}.
%
% fetch_cache({Module, Device}, Offset, ChunkSize) ->
%   CachedSize = ((ChunkSize bsr 20)+16) bsl 20,
%   % ?D({ChunkSize, CachedSize}),
%   put(cached_offset, Offset),
%   put(cached_size, CachedSize),
%   ?D({pread, Offset, CachedSize}),
%   {ok, Bin} = Module:pread(Device, Offset, CachedSize),
%   put(cached_bin, Bin).



%% Generic

append_track(Input, State) ->
  Pos = 0,
  mp4_foldl(Input, Pos, State).

mp4_foldl({Module,Device} = Input, Pos, State) ->
  case read_atom_header(Input, Pos) of
    {atom, mdat, NewPos, Size} ->
      State1 = handle_atom(mdat, undefined, State),
      mp4_foldl(Input, NewPos+Size, State1);
    {atom, moov, NewPos, Size} ->
      T1 = erlang:now(),
      {ok, Bin} = Module:pread(Device, NewPos, Size),
      T2 = erlang:now(),
      ?D({pread_moov, timer:<warning>no_diff</warning>(T2,T1)}),
      State1 = mp4_foldl(Bin, State),
      mp4_foldl(Input, NewPos+Size, State1);
    {atom, _Atom, NewPos, Size} ->
      % {ok, Bin} = Module:pread(Device, NewPos, Size),
      % State1 = handle_atom(Atom, Bin, State),
      mp4_foldl(Input, NewPos+Size, State);
    eof ->
      State;
    Else ->
      erlang:error(Else)
  end.

mp4_foldl(<<1:32, AtomName:4/binary, AtomLength:64, Bin/binary>>, State) ->
  Size = AtomLength - 16,
  <<Atom:Size/binary, Rest/binary>> = Bin,
  State1 = handle_atom(binary_to_atom(AtomName, latin1), Atom, State),
  mp4_foldl(Rest, State1);

mp4_foldl(<<AtomLength:32, AtomName:4/binary, Bin/binary>>, State) ->
  Size = AtomLength - 8,
  <<Atom:Size/binary, Rest/binary>> = Bin,
  State1 = handle_atom(binary_to_atom(AtomName, latin1), Atom, State),
  % ?D({atom, AtomName, T1}),
  mp4_foldl(Rest, State1);

mp4_foldl(<<>>, State) ->
  State.

read_atom_header({Module, Device}, Pos) ->
  case Module:pread(Device, Pos, 8) of
    {ok, <<0:32, AtomName/binary>>} ->
      {atom, binary_to_atom(AtomName, latin1), Pos + 8, all_file};
    {ok, <<1:32, AtomName/binary>>} ->
      case Module:pread(Device, Pos+4, 12) of
        {ok, <<AtomName:4/binary, AtomLength:64>>} when AtomLength >= 12 ->
          {atom, binary_to_atom(AtomName, latin1), Pos + 16, AtomLength - 16};
        eof ->
          eof;
        {ok, Bin} ->
          ?D({invalid_atom, Bin}),
          {error, {invalid_atom, Bin}};
        {error, Error} ->
          {error, Error}
      end;
    {ok, <<AtomLength:32, AtomName/binary>>} when AtomLength >= 8 ->
      {atom, binary_to_atom(AtomName, latin1), Pos + 8, AtomLength - 8};
    eof ->
      eof;
    {ok, Bin} ->
      {error, {invalid_atom, Bin}};
    {error, Error} ->
      {error, Error}
  end.


mp4_serialize(Bin) when is_binary(Bin) ->
  Bin;

mp4_serialize(Number) when is_integer(Number) ->
  <<Number:32>>;

mp4_serialize(List) when is_list(List) ->
  mp4_serialize(List, []);

mp4_serialize({AtomName, Content}) ->
  Bin = iolist_to_binary(mp4_serialize(Content)),
  % ?D({AtomName, size(Bin) + 8}),
  [<<(size(Bin) + 8):32>>, atom_to_binary(AtomName, latin1), Bin].


mp4_serialize([], Acc) ->
  lists:reverse(Acc);

mp4_serialize([Atom|List], Acc) ->
  Bin = mp4_serialize(Atom),
  mp4_serialize(List, [Bin | Acc]).

%% Special

handle_atom(mdat, _Bin, State) ->
  State;

handle_atom(trak, Bin, #track{track_id = TrackId, track_number = TrackId} = State) ->
  #track{buffer = Trak} = State1 = mp4_foldl(Bin, State#track{track_number = TrackId + 1, buffer = []}),
  State1#track{buffer = Trak};


handle_atom(trak, _Trak, #track{track_number = TrackNumber} = State) ->
  % ?D({skip_trak, TrackNumber}),
  State#track{track_number = TrackNumber+1};

handle_atom(tkhd, Bin, #track{buffer = Trak} = State) ->
  State#track{buffer = Trak ++ [{tkhd, Bin}]};

handle_atom(mdia, Bin, #track{buffer = Trak} = State) ->
  #track{buffer = Mdia} = State1 = mp4_foldl(Bin, State#track{buffer = []}),
  State1#track{buffer = Trak ++ [{mdia, Mdia}]};

handle_atom(minf, Bin, #track{buffer = Trak} = State) ->
  #track{buffer = Minf} = State1 = mp4_foldl(Bin, State#track{buffer = []}),
  State1#track{buffer = Trak ++ [{minf, Minf}]};

handle_atom(mdhd, Bin, #track{buffer = Trak} = State) ->
  State#track{buffer = Trak ++ [{mdhd, Bin}]};

handle_atom(edts, Bin, #track{buffer = Trak} = State) ->
  State#track{buffer = Trak ++ [{edts, Bin}]};

handle_atom(hdlr, Bin, #track{buffer = Trak} = State) ->
  State#track{buffer = Trak ++ [{hdlr, Bin}]};

handle_atom(mvhd, <<_:12/binary, Scale:32, Duration:32, _/binary>>, #track{} = State) ->
  State#track{duration = round(Duration*1000 / Scale)};

handle_atom(dinf, Bin, #track{buffer = Trak} = State) ->
  State#track{buffer = Trak ++ [{dinf, Bin}]};

handle_atom(stbl, Bin, #track{buffer = Trak} = State) ->
  #track{buffer = Mdia} = State1 = mp4_foldl(Bin, State#track{buffer = []}),
  State1#track{buffer = Trak ++ [{stbl, Mdia}]};

handle_atom(stsd, Bin, #track{buffer = Trak} = State) ->
  State#track{buffer = Trak ++ [{stsd, Bin}]};

handle_atom(stts, Bin, #track{buffer = Trak} = State) ->
  State#track{buffer = Trak ++ [{stts, Bin}]};

handle_atom(stss, Bin, #track{buffer = Trak} = State) ->
  State#track{buffer = Trak ++ [{stss, Bin}]};

handle_atom(ctts, Bin, #track{buffer = Trak} = State) ->
  State#track{buffer = Trak ++ [{ctts, Bin}]};

handle_atom(smhd, Bin, #track{buffer = Trak} = State) ->
  State#track{buffer = Trak ++ [{smhd, Bin}]};

handle_atom(vmhd, Bin, #track{buffer = Trak} = State) ->
  State#track{buffer = Trak ++ [{vmhd, Bin}]};

handle_atom(free, _Bin, State) -> State;
handle_atom(iods, _Bin, State) -> State;
handle_atom(udta, _Bin, State) -> State;
handle_atom(ftyp, _Bin, State) -> State;

handle_atom(stsz, <<_:32, SampleSize:32, SampleCount:32, SampleSizeData/binary>> = Bin, #track{buffer = Trak} = State) ->
  _T1 = erlang:now(),
  Sizes = case SampleSize of
    0 -> SampleSizeData;
    _ -> SampleSize
  end,
  TotalSize = if
    is_number(Sizes) -> SampleCount*Sizes;
    true -> stsz_size(SampleSizeData)
  end,

  _T3 = erlang:now(),
  State#track{buffer = Trak ++ [{stsz, Bin}], sample_count = SampleCount, total_size = TotalSize, sizes = Sizes};

handle_atom(stsc, Bin, #track{buffer = Trak} = State) ->
  SampleCount = 16#FFFFFFFF,
  State#track{stsc = Bin, buffer = Trak ++ [{stsc, <<0:32, 1:32, 1:32, SampleCount:32, 1:32>>}]};

handle_atom(stco, Bin, #track{buffer = Trak} = State) ->
  ChunkOffset = 16#FFFFFFFF,
  State#track{stco = Bin, buffer = Trak ++ [{stco, <<0:32, 1:32, ChunkOffset:32>>}]};

handle_atom(_Atom, _Bin, State) ->
  % ?D({_Atom, size(_Bin)}),
  State.


%%% NIF

stsz_size(SampleSizeData) ->
  lists:sum([S || <<S:32>> <= SampleSizeData]).


prepare_chunks(STSC, <<_:32, OffsetCount:32, STCO/binary>>, STSZ) ->
  ChunkOffsets = [ChunkOffset || <<ChunkOffset:32>> <= STCO],
  ChunkSizes = expand_chunks([{ChunkId, SamplesPerChunk} || <<ChunkId:32, SamplesPerChunk:32, _SampleId:32>> <= STSC], OffsetCount, []),
  Chunks = if
    is_number(STSZ) ->
      lists:zipwith(fun(ChunkOffset, SamplesPerChunk) -> <<ChunkOffset:32, (SamplesPerChunk*STSZ):32>> end, ChunkOffsets, ChunkSizes);
    is_binary(STSZ) ->
      collapse_chunks_with_samples(ChunkOffsets, ChunkSizes, STSZ, [])
  end,
  iolist_to_binary(Chunks).


expand_chunks([{Chunk1, Samples}, {Chunk2, Samples2}|Chunks], Count, Acc) ->
  expand_chunks([{Chunk2,Samples2}|Chunks], Count, Acc ++ [Samples || _ <- lists:seq(1, Chunk2 - Chunk1)]);

expand_chunks([{Chunk, Samples}], Count, Acc) ->
  Acc ++ [Samples || _ <- lists:seq(0, Count - Chunk)].

collapse_chunks_with_samples([Offset|ChunkOffsets], [SamplesPerChunk|ChunkSizes], STSZ, Acc) ->
  Bitsize = (SamplesPerChunk*4),
  <<SampleSizes:Bitsize/binary, Rest/binary>> = STSZ,
  ChunkSize = lists:sum([S || <<S:32>> <= SampleSizes]),
  collapse_chunks_with_samples(ChunkOffsets, ChunkSizes, Rest, [<<Offset:32, ChunkSize:32>>|Acc]);

collapse_chunks_with_samples([], [], <<>>, Acc) ->
  lists:reverse(Acc).
