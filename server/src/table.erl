-module(table).

-include("tarabish_types.hrl").

-behaviour(gen_server).

-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-export([chat/3, join/3, sit/4, start_game/2]).

% Seat record will have more, like cards, for now it matches the view.
-record(seat, {open, name}).
-record(person, {name, client, seat}).
-record(state, {id, seats, observers, members, game}).

% Public:
start(Id) ->
  gen_server:start(?MODULE, [Id], []).

%% TODO: check if table is still alive
chat(Table, From, Message) ->
  gen_server:cast(Table, {chat, From, Message}).

join(Table, ClientName, Client) ->
  gen_server:call(Table, {join, ClientName, Client}).

sit(Table, ClientName, Client, Seat) ->
  gen_server:call(Table, {sit, ClientName, Client, Seat}).

start_game(Table, ClientName) ->
  gen_server:call(Table, {start_game, ClientName}).

% gen_server:

init([Id]) ->
  Seats = make_seats(4),
  State = #state{id=Id, seats=Seats, observers=[], members=orddict:new(),
                 game=none},
  update_server(State),
  {ok, State}.

handle_call({join, ClientName, Client}, _From, State) ->
  case orddict:find(ClientName, State#state.members) of
    {ok, _Person} ->
      {reply, {error, already_joined}, State};
    error ->
      Person = #person{name=ClientName, client=Client, seat=none},
      NewMembers = orddict:store(ClientName, Person, State#state.members),
      Observers = [ClientName|State#state.observers],
      NewState = State#state{members=NewMembers, observers=Observers},
      update_server(NewState),
      {reply, ok, NewState}
  end;

handle_call({sit, ClientName, Client, SeatNum}, _From, State) ->
  Seat = get_seat(State, SeatNum),
  if Seat#seat.open == true ->
    Seat1 = #seat{name=ClientName, open=false},
    NewSeats = setelement(SeatNum + 1, State#state.seats, Seat1),
    case orddict:find(ClientName, State#state.members) of
      {ok, #person{seat=none} = Person} ->
        NewPerson = Person#person{seat=SeatNum},
        NewMembers = orddict:store(ClientName, NewPerson, State#state.members),
        NewState = State#state{members=NewMembers, seats=NewSeats},
        update_server(NewState),
        {reply, ok, NewState};
      {ok, _Person} ->
          {reply, {error, already_seated}, State};
      error -> % Not at table, join
        NewPerson = #person{name=ClientName, client=Client, seat=SeatNum},
        NewMembers = orddict:store(ClientName, NewPerson, State#state.members),
        NewState = State#state{members=NewMembers, seats=NewSeats},
        update_server(NewState),
        {reply, ok, NewState}
    end;
  true ->
    {reply, {error, seat_taken}, State}
  end;

handle_call({start_game, ClientName}, _From, #state{game=none} = State) ->
  case orddict:find(ClientName, State#state.members) of
    {ok, #person{seat=none}} ->
      {reply, {error, not_authorized}, State};
    {ok, _Person} ->
      % TODO: actually start game
      send_chat(State#state.id, "Game Started", State#state.members),
      {reply, ok, State#state{game=started}};
    error ->
      {reply, {error, not_at_table}, State}
  end;

handle_call({start_game, ClientName}, _From, State) ->
  {reply, {error, already_started}, State};

handle_call(Request, _From, State) ->
  io:format("~w received unknown call ~p~n",
    [?MODULE, Request]),
  {stop, "Bad Call", State}.

handle_cast({chat, From, Message}, State) ->
  ExpandedMessage = bjoin([From, <<" --> ">>, Message]),
  send_chat(State#state.id, ExpandedMessage, State#state.members),
  {noreply, State};

handle_cast(Msg, State) ->
  io:format("~w received unknown cast ~p~n",
    [?MODULE, Msg]),
  {stop, "Bad Cast", State}.

handle_info(Info, State) ->
  io:format("~w recieved unknown info ~p~n",
    [?MODULE, Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private:
send_chat_1(_TableId, _Message, []) ->
  ok;

send_chat_1(TableId, Message, [Person|Others]) ->
  Client = Person#person.client,
  client:recv_chat(Client, TableId, Message),
  send_chat_1(TableId, Message, Others).

send_chat(TableId, Message, MemberDict) ->
  {_Ids, Members} = lists:unzip(orddict:to_list(MemberDict)),
  send_chat_1(TableId, Message, Members).

bjoin(List) ->
  F = fun(A, B) -> <<A/binary, B/binary>> end,
  lists:foldr(F, <<>>, List).

% Passes a list, but returns a tuple.
make_seats(Num) ->
  make_seats(Num, []).

make_seats(0, Seats) ->
  erlang:list_to_tuple(Seats);

make_seats(Num, Seats) ->
  make_seats(Num - 1, [#seat{open=true}|Seats]).

get_seat(State, SeatNum) ->
  element(SeatNum + 1, State#state.seats).

update_server(State) ->
  View = make_table_view(State),
  tarabish_server:update_table_image(State#state.id, View).

make_table_view(State) ->
  Seats = erlang:tuple_to_list(State#state.seats),
  ViewSeats = make_seats_views(Seats),
  #tableView{tableId=State#state.id,
             seats=ViewSeats,
             observers=State#state.observers}.

make_seats_views(Seats) ->
  make_seats_views(Seats, []).

make_seats_views([], Views) ->
  lists:reverse(Views);

make_seats_views([Seat|Rest], Views) ->
  SeatView = make_one_seat_view(Seat),
  make_seats_views(Rest, [SeatView|Views]).

% Seat name to empty string for UI
make_one_seat_view(#seat{open=true} = _Seat) ->
  #seatView{isOpen=true, name=""};

make_one_seat_view(Seat) ->
  #seatView{isOpen=false, name=Seat#seat.name}.

