-module(table).

-include("tarabish_types.hrl").
-include("client.hrl").

-behaviour(gen_server).

-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-export([chat/3, join/3, part/2, sit/4, stand/2,
    start_game/2, call_trump/3, play_card/3]).

%% From game
-export([broadcast/2, deal3/3]).

-record(person, {name, client, seat}).
-record(state, {id, seats, observers, members, game}).

% Public:
start(Id) ->
  gen_server:start(?MODULE, [Id], []).

%% TODO: check if table is still alive
chat(Table, From, Message) ->
  ExpandedMessage = bjoin([From, <<" --> ">>, Message]),
  Event = #event{type=?tarabish_EventType_CHAT, message=ExpandedMessage},
  broadcast(Table, Event).

join(Table, ClientName, Client) ->
  gen_server:call(Table, {join, ClientName, Client}).

part(Table, ClientName) ->
  gen_server:call(Table, {part, ClientName}).

sit(Table, ClientName, Client, Seat) ->
  gen_server:call(Table, {sit, ClientName, Client, Seat}).

stand(Table, ClientName) ->
  gen_server:call(Table, {stand, ClientName}).

start_game(Table, ClientName) ->
  gen_server:call(Table, {start_game, ClientName}).

call_trump(Table, ClientName, Suit) ->
  gen_server:call(Table, {call_trump, ClientName, Suit}).

play_card(Table, ClientName, Card) ->
  gen_server:call(Table, {play_card, ClientName, Card}).

% From Game:
broadcast(Table, Event) ->
  gen_server:cast(Table, {broadcast, Event}).

deal3(Table, Dealer, Cards) ->
  gen_server:cast(Table, {deal3, Dealer, Cards}).

% gen_server:

init([Id]) ->
  process_flag(trap_exit, true),
  Seats = {empty, empty, empty, empty},
  State = #state{id=Id, seats=Seats, observers=[], members=orddict:new(),
                 game=none},
  update_server(State),
  {ok, State}.

handle_call({join, ClientName, Client}, _From, State) ->
  case orddict:find(ClientName, State#state.members) of
    {ok, _Person} ->
      {reply, {error, already_joined}, State};
    error ->
      Event = #event{type=?tarabish_EventType_JOIN,
                     name=ClientName},
      send_event_all(Event, State),
      Person = #person{name=ClientName, client=Client, seat=none},
      NewMembers = orddict:store(ClientName, Person, State#state.members),
      Observers = [ClientName|State#state.observers],
      NewState = State#state{members=NewMembers, observers=Observers},
      update_server(NewState),
      {reply, ok, NewState}
  end;

handle_call({part, ClientName}, _From, State) ->
  Event = #event{type=?tarabish_EventType_PART,
                 name=ClientName},
  case orddict:find(ClientName, State#state.members) of
    {ok, #person{seat=none} = _Person} ->
      send_event_all(Event, State),
      NewMembers = orddict:erase(ClientName, State#state.members),
      NewObservers = lists:delete(ClientName, State#state.observers),
      NewState = State#state{members=NewMembers, observers=NewObservers},
      update_server(NewState),
      {reply, ok, NewState};
    {ok, #person{seat=SeatNum} = _Person} ->
      send_event_all(Event, State),
      cancel_game(State),
      NewMembers = orddict:erase(ClientName, State#state.members),
      NewSeats = setelement(SeatNum + 1, State#state.seats, empty),
      NewState = State#state{members=NewMembers, seats=NewSeats, game=none},
      update_server(NewState),
      {reply, ok, NewState};
    error ->
      {reply, {error, no_at_table}, State}
  end;

handle_call({sit, ClientName, Client, SeatNum}, _From, State)
  when SeatNum >= 0, SeatNum < 4 ->
  Seat = get_seat(State, SeatNum),
  if Seat == empty ->
    ClientRec = #client{name=ClientName, pid=Client},
    NewSeats = setelement(SeatNum + 1, State#state.seats, ClientRec),
    Event = #event{type=?tarabish_EventType_SIT,
                   name=ClientName,
                   seat=SeatNum},
    case orddict:find(ClientName, State#state.members) of
      {ok, #person{seat=none} = Person} ->

        send_event_all(Event, State),
        NewPerson = Person#person{seat=SeatNum},
        NewMembers = orddict:store(ClientName, NewPerson, State#state.members),
        NewObservers = lists:delete(ClientName, State#state.observers),
        NewState = State#state{members=NewMembers, seats=NewSeats, observers=NewObservers},
        update_server(NewState),
        {reply, ok, NewState};
      {ok, _Person} ->
          {reply, {error, already_seated}, State};
      error -> % Not at table, join
        send_event_all(Event, State),
        NewPerson = #person{name=ClientName, client=Client, seat=SeatNum},
        NewMembers = orddict:store(ClientName, NewPerson, State#state.members),
        NewState = State#state{members=NewMembers, seats=NewSeats},
        update_server(NewState),
        {reply, ok, NewState}
    end;
  true ->
    {reply, {error, seat_taken}, State}
  end;

handle_call({sit, _ClientName, _Client, _SeatNum}, _From, State) ->
    {reply, {error, invalid_seat}, State};

handle_call({stand, ClientName}, _From, State) ->
  Event = #event{type=?tarabish_EventType_STAND,
                 name=ClientName},
  case orddict:find(ClientName, State#state.members) of
    {ok, #person{seat=none} = _Person} ->
      {reply, {error, not_seated}, State};
    {ok, #person{seat=SeatNum} = Person} ->
      send_event_all(Event, State),
      cancel_game(State),
      NewPerson = Person#person{seat=none},
      NewSeats = setelement(SeatNum + 1, State#state.seats, empty),
      NewMembers = orddict:store(ClientName, NewPerson, State#state.members),
      NewObservers = [ClientName|State#state.observers],

      NewState = State#state{members=NewMembers, seats=NewSeats,
        observers=NewObservers, game=none},
      update_server(NewState),
      {reply, ok, NewState};
    error ->
      {reply, {error, not_at_table}, State}
  end;

handle_call({start_game, ClientName}, _From, #state{game=none} = State) ->
  case orddict:find(ClientName, State#state.members) of
    {ok, #person{seat=none}} ->
      {reply, {error, not_authorized}, State};
    {ok, _Person} ->
      Event = #event{type=?tarabish_EventType_NEW_GAME},
      send_event_all(Event, State),
      {ok, Game} = game:start_link(self()),
      {reply, ok, State#state{game=Game}};
    error ->
      {reply, {error, not_at_table}, State}
  end;

handle_call({start_game, _ClientName}, _From, State) ->
  {reply, {error, already_started}, State};

handle_call({call_trump, _ClientName, _Suit}, _From, #state{game=none} = State) ->
  {reply, {error, no_game}, State};

handle_call({call_trump, ClientName, Suit}, _From, State) ->
  case orddict:find(ClientName, State#state.members) of
      {ok, #person{seat=none}} ->
        {reply, {error, not_authorized}, State};
      {ok, Person} ->
        Reply = game:call_trump(State#state.game, Person#person.seat, Suit),
        {reply, Reply, State};
      error ->
        {reply, {error, not_at_table}, State}
    end;

handle_call({play_card, _ClientName, _Card}, _From, #state{game=none} = State) ->
  {reply, {error, no_game}, State};

% TODO: not_authorized isn't really needed here, as it shoudl be checked in the game
handle_call({play_card, ClientName, Card}, _From, State) ->
  case orddict:find(ClientName, State#state.members) of
      {ok, #person{seat=none}} ->
        {reply, {error, not_authorized}, State};
      {ok, Person} ->
        Reply = game:play_card(State#state.game, Person#person.seat, Card),
        {reply, Reply, State};
      error ->
        {reply, {error, not_at_table}, State}
    end;

handle_call(Request, _From, State) ->
  io:format("~w received unknown call ~p~n",
    [?MODULE, Request]),
  {stop, "Bad Call", State}.

handle_cast({broadcast,
    #event{type=?tarabish_EventType_GAME_DONE} = Event}, State) ->
      send_event_all(Event, State),
      {noreply, State#state{game=none}};

handle_cast({broadcast, Event}, State) ->
  send_event_all(Event, State),
  {noreply, State};

handle_cast({deal3, Dealer, Cards}, State) ->
  send_cards(State#state.id, Dealer, Cards, State#state.members),
  {noreply, State};

handle_cast(Msg, State) ->
  io:format("~w received unknown cast ~p~n",
    [?MODULE, Msg]),
  {stop, "Bad Cast", State}.

% Game sends a done message on normal exits
handle_info({'EXIT', Game, normal}, #state{game=Game} = State) ->
  {noreply, State#state{game=none}};

handle_info({'EXIT', Game, Reason}, #state{game=Game} = State) ->
  cancel_game(Game),
  {noreply, State#state{game=none}};

handle_info(Info, State) ->
  io:format("~w recieved unknown info ~p~n",
    [?MODULE, Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private:
cancel_game(#state{game=none}) ->
  ok;

cancel_game(#state{game=Game} = State) ->
  Event = #event{type=?tarabish_EventType_GAME_CANCEL},
  game:stop(Game),
  send_event_all(Event, State).

send_event_all(Event, State) ->
  send_event(State#state.id, Event, State#state.members).

send_event(TableId, Event, MemberDict) ->
  {_Ids, Members} = lists:unzip(orddict:to_list(MemberDict)),
  TableEvent = Event#event{table=TableId},
  lists:map(fun(Person) -> client:recv_event(Person#person.client, TableEvent) end,
            Members).

send_cards1(_Event, _Cards, []) ->
  ok;

send_cards1(Event, Cards, [#person{} = Person|Rest])
    when Person#person.seat == none ->
  client:recv_event(Person#person.client, Event),
  send_cards1(Event, Cards, Rest);

send_cards1(Event, Cards, [#person{} = Person|Rest]) ->
  Event1 = Event#event{dealt = lists:nth(Person#person.seat + 1, Cards)},

  client:recv_event(Person#person.client, Event1),
  send_cards1(Event, Cards, Rest).

send_cards(TableId, Dealer, Cards, MembersDict) ->
  {_Ids, Persons} = lists:unzip(orddict:to_list(MembersDict)),

  Event = #event{type=?tarabish_EventType_DEAL,
                 table=TableId,
                 seat=Dealer},
  send_cards1(Event, Cards, Persons).

bjoin(List) ->
  F = fun(A, B) -> <<A/binary, B/binary>> end,
  lists:foldr(F, <<>>, List).

% Passes a list, but returns a tuple.
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
make_one_seat_view(empty) ->
  #seatView{isOpen=true, name=""};

make_one_seat_view(Seat) ->
  #seatView{isOpen=false, name=Seat#client.name}.

