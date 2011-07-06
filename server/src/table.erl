-module(table).

-include("tarabish_types.hrl").
-include("client.hrl").

-behaviour(gen_server).

-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-export([chat/3, join/3, sit/4, start_game/2]).

%% From game
-export([broadcast/2, deal3/2]).

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

sit(Table, ClientName, Client, Seat) ->
  gen_server:call(Table, {sit, ClientName, Client, Seat}).

start_game(Table, ClientName) ->
  gen_server:call(Table, {start_game, ClientName}).

% From Game:
broadcast(Table, Event) ->
  gen_server:cast(Table, {broadcast, Event}).

deal3(Table, #event{type=?tarabish_EventType_DEAL} = Event) ->
  gen_server:cast(Table, {deal3, Event}).

% gen_server:

init([Id]) ->
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
      Person = #person{name=ClientName, client=Client, seat=none},
      NewMembers = orddict:store(ClientName, Person, State#state.members),
      Observers = [ClientName|State#state.observers],
      NewState = State#state{members=NewMembers, observers=Observers},
      update_server(NewState),
      {reply, ok, NewState}
  end;

handle_call({sit, ClientName, Client, SeatNum}, _From, State) ->
  Seat = get_seat(State, SeatNum),
  if Seat == empty ->
    ClientRec = #client{name=ClientName, pid=Client},
    NewSeats = setelement(SeatNum + 1, State#state.seats, ClientRec),
    case orddict:find(ClientName, State#state.members) of
      {ok, #person{seat=none} = Person} ->
        NewPerson = Person#person{seat=SeatNum},
        NewMembers = orddict:store(ClientName, NewPerson, State#state.members),
        NewObservers = lists:delete(ClientName, State#state.observers),
        NewState = State#state{members=NewMembers, seats=NewSeats, observers=NewObservers},
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
      {ok, Game} = game:start(self()),
      {reply, ok, State#state{game=Game}};
    error ->
      {reply, {error, not_at_table}, State}
  end;

handle_call({start_game, ClientName}, _From, State) ->
  {reply, {error, already_started}, State};

handle_call(Request, _From, State) ->
  io:format("~w received unknown call ~p~n",
    [?MODULE, Request]),
  {stop, "Bad Call", State}.

handle_cast({broadcast, Event}, State) ->
  send_event(State#state.id, Event, State#state.members),
  {noreply, State};

handle_cast({deal3, Event}, State) ->
  send_cards(State#state.id, Event, State#state.members),
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
send_event(TableId, Event, MemberDict) ->
  {_Ids, Members} = lists:unzip(orddict:to_list(MemberDict)),
  TableEvent = Event#event{table=TableId},
  lists:map(fun(Person) -> client:recv_event(Person#person.client, TableEvent) end,
            Members).

send_cards1(_Event, []) ->
  ok;

send_cards1(Event, [#person{} = Person|Rest])
    when Person#person.seat == Event#event.seat ->
  client:recv_event(Person#person.client, Event),
  send_cards1(Event, Rest);

send_cards1(Event, [#person{} = Person|Rest]) ->
  Event1 = Event#event{cards = undefined},
  client:recv_event(Person#person.client, Event1),
  send_cards1(Event, Rest).

send_cards(TableId, Event, MembersDict) ->
  {_Ids, Persons} = lists:unzip(orddict:to_list(MembersDict)),
  Event1 = Event#event{table=TableId},
  send_cards1(Event1, Persons).

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

