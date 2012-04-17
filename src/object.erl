%%
%% object.erl
%%
%% ----------------------------------------------------------------------
%%
%%  eXAT, an erlang eXperimental Agent Tool
%%  Copyright (C) 2005-07 Corrado Santoro (csanto@diit.unict.it)
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%%
%%
-module(object).

-export([aclqueueof/1, agentof/1, bind/2, call/2,
         call/3, delete/1, do/2, executor/0, executorof/1, get/2,
         getAttributeNames/1, getAttributes/1, getClass/1,
         getParent/1, join/1, new/1, new/2, property_server/1,
         set/3, start/1, stop/1, super/1, super/2, super/3,
         super_/1, super_class/2, super_class/3, super_class/4]).

-author('csanto@diit.unict.it').

-include("object.hrl").

-define(PROPERTY_STATUS, '__status__').

-define(TERMINATING, '__terminating__').

-define(JOINING_SYNC, '__joining__').

-define(INIT_STATUS, '__init__').

-define(END_STATUS, '__end__').

-define(BOUND_AGENT, '__agent__').

-define(ACL_QUEUE, '__acl_queue__').

-define(NO_AGENT, '__no_agent__').

-define(MULTISYNC, '__multisync__').

new(Class, P) ->
    PropertyServerPid = spawn(object, property_server,
                              [dict:new()]),
    server_call(PropertyServerPid,
                {self(), set, ?PROPERTY_STATUS, ?INIT_STATUS}),
    server_call(PropertyServerPid,
                {self(), set, ?TERMINATING, false}),
    server_call(PropertyServerPid,
                {self(), set, ?JOINING_SYNC, nil}),
    server_call(PropertyServerPid,
                {self(), set, ?MULTISYNC, nil}),
    server_call(PropertyServerPid,
                {self(), set, class, Class}),
    ExecutorPid = spawn(object, executor, []),
    Object = #object{class = Class, context = Class,
                     property_server = PropertyServerPid,
                     executor = ExecutorPid},
    c_tor_call(Object, Class, P),
    Object.

new(Class) -> new(Class, []).

delete(Object) ->
    Class = Object#object.class,
    %%io:format("Before d_tor ~w\n", [Object]),
    d_tor_call(Object, Class, []),
    S = get(Object, ?JOINING_SYNC),
    %%io:format("Before d_tor ~w\n", [S]),
    if S =/= nil -> delete(S);
       true -> nil
    end,
    M = get(Object, ?MULTISYNC),
    if S =/= nil -> catch multisync:abort(M);
       true -> nil
    end,
    %% Destroy the property server
    %%io:format("After d_tor II ~w\n", [Object]),
    catch exit(Object#object.property_server, kill),
    %% Destroy the executor
    %%io:format("After d_tor ~w\n", [Object]),
    catch exit(Object#object.executor, kill),
    ok.

set(Object, AttributeName, AttributeValue) ->
    server_call(Object#object.property_server,
                {self(), set, AttributeName, AttributeValue}),
    AttributeValue.

get(Object, AttributeName) ->
    %%io:format("GET: ~w\n", [[Object, AttributeName]]),
    V = server_call(Object#object.property_server,
                    {self(), get, AttributeName}),
    case V of
        {value, Value} -> Value;
        _ -> exit({undef, [attribute, Object, AttributeName]})
    end.

call(Object, Method) -> call(Object, Method, []).

call(Object, Method, Params) ->
    %% only the virtual inheritance is implemented
    %%io:format("Calling1 ~w,~w\n", [Object,Method]),
    Obj1 = Object#object{context = Object#object.class},
    X = call(Obj1, Method, Object#object.class, Params),
    %%io:format("Result is ~w,~w\n", [Object,X]),
    X.

call(Object, Method, nil, _) ->
    exit({undef, [method, Object, Method]});
call(Object, Method, Class, Params) ->
    case public_call(Object, Method, Class, Params,
                     fun (M, C) -> M end)
    of
        {error, Error} ->
            %%exit({undef, [method, Object, Method]});
            exit(Error);
        {ok, Value} -> Value
    end.

super_class(Object, Class) ->
    super_class(Object, Class, Class, []).

super_class(Object, Class, Method) ->
    super_class(Object, Class, Method, []).

super_class(Object, Class, Method, Params) ->
    Obj1 = Object#object{context = Class},
    call(Obj1, Method, Obj1#object.context, Params).

%% calls the constructor of the ancestor class
super(Object) ->
    super_class(Object, getParent(Object#object.context)).

%% calls the constructor of the ancestor class with the given arguments
super(Object, Arguments) when is_list(Arguments) ->
    Parent = getParent(Object#object.context),
    super_class(Object, Parent, Parent, Arguments);
%% calls the method of the ancestor class
super(Object, Method) ->
    Parent = getParent(Object#object.context),
    super_class(Object, Parent, Method).

%% calls the method of the ancestor class with the given arguments
super(Object, Method, Arguments) ->
    Parent = getParent(Object#object.context),
    super_class(Object, Parent, Method, Arguments).

%% calls the destructor of the ancestor class
super_(Object) ->
    Parent = getParent(Object#object.context),
    Method = Parent ++ "_",
    super_class(Object, Parent, Method, []).

%%
%% behavioural procedures
%%
start(Object) ->
    V = get(Object, ?PROPERTY_STATUS),
    if V == (?INIT_STATUS) -> do(Object, start);
       true -> nil
    end,
    ok.

do(Object, State) ->
    set(Object, ?TERMINATING, false), %% FIXME: ?????
    V = get(Object, ?PROPERTY_STATUS),
    set(Object, ?PROPERTY_STATUS, State),
    if V == (?INIT_STATUS) ->
            case catch object:call(Object, on_starting) of
                {'EXIT', {undef, _}} -> ok;
                {'EXIT', Error} ->
                    io:format("Error in 'on_starting' for object ~w: "
                              "~w\n",
                              [Object, Error]);
                _ -> ok
            end,
            set(Object, ?JOINING_SYNC, object:new(sync)),
            trigger_executor(Object);
       true -> nil
    end,
    ok.

stop(Object) -> set(Object, ?TERMINATING, true).

join(Object) ->
    S = get(Object, ?JOINING_SYNC), object:call(S, wait).

bind(Object, Agent) ->
    set(Object, ?BOUND_AGENT, Agent),
    set(Object, ?ACL_QUEUE,
        list_to_atom(atom_to_list(Agent) ++ "__queue")).

agentof(Object) -> get(Object, ?BOUND_AGENT).

aclqueueof(Object) -> get(Object, ?ACL_QUEUE).

executorof(Object) -> Object#object.executor.

%%
%% reflection API
%%
getClass(Obj) -> Obj#object.class.

getParent(Class) ->
    %% calls "extends/0" function to obtain the parent class
    %%io:format("Calling getParent ~w\n", [Class]),
    X = call_(Class, extends),
    %%io:format("Called getParent ~w,~w\n", [Class, X]),
    X.

%% getMethods(Class) ->
%%  %% calls "public/0" function to obtain the method list
%%   call_ (Class, public).

getAttributeNames(Object) ->
    server_call(Object#object.property_server,
                {self(), list}).

getAttributes(Object) ->
    server_call(Object#object.property_server,
                {self(), list_values}).

%% ----------------------------------------------
%%
%%              INTERNAL ROUTINES
%%
%% ----------------------------------------------

%%
%% Calling a function of a module
%%
call_(Module, Func) -> call_(Module, Func, []).

call_(Module, Func, P) -> apply({Module, Func}, P).

%%
%% Performing a virtual call to a method of an object
%%
public_call(Object, Method, Class, Params, NameFun) ->
    public_call(Object, Method, Class, Params, NameFun,
                {undef, [method, Object, Method]}).

public_call(Object, Method, nil, Params, NameFun,
            LastException) ->
    {error, LastException};
public_call(Object, Method, Class, Params, NameFun,
            LastException) ->
    Parents = tolist(getParent(Class)),
    %%io:format("Parents = ~w,~w\n", [Object, Parents]),
    list_call(Object, Method, Class, Parents, Params,
              NameFun, LastException).

list_call(Object, Method, Class, [], Params, NameFun,
          LastException) ->
    {error,
     LastException}; %%{undef, [method, Object, Method]}};
list_call(Object, Method, Class, [ParentH | ParentT],
          Params, NameFun, LastException) ->
    {Result, Value} = deep_call(Object, Method, Class,
                                ParentH, Params, NameFun),
    if Result == ok -> {Result, Value};
       true ->
            list_call(Object, Method, Class, ParentT, Params,
                      NameFun, LastException)
    end.

deep_call(Object, Method, Class, Parent, Params,
          NameFun) ->
    case call_a_method(Object, Method, Class, Params,
                       NameFun)
    of
        {ok, Value} -> {ok, Value};
        {to_parent, Exception} ->
            Obj1 = Object#object{context = Parent},
            public_call(Obj1, Method, Parent, Params, NameFun,
                        Exception);
        {error, Exception} -> exit(Exception)
    end.

call_a_method(Object, Method, Class, Params, NameFun) ->
    %%io:format("Calling a method ~w,~w\n", [Object, Method]),
    MethodName = NameFun(Method, Class),
    case catch call_(Class, MethodName, [Object | Params])
    of
        {'EXIT',
         Error = {undef, [{Module, Function, _} | _]}} ->
            MethodNotPresend = (Module == Class) and
                                                   (Function == MethodName),
            check_for_returning_error(Error, MethodNotPresend);
        {'EXIT',
         Error = {function_clause,
                  [{Module, Function, _} | _]}} ->
            ClauseNotPresend = (Module == Class) and
                                                   (Function == MethodName),
            check_for_returning_error(Error, ClauseNotPresend);
        {'EXIT', Other} ->
            %%io:format("Error in calling ~w:~w =  ~w\n", [Object, MethodName,
            %%                                              Other]),
            {error, Other};
        Val -> {ok, Val}
    end.

check_for_returning_error(Error,
                          true) ->%% the method is not present
    {to_parent, Error};
check_for_returning_error(Error,
                          _) ->%% an internal exception occurred
    exit(Error).

%%
%% Performing a virtual call to the Ctor of an object
%%
c_tor_call(Object, Class, Params) ->
    case catch public_call(Object, Class, Class, Params,
                           fun (M, C) -> C end)
    of
        {'EXIT', {undef, _}} -> nil;
        {'EXIT', Other} -> exit(Other);
        _ -> nil
    end.

%% c_tor_call(Object, nil, Params) ->
%%   nil;
%% c_tor_call(Object, Class, Params) ->
%%   Parents = tolist(getParent(Class)),
%%   ctor_list_call(Object, Class, Parents, Params).

%% ctor_list_call(Object, nil, _, Params) ->
%%   nil;

%% ctor_list_call(Object, Class, [], Params) ->
%%   nil;

%% ctor_list_call(Object, Class, [ParentH | ParentT], Params) ->
%%   ctor_deep_call(Object, Class, ParentH, Params),
%%   ctor_list_call(Object, Class, ParentT, Params).

%% ctor_deep_call(Object, Class, Parent, Params) ->
%%   {Result, Value} = call_a_method(Object, method, Class, Params,
%%                                    fun(M,C) -> C end),
%%   if
%%     Result == error -> throw({bad_constructor, {Object, Class}});
%%     true -> nil
%%   end,
%%   Obj1 = Object#object {context = Parent},
%%   c_tor_call(Obj1, Parent, Params).

%%
%% Performing a virtual call to the Dtor of an object
%%
d_tor_call(Object, nil, Params) -> nil;
d_tor_call(Object, Class, Params) ->
    Parents = tolist(getParent(Class)),
    dtor_list_call(Object, Class, Parents, Params).

dtor_list_call(Object, nil, _, Params) -> nil;
dtor_list_call(Object, Class, [], Params) -> nil;
dtor_list_call(Object, Class, [ParentH | ParentT],
               Params) ->
    dtor_deep_call(Object, Class, ParentH, Params),
    dtor_list_call(Object, Class, ParentT, Params).

dtor_deep_call(Object, Class, Parent, Params) ->
    call_a_method(Object, method, Class, Params,
                  fun (M, C) -> list_to_atom(atom_to_list(C) ++ "_") end),
    Obj1 = Object#object{context = Parent},
    d_tor_call(Obj1, Parent, Params).

tolist(X) when is_atom(X) -> [X];
tolist(X) -> X.

%%
%% Execution Management
%%
trigger_executor(Object) ->
    Object#object.executor ! {go, Object}.

%%
%% Executor Server
%%
executor() ->
    receive
        {go, Object} -> executor_loop(Object);
        Other ->
            io:format("Executor[~w]: Invalid message ~w\n",
                      [self(), Other])
    end.

executor_loop(Object) ->
    %%io:format("Executor: ~w\n", [Object]),
    V = get(Object, ?PROPERTY_STATUS),
    T = not get(Object, ?TERMINATING),
    %%io:format("Value: ~w,~w\n", [Object, V]),
    if T ->
            case catch executor_do(Object, V) of
                {'EXIT', Reason} ->
                    io:format("\n=ERROR REPORT====\nError in object "
                              "behaviour ~w, state ~w, error value: "
                              "~w\n",
                              [Object, V, Reason]),
                    set(Object, ?TERMINATING, true);
                Other -> nil
            end,
            executor_loop(Object);
       true ->
            case catch object:call(Object, on_stopping) of
                {'EXIT', {undef, _}} -> ok;
                {'EXIT', Error} ->
                    io:format("Error in 'on_stopping' for object ~w: "
                              "~w\n",
                              [Object, Error]);
                _ -> ok
            end,
            S = get(Object, ?JOINING_SYNC),
            if S =/= nil -> object:call(S, signal_all);
               true -> nil
            end
    end.

%%io:format("Behaviour[~w]: Stopped.\n", [Object])

executor_do(Object, State) ->
    %%io:format("Executor_Do: ~w,~w\n", [Object, State]),
    Class = Object#object.class,
    %% get the event & proc list
    EventProc = call(Object, action, [State]),
    %% convert event-proc to a list(if not)
    if is_list(EventProc) -> EventProcList = EventProc;
       true -> EventProcList = [EventProc]
    end,
    CompleteEventList = make_event_list(Object,
                                        EventProcList),
    %%
    %% CompleteEventList is made of:
    %%   [ {EventType, Pattern, Proc}, .... ]
    %%
    %% now get the bound agent
    %%
    case catch agentof(Object) of
        {'EXIT', _} -> Agent = (?NO_AGENT);
        Other -> Agent = Other
    end,
    %%io:format("Event list = ~w\n", [CompleteEventList]),
    M = multisync:new(),
    set(Object, ?MULTISYNC, M),
    make_multi_sync_tasks(M, Agent, CompleteEventList),
    {FiredEvent, PatternValue, Proc} =
        multisync:wait_one(M),
    set(Object, ?MULTISYNC, nil),
    %%io:format("~w,~w\n", [FiredEvent, PatternValue]),
    call(Object, Proc, [FiredEvent, PatternValue, State]).

make_event_list(Object, []) -> [];
make_event_list(Object, [{Event, Proc} | T]) ->
    {EventType, PatternName} = call(Object, event, [Event]),
    Pattern = getpattern(Object, PatternName),
    [{EventType, Pattern, Proc} | make_event_list(Object,
                                                  T)].

make_multi_sync_tasks(M, Agent, []) -> ok;
make_multi_sync_tasks(M, Agent,
                      [{acl, Pattern, Proc} | T]) ->
    if Agent =/= (?NO_AGENT) ->
            multisync:add_task(M, eventmanager,
                               eventmanager:eventof(acl),
                               [Agent, Pattern, Proc]);
       true -> nil
    end,
    make_multi_sync_tasks(M, Agent, T);
make_multi_sync_tasks(M, Agent,
                      [{EventType, Pattern, Proc} | T]) ->
    multisync:add_task(M, eventmanager,
                       eventmanager:eventof(EventType),
                       [Agent, Pattern, Proc]),
    make_multi_sync_tasks(M, Agent, T).

getpattern(Object, $_) -> nil;
getpattern(Object, nil) -> nil;
getpattern(Object, PatternName) ->
    call(Object, pattern, [PatternName]).


%% get_matching_acl_message(AclQueue, Pattern) ->
%%   %Message = lq:dequeue(AclQueue),
%%   Message = agent:get_acl(AclQueue),
%%   %io:format("~w\n", [Message]),
%%   Match = match_lib:match(Pattern, Message),
%%   case Match of
%%     false -> get_matching_acl_message(AclQueue, Pattern);
%%     true -> Message
%%   end.

%%
%% Property Management Server
%%
server_call(Server, Data) ->
    %%io:format("~w\n", [[Server, Data]]),
    Server ! Data,
    receive
        {ack, X} -> X;
        Other ->
            io:format("Invalid reply = ~p on call ~p: ~p\n",
                      [Server, Data, Other]),
            exit({badtransaction, Other})
    end.

property_server(Dict) ->
    receive
        {From, get, AttributeName} ->
            case catch dict:fetch(AttributeName, Dict) of
                {'EXIT', _} -> From ! {ack, undef};
                Other -> From ! {ack, {value, Other}}
            end,
            property_server(Dict);
        {From, set, AttributeName, AttributeValue} ->
            From ! {ack, ok},
            property_server(dict:store(AttributeName,
                                       AttributeValue, Dict));
        {From, list} ->
            X = dict:fetch_keys(Dict),
            From ! {ack, X},
            property_server(Dict);
        {From, list_values} ->
            X = dict:to_list(Dict),
            From ! {ack, X},
            property_server(Dict);
        {From, exit} -> From ! {ack, ok};
        Other -> property_server(Dict)
    end.
