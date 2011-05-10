%
% sync.erl
%
% A synchronizer object
%
% ----------------------------------------------------------------------
% Copyright (c) 2003-04, Corrado Santoro <csanto@diit.unict.it>
% Department of Computer and Telecommunication Engineering,
% University of Catania, Italy. All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
% * Redistributions of source code must retain the above copyright notice,
%   this list of conditions and the following disclaimer.
%
% * Redistributions in binary form must reproduce the above copyright
%   notice, this list of conditions and the following disclaimer in the
%   documentation and/or other materials provided with the distribution.
%
% * Neither the name of Corrado Santoro nor the names of its
%   contributors may be used to endorse or promote products derived from this
%   software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
%
-module (sync).
-export ([extends/0,
          sync/1,
          sync_/1,
          wait/1,
          signal/1,
          signal/2,
          signal_all/1,
          signal_all/2,
          wait_queue/2]).

-define (SYNC_PID, '__sync_pid__').

extends () -> nil.

sync (Self) ->
  object:set (Self, ?SYNC_PID, spawn (?MODULE, wait_queue, [[],[]])).

sync_ (Self) ->
  Pid = object:get (Self, ?SYNC_PID),
  catch (exit (Pid, kill)).

wait (Self) ->
  Pid = object:get (Self, ?SYNC_PID),
  Pid ! {wait, self()},
  receive
    X -> X
  end.

signal (Self, Data) ->
  Pid = object:get (Self, ?SYNC_PID),
  Pid ! {signal, Data}.

signal (Self) -> signal (Self, ok).

signal_all (Self, Data) ->
  Pid = object:get (Self, ?SYNC_PID),
  Pid ! {signal_all, Data}.

signal_all (Self) -> signal_all (Self, ok).

%
% the wait server
%
wait_queue (AwaitingProcesses, AwaitingTokens) ->
  receive
    {wait, P} ->
      if
        length (AwaitingTokens) == 0 ->
          wait_queue (AwaitingProcesses ++ [P], []);
        true ->
          [Token | _] = AwaitingTokens,
          self () ! Token,
          wait_queue (AwaitingProcesses ++ [P], [])
      end;
    {signal, Data} ->
      %io:format ("AwaitingProcesses = ~w\n", [AwaitingProcesses]),
      if
        length (AwaitingProcesses) == 0 ->
          wait_queue (AwaitingProcesses, [{signal, Data}]);
        true ->
          [First | Tail] = AwaitingProcesses,
          do_signal (First, Data),
          wait_queue (Tail, [])
      end;
    {signal_all, Data} ->
      if
        length (AwaitingProcesses) == 0 ->
          wait_queue (AwaitingProcesses, [{signal_all, Data}]);
        true ->
          do_signal_all (AwaitingProcesses, Data),
          wait_queue ([], [])
      end;
    _ ->
      wait_queue (AwaitingProcesses, AwaitingTokens)
  end.


do_signal (P, Data) ->
  %io:format ("Signalling to ~w\n", [P]),
  P ! Data.

do_signal_all ([], Data) -> ok;
do_signal_all ([H|T], Data) ->
  do_signal (H, Data),
  do_signal_all (T, Data).

%
% CONSTRAINT!!! The 'wait' method MUST BE called from the same process
% that created the 'sync' object!
%
% public () -> [wait, signal].

% sync (Self) ->
%   object:set (Self, ?SYNC_PID, self ()).

% wait (Self) ->
%   receive
%     X -> X
%   end.

% signal (Self, Data) ->
%   case catch (object:get (Self, ?SYNC_PID)) of
%     {'EXIT', _} -> ok;
%     Pid -> catch (Pid ! Data), ok
%   end.

% signal (Self) -> signal (Self, ok).

