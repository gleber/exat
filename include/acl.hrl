%%
%% acl.hrl
%%
%%----------------------------------------------------------------------
%%
%% eXAT, an erlang eXperimental Agent Tool
%% Copyright (C) 2005-07 Corrado Santoro (csanto@diit.unict.it)
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>
%%
%%

-define (ACL_ANY, '_').

-record (aclmessage, {speechact = ?ACL_ANY,
                      sender = ?ACL_ANY,
                      receiver = ?ACL_ANY,
                      'reply-to' = ?ACL_ANY,
                      content = ?ACL_ANY,
                      language = ?ACL_ANY,
                      encoding = ?ACL_ANY,
                      ontology = ?ACL_ANY,
                      protocol = ?ACL_ANY,
                      'conversation-id' = ?ACL_ANY,
                      'reply-with' = ?ACL_ANY,
                      'in-reply-to' = ?ACL_ANY,
                      'reply-by' = ?ACL_ANY}).

%%-record (fipa_agent_identifier, {name, addresses}).
