-module(fipa_ontology_sl_codec).

-include("acl.hrl").

-include("fipa_ontology.hrl").

-export([decode/1, encode/1]).

encode(X) when is_record(X, 'agent-identifier') ->
    [<<"agent-identifier">>,
     {name,
      ontology:sl_encode_term(X#'agent-identifier'.name,
                              string)},
     {addresses,
      [<<"sequence">> | X#'agent-identifier'.addresses]}];
encode(X) when is_record(X, 'acl-message') ->
    [<<"acl-message">>,
     {sender, encode(X#'acl-message'.sender)},
     {receiver,
      [<<"set">> | encode(X#'acl-message'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'acl-message'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'acl-message'.content,
                              string)},
     {language,
      ontology:sl_encode_term(X#'acl-message'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'acl-message'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'acl-message'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'acl-message'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'acl-message'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'acl-message'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'acl-message'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'acl-message'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'ACCEPT-PROPOSAL') ->
    [<<"ACCEPT-PROPOSAL">>,
     {sender, encode(X#'ACCEPT-PROPOSAL'.sender)},
     {receiver,
      [<<"set">> | encode(X#'ACCEPT-PROPOSAL'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'ACCEPT-PROPOSAL'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'ACCEPT-PROPOSAL'.content,
                              string)},
     {language,
      ontology:sl_encode_term(X#'ACCEPT-PROPOSAL'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'ACCEPT-PROPOSAL'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'ACCEPT-PROPOSAL'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'ACCEPT-PROPOSAL'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'ACCEPT-PROPOSAL'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'ACCEPT-PROPOSAL'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'ACCEPT-PROPOSAL'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'ACCEPT-PROPOSAL'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'AGREE') ->
    [<<"AGREE">>, {sender, encode(X#'AGREE'.sender)},
     {receiver, [<<"set">> | encode(X#'AGREE'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'AGREE'.'reply-to', string)},
     {content,
      ontology:sl_encode_term(X#'AGREE'.content, string)},
     {language,
      ontology:sl_encode_term(X#'AGREE'.language, string)},
     {encoding,
      ontology:sl_encode_term(X#'AGREE'.encoding, string)},
     {ontology,
      ontology:sl_encode_term(X#'AGREE'.ontology, string)},
     {protocol,
      ontology:sl_encode_term(X#'AGREE'.protocol, string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'AGREE'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'AGREE'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'AGREE'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'AGREE'.'reply-by', string)}];
encode(X) when is_record(X, 'CANCEL') ->
    [<<"CANCEL">>, {sender, encode(X#'CANCEL'.sender)},
     {receiver, [<<"set">> | encode(X#'CANCEL'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'CANCEL'.'reply-to', string)},
     {content,
      ontology:sl_encode_term(X#'CANCEL'.content, string)},
     {language,
      ontology:sl_encode_term(X#'CANCEL'.language, string)},
     {encoding,
      ontology:sl_encode_term(X#'CANCEL'.encoding, string)},
     {ontology,
      ontology:sl_encode_term(X#'CANCEL'.ontology, string)},
     {protocol,
      ontology:sl_encode_term(X#'CANCEL'.protocol, string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'CANCEL'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'CANCEL'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'CANCEL'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'CANCEL'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'CFP') ->
    [<<"CFP">>, {sender, encode(X#'CFP'.sender)},
     {receiver, [<<"set">> | encode(X#'CFP'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'CFP'.'reply-to', string)},
     {content,
      ontology:sl_encode_term(X#'CFP'.content, string)},
     {language,
      ontology:sl_encode_term(X#'CFP'.language, string)},
     {encoding,
      ontology:sl_encode_term(X#'CFP'.encoding, string)},
     {ontology,
      ontology:sl_encode_term(X#'CFP'.ontology, string)},
     {protocol,
      ontology:sl_encode_term(X#'CFP'.protocol, string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'CFP'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'CFP'.'reply-with', string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'CFP'.'in-reply-to', string)},
     {'reply-by',
      ontology:sl_encode_term(X#'CFP'.'reply-by', string)}];
encode(X) when is_record(X, 'CONFIRM') ->
    [<<"CONFIRM">>, {sender, encode(X#'CONFIRM'.sender)},
     {receiver, [<<"set">> | encode(X#'CONFIRM'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'CONFIRM'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'CONFIRM'.content, string)},
     {language,
      ontology:sl_encode_term(X#'CONFIRM'.language, string)},
     {encoding,
      ontology:sl_encode_term(X#'CONFIRM'.encoding, string)},
     {ontology,
      ontology:sl_encode_term(X#'CONFIRM'.ontology, string)},
     {protocol,
      ontology:sl_encode_term(X#'CONFIRM'.protocol, string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'CONFIRM'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'CONFIRM'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'CONFIRM'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'CONFIRM'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'DISCONFIRM') ->
    [<<"DISCONFIRM">>,
     {sender, encode(X#'DISCONFIRM'.sender)},
     {receiver,
      [<<"set">> | encode(X#'DISCONFIRM'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'DISCONFIRM'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'DISCONFIRM'.content,
                              string)},
     {language,
      ontology:sl_encode_term(X#'DISCONFIRM'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'DISCONFIRM'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'DISCONFIRM'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'DISCONFIRM'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'DISCONFIRM'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'DISCONFIRM'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'DISCONFIRM'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'DISCONFIRM'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'INFORM') ->
    [<<"INFORM">>, {sender, encode(X#'INFORM'.sender)},
     {receiver, [<<"set">> | encode(X#'INFORM'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'INFORM'.'reply-to', string)},
     {content,
      ontology:sl_encode_term(X#'INFORM'.content, string)},
     {language,
      ontology:sl_encode_term(X#'INFORM'.language, string)},
     {encoding,
      ontology:sl_encode_term(X#'INFORM'.encoding, string)},
     {ontology,
      ontology:sl_encode_term(X#'INFORM'.ontology, string)},
     {protocol,
      ontology:sl_encode_term(X#'INFORM'.protocol, string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'INFORM'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'INFORM'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'INFORM'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'INFORM'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'INFORM-IF') ->
    [<<"INFORM-IF">>,
     {sender, encode(X#'INFORM-IF'.sender)},
     {receiver,
      [<<"set">> | encode(X#'INFORM-IF'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'INFORM-IF'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'INFORM-IF'.content, string)},
     {language,
      ontology:sl_encode_term(X#'INFORM-IF'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'INFORM-IF'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'INFORM-IF'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'INFORM-IF'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'INFORM-IF'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'INFORM-IF'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'INFORM-IF'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'INFORM-IF'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'INFORM-REF') ->
    [<<"INFORM-REF">>,
     {sender, encode(X#'INFORM-REF'.sender)},
     {receiver,
      [<<"set">> | encode(X#'INFORM-REF'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'INFORM-REF'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'INFORM-REF'.content,
                              string)},
     {language,
      ontology:sl_encode_term(X#'INFORM-REF'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'INFORM-REF'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'INFORM-REF'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'INFORM-REF'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'INFORM-REF'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'INFORM-REF'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'INFORM-REF'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'INFORM-REF'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'NOT-UNDERSTOOD') ->
    [<<"NOT-UNDERSTOOD">>,
     {sender, encode(X#'NOT-UNDERSTOOD'.sender)},
     {receiver,
      [<<"set">> | encode(X#'NOT-UNDERSTOOD'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'NOT-UNDERSTOOD'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'NOT-UNDERSTOOD'.content,
                              string)},
     {language,
      ontology:sl_encode_term(X#'NOT-UNDERSTOOD'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'NOT-UNDERSTOOD'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'NOT-UNDERSTOOD'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'NOT-UNDERSTOOD'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'NOT-UNDERSTOOD'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'NOT-UNDERSTOOD'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'NOT-UNDERSTOOD'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'NOT-UNDERSTOOD'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'PROPAGATE') ->
    [<<"PROPAGATE">>,
     {sender, encode(X#'PROPAGATE'.sender)},
     {receiver,
      [<<"set">> | encode(X#'PROPAGATE'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'PROPAGATE'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'PROPAGATE'.content, string)},
     {language,
      ontology:sl_encode_term(X#'PROPAGATE'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'PROPAGATE'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'PROPAGATE'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'PROPAGATE'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'PROPAGATE'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'PROPAGATE'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'PROPAGATE'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'PROPAGATE'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'PROPOSE') ->
    [<<"PROPOSE">>, {sender, encode(X#'PROPOSE'.sender)},
     {receiver, [<<"set">> | encode(X#'PROPOSE'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'PROPOSE'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'PROPOSE'.content, string)},
     {language,
      ontology:sl_encode_term(X#'PROPOSE'.language, string)},
     {encoding,
      ontology:sl_encode_term(X#'PROPOSE'.encoding, string)},
     {ontology,
      ontology:sl_encode_term(X#'PROPOSE'.ontology, string)},
     {protocol,
      ontology:sl_encode_term(X#'PROPOSE'.protocol, string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'PROPOSE'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'PROPOSE'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'PROPOSE'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'PROPOSE'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'PROXY') ->
    [<<"PROXY">>, {sender, encode(X#'PROXY'.sender)},
     {receiver, [<<"set">> | encode(X#'PROXY'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'PROXY'.'reply-to', string)},
     {content,
      ontology:sl_encode_term(X#'PROXY'.content, string)},
     {language,
      ontology:sl_encode_term(X#'PROXY'.language, string)},
     {encoding,
      ontology:sl_encode_term(X#'PROXY'.encoding, string)},
     {ontology,
      ontology:sl_encode_term(X#'PROXY'.ontology, string)},
     {protocol,
      ontology:sl_encode_term(X#'PROXY'.protocol, string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'PROXY'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'PROXY'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'PROXY'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'PROXY'.'reply-by', string)}];
encode(X) when is_record(X, 'QUERY-IF') ->
    [<<"QUERY-IF">>, {sender, encode(X#'QUERY-IF'.sender)},
     {receiver, [<<"set">> | encode(X#'QUERY-IF'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'QUERY-IF'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'QUERY-IF'.content, string)},
     {language,
      ontology:sl_encode_term(X#'QUERY-IF'.language, string)},
     {encoding,
      ontology:sl_encode_term(X#'QUERY-IF'.encoding, string)},
     {ontology,
      ontology:sl_encode_term(X#'QUERY-IF'.ontology, string)},
     {protocol,
      ontology:sl_encode_term(X#'QUERY-IF'.protocol, string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'QUERY-IF'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'QUERY-IF'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'QUERY-IF'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'QUERY-IF'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'QUERY-REF') ->
    [<<"QUERY-REF">>,
     {sender, encode(X#'QUERY-REF'.sender)},
     {receiver,
      [<<"set">> | encode(X#'QUERY-REF'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'QUERY-REF'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'QUERY-REF'.content, string)},
     {language,
      ontology:sl_encode_term(X#'QUERY-REF'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'QUERY-REF'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'QUERY-REF'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'QUERY-REF'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'QUERY-REF'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'QUERY-REF'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'QUERY-REF'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'QUERY-REF'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'REFUSE') ->
    [<<"REFUSE">>, {sender, encode(X#'REFUSE'.sender)},
     {receiver, [<<"set">> | encode(X#'REFUSE'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'REFUSE'.'reply-to', string)},
     {content,
      ontology:sl_encode_term(X#'REFUSE'.content, string)},
     {language,
      ontology:sl_encode_term(X#'REFUSE'.language, string)},
     {encoding,
      ontology:sl_encode_term(X#'REFUSE'.encoding, string)},
     {ontology,
      ontology:sl_encode_term(X#'REFUSE'.ontology, string)},
     {protocol,
      ontology:sl_encode_term(X#'REFUSE'.protocol, string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'REFUSE'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'REFUSE'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'REFUSE'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'REFUSE'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'REJECT-PROPOSAL') ->
    [<<"REJECT-PROPOSAL">>,
     {sender, encode(X#'REJECT-PROPOSAL'.sender)},
     {receiver,
      [<<"set">> | encode(X#'REJECT-PROPOSAL'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'REJECT-PROPOSAL'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'REJECT-PROPOSAL'.content,
                              string)},
     {language,
      ontology:sl_encode_term(X#'REJECT-PROPOSAL'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'REJECT-PROPOSAL'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'REJECT-PROPOSAL'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'REJECT-PROPOSAL'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'REJECT-PROPOSAL'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'REJECT-PROPOSAL'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'REJECT-PROPOSAL'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'REJECT-PROPOSAL'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'REQUEST') ->
    [<<"REQUEST">>, {sender, encode(X#'REQUEST'.sender)},
     {receiver, [<<"set">> | encode(X#'REQUEST'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'REQUEST'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'REQUEST'.content, string)},
     {language,
      ontology:sl_encode_term(X#'REQUEST'.language, string)},
     {encoding,
      ontology:sl_encode_term(X#'REQUEST'.encoding, string)},
     {ontology,
      ontology:sl_encode_term(X#'REQUEST'.ontology, string)},
     {protocol,
      ontology:sl_encode_term(X#'REQUEST'.protocol, string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'REQUEST'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'REQUEST'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'REQUEST'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'REQUEST'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'REQUEST-WHEN') ->
    [<<"REQUEST-WHEN">>,
     {sender, encode(X#'REQUEST-WHEN'.sender)},
     {receiver,
      [<<"set">> | encode(X#'REQUEST-WHEN'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'REQUEST-WHEN'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'REQUEST-WHEN'.content,
                              string)},
     {language,
      ontology:sl_encode_term(X#'REQUEST-WHEN'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'REQUEST-WHEN'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'REQUEST-WHEN'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'REQUEST-WHEN'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'REQUEST-WHEN'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'REQUEST-WHEN'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'REQUEST-WHEN'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'REQUEST-WHEN'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'REQUEST-WHENEVER') ->
    [<<"REQUEST-WHENEVER">>,
     {sender, encode(X#'REQUEST-WHENEVER'.sender)},
     {receiver,
      [<<"set">> | encode(X#'REQUEST-WHENEVER'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'REQUEST-WHENEVER'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'REQUEST-WHENEVER'.content,
                              string)},
     {language,
      ontology:sl_encode_term(X#'REQUEST-WHENEVER'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'REQUEST-WHENEVER'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'REQUEST-WHENEVER'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'REQUEST-WHENEVER'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'REQUEST-WHENEVER'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'REQUEST-WHENEVER'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'REQUEST-WHENEVER'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'REQUEST-WHENEVER'.'reply-by',
                              string)}];
encode(X) when is_record(X, 'SUBSCRIBE') ->
    [<<"SUBSCRIBE">>,
     {sender, encode(X#'SUBSCRIBE'.sender)},
     {receiver,
      [<<"set">> | encode(X#'SUBSCRIBE'.receiver)]},
     {'reply-to',
      ontology:sl_encode_term(X#'SUBSCRIBE'.'reply-to',
                              string)},
     {content,
      ontology:sl_encode_term(X#'SUBSCRIBE'.content, string)},
     {language,
      ontology:sl_encode_term(X#'SUBSCRIBE'.language,
                              string)},
     {encoding,
      ontology:sl_encode_term(X#'SUBSCRIBE'.encoding,
                              string)},
     {ontology,
      ontology:sl_encode_term(X#'SUBSCRIBE'.ontology,
                              string)},
     {protocol,
      ontology:sl_encode_term(X#'SUBSCRIBE'.protocol,
                              string)},
     {'conversation-id',
      ontology:sl_encode_term(X#'SUBSCRIBE'.'conversation-id',
                              string)},
     {'reply-with',
      ontology:sl_encode_term(X#'SUBSCRIBE'.'reply-with',
                              string)},
     {'in-reply-to',
      ontology:sl_encode_term(X#'SUBSCRIBE'.'in-reply-to',
                              string)},
     {'reply-by',
      ontology:sl_encode_term(X#'SUBSCRIBE'.'reply-by',
                              string)}];
encode(X) when is_record(X, action) ->
    [<<"action">>, encode(X#action.'0'),
     encode(X#action.'1')];
encode(X) when is_record(X, 'action-specification') ->
    [<<"action-specification">>];
encode(X) when is_record(X, 'get-description') ->
    [<<"get-description">>];
encode(X) when is_record(X, search) ->
    [<<"search">>, encode(X#search.'0'),
     encode(X#search.'1')];
encode(X)
  when is_record(X, 'AMS-OR-DF-agent-description') ->
    [<<"AMS-OR-DF-agent-description">>];
encode(X) when is_record(X, 'ams-agent-description') ->
    [<<"ams-agent-description">>,
     {name, encode(X#'ams-agent-description'.name)},
     {ownership,
      ontology:sl_encode_term(X#'ams-agent-description'.ownership,
                              string)},
     {state,
      ontology:sl_encode_term(X#'ams-agent-description'.state,
                              string)}];
encode(X) when is_record(X, 'search-constraints') ->
    [<<"search-constraints">>,
     {'max-depth',
      ontology:sl_encode_term(X#'search-constraints'.'max-depth',
                              integer)},
     {'max-results',
      ontology:sl_encode_term(X#'search-constraints'.'max-results',
                              integer)},
     {'search-id',
      ontology:sl_encode_term(X#'search-constraints'.'search-id',
                              string)}];
encode(X) when is_record(X, result) ->
    [<<"result">>, encode(X#result.'0'),
     [<<"sequence">> | encode(X#result.'1')]];
encode(X) when is_record(X, 'result-specification') ->
    [<<"result-specification">>];
encode(X) when is_record(X, 'ap-service') ->
    [<<"ap-service">>,
     {name,
      ontology:sl_encode_term(X#'ap-service'.name, string)},
     {type,
      ontology:sl_encode_term(X#'ap-service'.type, string)},
     {addresses,
      [<<"sequence">> | X#'ap-service'.addresses]}];
encode(X) when is_record(X, 'ap-description') ->
    [<<"ap-description">>,
     {name,
      ontology:sl_encode_term(X#'ap-description'.name,
                              string)},
     {'ap-services',
      [<<"sequence">>
           | encode(X#'ap-description'.'ap-services')]}];
encode(X) when is_list(X) -> [encode(Y) || Y <- X];
encode(nil) -> nil;
encode(X) -> exit({ontology_error, X}).

decode([<<"agent-identifier">> | T]) ->
    #'agent-identifier'{name =
                            ontology:sl_decode_term(sl:get_slot(name, T),
                                                    string),
                        addresses = sequence_of(sl:get_slot(addresses, T))};
decode([<<"acl-message">> | T]) ->
    #'acl-message'{sender = decode(sl:get_slot(sender, T)),
                   receiver = decode(set_of(sl:get_slot(receiver, T))),
                   'reply-to' =
                       ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                               string),
                   content =
                       ontology:sl_decode_term(sl:get_slot(content, T),
                                               string),
                   language =
                       ontology:sl_decode_term(sl:get_slot(language, T),
                                               string),
                   encoding =
                       ontology:sl_decode_term(sl:get_slot(encoding, T),
                                               string),
                   ontology =
                       ontology:sl_decode_term(sl:get_slot(ontology, T),
                                               string),
                   protocol =
                       ontology:sl_decode_term(sl:get_slot(protocol, T),
                                               string),
                   'conversation-id' =
                       ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                           T),
                                               string),
                   'reply-with' =
                       ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                               string),
                   'in-reply-to' =
                       ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                               string),
                   'reply-by' =
                       ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                               string)};
decode([<<"ACCEPT-PROPOSAL">> | T]) ->
    #'ACCEPT-PROPOSAL'{sender =
                           decode(sl:get_slot(sender, T)),
                       receiver = decode(set_of(sl:get_slot(receiver, T))),
                       'reply-to' =
                           ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                                   string),
                       content =
                           ontology:sl_decode_term(sl:get_slot(content, T),
                                                   string),
                       language =
                           ontology:sl_decode_term(sl:get_slot(language, T),
                                                   string),
                       encoding =
                           ontology:sl_decode_term(sl:get_slot(encoding, T),
                                                   string),
                       ontology =
                           ontology:sl_decode_term(sl:get_slot(ontology, T),
                                                   string),
                       protocol =
                           ontology:sl_decode_term(sl:get_slot(protocol, T),
                                                   string),
                       'conversation-id' =
                           ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                               T),
                                                   string),
                       'reply-with' =
                           ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                                   string),
                       'in-reply-to' =
                           ontology:sl_decode_term(sl:get_slot('in-reply-to',
                                                               T),
                                                   string),
                       'reply-by' =
                           ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                                   string)};
decode([<<"AGREE">> | T]) ->
    #'AGREE'{sender = decode(sl:get_slot(sender, T)),
             receiver = decode(set_of(sl:get_slot(receiver, T))),
             'reply-to' =
                 ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                         string),
             content =
                 ontology:sl_decode_term(sl:get_slot(content, T),
                                         string),
             language =
                 ontology:sl_decode_term(sl:get_slot(language, T),
                                         string),
             encoding =
                 ontology:sl_decode_term(sl:get_slot(encoding, T),
                                         string),
             ontology =
                 ontology:sl_decode_term(sl:get_slot(ontology, T),
                                         string),
             protocol =
                 ontology:sl_decode_term(sl:get_slot(protocol, T),
                                         string),
             'conversation-id' =
                 ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                     T),
                                         string),
             'reply-with' =
                 ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                         string),
             'in-reply-to' =
                 ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                         string),
             'reply-by' =
                 ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                         string)};
decode([<<"CANCEL">> | T]) ->
    #'CANCEL'{sender = decode(sl:get_slot(sender, T)),
              receiver = decode(set_of(sl:get_slot(receiver, T))),
              'reply-to' =
                  ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                          string),
              content =
                  ontology:sl_decode_term(sl:get_slot(content, T),
                                          string),
              language =
                  ontology:sl_decode_term(sl:get_slot(language, T),
                                          string),
              encoding =
                  ontology:sl_decode_term(sl:get_slot(encoding, T),
                                          string),
              ontology =
                  ontology:sl_decode_term(sl:get_slot(ontology, T),
                                          string),
              protocol =
                  ontology:sl_decode_term(sl:get_slot(protocol, T),
                                          string),
              'conversation-id' =
                  ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                      T),
                                          string),
              'reply-with' =
                  ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                          string),
              'in-reply-to' =
                  ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                          string),
              'reply-by' =
                  ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                          string)};
decode([<<"CFP">> | T]) ->
    #'CFP'{sender = decode(sl:get_slot(sender, T)),
           receiver = decode(set_of(sl:get_slot(receiver, T))),
           'reply-to' =
               ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                       string),
           content =
               ontology:sl_decode_term(sl:get_slot(content, T),
                                       string),
           language =
               ontology:sl_decode_term(sl:get_slot(language, T),
                                       string),
           encoding =
               ontology:sl_decode_term(sl:get_slot(encoding, T),
                                       string),
           ontology =
               ontology:sl_decode_term(sl:get_slot(ontology, T),
                                       string),
           protocol =
               ontology:sl_decode_term(sl:get_slot(protocol, T),
                                       string),
           'conversation-id' =
               ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                   T),
                                       string),
           'reply-with' =
               ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                       string),
           'in-reply-to' =
               ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                       string),
           'reply-by' =
               ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                       string)};
decode([<<"CONFIRM">> | T]) ->
    #'CONFIRM'{sender = decode(sl:get_slot(sender, T)),
               receiver = decode(set_of(sl:get_slot(receiver, T))),
               'reply-to' =
                   ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                           string),
               content =
                   ontology:sl_decode_term(sl:get_slot(content, T),
                                           string),
               language =
                   ontology:sl_decode_term(sl:get_slot(language, T),
                                           string),
               encoding =
                   ontology:sl_decode_term(sl:get_slot(encoding, T),
                                           string),
               ontology =
                   ontology:sl_decode_term(sl:get_slot(ontology, T),
                                           string),
               protocol =
                   ontology:sl_decode_term(sl:get_slot(protocol, T),
                                           string),
               'conversation-id' =
                   ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                       T),
                                           string),
               'reply-with' =
                   ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                           string),
               'in-reply-to' =
                   ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                           string),
               'reply-by' =
                   ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                           string)};
decode([<<"DISCONFIRM">> | T]) ->
    #'DISCONFIRM'{sender = decode(sl:get_slot(sender, T)),
                  receiver = decode(set_of(sl:get_slot(receiver, T))),
                  'reply-to' =
                      ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                              string),
                  content =
                      ontology:sl_decode_term(sl:get_slot(content, T),
                                              string),
                  language =
                      ontology:sl_decode_term(sl:get_slot(language, T),
                                              string),
                  encoding =
                      ontology:sl_decode_term(sl:get_slot(encoding, T),
                                              string),
                  ontology =
                      ontology:sl_decode_term(sl:get_slot(ontology, T),
                                              string),
                  protocol =
                      ontology:sl_decode_term(sl:get_slot(protocol, T),
                                              string),
                  'conversation-id' =
                      ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                          T),
                                              string),
                  'reply-with' =
                      ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                              string),
                  'in-reply-to' =
                      ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                              string),
                  'reply-by' =
                      ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                              string)};
decode([<<"INFORM">> | T]) ->
    #'INFORM'{sender = decode(sl:get_slot(sender, T)),
              receiver = decode(set_of(sl:get_slot(receiver, T))),
              'reply-to' =
                  ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                          string),
              content =
                  ontology:sl_decode_term(sl:get_slot(content, T),
                                          string),
              language =
                  ontology:sl_decode_term(sl:get_slot(language, T),
                                          string),
              encoding =
                  ontology:sl_decode_term(sl:get_slot(encoding, T),
                                          string),
              ontology =
                  ontology:sl_decode_term(sl:get_slot(ontology, T),
                                          string),
              protocol =
                  ontology:sl_decode_term(sl:get_slot(protocol, T),
                                          string),
              'conversation-id' =
                  ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                      T),
                                          string),
              'reply-with' =
                  ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                          string),
              'in-reply-to' =
                  ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                          string),
              'reply-by' =
                  ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                          string)};
decode([<<"INFORM-IF">> | T]) ->
    #'INFORM-IF'{sender = decode(sl:get_slot(sender, T)),
                 receiver = decode(set_of(sl:get_slot(receiver, T))),
                 'reply-to' =
                     ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                             string),
                 content =
                     ontology:sl_decode_term(sl:get_slot(content, T),
                                             string),
                 language =
                     ontology:sl_decode_term(sl:get_slot(language, T),
                                             string),
                 encoding =
                     ontology:sl_decode_term(sl:get_slot(encoding, T),
                                             string),
                 ontology =
                     ontology:sl_decode_term(sl:get_slot(ontology, T),
                                             string),
                 protocol =
                     ontology:sl_decode_term(sl:get_slot(protocol, T),
                                             string),
                 'conversation-id' =
                     ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                         T),
                                             string),
                 'reply-with' =
                     ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                             string),
                 'in-reply-to' =
                     ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                             string),
                 'reply-by' =
                     ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                             string)};
decode([<<"INFORM-REF">> | T]) ->
    #'INFORM-REF'{sender = decode(sl:get_slot(sender, T)),
                  receiver = decode(set_of(sl:get_slot(receiver, T))),
                  'reply-to' =
                      ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                              string),
                  content =
                      ontology:sl_decode_term(sl:get_slot(content, T),
                                              string),
                  language =
                      ontology:sl_decode_term(sl:get_slot(language, T),
                                              string),
                  encoding =
                      ontology:sl_decode_term(sl:get_slot(encoding, T),
                                              string),
                  ontology =
                      ontology:sl_decode_term(sl:get_slot(ontology, T),
                                              string),
                  protocol =
                      ontology:sl_decode_term(sl:get_slot(protocol, T),
                                              string),
                  'conversation-id' =
                      ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                          T),
                                              string),
                  'reply-with' =
                      ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                              string),
                  'in-reply-to' =
                      ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                              string),
                  'reply-by' =
                      ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                              string)};
decode([<<"NOT-UNDERSTOOD">> | T]) ->
    #'NOT-UNDERSTOOD'{sender =
                          decode(sl:get_slot(sender, T)),
                      receiver = decode(set_of(sl:get_slot(receiver, T))),
                      'reply-to' =
                          ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                                  string),
                      content =
                          ontology:sl_decode_term(sl:get_slot(content, T),
                                                  string),
                      language =
                          ontology:sl_decode_term(sl:get_slot(language, T),
                                                  string),
                      encoding =
                          ontology:sl_decode_term(sl:get_slot(encoding, T),
                                                  string),
                      ontology =
                          ontology:sl_decode_term(sl:get_slot(ontology, T),
                                                  string),
                      protocol =
                          ontology:sl_decode_term(sl:get_slot(protocol, T),
                                                  string),
                      'conversation-id' =
                          ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                              T),
                                                  string),
                      'reply-with' =
                          ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                                  string),
                      'in-reply-to' =
                          ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                                  string),
                      'reply-by' =
                          ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                                  string)};
decode([<<"PROPAGATE">> | T]) ->
    #'PROPAGATE'{sender = decode(sl:get_slot(sender, T)),
                 receiver = decode(set_of(sl:get_slot(receiver, T))),
                 'reply-to' =
                     ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                             string),
                 content =
                     ontology:sl_decode_term(sl:get_slot(content, T),
                                             string),
                 language =
                     ontology:sl_decode_term(sl:get_slot(language, T),
                                             string),
                 encoding =
                     ontology:sl_decode_term(sl:get_slot(encoding, T),
                                             string),
                 ontology =
                     ontology:sl_decode_term(sl:get_slot(ontology, T),
                                             string),
                 protocol =
                     ontology:sl_decode_term(sl:get_slot(protocol, T),
                                             string),
                 'conversation-id' =
                     ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                         T),
                                             string),
                 'reply-with' =
                     ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                             string),
                 'in-reply-to' =
                     ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                             string),
                 'reply-by' =
                     ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                             string)};
decode([<<"PROPOSE">> | T]) ->
    #'PROPOSE'{sender = decode(sl:get_slot(sender, T)),
               receiver = decode(set_of(sl:get_slot(receiver, T))),
               'reply-to' =
                   ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                           string),
               content =
                   ontology:sl_decode_term(sl:get_slot(content, T),
                                           string),
               language =
                   ontology:sl_decode_term(sl:get_slot(language, T),
                                           string),
               encoding =
                   ontology:sl_decode_term(sl:get_slot(encoding, T),
                                           string),
               ontology =
                   ontology:sl_decode_term(sl:get_slot(ontology, T),
                                           string),
               protocol =
                   ontology:sl_decode_term(sl:get_slot(protocol, T),
                                           string),
               'conversation-id' =
                   ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                       T),
                                           string),
               'reply-with' =
                   ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                           string),
               'in-reply-to' =
                   ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                           string),
               'reply-by' =
                   ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                           string)};
decode([<<"PROXY">> | T]) ->
    #'PROXY'{sender = decode(sl:get_slot(sender, T)),
             receiver = decode(set_of(sl:get_slot(receiver, T))),
             'reply-to' =
                 ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                         string),
             content =
                 ontology:sl_decode_term(sl:get_slot(content, T),
                                         string),
             language =
                 ontology:sl_decode_term(sl:get_slot(language, T),
                                         string),
             encoding =
                 ontology:sl_decode_term(sl:get_slot(encoding, T),
                                         string),
             ontology =
                 ontology:sl_decode_term(sl:get_slot(ontology, T),
                                         string),
             protocol =
                 ontology:sl_decode_term(sl:get_slot(protocol, T),
                                         string),
             'conversation-id' =
                 ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                     T),
                                         string),
             'reply-with' =
                 ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                         string),
             'in-reply-to' =
                 ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                         string),
             'reply-by' =
                 ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                         string)};
decode([<<"QUERY-IF">> | T]) ->
    #'QUERY-IF'{sender = decode(sl:get_slot(sender, T)),
                receiver = decode(set_of(sl:get_slot(receiver, T))),
                'reply-to' =
                    ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                            string),
                content =
                    ontology:sl_decode_term(sl:get_slot(content, T),
                                            string),
                language =
                    ontology:sl_decode_term(sl:get_slot(language, T),
                                            string),
                encoding =
                    ontology:sl_decode_term(sl:get_slot(encoding, T),
                                            string),
                ontology =
                    ontology:sl_decode_term(sl:get_slot(ontology, T),
                                            string),
                protocol =
                    ontology:sl_decode_term(sl:get_slot(protocol, T),
                                            string),
                'conversation-id' =
                    ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                        T),
                                            string),
                'reply-with' =
                    ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                            string),
                'in-reply-to' =
                    ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                            string),
                'reply-by' =
                    ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                            string)};
decode([<<"QUERY-REF">> | T]) ->
    #'QUERY-REF'{sender = decode(sl:get_slot(sender, T)),
                 receiver = decode(set_of(sl:get_slot(receiver, T))),
                 'reply-to' =
                     ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                             string),
                 content =
                     ontology:sl_decode_term(sl:get_slot(content, T),
                                             string),
                 language =
                     ontology:sl_decode_term(sl:get_slot(language, T),
                                             string),
                 encoding =
                     ontology:sl_decode_term(sl:get_slot(encoding, T),
                                             string),
                 ontology =
                     ontology:sl_decode_term(sl:get_slot(ontology, T),
                                             string),
                 protocol =
                     ontology:sl_decode_term(sl:get_slot(protocol, T),
                                             string),
                 'conversation-id' =
                     ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                         T),
                                             string),
                 'reply-with' =
                     ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                             string),
                 'in-reply-to' =
                     ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                             string),
                 'reply-by' =
                     ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                             string)};
decode([<<"REFUSE">> | T]) ->
    #'REFUSE'{sender = decode(sl:get_slot(sender, T)),
              receiver = decode(set_of(sl:get_slot(receiver, T))),
              'reply-to' =
                  ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                          string),
              content =
                  ontology:sl_decode_term(sl:get_slot(content, T),
                                          string),
              language =
                  ontology:sl_decode_term(sl:get_slot(language, T),
                                          string),
              encoding =
                  ontology:sl_decode_term(sl:get_slot(encoding, T),
                                          string),
              ontology =
                  ontology:sl_decode_term(sl:get_slot(ontology, T),
                                          string),
              protocol =
                  ontology:sl_decode_term(sl:get_slot(protocol, T),
                                          string),
              'conversation-id' =
                  ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                      T),
                                          string),
              'reply-with' =
                  ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                          string),
              'in-reply-to' =
                  ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                          string),
              'reply-by' =
                  ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                          string)};
decode([<<"REJECT-PROPOSAL">> | T]) ->
    #'REJECT-PROPOSAL'{sender =
                           decode(sl:get_slot(sender, T)),
                       receiver = decode(set_of(sl:get_slot(receiver, T))),
                       'reply-to' =
                           ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                                   string),
                       content =
                           ontology:sl_decode_term(sl:get_slot(content, T),
                                                   string),
                       language =
                           ontology:sl_decode_term(sl:get_slot(language, T),
                                                   string),
                       encoding =
                           ontology:sl_decode_term(sl:get_slot(encoding, T),
                                                   string),
                       ontology =
                           ontology:sl_decode_term(sl:get_slot(ontology, T),
                                                   string),
                       protocol =
                           ontology:sl_decode_term(sl:get_slot(protocol, T),
                                                   string),
                       'conversation-id' =
                           ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                               T),
                                                   string),
                       'reply-with' =
                           ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                                   string),
                       'in-reply-to' =
                           ontology:sl_decode_term(sl:get_slot('in-reply-to',
                                                               T),
                                                   string),
                       'reply-by' =
                           ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                                   string)};
decode([<<"REQUEST">> | T]) ->
    #'REQUEST'{sender = decode(sl:get_slot(sender, T)),
               receiver = decode(set_of(sl:get_slot(receiver, T))),
               'reply-to' =
                   ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                           string),
               content =
                   ontology:sl_decode_term(sl:get_slot(content, T),
                                           string),
               language =
                   ontology:sl_decode_term(sl:get_slot(language, T),
                                           string),
               encoding =
                   ontology:sl_decode_term(sl:get_slot(encoding, T),
                                           string),
               ontology =
                   ontology:sl_decode_term(sl:get_slot(ontology, T),
                                           string),
               protocol =
                   ontology:sl_decode_term(sl:get_slot(protocol, T),
                                           string),
               'conversation-id' =
                   ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                       T),
                                           string),
               'reply-with' =
                   ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                           string),
               'in-reply-to' =
                   ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                           string),
               'reply-by' =
                   ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                           string)};
decode([<<"REQUEST-WHEN">> | T]) ->
    #'REQUEST-WHEN'{sender = decode(sl:get_slot(sender, T)),
                    receiver = decode(set_of(sl:get_slot(receiver, T))),
                    'reply-to' =
                        ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                                string),
                    content =
                        ontology:sl_decode_term(sl:get_slot(content, T),
                                                string),
                    language =
                        ontology:sl_decode_term(sl:get_slot(language, T),
                                                string),
                    encoding =
                        ontology:sl_decode_term(sl:get_slot(encoding, T),
                                                string),
                    ontology =
                        ontology:sl_decode_term(sl:get_slot(ontology, T),
                                                string),
                    protocol =
                        ontology:sl_decode_term(sl:get_slot(protocol, T),
                                                string),
                    'conversation-id' =
                        ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                            T),
                                                string),
                    'reply-with' =
                        ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                                string),
                    'in-reply-to' =
                        ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                                string),
                    'reply-by' =
                        ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                                string)};
decode([<<"REQUEST-WHENEVER">> | T]) ->
    #'REQUEST-WHENEVER'{sender =
                            decode(sl:get_slot(sender, T)),
                        receiver = decode(set_of(sl:get_slot(receiver, T))),
                        'reply-to' =
                            ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                                    string),
                        content =
                            ontology:sl_decode_term(sl:get_slot(content, T),
                                                    string),
                        language =
                            ontology:sl_decode_term(sl:get_slot(language, T),
                                                    string),
                        encoding =
                            ontology:sl_decode_term(sl:get_slot(encoding, T),
                                                    string),
                        ontology =
                            ontology:sl_decode_term(sl:get_slot(ontology, T),
                                                    string),
                        protocol =
                            ontology:sl_decode_term(sl:get_slot(protocol, T),
                                                    string),
                        'conversation-id' =
                            ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                                T),
                                                    string),
                        'reply-with' =
                            ontology:sl_decode_term(sl:get_slot('reply-with',
                                                                T),
                                                    string),
                        'in-reply-to' =
                            ontology:sl_decode_term(sl:get_slot('in-reply-to',
                                                                T),
                                                    string),
                        'reply-by' =
                            ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                                    string)};
decode([<<"SUBSCRIBE">> | T]) ->
    #'SUBSCRIBE'{sender = decode(sl:get_slot(sender, T)),
                 receiver = decode(set_of(sl:get_slot(receiver, T))),
                 'reply-to' =
                     ontology:sl_decode_term(sl:get_slot('reply-to', T),
                                             string),
                 content =
                     ontology:sl_decode_term(sl:get_slot(content, T),
                                             string),
                 language =
                     ontology:sl_decode_term(sl:get_slot(language, T),
                                             string),
                 encoding =
                     ontology:sl_decode_term(sl:get_slot(encoding, T),
                                             string),
                 ontology =
                     ontology:sl_decode_term(sl:get_slot(ontology, T),
                                             string),
                 protocol =
                     ontology:sl_decode_term(sl:get_slot(protocol, T),
                                             string),
                 'conversation-id' =
                     ontology:sl_decode_term(sl:get_slot('conversation-id',
                                                         T),
                                             string),
                 'reply-with' =
                     ontology:sl_decode_term(sl:get_slot('reply-with', T),
                                             string),
                 'in-reply-to' =
                     ontology:sl_decode_term(sl:get_slot('in-reply-to', T),
                                             string),
                 'reply-by' =
                     ontology:sl_decode_term(sl:get_slot('reply-by', T),
                                             string)};
decode([<<"action">> | T]) ->
    #action{'0' = decode(lists:nth(1, T)),
            '1' = decode(lists:nth(2, T))};
decode([<<"action-specification">> | T]) ->
    #'action-specification'{};
decode([<<"get-description">> | T]) ->
    #'get-description'{};
decode([<<"search">> | T]) ->
    #search{'0' = decode(lists:nth(1, T)),
            '1' = decode(lists:nth(2, T))};
decode([<<"AMS-OR-DF-agent-description">> | T]) ->
    #'AMS-OR-DF-agent-description'{};
decode([<<"ams-agent-description">> | T]) ->
    #'ams-agent-description'{name =
                                 decode(sl:get_slot(name, T)),
                             ownership =
                                 ontology:sl_decode_term(sl:get_slot(ownership,
                                                                     T),
                                                         string),
                             state =
                                 ontology:sl_decode_term(sl:get_slot(state, T),
                                                         string)};
decode([<<"search-constraints">> | T]) ->
    #'search-constraints'{'max-depth' =
                              ontology:sl_decode_term(sl:get_slot('max-depth',
                                                                  T),
                                                      integer),
                          'max-results' =
                              ontology:sl_decode_term(sl:get_slot('max-results',
                                                                  T),
                                                      integer),
                          'search-id' =
                              ontology:sl_decode_term(sl:get_slot('search-id',
                                                                  T),
                                                      string)};
decode([<<"result">> | T]) ->
    #result{'0' = decode(lists:nth(1, T)),
            '1' = decode(lists:nth(2, T))};
decode([<<"result-specification">> | T]) ->
    #'result-specification'{};
decode([<<"ap-service">> | T]) ->
    #'ap-service'{name =
                      ontology:sl_decode_term(sl:get_slot(name, T), string),
                  type =
                      ontology:sl_decode_term(sl:get_slot(type, T), string),
                  addresses = sequence_of(sl:get_slot(addresses, T))};
decode([<<"ap-description">> | T]) ->
    #'ap-description'{name =
                          ontology:sl_decode_term(sl:get_slot(name, T), string),
                      'ap-services' =
                          decode(sequence_of(sl:get_slot('ap-services', T)))};
decode(nil) -> nil;
decode(X) when is_list(X) -> [decode(Y) || Y <- X];
decode(X) -> X.

set_of([<<"set">> | L]) -> decode(L).

sequence_of([<<"sequence">> | L]) -> decode(L).
