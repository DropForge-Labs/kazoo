%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wnm_flowroute).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).

-include("../wnm.hrl").

-define(WNM_FR_CONFIG_CAT, <<(?WNM_CONFIG_CAT)/binary, ".flowroute">>).

-define(FR_NUMBER_URL, whapps_config:get_string(?WNM_FR_CONFIG_CAT
                                                   ,<<"numbers_api_url">>
                                                   ,<<"https://api.flowroute.com/public/v1/available-tns/tns/">>)).

-define(FR_DEBUG, whapps_config:get_is_true(?WNM_FR_CONFIG_CAT, <<"debug">>, 'false')).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quantity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'ok', wh_json:object()} |
                          {'error', term()}.
find_numbers(<<"+", Rest/binary>>, Quantity, Opts) ->
    find_numbers(Rest, Quantity, Opts);
find_numbers(<<"1", Rest/binary>>, Quantity, Opts) ->
    find_numbers(Rest, Quantity, Opts);
find_numbers(<<NPA:3/binary>>, Quantity, _) ->
    Props = [{"npa", wh_util:to_list(NPA)}
             ,{"limit", wh_util:to_list(Quantity)}],
    case make_numbers_request(get, Props) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            Resp = [begin
                        [Num] = wh_json:get_keys(Number),
                        {Num, Number}
                    end
                    || Number <- wh_json:get_value(<<"tns">>, JObj)],
            {'ok', wh_json:from_list(Resp)}
    end;
find_numbers(Search, Quantity, _) ->
    Npa = binary:part(Search, 0, (case size(Search) of L when L < 3 -> L; _ -> 3 end)),
    Nxx = binary:part(Search, 3, (case size(Search) of L when L < 3 -> L; _ -> 3 end)),
    Props = [{"npa", wh_util:to_list(Npa)}
             ,{"nxx", wh_util:to_list(Nxx)}
             ,{"limit", wh_util:to_list(Quantity)}
            ],
    case make_numbers_request(get, Props) of
            {'error', _}=E -> E;
            {'ok', JObj} ->
                Resp = [begin
                            [Num] = wh_json:get_keys(Number),
                            {Num, Number}
                        end
                        || Number <- wh_json:get_value(<<"tns">>, JObj)],
                {'ok', wh_json:from_list(Resp)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number/1 :: (wnm_number()) -> wnm_number().
acquire_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number/1 :: (wnm_number()) -> wnm_number().
disconnect_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make a REST request to flowroute.com Numbers API to preform the
%% given verb (purchase, search, provision, ect).
%% @end
%%--------------------------------------------------------------------
-spec make_numbers_request/2 :: (atom(), proplist()) -> {ok, term()} | {error, term()}.
make_numbers_request(Props) ->
    lager:debug("making ~s request to flowroute.com ~s", ["GET", ?FR_NUMBER_URL]),
    TechPrefix = whapps_config:get_string(?WNM_FR_CONFIG_CAT, <<"tech_prefix">>, <<>>),
    SecretKey = whapps_config:get_string(?WNM_FR_CONFIG_CAT, <<"secret_key">>, <<>>),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time(now()),
    Timestamp = lists:flatten([wh_util:to_list(Year), "-", wh_util:to_list(Month), "-", wh_util:to_list(Day),
                              "T", wh_util:to_list(Hour), ":", wh_util:to_list(Minute), ":", wh_util:to_list(Second), "Z"]),
    Body = "",
    BodyMD5 = wh_util:binary_md5(Body),
    Query = mochiweb_util:urlencode(Props),
    URL = lists:flatten([?FR_NUMBER_URL, "?", Query]),
    MessageString = lists:flatten([Timestamp, "\n",
                                   "GET\n",
                                   wh_util:to_lower_string(BodyMD5), "\n",
                                   wh_util:to_lower_string(?FR_NUMBER_URL), "\n",
                                   Query, "\n"
                                  ]),
    <<Signature:160/integer>> = crypto:hmac(sha, SecretKey, MessageString),
    Headers = [{"Accept", "application/json"}
               ,{"User-Agent", ?WNM_USER_AGENT}
               ,{"X-Timestamp", Timestamp}
               ,{"Content-Type", "application/json"}],
    HTTPOptions = [{ssl,[{verify,0}]}
                   ,{basic_auth, {TechPrefix, Signature}}
                   ,{inactivity_timeout, 180000}
                   ,{connect_timeout, 180000}
                  ],
    ?FR_DEBUG andalso file:write_file("/tmp/flowroute.com.xml"
                                      ,io_lib:format("Request:~n~s ~s~n~s~n", [get, ?FR_NUMBER_URL, Body])),
    case ibrowse:send_req(URL, Headers, get, unicode:characters_to_binary(Body), HTTPOptions, 180000) of
        {ok, "401", _, _Response} ->
            ?FR_DEBUG andalso file:write_file("/tmp/flowroute.com.xml"
                                              ,io_lib:format("Response:~n401~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("flowroute.com request error: 401 (unauthenticated)"),
            {error, authentication};
        {ok, "403", _, _Response} ->
            ?FR_DEBUG andalso file:write_file("/tmp/flowroute.com.xml"
                                              ,io_lib:format("Response:~n403~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("flowroute.com request error: 403 (unauthorized)"),
            {error, authorization};
        {ok, "404", _, _Response} ->
            ?FR_DEBUG andalso file:write_file("/tmp/flowroute.com.xml"
                                              ,io_lib:format("Response:~n404~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("flowroute.com request error: 404 (not found)"),
            {error, not_found};
        {ok, "500", _, _Response} ->
            ?FR_DEBUG andalso file:write_file("/tmp/flowroute.com.xml"
                                              ,io_lib:format("Response:~n500~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("flowroute.com request error: 500 (server error)"),
            {error, server_error};
        {ok, "503", _, _Response} ->
            ?FR_DEBUG andalso file:write_file("/tmp/flowroute.com.xml"
                                              ,io_lib:format("Response:~n503~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("flowroute.com request error: 503"),
            {error, server_error};
        {ok, Code, _, [${|_]=Response} ->
            ?FR_DEBUG andalso file:write_file("/tmp/flowroute.com.xml"
                                              ,io_lib:format("Response:~n~p~n~s~n", [Code, Response])
                                              ,[append]),
            lager:debug("received response from flowroute.com"),
            try
                JObj = ejson:decode(Response),
                verify_response(JObj)
            catch
                _:R ->
                    lager:debug("failed to decode json: ~p", [R]),
                    {error, empty_response}
            end;
        {ok, Code, _, _Response} ->
            ?FR_DEBUG andalso file:write_file("/tmp/flowroute.com.xml"
                                              ,io_lib:format("Response:~n~p~n~s~n", [Code, _Response])
                                              ,[append]),
            lager:debug("flowroute.com empty response: ~p", [Code]),
            {error, empty_response};
        {error, _}=E ->
            lager:debug("flowroute.com request error: ~p", [E]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if the request was successful, and if not extract any
%% error text
%% @end
%%--------------------------------------------------------------------
-spec verify_response(wh_json:object()) ->
                             {'ok', wh_json:object()} |
                             {'error', api_binary() | ne_binaries()}.
verify_response(JObj) ->
    case wh_json:get_value(<<"details">>, JObj) of
        undefined ->
            lager:debug("request was successful"),
            {'ok', JObj};
        _ ->
            lager:debug("request failed"),
            {'error', JObj}
    end.