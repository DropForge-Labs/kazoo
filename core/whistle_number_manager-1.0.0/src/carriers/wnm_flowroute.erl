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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers/3 :: (ne_binary(), pos_integer(), wh_proplist()) -> {'error', _}.
find_numbers(_Number, _Quanity, _Opts) ->
    {error, non_available}.

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
%% Make a REST request to Bandwidth.com Numbers API to preform the
%% given verb (purchase, search, provision, ect).
%% @end
%%--------------------------------------------------------------------
-spec make_numbers_request/2 :: (atom(), proplist()) -> {ok, term()} | {error, term()}.
make_numbers_request(Verb, Props) ->
    lager:debug("making ~s request to flowroute.com ~s", [Verb, ?BW_NUMBER_URL]),
    AccessKey = whapps_config:get_string(?WNM_FR_CONFIG_CAT, <<"access_key">>, <<>>),
    SecretKey = whapps_config:get_string(?WNM_FR_CONFIG_CAT, <<"secret_key">>, <<>>),
    {{?INT_TO_STRING(Year), Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time(now()),
    Timestamp = lists:flatten([Year, "-", Month, "-", Day, "T", Hour, ":", Minute, ":", Second, "Z"])),
    Request = [{'developerKey', [DevKey]}
               | Props],
    Body = xmerl:export_simple([{Verb, ?BW_XML_NAMESPACE, Request}]
                               ,xmerl_xml
                               ,[{prolog, ?BW_XML_PROLOG}]),
    Headers = [{"Accept", "*/*"}
               ,{"User-Agent", ?WNM_USER_AGENT}
               ,{"X-Timestamp", Timestamp}
               ,{"Content-Type", "text/xml"}],
    HTTPOptions = [{ssl,[{verify,0}]}
                   ,{inactivity_timeout, 180000}
                   ,{connect_timeout, 180000}
                  ],
    ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                      ,io_lib:format("Request:~n~s ~s~n~s~n", [post, ?BW_NUMBER_URL, Body])),
    case ibrowse:send_req(?BW_NUMBER_URL, Headers, post, unicode:characters_to_binary(Body), HTTPOptions, 180000) of
        {ok, "401", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n401~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("bandwidth.com request error: 401 (unauthenticated)"),
            {error, authentication};
        {ok, "403", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n403~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("bandwidth.com request error: 403 (unauthorized)"),
            {error, authorization};
        {ok, "404", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n404~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("bandwidth.com request error: 404 (not found)"),
            {error, not_found};
        {ok, "500", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n500~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("bandwidth.com request error: 500 (server error)"),
            {error, server_error};
        {ok, "503", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n503~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("bandwidth.com request error: 503"),
            {error, server_error};
        {ok, Code, _, [$<,$?,$x,$m,$l|_]=Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n~p~n~s~n", [Code, Response])
                                              ,[append]),
            lager:debug("received response from bandwidth.com"),
            try
                {Xml, _} = xmerl_scan:string(Response),
                verify_response(Xml)
            catch
                _:R ->
                    lager:debug("failed to decode xml: ~p", [R]),
                    {error, empty_response}
            end;
        {ok, Code, _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n~p~n~s~n", [Code, _Response])
                                              ,[append]),
            lager:debug("bandwidth.com empty response: ~p", [Code]),
            {error, empty_response};
        {error, _}=E ->
            lager:debug("bandwidth.com request error: ~p", [E]),
            E
    end.
