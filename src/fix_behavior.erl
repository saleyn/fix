%%------------------------------------------------------------------------------
%% @doc NIF FIX encoding/decoding functions
%%------------------------------------------------------------------------------
%% @copyright 2022 Serge Aleynikov
%%------------------------------------------------------------------------------
-module(fix_behavior).

%% @doc This callback implements a consumer of FIX messages.
%% Arguments are:
%% <dl>
%% <dt>MsgTp</dt><dd>Message Type (e.g. 'NewOrderSingle')</dd>
%% <dt>Hdr</dt><dd>FIX header</dd>
%% <dt>Msg</dt><dd>FIX message</dd>
%% <dt>TS</dt><dd>Timestamp in microseconds since epoch</dd>
%% </dl>
-callback handle_fix(MsgTp::atom(), Hdr::map(), Msg::map(), TS::integer()) -> any().