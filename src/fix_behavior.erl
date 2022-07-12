%%------------------------------------------------------------------------------
%% @doc NIF FIX encoding/decoding functions
%%------------------------------------------------------------------------------
%% @copyright 2022 Serge Aleynikov
%% The work is derived from Maxim Lapshin's open source work:
%% https://github.com/maxlapshin/fix under the same open source MIT
%% licensing terms as the original.
%%------------------------------------------------------------------------------
-module(fix_behavior).

%% @doc This callback implements a consumer of FIX messages.
%% The API is free in implement the FIX message processing using this behavior.
-callback handle_fix(MsgType::atom(), MsgHeader::map(), Msg::map()) -> any().