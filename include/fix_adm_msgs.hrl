%%------------------------------------------------------------------------------
%% Administrative FIX messages
%%------------------------------------------------------------------------------
%% Author: Serge Aleynikov <saleyn at gmail dot com>
%%
%% The work is derived from Maxim Lapshin's open source work:
%% https://github.com/maxlapshin/fix under the same open source MIT
%% licensing terms as the original.
%%------------------------------------------------------------------------------
%% *** This file is auto-generated, don't modify by hand!!! ***
%%------------------------------------------------------------------------------

%% Message type: "0"
-record('Heartbeat', {
  fields = #{}
  %% Optional fields:
  %% ================
  %% Tag# 112: TestReqID
}).

%% Message type: "1"
-record('TestRequest', {
  fields = #{
      'TestReqID'                 => undefined %% Tag# 112
  }
}).

%% Message type: "2"
-record('ResendRequest', {
  fields = #{
      'BeginSeqNo'                => undefined %% Tag#   7
    , 'EndSeqNo'                  => undefined %% Tag#  16
  }
}).

%% Message type: "3"
-record('Reject', {
  fields = #{
      'RefSeqNum'                 => undefined %% Tag#  45
  }
  %% Optional fields:
  %% ================
  %% Tag# 371: RefTagID
  %% Tag# 372: RefMsgType
  %% Tag# 373: SessionRejectReason
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "4"
-record('SequenceReset', {
  fields = #{
      'NewSeqNo'                  => undefined %% Tag#  36
  }
  %% Optional fields:
  %% ================
  %% Tag# 123: GapFillFlag
}).

%% Message type: "5"
-record('Logout', {
  fields = #{}
  %% Optional fields:
  %% ================
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "A"
-record('Logon', {
  fields = #{
      'EncryptMethod'             => undefined %% Tag#  98
    , 'HeartBtInt'                => undefined %% Tag# 108
  }
  %% Optional fields:
  %% ================
  %% Tag#  95: RawDataLength
  %% Tag#  96: RawData
  %% Tag# 141: ResetSeqNumFlag
  %% Tag# 789: NextExpectedMsgSeqNum
  %% Tag# 383: MaxMessageSize
  %% Tag# 384: NoMsgTypes
  %% Tag# 464: TestMessageIndicator
  %% Tag# 553: Username
  %% Tag# 554: Password
}).

