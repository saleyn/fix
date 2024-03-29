-record('header', {
  fields = #{
      'MsgType'      => undefined %% Tag#  35
    , 'SenderCompID' => undefined %% Tag#  49
    , 'TargetCompID' => undefined %% Tag#  56
    , 'MsgSeqNum'    => undefined %% Tag#  34
    , 'SendingTime'  => undefined %% Tag#  52
  }
  %% Optional fields:
  %% ================
  %% Tag#   8: BeginString    %% present only when decoded with fix_nif:decode(..., [full])
  %% Tag#   9: BodyLength     %% present only when decoded with fix_nif:decode(..., [full])
  %% Tag#  43: PossDupFlag
  %% Tag#  97: PossResend
  %% Tag# 369: LastMsgSeqNumProcessed
  %% Tag# 115: OnBehalfOfCompID
  %% Tag# 128: DeliverToCompID
  %% Tag#  90: SecureDataLen
  %% Tag#  91: SecureData
  %% Tag#  50: SenderSubID
  %% Tag# 142: SenderLocationID
  %% Tag#  57: TargetSubID
  %% Tag# 143: TargetLocationID
  %% Tag# 116: OnBehalfOfSubID
  %% Tag# 144: OnBehalfOfLocationID
  %% Tag# 129: DeliverToSubID
  %% Tag# 145: DeliverToLocationID
  %% Tag# 122: OrigSendingTime
  %% Tag# 212: XmlDataLen
  %% Tag# 213: XmlData
  %% Tag# 347: MessageEncoding
  %% Tag# 627: NoHops
}).
