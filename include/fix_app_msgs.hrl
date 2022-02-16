%%------------------------------------------------------------------------------
%% Application FIX messages
%%------------------------------------------------------------------------------
%% Author: Serge Aleynikov <saleyn at gmail dot com>
%%
%% The work is derived from Maxim Lapshin's open source work:
%% https://github.com/maxlapshin/fix under the same open source MIT
%% licensing terms as the original.
%%------------------------------------------------------------------------------
%% *** This file is auto-generated, don't modify by hand!!! ***
%%------------------------------------------------------------------------------

%% Message type: "6"
-record('IOI', {
  fields = #{
      'IOIID'                                => undefined %% Tag#  23
    , 'IOITransType'                         => undefined %% Tag#  28
    , 'Side'                                 => undefined %% Tag#  54
    , 'IOIQty'                               => undefined %% Tag#  27
  }
  %% Optional fields:
  %% ================
  %% Tag#  26: IOIRefID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag# 854: QtyType
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag#  15: Currency
  %% Tag# 232: NoStipulations
  %% Tag# 555: NoLegs
  %% Tag# 423: PriceType
  %% Tag#  44: Price
  %% Tag#  62: ValidUntilTime
  %% Tag#  25: IOIQltyInd
  %% Tag# 130: IOINaturalFlag
  %% Tag# 199: NoIOIQualifiers
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag#  60: TransactTime
  %% Tag# 149: URLLink
  %% Tag# 215: NoRoutingIDs
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
}).

%% Message type: "7"
-record('Advertisement', {
  fields = #{
      'AdvId'                                => undefined %% Tag#   2
    , 'AdvTransType'                         => undefined %% Tag#   5
    , 'AdvSide'                              => undefined %% Tag#   4
    , 'Quantity'                             => undefined %% Tag#  53
  }
  %% Optional fields:
  %% ================
  %% Tag#   3: AdvRefID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 854: QtyType
  %% Tag#  44: Price
  %% Tag#  15: Currency
  %% Tag#  75: TradeDate
  %% Tag#  60: TransactTime
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 149: URLLink
  %% Tag#  30: LastMkt
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
}).

%% Message type: "8"
-record('ExecutionReport', {
  fields = #{
      'OrderID'                              => undefined %% Tag#  37
    , 'ExecID'                               => undefined %% Tag#  17
    , 'ExecType'                             => undefined %% Tag# 150
    , 'OrdStatus'                            => undefined %% Tag#  39
    , 'Side'                                 => undefined %% Tag#  54
    , 'LeavesQty'                            => undefined %% Tag# 151
    , 'CumQty'                               => undefined %% Tag#  14
    , 'AvgPx'                                => undefined %% Tag#   6
  }
  %% Optional fields:
  %% ================
  %% Tag# 198: SecondaryOrderID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 527: SecondaryExecID
  %% Tag#  11: ClOrdID
  %% Tag#  41: OrigClOrdID
  %% Tag# 583: ClOrdLinkID
  %% Tag# 693: QuoteRespID
  %% Tag# 790: OrdStatusReqID
  %% Tag# 584: MassStatusReqID
  %% Tag# 911: TotNumReports
  %% Tag# 912: LastRptRequested
  %% Tag# 453: NoPartyIDs
  %% Tag# 229: TradeOriginationDate
  %% Tag# 382: NoContraBrokers
  %% Tag#  66: ListID
  %% Tag# 548: CrossID
  %% Tag# 551: OrigCrossID
  %% Tag# 549: CrossType
  %% Tag#  19: ExecRefID
  %% Tag# 636: WorkingIndicator
  %% Tag# 103: OrdRejReason
  %% Tag# 378: ExecRestatementReason
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 589: DayBookingInst
  %% Tag# 590: BookingUnit
  %% Tag# 591: PreallocMethod
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 544: CashMargin
  %% Tag# 635: ClearingFeeIndicator
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag# 232: NoStipulations
  %% Tag# 854: QtyType
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag#  40: OrdType
  %% Tag# 423: PriceType
  %% Tag#  44: Price
  %% Tag#  99: StopPx
  %% Tag# 211: PegOffsetValue
  %% Tag# 835: PegMoveType
  %% Tag# 836: PegOffsetType
  %% Tag# 837: PegLimitType
  %% Tag# 838: PegRoundDirection
  %% Tag# 840: PegScope
  %% Tag# 388: DiscretionInst
  %% Tag# 389: DiscretionOffsetValue
  %% Tag# 841: DiscretionMoveType
  %% Tag# 842: DiscretionOffsetType
  %% Tag# 843: DiscretionLimitType
  %% Tag# 844: DiscretionRoundDirection
  %% Tag# 846: DiscretionScope
  %% Tag# 839: PeggedPrice
  %% Tag# 845: DiscretionPrice
  %% Tag# 847: TargetStrategy
  %% Tag# 848: TargetStrategyParameters
  %% Tag# 849: ParticipationRate
  %% Tag# 850: TargetStrategyPerformance
  %% Tag#  15: Currency
  %% Tag# 376: ComplianceID
  %% Tag# 377: SolicitedFlag
  %% Tag#  59: TimeInForce
  %% Tag# 168: EffectiveTime
  %% Tag# 432: ExpireDate
  %% Tag# 126: ExpireTime
  %% Tag#  18: ExecInst
  %% Tag# 528: OrderCapacity
  %% Tag# 529: OrderRestrictions
  %% Tag# 582: CustOrderCapacity
  %% Tag#  32: LastQty
  %% Tag# 652: UnderlyingLastQty
  %% Tag#  31: LastPx
  %% Tag# 651: UnderlyingLastPx
  %% Tag# 669: LastParPx
  %% Tag# 194: LastSpotRate
  %% Tag# 195: LastForwardPoints
  %% Tag#  30: LastMkt
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 943: TimeBracket
  %% Tag#  29: LastCapacity
  %% Tag# 424: DayOrderQty
  %% Tag# 425: DayCumQty
  %% Tag# 426: DayAvgPx
  %% Tag# 427: GTBookingInst
  %% Tag#  75: TradeDate
  %% Tag#  60: TransactTime
  %% Tag# 113: ReportToExch
  %% Tag#  12: Commission
  %% Tag#  13: CommType
  %% Tag# 479: CommCurrency
  %% Tag# 497: FundRenewWaiv
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
  %% Tag# 381: GrossTradeAmt
  %% Tag# 157: NumDaysInterest
  %% Tag# 230: ExDate
  %% Tag# 158: AccruedInterestRate
  %% Tag# 159: AccruedInterestAmt
  %% Tag# 738: InterestAtMaturity
  %% Tag# 920: EndAccruedInterestAmt
  %% Tag# 921: StartCash
  %% Tag# 922: EndCash
  %% Tag# 258: TradedFlatSwitch
  %% Tag# 259: BasisFeatureDate
  %% Tag# 260: BasisFeaturePrice
  %% Tag# 238: Concession
  %% Tag# 237: TotalTakedown
  %% Tag# 118: NetMoney
  %% Tag# 119: SettlCurrAmt
  %% Tag# 120: SettlCurrency
  %% Tag# 155: SettlCurrFxRate
  %% Tag# 156: SettlCurrFxRateCalc
  %% Tag#  21: HandlInst
  %% Tag# 110: MinQty
  %% Tag# 111: MaxFloor
  %% Tag#  77: PositionEffect
  %% Tag# 210: MaxShow
  %% Tag# 775: BookingType
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 193: SettlDate2
  %% Tag# 192: OrderQty2
  %% Tag# 641: LastForwardPoints2
  %% Tag# 442: MultiLegReportingType
  %% Tag# 480: CancellationRights
  %% Tag# 481: MoneyLaunderingStatus
  %% Tag# 513: RegistID
  %% Tag# 494: Designation
  %% Tag# 483: TransBkdTime
  %% Tag# 515: ExecValuationPoint
  %% Tag# 484: ExecPriceType
  %% Tag# 485: ExecPriceAdjustment
  %% Tag# 638: PriorityIndicator
  %% Tag# 639: PriceImprovement
  %% Tag# 851: LastLiquidityInd
  %% Tag# 518: NoContAmts
  %% Tag# 555: NoLegs
  %% Tag# 797: CopyMsgIndicator
  %% Tag# 136: NoMiscFees
}).

%% Message type: "9"
-record('OrderCancelReject', {
  fields = #{
      'OrderID'                              => undefined %% Tag#  37
    , 'ClOrdID'                              => undefined %% Tag#  11
    , 'OrigClOrdID'                          => undefined %% Tag#  41
    , 'OrdStatus'                            => undefined %% Tag#  39
    , 'CxlRejResponseTo'                     => undefined %% Tag# 434
  }
  %% Optional fields:
  %% ================
  %% Tag# 198: SecondaryOrderID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 583: ClOrdLinkID
  %% Tag# 636: WorkingIndicator
  %% Tag# 586: OrigOrdModTime
  %% Tag#  66: ListID
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 229: TradeOriginationDate
  %% Tag#  75: TradeDate
  %% Tag#  60: TransactTime
  %% Tag# 102: CxlRejReason
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "B"
-record('News', {
  fields = #{
      'Headline'                             => undefined %% Tag# 148
    , 'grpLinesOfText'                       => #{}       %% Tag#  33 (GroupLen: NoLinesOfText)
  }
  %% Optional fields:
  %% ================
  %% Tag#  42: OrigTime
  %% Tag#  61: Urgency
  %% Tag# 358: EncodedHeadlineLen
  %% Tag# 359: EncodedHeadline
  %% Tag# 215: NoRoutingIDs
  %% Tag# 146: NoRelatedSym
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 149: URLLink
  %% Tag#  95: RawDataLength
  %% Tag#  96: RawData
}).

%% Message type: "C"
-record('Email', {
  fields = #{
      'EmailThreadID'                        => undefined %% Tag# 164
    , 'EmailType'                            => undefined %% Tag#  94
    , 'Subject'                              => undefined %% Tag# 147
    , 'grpLinesOfText'                       => #{}       %% Tag#  33 (GroupLen: NoLinesOfText)
  }
  %% Optional fields:
  %% ================
  %% Tag#  42: OrigTime
  %% Tag# 356: EncodedSubjectLen
  %% Tag# 357: EncodedSubject
  %% Tag# 215: NoRoutingIDs
  %% Tag# 146: NoRelatedSym
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag#  37: OrderID
  %% Tag#  11: ClOrdID
  %% Tag#  95: RawDataLength
  %% Tag#  96: RawData
}).

%% Message type: "D"
-record('NewOrderSingle', {
  fields = #{
      'ClOrdID'                              => undefined %% Tag#  11
    , 'Side'                                 => undefined %% Tag#  54
    , 'TransactTime'                         => undefined %% Tag#  60
    , 'OrdType'                              => undefined %% Tag#  40
  }
  %% Optional fields:
  %% ================
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 583: ClOrdLinkID
  %% Tag# 453: NoPartyIDs
  %% Tag# 229: TradeOriginationDate
  %% Tag#  75: TradeDate
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 589: DayBookingInst
  %% Tag# 590: BookingUnit
  %% Tag# 591: PreallocMethod
  %% Tag#  70: AllocID
  %% Tag#  78: NoAllocs
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 544: CashMargin
  %% Tag# 635: ClearingFeeIndicator
  %% Tag#  21: HandlInst
  %% Tag#  18: ExecInst
  %% Tag# 110: MinQty
  %% Tag# 111: MaxFloor
  %% Tag# 100: ExDestination
  %% Tag# 386: NoTradingSessions
  %% Tag#  81: ProcessCode
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag# 140: PrevClosePx
  %% Tag# 114: LocateReqd
  %% Tag# 232: NoStipulations
  %% Tag# 854: QtyType
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag# 423: PriceType
  %% Tag#  44: Price
  %% Tag#  99: StopPx
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
  %% Tag#  15: Currency
  %% Tag# 376: ComplianceID
  %% Tag# 377: SolicitedFlag
  %% Tag#  23: IOIID
  %% Tag# 117: QuoteID
  %% Tag#  59: TimeInForce
  %% Tag# 168: EffectiveTime
  %% Tag# 432: ExpireDate
  %% Tag# 126: ExpireTime
  %% Tag# 427: GTBookingInst
  %% Tag#  12: Commission
  %% Tag#  13: CommType
  %% Tag# 479: CommCurrency
  %% Tag# 497: FundRenewWaiv
  %% Tag# 528: OrderCapacity
  %% Tag# 529: OrderRestrictions
  %% Tag# 582: CustOrderCapacity
  %% Tag# 121: ForexReq
  %% Tag# 120: SettlCurrency
  %% Tag# 775: BookingType
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 193: SettlDate2
  %% Tag# 192: OrderQty2
  %% Tag# 640: Price2
  %% Tag#  77: PositionEffect
  %% Tag# 203: CoveredOrUncovered
  %% Tag# 210: MaxShow
  %% Tag# 211: PegOffsetValue
  %% Tag# 835: PegMoveType
  %% Tag# 836: PegOffsetType
  %% Tag# 837: PegLimitType
  %% Tag# 838: PegRoundDirection
  %% Tag# 840: PegScope
  %% Tag# 388: DiscretionInst
  %% Tag# 389: DiscretionOffsetValue
  %% Tag# 841: DiscretionMoveType
  %% Tag# 842: DiscretionOffsetType
  %% Tag# 843: DiscretionLimitType
  %% Tag# 844: DiscretionRoundDirection
  %% Tag# 846: DiscretionScope
  %% Tag# 847: TargetStrategy
  %% Tag# 848: TargetStrategyParameters
  %% Tag# 849: ParticipationRate
  %% Tag# 480: CancellationRights
  %% Tag# 481: MoneyLaunderingStatus
  %% Tag# 513: RegistID
  %% Tag# 494: Designation
}).

%% Message type: "E"
-record('NewOrderList', {
  fields = #{
      'ListID'                               => undefined %% Tag#  66
    , 'BidType'                              => undefined %% Tag# 394
    , 'TotNoOrders'                          => undefined %% Tag#  68
    , 'grpOrders'                            => #{}       %% Tag#  73 (GroupLen: NoOrders)
  }
  %% Optional fields:
  %% ================
  %% Tag# 390: BidID
  %% Tag# 391: ClientBidID
  %% Tag# 414: ProgRptReqs
  %% Tag# 415: ProgPeriodInterval
  %% Tag# 480: CancellationRights
  %% Tag# 481: MoneyLaunderingStatus
  %% Tag# 513: RegistID
  %% Tag# 433: ListExecInstType
  %% Tag#  69: ListExecInst
  %% Tag# 352: EncodedListExecInstLen
  %% Tag# 353: EncodedListExecInst
  %% Tag# 765: AllowableOneSidednessPct
  %% Tag# 766: AllowableOneSidednessValue
  %% Tag# 767: AllowableOneSidednessCurr
  %% Tag# 893: LastFragment
}).

%% Message type: "F"
-record('OrderCancelRequest', {
  fields = #{
      'OrigClOrdID'                          => undefined %% Tag#  41
    , 'ClOrdID'                              => undefined %% Tag#  11
    , 'Side'                                 => undefined %% Tag#  54
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag#  37: OrderID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 583: ClOrdLinkID
  %% Tag#  66: ListID
  %% Tag# 586: OrigOrdModTime
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 453: NoPartyIDs
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag# 376: ComplianceID
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "G"
-record('OrderCancelReplaceRequest', {
  fields = #{
      'OrigClOrdID'                          => undefined %% Tag#  41
    , 'ClOrdID'                              => undefined %% Tag#  11
    , 'Side'                                 => undefined %% Tag#  54
    , 'TransactTime'                         => undefined %% Tag#  60
    , 'OrdType'                              => undefined %% Tag#  40
  }
  %% Optional fields:
  %% ================
  %% Tag#  37: OrderID
  %% Tag# 453: NoPartyIDs
  %% Tag# 229: TradeOriginationDate
  %% Tag#  75: TradeDate
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 583: ClOrdLinkID
  %% Tag#  66: ListID
  %% Tag# 586: OrigOrdModTime
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 589: DayBookingInst
  %% Tag# 590: BookingUnit
  %% Tag# 591: PreallocMethod
  %% Tag#  70: AllocID
  %% Tag#  78: NoAllocs
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 544: CashMargin
  %% Tag# 635: ClearingFeeIndicator
  %% Tag#  21: HandlInst
  %% Tag#  18: ExecInst
  %% Tag# 110: MinQty
  %% Tag# 111: MaxFloor
  %% Tag# 100: ExDestination
  %% Tag# 386: NoTradingSessions
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag# 854: QtyType
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag# 423: PriceType
  %% Tag#  44: Price
  %% Tag#  99: StopPx
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
  %% Tag# 211: PegOffsetValue
  %% Tag# 835: PegMoveType
  %% Tag# 836: PegOffsetType
  %% Tag# 837: PegLimitType
  %% Tag# 838: PegRoundDirection
  %% Tag# 840: PegScope
  %% Tag# 388: DiscretionInst
  %% Tag# 389: DiscretionOffsetValue
  %% Tag# 841: DiscretionMoveType
  %% Tag# 842: DiscretionOffsetType
  %% Tag# 843: DiscretionLimitType
  %% Tag# 844: DiscretionRoundDirection
  %% Tag# 846: DiscretionScope
  %% Tag# 847: TargetStrategy
  %% Tag# 848: TargetStrategyParameters
  %% Tag# 849: ParticipationRate
  %% Tag# 376: ComplianceID
  %% Tag# 377: SolicitedFlag
  %% Tag#  15: Currency
  %% Tag#  59: TimeInForce
  %% Tag# 168: EffectiveTime
  %% Tag# 432: ExpireDate
  %% Tag# 126: ExpireTime
  %% Tag# 427: GTBookingInst
  %% Tag#  12: Commission
  %% Tag#  13: CommType
  %% Tag# 479: CommCurrency
  %% Tag# 497: FundRenewWaiv
  %% Tag# 528: OrderCapacity
  %% Tag# 529: OrderRestrictions
  %% Tag# 582: CustOrderCapacity
  %% Tag# 121: ForexReq
  %% Tag# 120: SettlCurrency
  %% Tag# 775: BookingType
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 193: SettlDate2
  %% Tag# 192: OrderQty2
  %% Tag# 640: Price2
  %% Tag#  77: PositionEffect
  %% Tag# 203: CoveredOrUncovered
  %% Tag# 210: MaxShow
  %% Tag# 114: LocateReqd
  %% Tag# 480: CancellationRights
  %% Tag# 481: MoneyLaunderingStatus
  %% Tag# 513: RegistID
  %% Tag# 494: Designation
}).

%% Message type: "H"
-record('OrderStatusRequest', {
  fields = #{
      'ClOrdID'                              => undefined %% Tag#  11
    , 'Side'                                 => undefined %% Tag#  54
  }
  %% Optional fields:
  %% ================
  %% Tag#  37: OrderID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 583: ClOrdLinkID
  %% Tag# 453: NoPartyIDs
  %% Tag# 790: OrdStatusReqID
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
}).

%% Message type: "J"
-record('AllocationInstruction', {
  fields = #{
      'AllocID'                              => undefined %% Tag#  70
    , 'AllocTransType'                       => undefined %% Tag#  71
    , 'AllocType'                            => undefined %% Tag# 626
    , 'AllocNoOrdersType'                    => undefined %% Tag# 857
    , 'Side'                                 => undefined %% Tag#  54
    , 'Quantity'                             => undefined %% Tag#  53
    , 'AvgPx'                                => undefined %% Tag#   6
    , 'TradeDate'                            => undefined %% Tag#  75
  }
  %% Optional fields:
  %% ================
  %% Tag# 793: SecondaryAllocID
  %% Tag#  72: RefAllocID
  %% Tag# 796: AllocCancReplaceReason
  %% Tag# 808: AllocIntermedReqType
  %% Tag# 196: AllocLinkID
  %% Tag# 197: AllocLinkType
  %% Tag# 466: BookingRefID
  %% Tag#  73: NoOrders
  %% Tag# 124: NoExecs
  %% Tag# 570: PreviouslyReported
  %% Tag# 700: ReversalIndicator
  %% Tag# 574: MatchType
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 668: DeliveryForm
  %% Tag# 869: PctAtRisk
  %% Tag# 870: NoInstrAttrib
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag# 854: QtyType
  %% Tag#  30: LastMkt
  %% Tag# 229: TradeOriginationDate
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 423: PriceType
  %% Tag# 860: AvgParPx
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag#  15: Currency
  %% Tag#  74: AvgPxPrecision
  %% Tag# 453: NoPartyIDs
  %% Tag#  60: TransactTime
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 775: BookingType
  %% Tag# 381: GrossTradeAmt
  %% Tag# 238: Concession
  %% Tag# 237: TotalTakedown
  %% Tag# 118: NetMoney
  %% Tag#  77: PositionEffect
  %% Tag# 754: AutoAcceptIndicator
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 157: NumDaysInterest
  %% Tag# 158: AccruedInterestRate
  %% Tag# 159: AccruedInterestAmt
  %% Tag# 540: TotalAccruedInterestAmt
  %% Tag# 738: InterestAtMaturity
  %% Tag# 920: EndAccruedInterestAmt
  %% Tag# 921: StartCash
  %% Tag# 922: EndCash
  %% Tag# 650: LegalConfirm
  %% Tag# 232: NoStipulations
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
  %% Tag# 892: TotNoAllocs
  %% Tag# 893: LastFragment
  %% Tag#  78: NoAllocs
}).

%% Message type: "K"
-record('ListCancelRequest', {
  fields = #{
      'ListID'                               => undefined %% Tag#  66
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag# 229: TradeOriginationDate
  %% Tag#  75: TradeDate
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "L"
-record('ListExecute', {
  fields = #{
      'ListID'                               => undefined %% Tag#  66
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag# 391: ClientBidID
  %% Tag# 390: BidID
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "M"
-record('ListStatusRequest', {
  fields = #{
      'ListID'                               => undefined %% Tag#  66
  }
  %% Optional fields:
  %% ================
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "N"
-record('ListStatus', {
  fields = #{
      'ListID'                               => undefined %% Tag#  66
    , 'ListStatusType'                       => undefined %% Tag# 429
    , 'NoRpts'                               => undefined %% Tag#  82
    , 'ListOrderStatus'                      => undefined %% Tag# 431
    , 'RptSeq'                               => undefined %% Tag#  83
    , 'TotNoOrders'                          => undefined %% Tag#  68
    , 'grpOrders'                            => #{}       %% Tag#  73 (GroupLen: NoOrders)
  }
  %% Optional fields:
  %% ================
  %% Tag# 444: ListStatusText
  %% Tag# 445: EncodedListStatusTextLen
  %% Tag# 446: EncodedListStatusText
  %% Tag#  60: TransactTime
  %% Tag# 893: LastFragment
}).

%% Message type: "P"
-record('AllocationInstructionAck', {
  fields = #{
      'AllocID'                              => undefined %% Tag#  70
    , 'TransactTime'                         => undefined %% Tag#  60
    , 'AllocStatus'                          => undefined %% Tag#  87
  }
  %% Optional fields:
  %% ================
  %% Tag# 453: NoPartyIDs
  %% Tag# 793: SecondaryAllocID
  %% Tag#  75: TradeDate
  %% Tag#  88: AllocRejCode
  %% Tag# 626: AllocType
  %% Tag# 808: AllocIntermedReqType
  %% Tag# 573: MatchStatus
  %% Tag# 460: Product
  %% Tag# 167: SecurityType
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag#  78: NoAllocs
}).

%% Message type: "Q"
-record('DontKnowTrade', {
  fields = #{
      'OrderID'                              => undefined %% Tag#  37
    , 'ExecID'                               => undefined %% Tag#  17
    , 'DKReason'                             => undefined %% Tag# 127
    , 'Side'                                 => undefined %% Tag#  54
  }
  %% Optional fields:
  %% ================
  %% Tag# 198: SecondaryOrderID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag#  32: LastQty
  %% Tag#  31: LastPx
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "R"
-record('QuoteRequest', {
  fields = #{
      'QuoteReqID'                           => undefined %% Tag# 131
    , 'grpRelatedSym'                        => #{}       %% Tag# 146 (GroupLen: NoRelatedSym)
  }
  %% Optional fields:
  %% ================
  %% Tag# 644: RFQReqID
  %% Tag#  11: ClOrdID
  %% Tag# 528: OrderCapacity
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "S"
-record('Quote', {
  fields = #{
      'QuoteID'                              => undefined %% Tag# 117
  }
  %% Optional fields:
  %% ================
  %% Tag# 131: QuoteReqID
  %% Tag# 693: QuoteRespID
  %% Tag# 537: QuoteType
  %% Tag# 735: NoQuoteQualifiers
  %% Tag# 301: QuoteResponseLevel
  %% Tag# 453: NoPartyIDs
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag#  54: Side
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 193: SettlDate2
  %% Tag# 192: OrderQty2
  %% Tag#  15: Currency
  %% Tag# 232: NoStipulations
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 555: NoLegs
  %% Tag# 132: BidPx
  %% Tag# 133: OfferPx
  %% Tag# 645: MktBidPx
  %% Tag# 646: MktOfferPx
  %% Tag# 647: MinBidSize
  %% Tag# 134: BidSize
  %% Tag# 648: MinOfferSize
  %% Tag# 135: OfferSize
  %% Tag#  62: ValidUntilTime
  %% Tag# 188: BidSpotRate
  %% Tag# 190: OfferSpotRate
  %% Tag# 189: BidForwardPoints
  %% Tag# 191: OfferForwardPoints
  %% Tag# 631: MidPx
  %% Tag# 632: BidYield
  %% Tag# 633: MidYield
  %% Tag# 634: OfferYield
  %% Tag#  60: TransactTime
  %% Tag#  40: OrdType
  %% Tag# 642: BidForwardPoints2
  %% Tag# 643: OfferForwardPoints2
  %% Tag# 656: SettlCurrBidFxRate
  %% Tag# 657: SettlCurrOfferFxRate
  %% Tag# 156: SettlCurrFxRateCalc
  %% Tag#  13: CommType
  %% Tag#  12: Commission
  %% Tag# 582: CustOrderCapacity
  %% Tag# 100: ExDestination
  %% Tag# 528: OrderCapacity
  %% Tag# 423: PriceType
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "T"
-record('SettlementInstructions', {
  fields = #{
      'SettlInstMsgID'                       => undefined %% Tag# 777
    , 'SettlInstMode'                        => undefined %% Tag# 160
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag# 791: SettlInstReqID
  %% Tag# 792: SettlInstReqRejCode
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag#  11: ClOrdID
  %% Tag# 778: NoSettlInst
}).

%% Message type: "V"
-record('MarketDataRequest', {
  fields = #{
      'MDReqID'                              => undefined %% Tag# 262
    , 'SubscriptionRequestType'              => undefined %% Tag# 263
    , 'MarketDepth'                          => undefined %% Tag# 264
    , 'grpMDEntryTypes'                      => #{}       %% Tag# 267 (GroupLen: NoMDEntryTypes)
    , 'grpRelatedSym'                        => #{}       %% Tag# 146 (GroupLen: NoRelatedSym)
  }
  %% Optional fields:
  %% ================
  %% Tag# 265: MDUpdateType
  %% Tag# 266: AggregatedBook
  %% Tag# 286: OpenCloseSettlFlag
  %% Tag# 546: Scope
  %% Tag# 547: MDImplicitDelete
  %% Tag# 386: NoTradingSessions
  %% Tag# 815: ApplQueueAction
  %% Tag# 812: ApplQueueMax
}).

%% Message type: "W"
-record('MarketDataSnapshotFullRefresh', {
  fields = #{
      'grpMDEntries'                         => #{}       %% Tag# 268 (GroupLen: NoMDEntries)
  }
  %% Optional fields:
  %% ================
  %% Tag# 262: MDReqID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag# 291: FinancialStatus
  %% Tag# 292: CorporateAction
  %% Tag# 451: NetChgPrevDay
  %% Tag# 813: ApplQueueDepth
  %% Tag# 814: ApplQueueResolution
}).

%% Message type: "X"
-record('MarketDataIncrementalRefresh', {
  fields = #{
      'grpMDEntries'                         => #{}       %% Tag# 268 (GroupLen: NoMDEntries)
  }
  %% Optional fields:
  %% ================
  %% Tag# 262: MDReqID
  %% Tag# 813: ApplQueueDepth
  %% Tag# 814: ApplQueueResolution
}).

%% Message type: "Y"
-record('MarketDataRequestReject', {
  fields = #{
      'MDReqID'                              => undefined %% Tag# 262
  }
  %% Optional fields:
  %% ================
  %% Tag# 281: MDReqRejReason
  %% Tag# 816: NoAltMDSource
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "Z"
-record('QuoteCancel', {
  fields = #{
      'QuoteID'                              => undefined %% Tag# 117
    , 'QuoteCancelType'                      => undefined %% Tag# 298
  }
  %% Optional fields:
  %% ================
  %% Tag# 131: QuoteReqID
  %% Tag# 301: QuoteResponseLevel
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 295: NoQuoteEntries
}).

%% Message type: "a"
-record('QuoteStatusRequest', {
  fields = #{}
  %% Optional fields:
  %% ================
  %% Tag# 649: QuoteStatusReqID
  %% Tag# 117: QuoteID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 263: SubscriptionRequestType
}).

%% Message type: "b"
-record('MassQuoteAcknowledgement', {
  fields = #{
      'QuoteStatus'                          => undefined %% Tag# 297
  }
  %% Optional fields:
  %% ================
  %% Tag# 131: QuoteReqID
  %% Tag# 117: QuoteID
  %% Tag# 300: QuoteRejectReason
  %% Tag# 301: QuoteResponseLevel
  %% Tag# 537: QuoteType
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 296: NoQuoteSets
}).

%% Message type: "c"
-record('SecurityDefinitionRequest', {
  fields = #{
      'SecurityReqID'                        => undefined %% Tag# 320
    , 'SecurityRequestType'                  => undefined %% Tag# 321
  }
  %% Optional fields:
  %% ================
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 668: DeliveryForm
  %% Tag# 869: PctAtRisk
  %% Tag# 870: NoInstrAttrib
  %% Tag# 711: NoUnderlyings
  %% Tag#  15: Currency
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 555: NoLegs
  %% Tag# 827: ExpirationCycle
  %% Tag# 263: SubscriptionRequestType
}).

%% Message type: "d"
-record('SecurityDefinition', {
  fields = #{
      'SecurityReqID'                        => undefined %% Tag# 320
    , 'SecurityResponseID'                   => undefined %% Tag# 322
    , 'SecurityResponseType'                 => undefined %% Tag# 323
  }
  %% Optional fields:
  %% ================
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 668: DeliveryForm
  %% Tag# 869: PctAtRisk
  %% Tag# 870: NoInstrAttrib
  %% Tag# 711: NoUnderlyings
  %% Tag#  15: Currency
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 555: NoLegs
  %% Tag# 827: ExpirationCycle
  %% Tag# 561: RoundLot
  %% Tag# 562: MinTradeVol
}).

%% Message type: "e"
-record('SecurityStatusRequest', {
  fields = #{
      'SecurityStatusReqID'                  => undefined %% Tag# 324
    , 'SubscriptionRequestType'              => undefined %% Tag# 263
  }
  %% Optional fields:
  %% ================
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 668: DeliveryForm
  %% Tag# 869: PctAtRisk
  %% Tag# 870: NoInstrAttrib
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag#  15: Currency
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
}).

%% Message type: "f"
-record('SecurityStatus', {
  fields = #{}
  %% Optional fields:
  %% ================
  %% Tag# 324: SecurityStatusReqID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 668: DeliveryForm
  %% Tag# 869: PctAtRisk
  %% Tag# 870: NoInstrAttrib
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag#  15: Currency
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 325: UnsolicitedIndicator
  %% Tag# 326: SecurityTradingStatus
  %% Tag# 291: FinancialStatus
  %% Tag# 292: CorporateAction
  %% Tag# 327: HaltReasonChar
  %% Tag# 328: InViewOfCommon
  %% Tag# 329: DueToRelated
  %% Tag# 330: BuyVolume
  %% Tag# 331: SellVolume
  %% Tag# 332: HighPx
  %% Tag# 333: LowPx
  %% Tag#  31: LastPx
  %% Tag#  60: TransactTime
  %% Tag# 334: Adjustment
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "g"
-record('TradingSessionStatusRequest', {
  fields = #{
      'TradSesReqID'                         => undefined %% Tag# 335
    , 'SubscriptionRequestType'              => undefined %% Tag# 263
  }
  %% Optional fields:
  %% ================
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 338: TradSesMethod
  %% Tag# 339: TradSesMode
}).

%% Message type: "h"
-record('TradingSessionStatus', {
  fields = #{
      'TradingSessionID'                     => undefined %% Tag# 336
    , 'TradSesStatus'                        => undefined %% Tag# 340
  }
  %% Optional fields:
  %% ================
  %% Tag# 335: TradSesReqID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 338: TradSesMethod
  %% Tag# 339: TradSesMode
  %% Tag# 325: UnsolicitedIndicator
  %% Tag# 567: TradSesStatusRejReason
  %% Tag# 341: TradSesStartTime
  %% Tag# 342: TradSesOpenTime
  %% Tag# 343: TradSesPreCloseTime
  %% Tag# 344: TradSesCloseTime
  %% Tag# 345: TradSesEndTime
  %% Tag# 387: TotalVolumeTraded
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "i"
-record('MassQuote', {
  fields = #{
      'QuoteID'                              => undefined %% Tag# 117
    , 'grpQuoteSets'                         => #{}       %% Tag# 296 (GroupLen: NoQuoteSets)
  }
  %% Optional fields:
  %% ================
  %% Tag# 131: QuoteReqID
  %% Tag# 537: QuoteType
  %% Tag# 301: QuoteResponseLevel
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 293: DefBidSize
  %% Tag# 294: DefOfferSize
}).

%% Message type: "j"
-record('BusinessMessageReject', {
  fields = #{
      'RefMsgType'                           => undefined %% Tag# 372
    , 'BusinessRejectReason'                 => undefined %% Tag# 380
  }
  %% Optional fields:
  %% ================
  %% Tag#  45: RefSeqNum
  %% Tag# 379: BusinessRejectRefID
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "k"
-record('BidRequest', {
  fields = #{
      'ClientBidID'                          => undefined %% Tag# 391
    , 'BidRequestTransType'                  => undefined %% Tag# 374
    , 'TotNoRelatedSym'                      => undefined %% Tag# 393
    , 'BidType'                              => undefined %% Tag# 394
    , 'BidTradeType'                         => undefined %% Tag# 418
    , 'BasisPxType'                          => undefined %% Tag# 419
  }
  %% Optional fields:
  %% ================
  %% Tag# 390: BidID
  %% Tag# 392: ListName
  %% Tag# 395: NumTickets
  %% Tag#  15: Currency
  %% Tag# 396: SideValue1
  %% Tag# 397: SideValue2
  %% Tag# 398: NoBidDescriptors
  %% Tag# 420: NoBidComponents
  %% Tag# 409: LiquidityIndType
  %% Tag# 410: WtAverageLiquidity
  %% Tag# 411: ExchangeForPhysical
  %% Tag# 412: OutMainCntryUIndex
  %% Tag# 413: CrossPercent
  %% Tag# 414: ProgRptReqs
  %% Tag# 415: ProgPeriodInterval
  %% Tag# 416: IncTaxInd
  %% Tag# 121: ForexReq
  %% Tag# 417: NumBidders
  %% Tag#  75: TradeDate
  %% Tag# 443: StrikeTime
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "l"
-record('BidResponse', {
  fields = #{
      'grpBidComponents'                     => #{}       %% Tag# 420 (GroupLen: NoBidComponents)
  }
  %% Optional fields:
  %% ================
  %% Tag# 390: BidID
  %% Tag# 391: ClientBidID
}).

%% Message type: "m"
-record('ListStrikePrice', {
  fields = #{
      'ListID'                               => undefined %% Tag#  66
    , 'TotNoStrikes'                         => undefined %% Tag# 422
    , 'grpStrikes'                           => #{}       %% Tag# 428 (GroupLen: NoStrikes)
  }
  %% Optional fields:
  %% ================
  %% Tag# 893: LastFragment
  %% Tag# 711: NoUnderlyings
}).

%% Message type: "o"
-record('RegistrationInstructions', {
  fields = #{
      'RegistID'                             => undefined %% Tag# 513
    , 'RegistTransType'                      => undefined %% Tag# 514
    , 'RegistRefID'                          => undefined %% Tag# 508
  }
  %% Optional fields:
  %% ================
  %% Tag#  11: ClOrdID
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 493: RegistAcctType
  %% Tag# 495: TaxAdvantageType
  %% Tag# 517: OwnershipType
  %% Tag# 473: NoRegistDtls
  %% Tag# 510: NoDistribInsts
}).

%% Message type: "p"
-record('RegistrationInstructionsResponse', {
  fields = #{
      'RegistID'                             => undefined %% Tag# 513
    , 'RegistTransType'                      => undefined %% Tag# 514
    , 'RegistRefID'                          => undefined %% Tag# 508
    , 'RegistStatus'                         => undefined %% Tag# 506
  }
  %% Optional fields:
  %% ================
  %% Tag#  11: ClOrdID
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 507: RegistRejReasonCode
  %% Tag# 496: RegistRejReasonText
}).

%% Message type: "q"
-record('OrderMassCancelRequest', {
  fields = #{
      'ClOrdID'                              => undefined %% Tag#  11
    , 'MassCancelRequestType'                => undefined %% Tag# 530
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 311: UnderlyingSymbol
  %% Tag# 312: UnderlyingSymbolSfx
  %% Tag# 309: UnderlyingSecurityID
  %% Tag# 305: UnderlyingSecurityIDSource
  %% Tag# 457: NoUnderlyingSecurityAltID
  %% Tag# 462: UnderlyingProduct
  %% Tag# 463: UnderlyingCFICode
  %% Tag# 310: UnderlyingSecurityType
  %% Tag# 763: UnderlyingSecuritySubType
  %% Tag# 313: UnderlyingMaturityMonthYear
  %% Tag# 542: UnderlyingMaturityDate
  %% Tag# 315: UnderlyingPutOrCall
  %% Tag# 241: UnderlyingCouponPaymentDate
  %% Tag# 242: UnderlyingIssueDate
  %% Tag# 243: UnderlyingRepoCollateralSecurityType
  %% Tag# 244: UnderlyingRepurchaseTerm
  %% Tag# 245: UnderlyingRepurchaseRate
  %% Tag# 246: UnderlyingFactor
  %% Tag# 256: UnderlyingCreditRating
  %% Tag# 595: UnderlyingInstrRegistry
  %% Tag# 592: UnderlyingCountryOfIssue
  %% Tag# 593: UnderlyingStateOrProvinceOfIssue
  %% Tag# 594: UnderlyingLocaleOfIssue
  %% Tag# 247: UnderlyingRedemptionDate
  %% Tag# 316: UnderlyingStrikePrice
  %% Tag# 941: UnderlyingStrikeCurrency
  %% Tag# 317: UnderlyingOptAttribute
  %% Tag# 436: UnderlyingContractMultiplier
  %% Tag# 435: UnderlyingCouponRate
  %% Tag# 308: UnderlyingSecurityExchange
  %% Tag# 306: UnderlyingIssuer
  %% Tag# 362: EncodedUnderlyingIssuerLen
  %% Tag# 363: EncodedUnderlyingIssuer
  %% Tag# 307: UnderlyingSecurityDesc
  %% Tag# 364: EncodedUnderlyingSecurityDescLen
  %% Tag# 365: EncodedUnderlyingSecurityDesc
  %% Tag# 877: UnderlyingCPProgram
  %% Tag# 878: UnderlyingCPRegType
  %% Tag# 318: UnderlyingCurrency
  %% Tag# 879: UnderlyingQty
  %% Tag# 810: UnderlyingPx
  %% Tag# 882: UnderlyingDirtyPrice
  %% Tag# 883: UnderlyingEndPrice
  %% Tag# 884: UnderlyingStartValue
  %% Tag# 885: UnderlyingCurrentValue
  %% Tag# 886: UnderlyingEndValue
  %% Tag# 887: NoUnderlyingStips
  %% Tag#  54: Side
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "r"
-record('OrderMassCancelReport', {
  fields = #{
      'OrderID'                              => undefined %% Tag#  37
    , 'MassCancelRequestType'                => undefined %% Tag# 530
    , 'MassCancelResponse'                   => undefined %% Tag# 531
  }
  %% Optional fields:
  %% ================
  %% Tag#  11: ClOrdID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 198: SecondaryOrderID
  %% Tag# 532: MassCancelRejectReason
  %% Tag# 533: TotalAffectedOrders
  %% Tag# 534: NoAffectedOrders
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 311: UnderlyingSymbol
  %% Tag# 312: UnderlyingSymbolSfx
  %% Tag# 309: UnderlyingSecurityID
  %% Tag# 305: UnderlyingSecurityIDSource
  %% Tag# 457: NoUnderlyingSecurityAltID
  %% Tag# 462: UnderlyingProduct
  %% Tag# 463: UnderlyingCFICode
  %% Tag# 310: UnderlyingSecurityType
  %% Tag# 763: UnderlyingSecuritySubType
  %% Tag# 313: UnderlyingMaturityMonthYear
  %% Tag# 542: UnderlyingMaturityDate
  %% Tag# 315: UnderlyingPutOrCall
  %% Tag# 241: UnderlyingCouponPaymentDate
  %% Tag# 242: UnderlyingIssueDate
  %% Tag# 243: UnderlyingRepoCollateralSecurityType
  %% Tag# 244: UnderlyingRepurchaseTerm
  %% Tag# 245: UnderlyingRepurchaseRate
  %% Tag# 246: UnderlyingFactor
  %% Tag# 256: UnderlyingCreditRating
  %% Tag# 595: UnderlyingInstrRegistry
  %% Tag# 592: UnderlyingCountryOfIssue
  %% Tag# 593: UnderlyingStateOrProvinceOfIssue
  %% Tag# 594: UnderlyingLocaleOfIssue
  %% Tag# 247: UnderlyingRedemptionDate
  %% Tag# 316: UnderlyingStrikePrice
  %% Tag# 941: UnderlyingStrikeCurrency
  %% Tag# 317: UnderlyingOptAttribute
  %% Tag# 436: UnderlyingContractMultiplier
  %% Tag# 435: UnderlyingCouponRate
  %% Tag# 308: UnderlyingSecurityExchange
  %% Tag# 306: UnderlyingIssuer
  %% Tag# 362: EncodedUnderlyingIssuerLen
  %% Tag# 363: EncodedUnderlyingIssuer
  %% Tag# 307: UnderlyingSecurityDesc
  %% Tag# 364: EncodedUnderlyingSecurityDescLen
  %% Tag# 365: EncodedUnderlyingSecurityDesc
  %% Tag# 877: UnderlyingCPProgram
  %% Tag# 878: UnderlyingCPRegType
  %% Tag# 318: UnderlyingCurrency
  %% Tag# 879: UnderlyingQty
  %% Tag# 810: UnderlyingPx
  %% Tag# 882: UnderlyingDirtyPrice
  %% Tag# 883: UnderlyingEndPrice
  %% Tag# 884: UnderlyingStartValue
  %% Tag# 885: UnderlyingCurrentValue
  %% Tag# 886: UnderlyingEndValue
  %% Tag# 887: NoUnderlyingStips
  %% Tag#  54: Side
  %% Tag#  60: TransactTime
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "s"
-record('NewOrderCross', {
  fields = #{
      'CrossID'                              => undefined %% Tag# 548
    , 'CrossType'                            => undefined %% Tag# 549
    , 'CrossPrioritization'                  => undefined %% Tag# 550
    , 'grpSides'                             => #{}       %% Tag# 552 (GroupLen: NoSides)
    , 'TransactTime'                         => undefined %% Tag#  60
    , 'OrdType'                              => undefined %% Tag#  40
  }
  %% Optional fields:
  %% ================
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag#  21: HandlInst
  %% Tag#  18: ExecInst
  %% Tag# 110: MinQty
  %% Tag# 111: MaxFloor
  %% Tag# 100: ExDestination
  %% Tag# 386: NoTradingSessions
  %% Tag#  81: ProcessCode
  %% Tag# 140: PrevClosePx
  %% Tag# 114: LocateReqd
  %% Tag# 232: NoStipulations
  %% Tag# 423: PriceType
  %% Tag#  44: Price
  %% Tag#  99: StopPx
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
  %% Tag#  15: Currency
  %% Tag# 376: ComplianceID
  %% Tag#  23: IOIID
  %% Tag# 117: QuoteID
  %% Tag#  59: TimeInForce
  %% Tag# 168: EffectiveTime
  %% Tag# 432: ExpireDate
  %% Tag# 126: ExpireTime
  %% Tag# 427: GTBookingInst
  %% Tag# 210: MaxShow
  %% Tag# 211: PegOffsetValue
  %% Tag# 835: PegMoveType
  %% Tag# 836: PegOffsetType
  %% Tag# 837: PegLimitType
  %% Tag# 838: PegRoundDirection
  %% Tag# 840: PegScope
  %% Tag# 388: DiscretionInst
  %% Tag# 389: DiscretionOffsetValue
  %% Tag# 841: DiscretionMoveType
  %% Tag# 842: DiscretionOffsetType
  %% Tag# 843: DiscretionLimitType
  %% Tag# 844: DiscretionRoundDirection
  %% Tag# 846: DiscretionScope
  %% Tag# 847: TargetStrategy
  %% Tag# 848: TargetStrategyParameters
  %% Tag# 849: ParticipationRate
  %% Tag# 480: CancellationRights
  %% Tag# 481: MoneyLaunderingStatus
  %% Tag# 513: RegistID
  %% Tag# 494: Designation
}).

%% Message type: "t"
-record('CrossOrderCancelReplaceRequest', {
  fields = #{
      'CrossID'                              => undefined %% Tag# 548
    , 'OrigCrossID'                          => undefined %% Tag# 551
    , 'CrossType'                            => undefined %% Tag# 549
    , 'CrossPrioritization'                  => undefined %% Tag# 550
    , 'grpSides'                             => #{}       %% Tag# 552 (GroupLen: NoSides)
    , 'TransactTime'                         => undefined %% Tag#  60
    , 'OrdType'                              => undefined %% Tag#  40
  }
  %% Optional fields:
  %% ================
  %% Tag#  37: OrderID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag#  21: HandlInst
  %% Tag#  18: ExecInst
  %% Tag# 110: MinQty
  %% Tag# 111: MaxFloor
  %% Tag# 100: ExDestination
  %% Tag# 386: NoTradingSessions
  %% Tag#  81: ProcessCode
  %% Tag# 140: PrevClosePx
  %% Tag# 114: LocateReqd
  %% Tag# 232: NoStipulations
  %% Tag# 423: PriceType
  %% Tag#  44: Price
  %% Tag#  99: StopPx
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
  %% Tag#  15: Currency
  %% Tag# 376: ComplianceID
  %% Tag#  23: IOIID
  %% Tag# 117: QuoteID
  %% Tag#  59: TimeInForce
  %% Tag# 168: EffectiveTime
  %% Tag# 432: ExpireDate
  %% Tag# 126: ExpireTime
  %% Tag# 427: GTBookingInst
  %% Tag# 210: MaxShow
  %% Tag# 211: PegOffsetValue
  %% Tag# 835: PegMoveType
  %% Tag# 836: PegOffsetType
  %% Tag# 837: PegLimitType
  %% Tag# 838: PegRoundDirection
  %% Tag# 840: PegScope
  %% Tag# 388: DiscretionInst
  %% Tag# 389: DiscretionOffsetValue
  %% Tag# 841: DiscretionMoveType
  %% Tag# 842: DiscretionOffsetType
  %% Tag# 843: DiscretionLimitType
  %% Tag# 844: DiscretionRoundDirection
  %% Tag# 846: DiscretionScope
  %% Tag# 847: TargetStrategy
  %% Tag# 848: TargetStrategyParameters
  %% Tag# 849: ParticipationRate
  %% Tag# 480: CancellationRights
  %% Tag# 481: MoneyLaunderingStatus
  %% Tag# 513: RegistID
  %% Tag# 494: Designation
}).

%% Message type: "u"
-record('CrossOrderCancelRequest', {
  fields = #{
      'CrossID'                              => undefined %% Tag# 548
    , 'OrigCrossID'                          => undefined %% Tag# 551
    , 'CrossType'                            => undefined %% Tag# 549
    , 'CrossPrioritization'                  => undefined %% Tag# 550
    , 'grpSides'                             => #{}       %% Tag# 552 (GroupLen: NoSides)
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag#  37: OrderID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
}).

%% Message type: "v"
-record('SecurityTypeRequest', {
  fields = #{
      'SecurityReqID'                        => undefined %% Tag# 320
  }
  %% Optional fields:
  %% ================
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 460: Product
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
}).

%% Message type: "w"
-record('SecurityTypes', {
  fields = #{
      'SecurityReqID'                        => undefined %% Tag# 320
    , 'SecurityResponseID'                   => undefined %% Tag# 322
    , 'SecurityResponseType'                 => undefined %% Tag# 323
  }
  %% Optional fields:
  %% ================
  %% Tag# 557: TotNoSecurityTypes
  %% Tag# 893: LastFragment
  %% Tag# 558: NoSecurityTypes
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 263: SubscriptionRequestType
}).

%% Message type: "x"
-record('SecurityListRequest', {
  fields = #{
      'SecurityReqID'                        => undefined %% Tag# 320
    , 'SecurityListRequestType'              => undefined %% Tag# 559
  }
  %% Optional fields:
  %% ================
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 668: DeliveryForm
  %% Tag# 869: PctAtRisk
  %% Tag# 870: NoInstrAttrib
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag#  15: Currency
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 263: SubscriptionRequestType
}).

%% Message type: "y"
-record('SecurityList', {
  fields = #{
      'SecurityReqID'                        => undefined %% Tag# 320
    , 'SecurityResponseID'                   => undefined %% Tag# 322
    , 'SecurityRequestResult'                => undefined %% Tag# 560
  }
  %% Optional fields:
  %% ================
  %% Tag# 393: TotNoRelatedSym
  %% Tag# 893: LastFragment
  %% Tag# 146: NoRelatedSym
}).

%% Message type: "z"
-record('DerivativeSecurityListRequest', {
  fields = #{
      'SecurityReqID'                        => undefined %% Tag# 320
    , 'SecurityListRequestType'              => undefined %% Tag# 559
  }
  %% Optional fields:
  %% ================
  %% Tag# 311: UnderlyingSymbol
  %% Tag# 312: UnderlyingSymbolSfx
  %% Tag# 309: UnderlyingSecurityID
  %% Tag# 305: UnderlyingSecurityIDSource
  %% Tag# 457: NoUnderlyingSecurityAltID
  %% Tag# 462: UnderlyingProduct
  %% Tag# 463: UnderlyingCFICode
  %% Tag# 310: UnderlyingSecurityType
  %% Tag# 763: UnderlyingSecuritySubType
  %% Tag# 313: UnderlyingMaturityMonthYear
  %% Tag# 542: UnderlyingMaturityDate
  %% Tag# 315: UnderlyingPutOrCall
  %% Tag# 241: UnderlyingCouponPaymentDate
  %% Tag# 242: UnderlyingIssueDate
  %% Tag# 243: UnderlyingRepoCollateralSecurityType
  %% Tag# 244: UnderlyingRepurchaseTerm
  %% Tag# 245: UnderlyingRepurchaseRate
  %% Tag# 246: UnderlyingFactor
  %% Tag# 256: UnderlyingCreditRating
  %% Tag# 595: UnderlyingInstrRegistry
  %% Tag# 592: UnderlyingCountryOfIssue
  %% Tag# 593: UnderlyingStateOrProvinceOfIssue
  %% Tag# 594: UnderlyingLocaleOfIssue
  %% Tag# 247: UnderlyingRedemptionDate
  %% Tag# 316: UnderlyingStrikePrice
  %% Tag# 941: UnderlyingStrikeCurrency
  %% Tag# 317: UnderlyingOptAttribute
  %% Tag# 436: UnderlyingContractMultiplier
  %% Tag# 435: UnderlyingCouponRate
  %% Tag# 308: UnderlyingSecurityExchange
  %% Tag# 306: UnderlyingIssuer
  %% Tag# 362: EncodedUnderlyingIssuerLen
  %% Tag# 363: EncodedUnderlyingIssuer
  %% Tag# 307: UnderlyingSecurityDesc
  %% Tag# 364: EncodedUnderlyingSecurityDescLen
  %% Tag# 365: EncodedUnderlyingSecurityDesc
  %% Tag# 877: UnderlyingCPProgram
  %% Tag# 878: UnderlyingCPRegType
  %% Tag# 318: UnderlyingCurrency
  %% Tag# 879: UnderlyingQty
  %% Tag# 810: UnderlyingPx
  %% Tag# 882: UnderlyingDirtyPrice
  %% Tag# 883: UnderlyingEndPrice
  %% Tag# 884: UnderlyingStartValue
  %% Tag# 885: UnderlyingCurrentValue
  %% Tag# 886: UnderlyingEndValue
  %% Tag# 887: NoUnderlyingStips
  %% Tag# 762: SecuritySubType
  %% Tag#  15: Currency
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 263: SubscriptionRequestType
}).

%% Message type: "AA"
-record('DerivativeSecurityList', {
  fields = #{
      'SecurityReqID'                        => undefined %% Tag# 320
    , 'SecurityResponseID'                   => undefined %% Tag# 322
    , 'SecurityRequestResult'                => undefined %% Tag# 560
  }
  %% Optional fields:
  %% ================
  %% Tag# 311: UnderlyingSymbol
  %% Tag# 312: UnderlyingSymbolSfx
  %% Tag# 309: UnderlyingSecurityID
  %% Tag# 305: UnderlyingSecurityIDSource
  %% Tag# 457: NoUnderlyingSecurityAltID
  %% Tag# 462: UnderlyingProduct
  %% Tag# 463: UnderlyingCFICode
  %% Tag# 310: UnderlyingSecurityType
  %% Tag# 763: UnderlyingSecuritySubType
  %% Tag# 313: UnderlyingMaturityMonthYear
  %% Tag# 542: UnderlyingMaturityDate
  %% Tag# 315: UnderlyingPutOrCall
  %% Tag# 241: UnderlyingCouponPaymentDate
  %% Tag# 242: UnderlyingIssueDate
  %% Tag# 243: UnderlyingRepoCollateralSecurityType
  %% Tag# 244: UnderlyingRepurchaseTerm
  %% Tag# 245: UnderlyingRepurchaseRate
  %% Tag# 246: UnderlyingFactor
  %% Tag# 256: UnderlyingCreditRating
  %% Tag# 595: UnderlyingInstrRegistry
  %% Tag# 592: UnderlyingCountryOfIssue
  %% Tag# 593: UnderlyingStateOrProvinceOfIssue
  %% Tag# 594: UnderlyingLocaleOfIssue
  %% Tag# 247: UnderlyingRedemptionDate
  %% Tag# 316: UnderlyingStrikePrice
  %% Tag# 941: UnderlyingStrikeCurrency
  %% Tag# 317: UnderlyingOptAttribute
  %% Tag# 436: UnderlyingContractMultiplier
  %% Tag# 435: UnderlyingCouponRate
  %% Tag# 308: UnderlyingSecurityExchange
  %% Tag# 306: UnderlyingIssuer
  %% Tag# 362: EncodedUnderlyingIssuerLen
  %% Tag# 363: EncodedUnderlyingIssuer
  %% Tag# 307: UnderlyingSecurityDesc
  %% Tag# 364: EncodedUnderlyingSecurityDescLen
  %% Tag# 365: EncodedUnderlyingSecurityDesc
  %% Tag# 877: UnderlyingCPProgram
  %% Tag# 878: UnderlyingCPRegType
  %% Tag# 318: UnderlyingCurrency
  %% Tag# 879: UnderlyingQty
  %% Tag# 810: UnderlyingPx
  %% Tag# 882: UnderlyingDirtyPrice
  %% Tag# 883: UnderlyingEndPrice
  %% Tag# 884: UnderlyingStartValue
  %% Tag# 885: UnderlyingCurrentValue
  %% Tag# 886: UnderlyingEndValue
  %% Tag# 887: NoUnderlyingStips
  %% Tag# 393: TotNoRelatedSym
  %% Tag# 893: LastFragment
  %% Tag# 146: NoRelatedSym
}).

%% Message type: "AB"
-record('NewOrderMultileg', {
  fields = #{
      'ClOrdID'                              => undefined %% Tag#  11
    , 'Side'                                 => undefined %% Tag#  54
    , 'grpLegs'                              => #{}       %% Tag# 555 (GroupLen: NoLegs)
    , 'TransactTime'                         => undefined %% Tag#  60
    , 'OrdType'                              => undefined %% Tag#  40
  }
  %% Optional fields:
  %% ================
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 583: ClOrdLinkID
  %% Tag# 453: NoPartyIDs
  %% Tag# 229: TradeOriginationDate
  %% Tag#  75: TradeDate
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 589: DayBookingInst
  %% Tag# 590: BookingUnit
  %% Tag# 591: PreallocMethod
  %% Tag#  70: AllocID
  %% Tag#  78: NoAllocs
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 544: CashMargin
  %% Tag# 635: ClearingFeeIndicator
  %% Tag#  21: HandlInst
  %% Tag#  18: ExecInst
  %% Tag# 110: MinQty
  %% Tag# 111: MaxFloor
  %% Tag# 100: ExDestination
  %% Tag# 386: NoTradingSessions
  %% Tag#  81: ProcessCode
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 711: NoUnderlyings
  %% Tag# 140: PrevClosePx
  %% Tag# 114: LocateReqd
  %% Tag# 854: QtyType
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag# 423: PriceType
  %% Tag#  44: Price
  %% Tag#  99: StopPx
  %% Tag#  15: Currency
  %% Tag# 376: ComplianceID
  %% Tag# 377: SolicitedFlag
  %% Tag#  23: IOIID
  %% Tag# 117: QuoteID
  %% Tag#  59: TimeInForce
  %% Tag# 168: EffectiveTime
  %% Tag# 432: ExpireDate
  %% Tag# 126: ExpireTime
  %% Tag# 427: GTBookingInst
  %% Tag#  12: Commission
  %% Tag#  13: CommType
  %% Tag# 479: CommCurrency
  %% Tag# 497: FundRenewWaiv
  %% Tag# 528: OrderCapacity
  %% Tag# 529: OrderRestrictions
  %% Tag# 582: CustOrderCapacity
  %% Tag# 121: ForexReq
  %% Tag# 120: SettlCurrency
  %% Tag# 775: BookingType
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag#  77: PositionEffect
  %% Tag# 203: CoveredOrUncovered
  %% Tag# 210: MaxShow
  %% Tag# 211: PegOffsetValue
  %% Tag# 835: PegMoveType
  %% Tag# 836: PegOffsetType
  %% Tag# 837: PegLimitType
  %% Tag# 838: PegRoundDirection
  %% Tag# 840: PegScope
  %% Tag# 388: DiscretionInst
  %% Tag# 389: DiscretionOffsetValue
  %% Tag# 841: DiscretionMoveType
  %% Tag# 842: DiscretionOffsetType
  %% Tag# 843: DiscretionLimitType
  %% Tag# 844: DiscretionRoundDirection
  %% Tag# 846: DiscretionScope
  %% Tag# 847: TargetStrategy
  %% Tag# 848: TargetStrategyParameters
  %% Tag# 849: ParticipationRate
  %% Tag# 480: CancellationRights
  %% Tag# 481: MoneyLaunderingStatus
  %% Tag# 513: RegistID
  %% Tag# 494: Designation
  %% Tag# 563: MultiLegRptTypeReq
}).

%% Message type: "AC"
-record('MultilegOrderCancelReplace', {
  fields = #{
      'OrigClOrdID'                          => undefined %% Tag#  41
    , 'ClOrdID'                              => undefined %% Tag#  11
    , 'Side'                                 => undefined %% Tag#  54
    , 'grpLegs'                              => #{}       %% Tag# 555 (GroupLen: NoLegs)
    , 'TransactTime'                         => undefined %% Tag#  60
    , 'OrdType'                              => undefined %% Tag#  40
  }
  %% Optional fields:
  %% ================
  %% Tag#  37: OrderID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 583: ClOrdLinkID
  %% Tag# 586: OrigOrdModTime
  %% Tag# 453: NoPartyIDs
  %% Tag# 229: TradeOriginationDate
  %% Tag#  75: TradeDate
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 589: DayBookingInst
  %% Tag# 590: BookingUnit
  %% Tag# 591: PreallocMethod
  %% Tag#  70: AllocID
  %% Tag#  78: NoAllocs
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 544: CashMargin
  %% Tag# 635: ClearingFeeIndicator
  %% Tag#  21: HandlInst
  %% Tag#  18: ExecInst
  %% Tag# 110: MinQty
  %% Tag# 111: MaxFloor
  %% Tag# 100: ExDestination
  %% Tag# 386: NoTradingSessions
  %% Tag#  81: ProcessCode
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 711: NoUnderlyings
  %% Tag# 140: PrevClosePx
  %% Tag# 114: LocateReqd
  %% Tag# 854: QtyType
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag# 423: PriceType
  %% Tag#  44: Price
  %% Tag#  99: StopPx
  %% Tag#  15: Currency
  %% Tag# 376: ComplianceID
  %% Tag# 377: SolicitedFlag
  %% Tag#  23: IOIID
  %% Tag# 117: QuoteID
  %% Tag#  59: TimeInForce
  %% Tag# 168: EffectiveTime
  %% Tag# 432: ExpireDate
  %% Tag# 126: ExpireTime
  %% Tag# 427: GTBookingInst
  %% Tag#  12: Commission
  %% Tag#  13: CommType
  %% Tag# 479: CommCurrency
  %% Tag# 497: FundRenewWaiv
  %% Tag# 528: OrderCapacity
  %% Tag# 529: OrderRestrictions
  %% Tag# 582: CustOrderCapacity
  %% Tag# 121: ForexReq
  %% Tag# 120: SettlCurrency
  %% Tag# 775: BookingType
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag#  77: PositionEffect
  %% Tag# 203: CoveredOrUncovered
  %% Tag# 210: MaxShow
  %% Tag# 211: PegOffsetValue
  %% Tag# 835: PegMoveType
  %% Tag# 836: PegOffsetType
  %% Tag# 837: PegLimitType
  %% Tag# 838: PegRoundDirection
  %% Tag# 840: PegScope
  %% Tag# 388: DiscretionInst
  %% Tag# 389: DiscretionOffsetValue
  %% Tag# 841: DiscretionMoveType
  %% Tag# 842: DiscretionOffsetType
  %% Tag# 843: DiscretionLimitType
  %% Tag# 844: DiscretionRoundDirection
  %% Tag# 846: DiscretionScope
  %% Tag# 847: TargetStrategy
  %% Tag# 848: TargetStrategyParameters
  %% Tag# 849: ParticipationRate
  %% Tag# 480: CancellationRights
  %% Tag# 481: MoneyLaunderingStatus
  %% Tag# 513: RegistID
  %% Tag# 494: Designation
  %% Tag# 563: MultiLegRptTypeReq
}).

%% Message type: "AD"
-record('TradeCaptureReportRequest', {
  fields = #{
      'TradeRequestID'                       => undefined %% Tag# 568
    , 'TradeRequestType'                     => undefined %% Tag# 569
  }
  %% Optional fields:
  %% ================
  %% Tag# 263: SubscriptionRequestType
  %% Tag# 571: TradeReportID
  %% Tag# 818: SecondaryTradeReportID
  %% Tag#  17: ExecID
  %% Tag# 150: ExecType
  %% Tag#  37: OrderID
  %% Tag#  11: ClOrdID
  %% Tag# 573: MatchStatus
  %% Tag# 828: TrdType
  %% Tag# 829: TrdSubType
  %% Tag# 830: TransferReason
  %% Tag# 855: SecondaryTrdType
  %% Tag# 820: TradeLinkID
  %% Tag# 880: TrdMatchID
  %% Tag# 453: NoPartyIDs
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 668: DeliveryForm
  %% Tag# 869: PctAtRisk
  %% Tag# 870: NoInstrAttrib
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag# 580: NoDates
  %% Tag# 715: ClearingBusinessDate
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 943: TimeBracket
  %% Tag#  54: Side
  %% Tag# 442: MultiLegReportingType
  %% Tag# 578: TradeInputSource
  %% Tag# 579: TradeInputDevice
  %% Tag# 725: ResponseTransportType
  %% Tag# 726: ResponseDestination
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AE"
-record('TradeCaptureReport', {
  fields = #{
      'TradeReportID'                        => undefined %% Tag# 571
    , 'PreviouslyReported'                   => undefined %% Tag# 570
    , 'LastQty'                              => undefined %% Tag#  32
    , 'LastPx'                               => undefined %% Tag#  31
    , 'TradeDate'                            => undefined %% Tag#  75
    , 'TransactTime'                         => undefined %% Tag#  60
    , 'grpSides'                             => #{}       %% Tag# 552 (GroupLen: NoSides)
  }
  %% Optional fields:
  %% ================
  %% Tag# 487: TradeReportTransType
  %% Tag# 856: TradeReportType
  %% Tag# 568: TradeRequestID
  %% Tag# 828: TrdType
  %% Tag# 829: TrdSubType
  %% Tag# 855: SecondaryTrdType
  %% Tag# 830: TransferReason
  %% Tag# 150: ExecType
  %% Tag# 748: TotNumTradeReports
  %% Tag# 912: LastRptRequested
  %% Tag# 325: UnsolicitedIndicator
  %% Tag# 263: SubscriptionRequestType
  %% Tag# 572: TradeReportRefID
  %% Tag# 881: SecondaryTradeReportRefID
  %% Tag# 818: SecondaryTradeReportID
  %% Tag# 820: TradeLinkID
  %% Tag# 880: TrdMatchID
  %% Tag#  17: ExecID
  %% Tag#  39: OrdStatus
  %% Tag# 527: SecondaryExecID
  %% Tag# 378: ExecRestatementReason
  %% Tag# 423: PriceType
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag# 854: QtyType
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
  %% Tag# 711: NoUnderlyings
  %% Tag# 822: UnderlyingTradingSessionID
  %% Tag# 823: UnderlyingTradingSessionSubID
  %% Tag# 669: LastParPx
  %% Tag# 194: LastSpotRate
  %% Tag# 195: LastForwardPoints
  %% Tag#  30: LastMkt
  %% Tag# 715: ClearingBusinessDate
  %% Tag#   6: AvgPx
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 819: AvgPxIndicator
  %% Tag# 753: NoPosAmt
  %% Tag# 442: MultiLegReportingType
  %% Tag# 824: TradeLegRefID
  %% Tag# 555: NoLegs
  %% Tag# 768: NoTrdRegTimestamps
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 573: MatchStatus
  %% Tag# 574: MatchType
  %% Tag# 797: CopyMsgIndicator
  %% Tag# 852: PublishTrdIndicator
  %% Tag# 853: ShortSaleReason
}).

%% Message type: "AF"
-record('OrderMassStatusRequest', {
  fields = #{
      'MassStatusReqID'                      => undefined %% Tag# 584
    , 'MassStatusReqType'                    => undefined %% Tag# 585
  }
  %% Optional fields:
  %% ================
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 311: UnderlyingSymbol
  %% Tag# 312: UnderlyingSymbolSfx
  %% Tag# 309: UnderlyingSecurityID
  %% Tag# 305: UnderlyingSecurityIDSource
  %% Tag# 457: NoUnderlyingSecurityAltID
  %% Tag# 462: UnderlyingProduct
  %% Tag# 463: UnderlyingCFICode
  %% Tag# 310: UnderlyingSecurityType
  %% Tag# 763: UnderlyingSecuritySubType
  %% Tag# 313: UnderlyingMaturityMonthYear
  %% Tag# 542: UnderlyingMaturityDate
  %% Tag# 315: UnderlyingPutOrCall
  %% Tag# 241: UnderlyingCouponPaymentDate
  %% Tag# 242: UnderlyingIssueDate
  %% Tag# 243: UnderlyingRepoCollateralSecurityType
  %% Tag# 244: UnderlyingRepurchaseTerm
  %% Tag# 245: UnderlyingRepurchaseRate
  %% Tag# 246: UnderlyingFactor
  %% Tag# 256: UnderlyingCreditRating
  %% Tag# 595: UnderlyingInstrRegistry
  %% Tag# 592: UnderlyingCountryOfIssue
  %% Tag# 593: UnderlyingStateOrProvinceOfIssue
  %% Tag# 594: UnderlyingLocaleOfIssue
  %% Tag# 247: UnderlyingRedemptionDate
  %% Tag# 316: UnderlyingStrikePrice
  %% Tag# 941: UnderlyingStrikeCurrency
  %% Tag# 317: UnderlyingOptAttribute
  %% Tag# 436: UnderlyingContractMultiplier
  %% Tag# 435: UnderlyingCouponRate
  %% Tag# 308: UnderlyingSecurityExchange
  %% Tag# 306: UnderlyingIssuer
  %% Tag# 362: EncodedUnderlyingIssuerLen
  %% Tag# 363: EncodedUnderlyingIssuer
  %% Tag# 307: UnderlyingSecurityDesc
  %% Tag# 364: EncodedUnderlyingSecurityDescLen
  %% Tag# 365: EncodedUnderlyingSecurityDesc
  %% Tag# 877: UnderlyingCPProgram
  %% Tag# 878: UnderlyingCPRegType
  %% Tag# 318: UnderlyingCurrency
  %% Tag# 879: UnderlyingQty
  %% Tag# 810: UnderlyingPx
  %% Tag# 882: UnderlyingDirtyPrice
  %% Tag# 883: UnderlyingEndPrice
  %% Tag# 884: UnderlyingStartValue
  %% Tag# 885: UnderlyingCurrentValue
  %% Tag# 886: UnderlyingEndValue
  %% Tag# 887: NoUnderlyingStips
  %% Tag#  54: Side
}).

%% Message type: "AG"
-record('QuoteRequestReject', {
  fields = #{
      'QuoteReqID'                           => undefined %% Tag# 131
    , 'QuoteRequestRejectReason'             => undefined %% Tag# 658
    , 'grpRelatedSym'                        => #{}       %% Tag# 146 (GroupLen: NoRelatedSym)
  }
  %% Optional fields:
  %% ================
  %% Tag# 644: RFQReqID
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AH"
-record('RFQRequest', {
  fields = #{
      'RFQReqID'                             => undefined %% Tag# 644
    , 'grpRelatedSym'                        => #{}       %% Tag# 146 (GroupLen: NoRelatedSym)
  }
  %% Optional fields:
  %% ================
  %% Tag# 263: SubscriptionRequestType
}).

%% Message type: "AI"
-record('QuoteStatusReport', {
  fields = #{
      'QuoteID'                              => undefined %% Tag# 117
  }
  %% Optional fields:
  %% ================
  %% Tag# 649: QuoteStatusReqID
  %% Tag# 131: QuoteReqID
  %% Tag# 693: QuoteRespID
  %% Tag# 537: QuoteType
  %% Tag# 453: NoPartyIDs
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag#  54: Side
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 193: SettlDate2
  %% Tag# 192: OrderQty2
  %% Tag#  15: Currency
  %% Tag# 232: NoStipulations
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 555: NoLegs
  %% Tag# 735: NoQuoteQualifiers
  %% Tag# 126: ExpireTime
  %% Tag#  44: Price
  %% Tag# 423: PriceType
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
  %% Tag# 132: BidPx
  %% Tag# 133: OfferPx
  %% Tag# 645: MktBidPx
  %% Tag# 646: MktOfferPx
  %% Tag# 647: MinBidSize
  %% Tag# 134: BidSize
  %% Tag# 648: MinOfferSize
  %% Tag# 135: OfferSize
  %% Tag#  62: ValidUntilTime
  %% Tag# 188: BidSpotRate
  %% Tag# 190: OfferSpotRate
  %% Tag# 189: BidForwardPoints
  %% Tag# 191: OfferForwardPoints
  %% Tag# 631: MidPx
  %% Tag# 632: BidYield
  %% Tag# 633: MidYield
  %% Tag# 634: OfferYield
  %% Tag#  60: TransactTime
  %% Tag#  40: OrdType
  %% Tag# 642: BidForwardPoints2
  %% Tag# 643: OfferForwardPoints2
  %% Tag# 656: SettlCurrBidFxRate
  %% Tag# 657: SettlCurrOfferFxRate
  %% Tag# 156: SettlCurrFxRateCalc
  %% Tag#  13: CommType
  %% Tag#  12: Commission
  %% Tag# 582: CustOrderCapacity
  %% Tag# 100: ExDestination
  %% Tag# 297: QuoteStatus
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AJ"
-record('QuoteResponse', {
  fields = #{
      'QuoteRespID'                          => undefined %% Tag# 693
    , 'QuoteRespType'                        => undefined %% Tag# 694
  }
  %% Optional fields:
  %% ================
  %% Tag# 117: QuoteID
  %% Tag#  11: ClOrdID
  %% Tag# 528: OrderCapacity
  %% Tag#  23: IOIID
  %% Tag# 537: QuoteType
  %% Tag# 735: NoQuoteQualifiers
  %% Tag# 453: NoPartyIDs
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag#  54: Side
  %% Tag#  38: OrderQty
  %% Tag# 152: CashOrderQty
  %% Tag# 516: OrderPercent
  %% Tag# 468: RoundingDirection
  %% Tag# 469: RoundingModulus
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 193: SettlDate2
  %% Tag# 192: OrderQty2
  %% Tag#  15: Currency
  %% Tag# 232: NoStipulations
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag# 555: NoLegs
  %% Tag# 132: BidPx
  %% Tag# 133: OfferPx
  %% Tag# 645: MktBidPx
  %% Tag# 646: MktOfferPx
  %% Tag# 647: MinBidSize
  %% Tag# 134: BidSize
  %% Tag# 648: MinOfferSize
  %% Tag# 135: OfferSize
  %% Tag#  62: ValidUntilTime
  %% Tag# 188: BidSpotRate
  %% Tag# 190: OfferSpotRate
  %% Tag# 189: BidForwardPoints
  %% Tag# 191: OfferForwardPoints
  %% Tag# 631: MidPx
  %% Tag# 632: BidYield
  %% Tag# 633: MidYield
  %% Tag# 634: OfferYield
  %% Tag#  60: TransactTime
  %% Tag#  40: OrdType
  %% Tag# 642: BidForwardPoints2
  %% Tag# 643: OfferForwardPoints2
  %% Tag# 656: SettlCurrBidFxRate
  %% Tag# 657: SettlCurrOfferFxRate
  %% Tag# 156: SettlCurrFxRateCalc
  %% Tag#  12: Commission
  %% Tag#  13: CommType
  %% Tag# 582: CustOrderCapacity
  %% Tag# 100: ExDestination
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag#  44: Price
  %% Tag# 423: PriceType
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
}).

%% Message type: "AK"
-record('Confirmation', {
  fields = #{
      'ConfirmID'                            => undefined %% Tag# 664
    , 'ConfirmTransType'                     => undefined %% Tag# 666
    , 'ConfirmType'                          => undefined %% Tag# 773
    , 'ConfirmStatus'                        => undefined %% Tag# 665
    , 'TransactTime'                         => undefined %% Tag#  60
    , 'TradeDate'                            => undefined %% Tag#  75
    , 'AllocQty'                             => undefined %% Tag#  80
    , 'Side'                                 => undefined %% Tag#  54
    , 'grpCapacities'                        => #{}       %% Tag# 862 (GroupLen: NoCapacities)
    , 'AllocAccount'                         => undefined %% Tag#  79
    , 'AvgPx'                                => undefined %% Tag#   6
    , 'GrossTradeAmt'                        => undefined %% Tag# 381
    , 'NetMoney'                             => undefined %% Tag# 118
  }
  %% Optional fields:
  %% ================
  %% Tag# 772: ConfirmRefID
  %% Tag# 859: ConfirmReqID
  %% Tag# 797: CopyMsgIndicator
  %% Tag# 650: LegalConfirm
  %% Tag# 453: NoPartyIDs
  %% Tag#  73: NoOrders
  %% Tag#  70: AllocID
  %% Tag# 793: SecondaryAllocID
  %% Tag# 467: IndividualAllocID
  %% Tag# 768: NoTrdRegTimestamps
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 668: DeliveryForm
  %% Tag# 869: PctAtRisk
  %% Tag# 870: NoInstrAttrib
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
  %% Tag# 854: QtyType
  %% Tag#  15: Currency
  %% Tag#  30: LastMkt
  %% Tag# 661: AllocAcctIDSource
  %% Tag# 798: AllocAccountType
  %% Tag#  74: AvgPxPrecision
  %% Tag# 423: PriceType
  %% Tag# 860: AvgParPx
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 861: ReportedPx
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag#  81: ProcessCode
  %% Tag# 157: NumDaysInterest
  %% Tag# 230: ExDate
  %% Tag# 158: AccruedInterestRate
  %% Tag# 159: AccruedInterestAmt
  %% Tag# 738: InterestAtMaturity
  %% Tag# 920: EndAccruedInterestAmt
  %% Tag# 921: StartCash
  %% Tag# 922: EndCash
  %% Tag# 238: Concession
  %% Tag# 237: TotalTakedown
  %% Tag# 890: MaturityNetMoney
  %% Tag# 119: SettlCurrAmt
  %% Tag# 120: SettlCurrency
  %% Tag# 155: SettlCurrFxRate
  %% Tag# 156: SettlCurrFxRateCalc
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 172: SettlDeliveryType
  %% Tag# 169: StandInstDbType
  %% Tag# 170: StandInstDbName
  %% Tag# 171: StandInstDbID
  %% Tag#  85: NoDlvyInst
  %% Tag#  12: Commission
  %% Tag#  13: CommType
  %% Tag# 479: CommCurrency
  %% Tag# 497: FundRenewWaiv
  %% Tag# 858: SharedCommission
  %% Tag# 232: NoStipulations
  %% Tag# 136: NoMiscFees
}).

%% Message type: "AL"
-record('PositionMaintenanceRequest', {
  fields = #{
      'PosReqID'                             => undefined %% Tag# 710
    , 'PosTransType'                         => undefined %% Tag# 709
    , 'PosMaintAction'                       => undefined %% Tag# 712
    , 'ClearingBusinessDate'                 => undefined %% Tag# 715
    , 'Account'                              => undefined %% Tag#   1
    , 'AccountType'                          => undefined %% Tag# 581
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag# 713: OrigPosReqRefID
  %% Tag# 714: PosMaintRptRefID
  %% Tag# 716: SettlSessID
  %% Tag# 717: SettlSessSubID
  %% Tag# 453: NoPartyIDs
  %% Tag# 660: AcctIDSource
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 386: NoTradingSessions
  %% Tag# 702: NoPositions
  %% Tag# 718: AdjustmentType
  %% Tag# 719: ContraryInstructionIndicator
  %% Tag# 720: PriorSpreadIndicator
  %% Tag# 834: ThresholdAmount
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AM"
-record('PositionMaintenanceReport', {
  fields = #{
      'PosMaintRptID'                        => undefined %% Tag# 721
    , 'PosTransType'                         => undefined %% Tag# 709
    , 'PosMaintAction'                       => undefined %% Tag# 712
    , 'OrigPosReqRefID'                      => undefined %% Tag# 713
    , 'PosMaintStatus'                       => undefined %% Tag# 722
    , 'ClearingBusinessDate'                 => undefined %% Tag# 715
    , 'Account'                              => undefined %% Tag#   1
    , 'AccountType'                          => undefined %% Tag# 581
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag# 710: PosReqID
  %% Tag# 723: PosMaintResult
  %% Tag# 716: SettlSessID
  %% Tag# 717: SettlSessSubID
  %% Tag# 453: NoPartyIDs
  %% Tag# 660: AcctIDSource
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 386: NoTradingSessions
  %% Tag# 702: NoPositions
  %% Tag# 753: NoPosAmt
  %% Tag# 718: AdjustmentType
  %% Tag# 834: ThresholdAmount
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AN"
-record('RequestForPositions', {
  fields = #{
      'PosReqID'                             => undefined %% Tag# 710
    , 'PosReqType'                           => undefined %% Tag# 724
    , 'Account'                              => undefined %% Tag#   1
    , 'AccountType'                          => undefined %% Tag# 581
    , 'ClearingBusinessDate'                 => undefined %% Tag# 715
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag# 573: MatchStatus
  %% Tag# 263: SubscriptionRequestType
  %% Tag# 453: NoPartyIDs
  %% Tag# 660: AcctIDSource
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 716: SettlSessID
  %% Tag# 717: SettlSessSubID
  %% Tag# 386: NoTradingSessions
  %% Tag# 725: ResponseTransportType
  %% Tag# 726: ResponseDestination
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AO"
-record('RequestForPositionsAck', {
  fields = #{
      'PosMaintRptID'                        => undefined %% Tag# 721
    , 'PosReqResult'                         => undefined %% Tag# 728
    , 'PosReqStatus'                         => undefined %% Tag# 729
    , 'Account'                              => undefined %% Tag#   1
    , 'AccountType'                          => undefined %% Tag# 581
  }
  %% Optional fields:
  %% ================
  %% Tag# 710: PosReqID
  %% Tag# 727: TotalNumPosReports
  %% Tag# 325: UnsolicitedIndicator
  %% Tag# 453: NoPartyIDs
  %% Tag# 660: AcctIDSource
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 725: ResponseTransportType
  %% Tag# 726: ResponseDestination
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AP"
-record('PositionReport', {
  fields = #{
      'PosMaintRptID'                        => undefined %% Tag# 721
    , 'PosReqResult'                         => undefined %% Tag# 728
    , 'ClearingBusinessDate'                 => undefined %% Tag# 715
    , 'Account'                              => undefined %% Tag#   1
    , 'AccountType'                          => undefined %% Tag# 581
    , 'SettlPrice'                           => undefined %% Tag# 730
    , 'SettlPriceType'                       => undefined %% Tag# 731
    , 'PriorSettlPrice'                      => undefined %% Tag# 734
  }
  %% Optional fields:
  %% ================
  %% Tag# 710: PosReqID
  %% Tag# 724: PosReqType
  %% Tag# 263: SubscriptionRequestType
  %% Tag# 727: TotalNumPosReports
  %% Tag# 325: UnsolicitedIndicator
  %% Tag# 716: SettlSessID
  %% Tag# 717: SettlSessSubID
  %% Tag# 453: NoPartyIDs
  %% Tag# 660: AcctIDSource
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 702: NoPositions
  %% Tag# 753: NoPosAmt
  %% Tag# 506: RegistStatus
  %% Tag# 743: DeliveryDate
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AQ"
-record('TradeCaptureReportRequestAck', {
  fields = #{
      'TradeRequestID'                       => undefined %% Tag# 568
    , 'TradeRequestType'                     => undefined %% Tag# 569
    , 'TradeRequestResult'                   => undefined %% Tag# 749
    , 'TradeRequestStatus'                   => undefined %% Tag# 750
  }
  %% Optional fields:
  %% ================
  %% Tag# 263: SubscriptionRequestType
  %% Tag# 748: TotNumTradeReports
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag# 442: MultiLegReportingType
  %% Tag# 725: ResponseTransportType
  %% Tag# 726: ResponseDestination
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AR"
-record('TradeCaptureReportAck', {
  fields = #{
      'TradeReportID'                        => undefined %% Tag# 571
    , 'ExecType'                             => undefined %% Tag# 150
  }
  %% Optional fields:
  %% ================
  %% Tag# 487: TradeReportTransType
  %% Tag# 856: TradeReportType
  %% Tag# 828: TrdType
  %% Tag# 829: TrdSubType
  %% Tag# 855: SecondaryTrdType
  %% Tag# 830: TransferReason
  %% Tag# 572: TradeReportRefID
  %% Tag# 881: SecondaryTradeReportRefID
  %% Tag# 939: TrdRptStatus
  %% Tag# 751: TradeReportRejectReason
  %% Tag# 818: SecondaryTradeReportID
  %% Tag# 263: SubscriptionRequestType
  %% Tag# 820: TradeLinkID
  %% Tag# 880: TrdMatchID
  %% Tag#  17: ExecID
  %% Tag# 527: SecondaryExecID
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag#  60: TransactTime
  %% Tag# 768: NoTrdRegTimestamps
  %% Tag# 725: ResponseTransportType
  %% Tag# 726: ResponseDestination
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 555: NoLegs
  %% Tag# 635: ClearingFeeIndicator
  %% Tag# 528: OrderCapacity
  %% Tag# 529: OrderRestrictions
  %% Tag# 582: CustOrderCapacity
  %% Tag#   1: Account
  %% Tag# 660: AcctIDSource
  %% Tag# 581: AccountType
  %% Tag#  77: PositionEffect
  %% Tag# 591: PreallocMethod
  %% Tag#  78: NoAllocs
}).

%% Message type: "AS"
-record('AllocationReport', {
  fields = #{
      'AllocReportID'                        => undefined %% Tag# 755
    , 'AllocTransType'                       => undefined %% Tag#  71
    , 'AllocReportType'                      => undefined %% Tag# 794
    , 'AllocStatus'                          => undefined %% Tag#  87
    , 'AllocNoOrdersType'                    => undefined %% Tag# 857
    , 'Side'                                 => undefined %% Tag#  54
    , 'Quantity'                             => undefined %% Tag#  53
    , 'AvgPx'                                => undefined %% Tag#   6
    , 'TradeDate'                            => undefined %% Tag#  75
  }
  %% Optional fields:
  %% ================
  %% Tag#  70: AllocID
  %% Tag# 795: AllocReportRefID
  %% Tag# 796: AllocCancReplaceReason
  %% Tag# 793: SecondaryAllocID
  %% Tag#  88: AllocRejCode
  %% Tag#  72: RefAllocID
  %% Tag# 808: AllocIntermedReqType
  %% Tag# 196: AllocLinkID
  %% Tag# 197: AllocLinkType
  %% Tag# 466: BookingRefID
  %% Tag#  73: NoOrders
  %% Tag# 124: NoExecs
  %% Tag# 570: PreviouslyReported
  %% Tag# 700: ReversalIndicator
  %% Tag# 574: MatchType
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 668: DeliveryForm
  %% Tag# 869: PctAtRisk
  %% Tag# 870: NoInstrAttrib
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag# 711: NoUnderlyings
  %% Tag# 555: NoLegs
  %% Tag# 854: QtyType
  %% Tag#  30: LastMkt
  %% Tag# 229: TradeOriginationDate
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 423: PriceType
  %% Tag# 860: AvgParPx
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag#  15: Currency
  %% Tag#  74: AvgPxPrecision
  %% Tag# 453: NoPartyIDs
  %% Tag#  60: TransactTime
  %% Tag#  63: SettlType
  %% Tag#  64: SettlDate
  %% Tag# 775: BookingType
  %% Tag# 381: GrossTradeAmt
  %% Tag# 238: Concession
  %% Tag# 237: TotalTakedown
  %% Tag# 118: NetMoney
  %% Tag#  77: PositionEffect
  %% Tag# 754: AutoAcceptIndicator
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag# 157: NumDaysInterest
  %% Tag# 158: AccruedInterestRate
  %% Tag# 159: AccruedInterestAmt
  %% Tag# 540: TotalAccruedInterestAmt
  %% Tag# 738: InterestAtMaturity
  %% Tag# 920: EndAccruedInterestAmt
  %% Tag# 921: StartCash
  %% Tag# 922: EndCash
  %% Tag# 650: LegalConfirm
  %% Tag# 232: NoStipulations
  %% Tag# 235: YieldType
  %% Tag# 236: Yield
  %% Tag# 701: YieldCalcDate
  %% Tag# 696: YieldRedemptionDate
  %% Tag# 697: YieldRedemptionPrice
  %% Tag# 698: YieldRedemptionPriceType
  %% Tag# 892: TotNoAllocs
  %% Tag# 893: LastFragment
  %% Tag#  78: NoAllocs
}).

%% Message type: "AT"
-record('AllocationReportAck', {
  fields = #{
      'AllocReportID'                        => undefined %% Tag# 755
    , 'AllocID'                              => undefined %% Tag#  70
    , 'TransactTime'                         => undefined %% Tag#  60
    , 'AllocStatus'                          => undefined %% Tag#  87
  }
  %% Optional fields:
  %% ================
  %% Tag# 453: NoPartyIDs
  %% Tag# 793: SecondaryAllocID
  %% Tag#  75: TradeDate
  %% Tag#  88: AllocRejCode
  %% Tag# 794: AllocReportType
  %% Tag# 808: AllocIntermedReqType
  %% Tag# 573: MatchStatus
  %% Tag# 460: Product
  %% Tag# 167: SecurityType
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
  %% Tag#  78: NoAllocs
}).

%% Message type: "AU"
-record('ConfirmationAck', {
  fields = #{
      'ConfirmID'                            => undefined %% Tag# 664
    , 'TradeDate'                            => undefined %% Tag#  75
    , 'TransactTime'                         => undefined %% Tag#  60
    , 'AffirmStatus'                         => undefined %% Tag# 940
  }
  %% Optional fields:
  %% ================
  %% Tag# 774: ConfirmRejReason
  %% Tag# 573: MatchStatus
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AV"
-record('SettlementInstructionRequest', {
  fields = #{
      'SettlInstReqID'                       => undefined %% Tag# 791
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag# 453: NoPartyIDs
  %% Tag#  79: AllocAccount
  %% Tag# 661: AllocAcctIDSource
  %% Tag#  54: Side
  %% Tag# 460: Product
  %% Tag# 167: SecurityType
  %% Tag# 461: CFICode
  %% Tag# 168: EffectiveTime
  %% Tag# 126: ExpireTime
  %% Tag# 779: LastUpdateTime
  %% Tag# 169: StandInstDbType
  %% Tag# 170: StandInstDbName
  %% Tag# 171: StandInstDbID
}).

%% Message type: "AW"
-record('AssignmentReport', {
  fields = #{
      'AsgnRptID'                            => undefined %% Tag# 833
    , 'AccountType'                          => undefined %% Tag# 581
    , 'SettlPrice'                           => undefined %% Tag# 730
    , 'SettlPriceType'                       => undefined %% Tag# 731
    , 'UnderlyingSettlPrice'                 => undefined %% Tag# 732
    , 'AssignmentMethod'                     => undefined %% Tag# 744
    , 'OpenInterest'                         => undefined %% Tag# 746
    , 'ExerciseMethod'                       => undefined %% Tag# 747
    , 'SettlSessID'                          => undefined %% Tag# 716
    , 'SettlSessSubID'                       => undefined %% Tag# 717
    , 'ClearingBusinessDate'                 => undefined %% Tag# 715
  }
  %% Optional fields:
  %% ================
  %% Tag# 832: TotNumAssignmentReports
  %% Tag# 912: LastRptRequested
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 702: NoPositions
  %% Tag# 753: NoPosAmt
  %% Tag# 834: ThresholdAmount
  %% Tag# 432: ExpireDate
  %% Tag# 745: AssignmentUnit
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AX"
-record('CollateralRequest', {
  fields = #{
      'CollReqID'                            => undefined %% Tag# 894
    , 'CollAsgnReason'                       => undefined %% Tag# 895
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag# 126: ExpireTime
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 581: AccountType
  %% Tag#  11: ClOrdID
  %% Tag#  37: OrderID
  %% Tag# 198: SecondaryOrderID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 124: NoExecs
  %% Tag# 897: NoTrades
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag#  64: SettlDate
  %% Tag#  53: Quantity
  %% Tag# 854: QtyType
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 899: MarginExcess
  %% Tag# 900: TotalNetValue
  %% Tag# 901: CashOutstanding
  %% Tag# 768: NoTrdRegTimestamps
  %% Tag#  54: Side
  %% Tag# 136: NoMiscFees
  %% Tag#  44: Price
  %% Tag# 423: PriceType
  %% Tag# 159: AccruedInterestAmt
  %% Tag# 920: EndAccruedInterestAmt
  %% Tag# 921: StartCash
  %% Tag# 922: EndCash
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 232: NoStipulations
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 716: SettlSessID
  %% Tag# 717: SettlSessSubID
  %% Tag# 715: ClearingBusinessDate
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AY"
-record('CollateralAssignment', {
  fields = #{
      'CollAsgnID'                           => undefined %% Tag# 902
    , 'CollAsgnReason'                       => undefined %% Tag# 895
    , 'CollAsgnTransType'                    => undefined %% Tag# 903
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag# 894: CollReqID
  %% Tag# 907: CollAsgnRefID
  %% Tag# 126: ExpireTime
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 581: AccountType
  %% Tag#  11: ClOrdID
  %% Tag#  37: OrderID
  %% Tag# 198: SecondaryOrderID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 124: NoExecs
  %% Tag# 897: NoTrades
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag#  64: SettlDate
  %% Tag#  53: Quantity
  %% Tag# 854: QtyType
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 899: MarginExcess
  %% Tag# 900: TotalNetValue
  %% Tag# 901: CashOutstanding
  %% Tag# 768: NoTrdRegTimestamps
  %% Tag#  54: Side
  %% Tag# 136: NoMiscFees
  %% Tag#  44: Price
  %% Tag# 423: PriceType
  %% Tag# 159: AccruedInterestAmt
  %% Tag# 920: EndAccruedInterestAmt
  %% Tag# 921: StartCash
  %% Tag# 922: EndCash
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 232: NoStipulations
  %% Tag# 172: SettlDeliveryType
  %% Tag# 169: StandInstDbType
  %% Tag# 170: StandInstDbName
  %% Tag# 171: StandInstDbID
  %% Tag#  85: NoDlvyInst
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 716: SettlSessID
  %% Tag# 717: SettlSessSubID
  %% Tag# 715: ClearingBusinessDate
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "AZ"
-record('CollateralResponse', {
  fields = #{
      'CollRespID'                           => undefined %% Tag# 904
    , 'CollAsgnID'                           => undefined %% Tag# 902
    , 'CollAsgnReason'                       => undefined %% Tag# 895
    , 'CollAsgnRespType'                     => undefined %% Tag# 905
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag# 894: CollReqID
  %% Tag# 903: CollAsgnTransType
  %% Tag# 906: CollAsgnRejectReason
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 581: AccountType
  %% Tag#  11: ClOrdID
  %% Tag#  37: OrderID
  %% Tag# 198: SecondaryOrderID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 124: NoExecs
  %% Tag# 897: NoTrades
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag#  64: SettlDate
  %% Tag#  53: Quantity
  %% Tag# 854: QtyType
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 899: MarginExcess
  %% Tag# 900: TotalNetValue
  %% Tag# 901: CashOutstanding
  %% Tag# 768: NoTrdRegTimestamps
  %% Tag#  54: Side
  %% Tag# 136: NoMiscFees
  %% Tag#  44: Price
  %% Tag# 423: PriceType
  %% Tag# 159: AccruedInterestAmt
  %% Tag# 920: EndAccruedInterestAmt
  %% Tag# 921: StartCash
  %% Tag# 922: EndCash
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 232: NoStipulations
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "BA"
-record('CollateralReport', {
  fields = #{
      'CollRptID'                            => undefined %% Tag# 908
    , 'CollStatus'                           => undefined %% Tag# 910
  }
  %% Optional fields:
  %% ================
  %% Tag# 909: CollInquiryID
  %% Tag# 911: TotNumReports
  %% Tag# 912: LastRptRequested
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 581: AccountType
  %% Tag#  11: ClOrdID
  %% Tag#  37: OrderID
  %% Tag# 198: SecondaryOrderID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 124: NoExecs
  %% Tag# 897: NoTrades
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag#  64: SettlDate
  %% Tag#  53: Quantity
  %% Tag# 854: QtyType
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 899: MarginExcess
  %% Tag# 900: TotalNetValue
  %% Tag# 901: CashOutstanding
  %% Tag# 768: NoTrdRegTimestamps
  %% Tag#  54: Side
  %% Tag# 136: NoMiscFees
  %% Tag#  44: Price
  %% Tag# 423: PriceType
  %% Tag# 159: AccruedInterestAmt
  %% Tag# 920: EndAccruedInterestAmt
  %% Tag# 921: StartCash
  %% Tag# 922: EndCash
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 232: NoStipulations
  %% Tag# 172: SettlDeliveryType
  %% Tag# 169: StandInstDbType
  %% Tag# 170: StandInstDbName
  %% Tag# 171: StandInstDbID
  %% Tag#  85: NoDlvyInst
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 716: SettlSessID
  %% Tag# 717: SettlSessSubID
  %% Tag# 715: ClearingBusinessDate
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "BB"
-record('CollateralInquiry', {
  fields = #{}
  %% Optional fields:
  %% ================
  %% Tag# 909: CollInquiryID
  %% Tag# 938: NoCollInquiryQualifier
  %% Tag# 263: SubscriptionRequestType
  %% Tag# 725: ResponseTransportType
  %% Tag# 726: ResponseDestination
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 581: AccountType
  %% Tag#  11: ClOrdID
  %% Tag#  37: OrderID
  %% Tag# 198: SecondaryOrderID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 124: NoExecs
  %% Tag# 897: NoTrades
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag#  64: SettlDate
  %% Tag#  53: Quantity
  %% Tag# 854: QtyType
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 899: MarginExcess
  %% Tag# 900: TotalNetValue
  %% Tag# 901: CashOutstanding
  %% Tag# 768: NoTrdRegTimestamps
  %% Tag#  54: Side
  %% Tag#  44: Price
  %% Tag# 423: PriceType
  %% Tag# 159: AccruedInterestAmt
  %% Tag# 920: EndAccruedInterestAmt
  %% Tag# 921: StartCash
  %% Tag# 922: EndCash
  %% Tag# 218: Spread
  %% Tag# 220: BenchmarkCurveCurrency
  %% Tag# 221: BenchmarkCurveName
  %% Tag# 222: BenchmarkCurvePoint
  %% Tag# 662: BenchmarkPrice
  %% Tag# 663: BenchmarkPriceType
  %% Tag# 699: BenchmarkSecurityID
  %% Tag# 761: BenchmarkSecurityIDSource
  %% Tag# 232: NoStipulations
  %% Tag# 172: SettlDeliveryType
  %% Tag# 169: StandInstDbType
  %% Tag# 170: StandInstDbName
  %% Tag# 171: StandInstDbID
  %% Tag#  85: NoDlvyInst
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 716: SettlSessID
  %% Tag# 717: SettlSessSubID
  %% Tag# 715: ClearingBusinessDate
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "BC"
-record('NetworkCounterpartySystemStatusRequest', {
  fields = #{
      'NetworkRequestType'                   => undefined %% Tag# 935
    , 'NetworkRequestID'                     => undefined %% Tag# 933
  }
  %% Optional fields:
  %% ================
  %% Tag# 936: NoCompIDs
}).

%% Message type: "BD"
-record('NetworkCounterpartySystemStatusResponse', {
  fields = #{
      'NetworkStatusResponseType'            => undefined %% Tag# 937
    , 'NetworkResponseID'                    => undefined %% Tag# 932
    , 'grpCompIDs'                           => #{}       %% Tag# 936 (GroupLen: NoCompIDs)
  }
  %% Optional fields:
  %% ================
  %% Tag# 933: NetworkRequestID
  %% Tag# 934: LastNetworkResponseID
}).

%% Message type: "BE"
-record('UserRequest', {
  fields = #{
      'UserRequestID'                        => undefined %% Tag# 923
    , 'UserRequestType'                      => undefined %% Tag# 924
    , 'Username'                             => undefined %% Tag# 553
  }
  %% Optional fields:
  %% ================
  %% Tag# 554: Password
  %% Tag# 925: NewPassword
  %% Tag#  95: RawDataLength
  %% Tag#  96: RawData
}).

%% Message type: "BF"
-record('UserResponse', {
  fields = #{
      'UserRequestID'                        => undefined %% Tag# 923
    , 'Username'                             => undefined %% Tag# 553
  }
  %% Optional fields:
  %% ================
  %% Tag# 926: UserStatus
  %% Tag# 927: UserStatusText
}).

%% Message type: "BG"
-record('CollateralInquiryAck', {
  fields = #{
      'CollInquiryID'                        => undefined %% Tag# 909
    , 'CollInquiryStatus'                    => undefined %% Tag# 945
  }
  %% Optional fields:
  %% ================
  %% Tag# 946: CollInquiryResult
  %% Tag# 938: NoCollInquiryQualifier
  %% Tag# 911: TotNumReports
  %% Tag# 453: NoPartyIDs
  %% Tag#   1: Account
  %% Tag# 581: AccountType
  %% Tag#  11: ClOrdID
  %% Tag#  37: OrderID
  %% Tag# 198: SecondaryOrderID
  %% Tag# 526: SecondaryClOrdID
  %% Tag# 124: NoExecs
  %% Tag# 897: NoTrades
  %% Tag#  55: Symbol
  %% Tag#  65: SymbolSfx
  %% Tag#  48: SecurityID
  %% Tag#  22: SecurityIDSource
  %% Tag# 454: NoSecurityAltID
  %% Tag# 460: Product
  %% Tag# 461: CFICode
  %% Tag# 167: SecurityType
  %% Tag# 762: SecuritySubType
  %% Tag# 200: MaturityMonthYear
  %% Tag# 541: MaturityDate
  %% Tag# 201: PutOrCall
  %% Tag# 224: CouponPaymentDate
  %% Tag# 225: IssueDate
  %% Tag# 239: RepoCollateralSecurityType
  %% Tag# 226: RepurchaseTerm
  %% Tag# 227: RepurchaseRate
  %% Tag# 228: Factor
  %% Tag# 255: CreditRating
  %% Tag# 543: InstrRegistry
  %% Tag# 470: CountryOfIssue
  %% Tag# 471: StateOrProvinceOfIssue
  %% Tag# 472: LocaleOfIssue
  %% Tag# 240: RedemptionDate
  %% Tag# 202: StrikePrice
  %% Tag# 947: StrikeCurrency
  %% Tag# 206: OptAttribute
  %% Tag# 231: ContractMultiplier
  %% Tag# 223: CouponRate
  %% Tag# 207: SecurityExchange
  %% Tag# 106: Issuer
  %% Tag# 348: EncodedIssuerLen
  %% Tag# 349: EncodedIssuer
  %% Tag# 107: SecurityDesc
  %% Tag# 350: EncodedSecurityDescLen
  %% Tag# 351: EncodedSecurityDesc
  %% Tag# 691: Pool
  %% Tag# 667: ContractSettlMonth
  %% Tag# 875: CPProgram
  %% Tag# 876: CPRegType
  %% Tag# 864: NoEvents
  %% Tag# 873: DatedDate
  %% Tag# 874: InterestAccrualDate
  %% Tag# 913: AgreementDesc
  %% Tag# 914: AgreementID
  %% Tag# 915: AgreementDate
  %% Tag# 918: AgreementCurrency
  %% Tag# 788: TerminationType
  %% Tag# 916: StartDate
  %% Tag# 917: EndDate
  %% Tag# 919: DeliveryType
  %% Tag# 898: MarginRatio
  %% Tag#  64: SettlDate
  %% Tag#  53: Quantity
  %% Tag# 854: QtyType
  %% Tag#  15: Currency
  %% Tag# 555: NoLegs
  %% Tag# 711: NoUnderlyings
  %% Tag# 336: TradingSessionID
  %% Tag# 625: TradingSessionSubID
  %% Tag# 716: SettlSessID
  %% Tag# 717: SettlSessSubID
  %% Tag# 715: ClearingBusinessDate
  %% Tag# 725: ResponseTransportType
  %% Tag# 726: ResponseDestination
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

%% Message type: "BH"
-record('ConfirmationRequest', {
  fields = #{
      'ConfirmReqID'                         => undefined %% Tag# 859
    , 'ConfirmType'                          => undefined %% Tag# 773
    , 'TransactTime'                         => undefined %% Tag#  60
  }
  %% Optional fields:
  %% ================
  %% Tag#  73: NoOrders
  %% Tag#  70: AllocID
  %% Tag# 793: SecondaryAllocID
  %% Tag# 467: IndividualAllocID
  %% Tag#  79: AllocAccount
  %% Tag# 661: AllocAcctIDSource
  %% Tag# 798: AllocAccountType
  %% Tag#  58: Text
  %% Tag# 354: EncodedTextLen
  %% Tag# 355: EncodedText
}).

