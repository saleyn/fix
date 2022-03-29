%%------------------------------------------------------------------------------
%% Author: Serge Aleynikov <saleyn at gmail dot com>
%%
%% The work is derived from Maxim Lapshin's open source work:
%% https://github.com/maxlapshin/fix under the same open source MIT
%% licensing terms as the original.
%%------------------------------------------------------------------------------
%% *** This file is auto-generated, don't modify by hand!!! ***
%%------------------------------------------------------------------------------

-module(fix_fields).
-export([field/1, field_tag/1]).

-define(SOH, 1).

field(  1) -> {'Account'                             , string, false};
field(  2) -> {'AdvId'                               , string, false};
field(  3) -> {'AdvRefID'                            , string, false};
field(  4) -> {'AdvSide'                             , char  , fun(V) -> decode_fld_val4(V) end};
field(  5) -> {'AdvTransType'                        , string, fun(V) -> decode_fld_val5(V) end};
field(  6) -> {'AvgPx'                               , float , false};
field(  7) -> {'BeginSeqNo'                          , int   , false};
field(  8) -> {'BeginString'                         , string, false};
field(  9) -> {'BodyLength'                          , length, false};
field( 10) -> {'CheckSum'                            , string, false};
field( 11) -> {'ClOrdID'                             , string, false};
field( 12) -> {'Commission'                          , float , false};
field( 13) -> {'CommType'                            , char  , fun(V) -> decode_fld_val13(V) end};
field( 14) -> {'CumQty'                              , float , false};
field( 15) -> {'Currency'                            , string, false};
field( 16) -> {'EndSeqNo'                            , int   , false};
field( 17) -> {'ExecID'                              , string, false};
field( 18) -> {'ExecInst'                            , string, fun(V) -> decode_fld_val18(V) end};
field( 19) -> {'ExecRefID'                           , string, false};
field( 21) -> {'HandlInst'                           , char  , fun(V) -> decode_fld_val21(V) end};
field( 22) -> {'SecurityIDSource'                    , string, fun(V) -> decode_fld_val22(V) end};
field( 23) -> {'IOIID'                               , string, false};
field( 25) -> {'IOIQltyInd'                          , char  , fun(V) -> decode_fld_val25(V) end};
field( 26) -> {'IOIRefID'                            , string, false};
field( 27) -> {'IOIQty'                              , string, fun(V) -> decode_fld_val27(V) end};
field( 28) -> {'IOITransType'                        , char  , fun(V) -> decode_fld_val28(V) end};
field( 29) -> {'LastCapacity'                        , char  , fun(V) -> decode_fld_val29(V) end};
field( 30) -> {'LastMkt'                             , string, false};
field( 31) -> {'LastPx'                              , float , false};
field( 32) -> {'LastQty'                             , float , false};
field( 33) -> {'NoLinesOfText'                       , group , false};
field( 34) -> {'MsgSeqNum'                           , int   , false};
field( 35) -> {'MsgType'                             , string, fun(V) -> decode_fld_val35(V) end};
field( 36) -> {'NewSeqNo'                            , int   , false};
field( 37) -> {'OrderID'                             , string, false};
field( 38) -> {'OrderQty'                            , float , false};
field( 39) -> {'OrdStatus'                           , char  , fun(V) -> decode_fld_val39(V) end};
field( 40) -> {'OrdType'                             , char  , fun(V) -> decode_fld_val40(V) end};
field( 41) -> {'OrigClOrdID'                         , string, false};
field( 42) -> {'OrigTime'                            , datetm, false};
field( 43) -> {'PossDupFlag'                         , bool  , fun(V) -> decode_fld_val43(V) end};
field( 44) -> {'Price'                               , float , false};
field( 45) -> {'RefSeqNum'                           , int   , false};
field( 48) -> {'SecurityID'                          , string, false};
field( 49) -> {'SenderCompID'                        , string, false};
field( 50) -> {'SenderSubID'                         , string, false};
field( 52) -> {'SendingTime'                         , datetm, false};
field( 53) -> {'Quantity'                            , float , false};
field( 54) -> {'Side'                                , char  , fun(V) -> decode_fld_val54(V) end};
field( 55) -> {'Symbol'                              , string, false};
field( 56) -> {'TargetCompID'                        , string, false};
field( 57) -> {'TargetSubID'                         , string, false};
field( 58) -> {'Text'                                , string, false};
field( 59) -> {'TimeInForce'                         , char  , fun(V) -> decode_fld_val59(V) end};
field( 60) -> {'TransactTime'                        , datetm, false};
field( 61) -> {'Urgency'                             , char  , fun(V) -> decode_fld_val61(V) end};
field( 62) -> {'ValidUntilTime'                      , datetm, false};
field( 63) -> {'SettlType'                           , char  , fun(V) -> decode_fld_val63(V) end};
field( 64) -> {'SettlDate'                           , string, false};
field( 65) -> {'SymbolSfx'                           , string, false};
field( 66) -> {'ListID'                              , string, false};
field( 67) -> {'ListSeqNo'                           , int   , false};
field( 68) -> {'TotNoOrders'                         , int   , false};
field( 69) -> {'ListExecInst'                        , string, false};
field( 70) -> {'AllocID'                             , string, false};
field( 71) -> {'AllocTransType'                      , char  , fun(V) -> decode_fld_val71(V) end};
field( 72) -> {'RefAllocID'                          , string, false};
field( 73) -> {'NoOrders'                            , group , false};
field( 74) -> {'AvgPxPrecision'                      , int   , false};
field( 75) -> {'TradeDate'                           , string, false};
field( 77) -> {'PositionEffect'                      , char  , fun(V) -> decode_fld_val77(V) end};
field( 78) -> {'NoAllocs'                            , group , false};
field( 79) -> {'AllocAccount'                        , string, false};
field( 80) -> {'AllocQty'                            , float , false};
field( 81) -> {'ProcessCode'                         , char  , fun(V) -> decode_fld_val81(V) end};
field( 82) -> {'NoRpts'                              , int   , false};
field( 83) -> {'RptSeq'                              , int   , false};
field( 84) -> {'CxlQty'                              , float , false};
field( 85) -> {'NoDlvyInst'                          , group , false};
field( 87) -> {'AllocStatus'                         , int   , fun(V) -> decode_fld_val87(V) end};
field( 88) -> {'AllocRejCode'                        , int   , fun(V) -> decode_fld_val88(V) end};
field( 89) -> {'Signature'                           , binary, false};
field( 90) -> {'SecureDataLen'                       , length, false};
field( 91) -> {'SecureData'                          , binary, false};
field( 93) -> {'SignatureLength'                     , length, false};
field( 94) -> {'EmailType'                           , char  , fun(V) -> decode_fld_val94(V) end};
field( 95) -> {'RawDataLength'                       , length, false};
field( 96) -> {'RawData'                             , binary, false};
field( 97) -> {'PossResend'                          , bool  , fun(V) -> decode_fld_val97(V) end};
field( 98) -> {'EncryptMethod'                       , int   , fun(V) -> decode_fld_val98(V) end};
field( 99) -> {'StopPx'                              , float , false};
field(100) -> {'ExDestination'                       , string, false};
field(102) -> {'CxlRejReason'                        , int   , fun(V) -> decode_fld_val102(V) end};
field(103) -> {'OrdRejReason'                        , int   , fun(V) -> decode_fld_val103(V) end};
field(104) -> {'IOIQualifier'                        , char  , fun(V) -> decode_fld_val104(V) end};
field(106) -> {'Issuer'                              , string, false};
field(107) -> {'SecurityDesc'                        , string, false};
field(108) -> {'HeartBtInt'                          , int   , false};
field(110) -> {'MinQty'                              , float , false};
field(111) -> {'MaxFloor'                            , float , false};
field(112) -> {'TestReqID'                           , string, false};
field(113) -> {'ReportToExch'                        , bool  , fun(V) -> decode_fld_val113(V) end};
field(114) -> {'LocateReqd'                          , bool  , fun(V) -> decode_fld_val114(V) end};
field(115) -> {'OnBehalfOfCompID'                    , string, false};
field(116) -> {'OnBehalfOfSubID'                     , string, false};
field(117) -> {'QuoteID'                             , string, false};
field(118) -> {'NetMoney'                            , float , false};
field(119) -> {'SettlCurrAmt'                        , float , false};
field(120) -> {'SettlCurrency'                       , string, false};
field(121) -> {'ForexReq'                            , bool  , fun(V) -> decode_fld_val121(V) end};
field(122) -> {'OrigSendingTime'                     , datetm, false};
field(123) -> {'GapFillFlag'                         , bool  , fun(V) -> decode_fld_val123(V) end};
field(124) -> {'NoExecs'                             , group , false};
field(126) -> {'ExpireTime'                          , datetm, false};
field(127) -> {'DKReason'                            , char  , fun(V) -> decode_fld_val127(V) end};
field(128) -> {'DeliverToCompID'                     , string, false};
field(129) -> {'DeliverToSubID'                      , string, false};
field(130) -> {'IOINaturalFlag'                      , bool  , fun(V) -> decode_fld_val130(V) end};
field(131) -> {'QuoteReqID'                          , string, false};
field(132) -> {'BidPx'                               , float , false};
field(133) -> {'OfferPx'                             , float , false};
field(134) -> {'BidSize'                             , float , false};
field(135) -> {'OfferSize'                           , float , false};
field(136) -> {'NoMiscFees'                          , group , false};
field(137) -> {'MiscFeeAmt'                          , float , false};
field(138) -> {'MiscFeeCurr'                         , string, false};
field(139) -> {'MiscFeeType'                         , char  , fun(V) -> decode_fld_val139(V) end};
field(140) -> {'PrevClosePx'                         , float , false};
field(141) -> {'ResetSeqNumFlag'                     , bool  , fun(V) -> decode_fld_val141(V) end};
field(142) -> {'SenderLocationID'                    , string, false};
field(143) -> {'TargetLocationID'                    , string, false};
field(144) -> {'OnBehalfOfLocationID'                , string, false};
field(145) -> {'DeliverToLocationID'                 , string, false};
field(146) -> {'NoRelatedSym'                        , group , false};
field(147) -> {'Subject'                             , string, false};
field(148) -> {'Headline'                            , string, false};
field(149) -> {'URLLink'                             , string, false};
field(150) -> {'ExecType'                            , char  , fun(V) -> decode_fld_val150(V) end};
field(151) -> {'LeavesQty'                           , float , false};
field(152) -> {'CashOrderQty'                        , float , false};
field(153) -> {'AllocAvgPx'                          , float , false};
field(154) -> {'AllocNetMoney'                       , float , false};
field(155) -> {'SettlCurrFxRate'                     , float , false};
field(156) -> {'SettlCurrFxRateCalc'                 , char  , fun(V) -> decode_fld_val156(V) end};
field(157) -> {'NumDaysInterest'                     , int   , false};
field(158) -> {'AccruedInterestRate'                 , float , false};
field(159) -> {'AccruedInterestAmt'                  , float , false};
field(160) -> {'SettlInstMode'                       , char  , fun(V) -> decode_fld_val160(V) end};
field(161) -> {'AllocText'                           , string, false};
field(162) -> {'SettlInstID'                         , string, false};
field(163) -> {'SettlInstTransType'                  , char  , fun(V) -> decode_fld_val163(V) end};
field(164) -> {'EmailThreadID'                       , string, false};
field(165) -> {'SettlInstSource'                     , char  , fun(V) -> decode_fld_val165(V) end};
field(167) -> {'SecurityType'                        , string, fun(V) -> decode_fld_val167(V) end};
field(168) -> {'EffectiveTime'                       , datetm, false};
field(169) -> {'StandInstDbType'                     , int   , fun(V) -> decode_fld_val169(V) end};
field(170) -> {'StandInstDbName'                     , string, false};
field(171) -> {'StandInstDbID'                       , string, false};
field(172) -> {'SettlDeliveryType'                   , int   , fun(V) -> decode_fld_val172(V) end};
field(188) -> {'BidSpotRate'                         , float , false};
field(189) -> {'BidForwardPoints'                    , float , false};
field(190) -> {'OfferSpotRate'                       , float , false};
field(191) -> {'OfferForwardPoints'                  , float , false};
field(192) -> {'OrderQty2'                           , float , false};
field(193) -> {'SettlDate2'                          , string, false};
field(194) -> {'LastSpotRate'                        , float , false};
field(195) -> {'LastForwardPoints'                   , float , false};
field(196) -> {'AllocLinkID'                         , string, false};
field(197) -> {'AllocLinkType'                       , int   , fun(V) -> decode_fld_val197(V) end};
field(198) -> {'SecondaryOrderID'                    , string, false};
field(199) -> {'NoIOIQualifiers'                     , group , false};
field(200) -> {'MaturityMonthYear'                   , string, false};
field(201) -> {'PutOrCall'                           , int   , fun(V) -> decode_fld_val201(V) end};
field(202) -> {'StrikePrice'                         , float , false};
field(203) -> {'CoveredOrUncovered'                  , int   , fun(V) -> decode_fld_val203(V) end};
field(206) -> {'OptAttribute'                        , char  , false};
field(207) -> {'SecurityExchange'                    , string, false};
field(208) -> {'NotifyBrokerOfCredit'                , bool  , fun(V) -> decode_fld_val208(V) end};
field(209) -> {'AllocHandlInst'                      , int   , fun(V) -> decode_fld_val209(V) end};
field(210) -> {'MaxShow'                             , float , false};
field(211) -> {'PegOffsetValue'                      , float , false};
field(212) -> {'XmlDataLen'                          , length, false};
field(213) -> {'XmlData'                             , binary, false};
field(214) -> {'SettlInstRefID'                      , string, false};
field(215) -> {'NoRoutingIDs'                        , group , false};
field(216) -> {'RoutingType'                         , int   , fun(V) -> decode_fld_val216(V) end};
field(217) -> {'RoutingID'                           , string, false};
field(218) -> {'Spread'                              , float , false};
field(220) -> {'BenchmarkCurveCurrency'              , string, false};
field(221) -> {'BenchmarkCurveName'                  , string, false};
field(222) -> {'BenchmarkCurvePoint'                 , string, false};
field(223) -> {'CouponRate'                          , float , false};
field(224) -> {'CouponPaymentDate'                   , string, false};
field(225) -> {'IssueDate'                           , string, false};
field(226) -> {'RepurchaseTerm'                      , int   , false};
field(227) -> {'RepurchaseRate'                      , float , false};
field(228) -> {'Factor'                              , float , false};
field(229) -> {'TradeOriginationDate'                , string, false};
field(230) -> {'ExDate'                              , string, false};
field(231) -> {'ContractMultiplier'                  , float , false};
field(232) -> {'NoStipulations'                      , group , false};
field(233) -> {'StipulationType'                     , string, fun(V) -> decode_fld_val233(V) end};
field(234) -> {'StipulationValue'                    , string, false};
field(235) -> {'YieldType'                           , string, fun(V) -> decode_fld_val235(V) end};
field(236) -> {'Yield'                               , float , false};
field(237) -> {'TotalTakedown'                       , float , false};
field(238) -> {'Concession'                          , float , false};
field(239) -> {'RepoCollateralSecurityType'          , string, false};
field(240) -> {'RedemptionDate'                      , string, false};
field(241) -> {'UnderlyingCouponPaymentDate'         , string, false};
field(242) -> {'UnderlyingIssueDate'                 , string, false};
field(243) -> {'UnderlyingRepoCollateralSecurityType', string, false};
field(244) -> {'UnderlyingRepurchaseTerm'            , int   , false};
field(245) -> {'UnderlyingRepurchaseRate'            , float , false};
field(246) -> {'UnderlyingFactor'                    , float , false};
field(247) -> {'UnderlyingRedemptionDate'            , string, false};
field(248) -> {'LegCouponPaymentDate'                , string, false};
field(249) -> {'LegIssueDate'                        , string, false};
field(250) -> {'LegRepoCollateralSecurityType'       , string, false};
field(251) -> {'LegRepurchaseTerm'                   , int   , false};
field(252) -> {'LegRepurchaseRate'                   , float , false};
field(253) -> {'LegFactor'                           , float , false};
field(254) -> {'LegRedemptionDate'                   , string, false};
field(255) -> {'CreditRating'                        , string, false};
field(256) -> {'UnderlyingCreditRating'              , string, false};
field(257) -> {'LegCreditRating'                     , string, false};
field(258) -> {'TradedFlatSwitch'                    , bool  , fun(V) -> decode_fld_val258(V) end};
field(259) -> {'BasisFeatureDate'                    , string, false};
field(260) -> {'BasisFeaturePrice'                   , float , false};
field(262) -> {'MDReqID'                             , string, false};
field(263) -> {'SubscriptionRequestType'             , char  , fun(V) -> decode_fld_val263(V) end};
field(264) -> {'MarketDepth'                         , int   , false};
field(265) -> {'MDUpdateType'                        , int   , fun(V) -> decode_fld_val265(V) end};
field(266) -> {'AggregatedBook'                      , bool  , fun(V) -> decode_fld_val266(V) end};
field(267) -> {'NoMDEntryTypes'                      , group , false};
field(268) -> {'NoMDEntries'                         , group , false};
field(269) -> {'MDEntryType'                         , char  , fun(V) -> decode_fld_val269(V) end};
field(270) -> {'MDEntryPx'                           , float , false};
field(271) -> {'MDEntrySize'                         , float , false};
field(272) -> {'MDEntryDate'                         , datetm, false};
field(273) -> {'MDEntryTime'                         , datetm, false};
field(274) -> {'TickDirection'                       , char  , fun(V) -> decode_fld_val274(V) end};
field(275) -> {'MDMkt'                               , string, false};
field(276) -> {'QuoteCondition'                      , string, fun(V) -> decode_fld_val276(V) end};
field(277) -> {'TradeCondition'                      , string, fun(V) -> decode_fld_val277(V) end};
field(278) -> {'MDEntryID'                           , string, false};
field(279) -> {'MDUpdateAction'                      , char  , fun(V) -> decode_fld_val279(V) end};
field(280) -> {'MDEntryRefID'                        , string, false};
field(281) -> {'MDReqRejReason'                      , char  , fun(V) -> decode_fld_val281(V) end};
field(282) -> {'MDEntryOriginator'                   , string, false};
field(283) -> {'LocationID'                          , string, false};
field(284) -> {'DeskID'                              , string, false};
field(285) -> {'DeleteReason'                        , char  , fun(V) -> decode_fld_val285(V) end};
field(286) -> {'OpenCloseSettlFlag'                  , string, fun(V) -> decode_fld_val286(V) end};
field(287) -> {'SellerDays'                          , int   , false};
field(288) -> {'MDEntryBuyer'                        , string, false};
field(289) -> {'MDEntrySeller'                       , string, false};
field(290) -> {'MDEntryPositionNo'                   , int   , false};
field(291) -> {'FinancialStatus'                     , string, fun(V) -> decode_fld_val291(V) end};
field(292) -> {'CorporateAction'                     , string, fun(V) -> decode_fld_val292(V) end};
field(293) -> {'DefBidSize'                          , float , false};
field(294) -> {'DefOfferSize'                        , float , false};
field(295) -> {'NoQuoteEntries'                      , group , false};
field(296) -> {'NoQuoteSets'                         , group , false};
field(297) -> {'QuoteStatus'                         , int   , fun(V) -> decode_fld_val297(V) end};
field(298) -> {'QuoteCancelType'                     , int   , fun(V) -> decode_fld_val298(V) end};
field(299) -> {'QuoteEntryID'                        , string, false};
field(300) -> {'QuoteRejectReason'                   , int   , fun(V) -> decode_fld_val300(V) end};
field(301) -> {'QuoteResponseLevel'                  , int   , fun(V) -> decode_fld_val301(V) end};
field(302) -> {'QuoteSetID'                          , string, false};
field(303) -> {'QuoteRequestType'                    , int   , fun(V) -> decode_fld_val303(V) end};
field(304) -> {'TotNoQuoteEntries'                   , int   , false};
field(305) -> {'UnderlyingSecurityIDSource'          , string, false};
field(306) -> {'UnderlyingIssuer'                    , string, false};
field(307) -> {'UnderlyingSecurityDesc'              , string, false};
field(308) -> {'UnderlyingSecurityExchange'          , string, false};
field(309) -> {'UnderlyingSecurityID'                , string, false};
field(310) -> {'UnderlyingSecurityType'              , string, false};
field(311) -> {'UnderlyingSymbol'                    , string, false};
field(312) -> {'UnderlyingSymbolSfx'                 , string, false};
field(313) -> {'UnderlyingMaturityMonthYear'         , string, false};
field(315) -> {'UnderlyingPutOrCall'                 , int   , false};
field(316) -> {'UnderlyingStrikePrice'               , float , false};
field(317) -> {'UnderlyingOptAttribute'              , char  , false};
field(318) -> {'UnderlyingCurrency'                  , string, false};
field(320) -> {'SecurityReqID'                       , string, false};
field(321) -> {'SecurityRequestType'                 , int   , fun(V) -> decode_fld_val321(V) end};
field(322) -> {'SecurityResponseID'                  , string, false};
field(323) -> {'SecurityResponseType'                , int   , fun(V) -> decode_fld_val323(V) end};
field(324) -> {'SecurityStatusReqID'                 , string, false};
field(325) -> {'UnsolicitedIndicator'                , bool  , fun(V) -> decode_fld_val325(V) end};
field(326) -> {'SecurityTradingStatus'               , int   , fun(V) -> decode_fld_val326(V) end};
field(327) -> {'HaltReasonChar'                      , char  , fun(V) -> decode_fld_val327(V) end};
field(328) -> {'InViewOfCommon'                      , bool  , fun(V) -> decode_fld_val328(V) end};
field(329) -> {'DueToRelated'                        , bool  , fun(V) -> decode_fld_val329(V) end};
field(330) -> {'BuyVolume'                           , float , false};
field(331) -> {'SellVolume'                          , float , false};
field(332) -> {'HighPx'                              , float , false};
field(333) -> {'LowPx'                               , float , false};
field(334) -> {'Adjustment'                          , int   , fun(V) -> decode_fld_val334(V) end};
field(335) -> {'TradSesReqID'                        , string, false};
field(336) -> {'TradingSessionID'                    , string, false};
field(337) -> {'ContraTrader'                        , string, false};
field(338) -> {'TradSesMethod'                       , int   , fun(V) -> decode_fld_val338(V) end};
field(339) -> {'TradSesMode'                         , int   , fun(V) -> decode_fld_val339(V) end};
field(340) -> {'TradSesStatus'                       , int   , fun(V) -> decode_fld_val340(V) end};
field(341) -> {'TradSesStartTime'                    , datetm, false};
field(342) -> {'TradSesOpenTime'                     , datetm, false};
field(343) -> {'TradSesPreCloseTime'                 , datetm, false};
field(344) -> {'TradSesCloseTime'                    , datetm, false};
field(345) -> {'TradSesEndTime'                      , datetm, false};
field(346) -> {'NumberOfOrders'                      , int   , false};
field(347) -> {'MessageEncoding'                     , string, fun(V) -> decode_fld_val347(V) end};
field(348) -> {'EncodedIssuerLen'                    , length, false};
field(349) -> {'EncodedIssuer'                       , binary, false};
field(350) -> {'EncodedSecurityDescLen'              , length, false};
field(351) -> {'EncodedSecurityDesc'                 , binary, false};
field(352) -> {'EncodedListExecInstLen'              , length, false};
field(353) -> {'EncodedListExecInst'                 , binary, false};
field(354) -> {'EncodedTextLen'                      , length, false};
field(355) -> {'EncodedText'                         , binary, false};
field(356) -> {'EncodedSubjectLen'                   , length, false};
field(357) -> {'EncodedSubject'                      , binary, false};
field(358) -> {'EncodedHeadlineLen'                  , length, false};
field(359) -> {'EncodedHeadline'                     , binary, false};
field(360) -> {'EncodedAllocTextLen'                 , length, false};
field(361) -> {'EncodedAllocText'                    , binary, false};
field(362) -> {'EncodedUnderlyingIssuerLen'          , length, false};
field(363) -> {'EncodedUnderlyingIssuer'             , binary, false};
field(364) -> {'EncodedUnderlyingSecurityDescLen'    , length, false};
field(365) -> {'EncodedUnderlyingSecurityDesc'       , binary, false};
field(366) -> {'AllocPrice'                          , float , false};
field(367) -> {'QuoteSetValidUntilTime'              , datetm, false};
field(368) -> {'QuoteEntryRejectReason'              , int   , false};
field(369) -> {'LastMsgSeqNumProcessed'              , int   , false};
field(371) -> {'RefTagID'                            , int   , false};
field(372) -> {'RefMsgType'                          , string, false};
field(373) -> {'SessionRejectReason'                 , int   , fun(V) -> decode_fld_val373(V) end};
field(374) -> {'BidRequestTransType'                 , char  , fun(V) -> decode_fld_val374(V) end};
field(375) -> {'ContraBroker'                        , string, false};
field(376) -> {'ComplianceID'                        , string, false};
field(377) -> {'SolicitedFlag'                       , bool  , fun(V) -> decode_fld_val377(V) end};
field(378) -> {'ExecRestatementReason'               , int   , fun(V) -> decode_fld_val378(V) end};
field(379) -> {'BusinessRejectRefID'                 , string, false};
field(380) -> {'BusinessRejectReason'                , int   , fun(V) -> decode_fld_val380(V) end};
field(381) -> {'GrossTradeAmt'                       , float , false};
field(382) -> {'NoContraBrokers'                     , group , false};
field(383) -> {'MaxMessageSize'                      , length, false};
field(384) -> {'NoMsgTypes'                          , group , false};
field(385) -> {'MsgDirection'                        , char  , fun(V) -> decode_fld_val385(V) end};
field(386) -> {'NoTradingSessions'                   , group , false};
field(387) -> {'TotalVolumeTraded'                   , float , false};
field(388) -> {'DiscretionInst'                      , char  , fun(V) -> decode_fld_val388(V) end};
field(389) -> {'DiscretionOffsetValue'               , float , false};
field(390) -> {'BidID'                               , string, false};
field(391) -> {'ClientBidID'                         , string, false};
field(392) -> {'ListName'                            , string, false};
field(393) -> {'TotNoRelatedSym'                     , int   , false};
field(394) -> {'BidType'                             , int   , fun(V) -> decode_fld_val394(V) end};
field(395) -> {'NumTickets'                          , int   , false};
field(396) -> {'SideValue1'                          , float , false};
field(397) -> {'SideValue2'                          , float , false};
field(398) -> {'NoBidDescriptors'                    , group , false};
field(399) -> {'BidDescriptorType'                   , int   , fun(V) -> decode_fld_val399(V) end};
field(400) -> {'BidDescriptor'                       , string, false};
field(401) -> {'SideValueInd'                        , int   , fun(V) -> decode_fld_val401(V) end};
field(402) -> {'LiquidityPctLow'                     , float , false};
field(403) -> {'LiquidityPctHigh'                    , float , false};
field(404) -> {'LiquidityValue'                      , float , false};
field(405) -> {'EFPTrackingError'                    , float , false};
field(406) -> {'FairValue'                           , float , false};
field(407) -> {'OutsideIndexPct'                     , float , false};
field(408) -> {'ValueOfFutures'                      , float , false};
field(409) -> {'LiquidityIndType'                    , int   , fun(V) -> decode_fld_val409(V) end};
field(410) -> {'WtAverageLiquidity'                  , float , false};
field(411) -> {'ExchangeForPhysical'                 , bool  , fun(V) -> decode_fld_val411(V) end};
field(412) -> {'OutMainCntryUIndex'                  , float , false};
field(413) -> {'CrossPercent'                        , float , false};
field(414) -> {'ProgRptReqs'                         , int   , fun(V) -> decode_fld_val414(V) end};
field(415) -> {'ProgPeriodInterval'                  , int   , false};
field(416) -> {'IncTaxInd'                           , int   , fun(V) -> decode_fld_val416(V) end};
field(417) -> {'NumBidders'                          , int   , false};
field(418) -> {'BidTradeType'                        , char  , fun(V) -> decode_fld_val418(V) end};
field(419) -> {'BasisPxType'                         , char  , fun(V) -> decode_fld_val419(V) end};
field(420) -> {'NoBidComponents'                     , group , false};
field(421) -> {'Country'                             , string, false};
field(422) -> {'TotNoStrikes'                        , int   , false};
field(423) -> {'PriceType'                           , int   , fun(V) -> decode_fld_val423(V) end};
field(424) -> {'DayOrderQty'                         , float , false};
field(425) -> {'DayCumQty'                           , float , false};
field(426) -> {'DayAvgPx'                            , float , false};
field(427) -> {'GTBookingInst'                       , int   , fun(V) -> decode_fld_val427(V) end};
field(428) -> {'NoStrikes'                           , group , false};
field(429) -> {'ListStatusType'                      , int   , fun(V) -> decode_fld_val429(V) end};
field(430) -> {'NetGrossInd'                         , int   , fun(V) -> decode_fld_val430(V) end};
field(431) -> {'ListOrderStatus'                     , int   , fun(V) -> decode_fld_val431(V) end};
field(432) -> {'ExpireDate'                          , string, false};
field(433) -> {'ListExecInstType'                    , char  , fun(V) -> decode_fld_val433(V) end};
field(434) -> {'CxlRejResponseTo'                    , char  , fun(V) -> decode_fld_val434(V) end};
field(435) -> {'UnderlyingCouponRate'                , float , false};
field(436) -> {'UnderlyingContractMultiplier'        , float , false};
field(437) -> {'ContraTradeQty'                      , float , false};
field(438) -> {'ContraTradeTime'                     , datetm, false};
field(441) -> {'LiquidityNumSecurities'              , int   , false};
field(442) -> {'MultiLegReportingType'               , char  , fun(V) -> decode_fld_val442(V) end};
field(443) -> {'StrikeTime'                          , datetm, false};
field(444) -> {'ListStatusText'                      , string, false};
field(445) -> {'EncodedListStatusTextLen'            , length, false};
field(446) -> {'EncodedListStatusText'               , binary, false};
field(447) -> {'PartyIDSource'                       , char  , fun(V) -> decode_fld_val447(V) end};
field(448) -> {'PartyID'                             , string, false};
field(451) -> {'NetChgPrevDay'                       , float , false};
field(452) -> {'PartyRole'                           , int   , fun(V) -> decode_fld_val452(V) end};
field(453) -> {'NoPartyIDs'                          , group , false};
field(454) -> {'NoSecurityAltID'                     , group , false};
field(455) -> {'SecurityAltID'                       , string, false};
field(456) -> {'SecurityAltIDSource'                 , string, false};
field(457) -> {'NoUnderlyingSecurityAltID'           , group , false};
field(458) -> {'UnderlyingSecurityAltID'             , string, false};
field(459) -> {'UnderlyingSecurityAltIDSource'       , string, false};
field(460) -> {'Product'                             , int   , fun(V) -> decode_fld_val460(V) end};
field(461) -> {'CFICode'                             , string, false};
field(462) -> {'UnderlyingProduct'                   , int   , false};
field(463) -> {'UnderlyingCFICode'                   , string, false};
field(464) -> {'TestMessageIndicator'                , bool  , fun(V) -> decode_fld_val464(V) end};
field(466) -> {'BookingRefID'                        , string, false};
field(467) -> {'IndividualAllocID'                   , string, false};
field(468) -> {'RoundingDirection'                   , char  , fun(V) -> decode_fld_val468(V) end};
field(469) -> {'RoundingModulus'                     , float , false};
field(470) -> {'CountryOfIssue'                      , string, false};
field(471) -> {'StateOrProvinceOfIssue'              , string, false};
field(472) -> {'LocaleOfIssue'                       , string, false};
field(473) -> {'NoRegistDtls'                        , group , false};
field(474) -> {'MailingDtls'                         , string, false};
field(475) -> {'InvestorCountryOfResidence'          , string, false};
field(476) -> {'PaymentRef'                          , string, false};
field(477) -> {'DistribPaymentMethod'                , int   , fun(V) -> decode_fld_val477(V) end};
field(478) -> {'CashDistribCurr'                     , string, false};
field(479) -> {'CommCurrency'                        , string, false};
field(480) -> {'CancellationRights'                  , char  , fun(V) -> decode_fld_val480(V) end};
field(481) -> {'MoneyLaunderingStatus'               , char  , fun(V) -> decode_fld_val481(V) end};
field(482) -> {'MailingInst'                         , string, false};
field(483) -> {'TransBkdTime'                        , datetm, false};
field(484) -> {'ExecPriceType'                       , char  , fun(V) -> decode_fld_val484(V) end};
field(485) -> {'ExecPriceAdjustment'                 , float , false};
field(486) -> {'DateOfBirth'                         , string, false};
field(487) -> {'TradeReportTransType'                , int   , false};
field(488) -> {'CardHolderName'                      , string, false};
field(489) -> {'CardNumber'                          , string, false};
field(490) -> {'CardExpDate'                         , string, false};
field(491) -> {'CardIssNum'                          , string, false};
field(492) -> {'PaymentMethod'                       , int   , fun(V) -> decode_fld_val492(V) end};
field(493) -> {'RegistAcctType'                      , string, false};
field(494) -> {'Designation'                         , string, false};
field(495) -> {'TaxAdvantageType'                    , int   , fun(V) -> decode_fld_val495(V) end};
field(496) -> {'RegistRejReasonText'                 , string, false};
field(497) -> {'FundRenewWaiv'                       , char  , fun(V) -> decode_fld_val497(V) end};
field(498) -> {'CashDistribAgentName'                , string, false};
field(499) -> {'CashDistribAgentCode'                , string, false};
field(500) -> {'CashDistribAgentAcctNumber'          , string, false};
field(501) -> {'CashDistribPayRef'                   , string, false};
field(502) -> {'CashDistribAgentAcctName'            , string, false};
field(503) -> {'CardStartDate'                       , string, false};
field(504) -> {'PaymentDate'                         , string, false};
field(505) -> {'PaymentRemitterID'                   , string, false};
field(506) -> {'RegistStatus'                        , char  , fun(V) -> decode_fld_val506(V) end};
field(507) -> {'RegistRejReasonCode'                 , int   , fun(V) -> decode_fld_val507(V) end};
field(508) -> {'RegistRefID'                         , string, false};
field(509) -> {'RegistDtls'                          , string, false};
field(510) -> {'NoDistribInsts'                      , group , false};
field(511) -> {'RegistEmail'                         , string, false};
field(512) -> {'DistribPercentage'                   , float , false};
field(513) -> {'RegistID'                            , string, false};
field(514) -> {'RegistTransType'                     , char  , fun(V) -> decode_fld_val514(V) end};
field(515) -> {'ExecValuationPoint'                  , datetm, false};
field(516) -> {'OrderPercent'                        , float , false};
field(517) -> {'OwnershipType'                       , char  , fun(V) -> decode_fld_val517(V) end};
field(518) -> {'NoContAmts'                          , group , false};
field(519) -> {'ContAmtType'                         , int   , fun(V) -> decode_fld_val519(V) end};
field(520) -> {'ContAmtValue'                        , float , false};
field(521) -> {'ContAmtCurr'                         , string, false};
field(522) -> {'OwnerType'                           , int   , fun(V) -> decode_fld_val522(V) end};
field(523) -> {'PartySubID'                          , string, false};
field(524) -> {'NestedPartyID'                       , string, false};
field(525) -> {'NestedPartyIDSource'                 , char  , false};
field(526) -> {'SecondaryClOrdID'                    , string, false};
field(527) -> {'SecondaryExecID'                     , string, false};
field(528) -> {'OrderCapacity'                       , char  , fun(V) -> decode_fld_val528(V) end};
field(529) -> {'OrderRestrictions'                   , string, fun(V) -> decode_fld_val529(V) end};
field(530) -> {'MassCancelRequestType'               , char  , fun(V) -> decode_fld_val530(V) end};
field(531) -> {'MassCancelResponse'                  , char  , fun(V) -> decode_fld_val531(V) end};
field(532) -> {'MassCancelRejectReason'              , char  , fun(V) -> decode_fld_val532(V) end};
field(533) -> {'TotalAffectedOrders'                 , int   , false};
field(534) -> {'NoAffectedOrders'                    , group , false};
field(535) -> {'AffectedOrderID'                     , string, false};
field(536) -> {'AffectedSecondaryOrderID'            , string, false};
field(537) -> {'QuoteType'                           , int   , fun(V) -> decode_fld_val537(V) end};
field(538) -> {'NestedPartyRole'                     , int   , false};
field(539) -> {'NoNestedPartyIDs'                    , group , false};
field(540) -> {'TotalAccruedInterestAmt'             , float , false};
field(541) -> {'MaturityDate'                        , string, false};
field(542) -> {'UnderlyingMaturityDate'              , string, false};
field(543) -> {'InstrRegistry'                       , string, false};
field(544) -> {'CashMargin'                          , char  , fun(V) -> decode_fld_val544(V) end};
field(545) -> {'NestedPartySubID'                    , string, false};
field(546) -> {'Scope'                               , string, fun(V) -> decode_fld_val546(V) end};
field(547) -> {'MDImplicitDelete'                    , bool  , fun(V) -> decode_fld_val547(V) end};
field(548) -> {'CrossID'                             , string, false};
field(549) -> {'CrossType'                           , int   , fun(V) -> decode_fld_val549(V) end};
field(550) -> {'CrossPrioritization'                 , int   , fun(V) -> decode_fld_val550(V) end};
field(551) -> {'OrigCrossID'                         , string, false};
field(552) -> {'NoSides'                             , group , fun(V) -> decode_fld_val552(V) end};
field(553) -> {'Username'                            , string, false};
field(554) -> {'Password'                            , string, false};
field(555) -> {'NoLegs'                              , group , false};
field(556) -> {'LegCurrency'                         , string, false};
field(557) -> {'TotNoSecurityTypes'                  , int   , false};
field(558) -> {'NoSecurityTypes'                     , group , false};
field(559) -> {'SecurityListRequestType'             , int   , fun(V) -> decode_fld_val559(V) end};
field(560) -> {'SecurityRequestResult'               , int   , fun(V) -> decode_fld_val560(V) end};
field(561) -> {'RoundLot'                            , float , false};
field(562) -> {'MinTradeVol'                         , float , false};
field(563) -> {'MultiLegRptTypeReq'                  , int   , fun(V) -> decode_fld_val563(V) end};
field(564) -> {'LegPositionEffect'                   , char  , false};
field(565) -> {'LegCoveredOrUncovered'               , int   , false};
field(566) -> {'LegPrice'                            , float , false};
field(567) -> {'TradSesStatusRejReason'              , int   , fun(V) -> decode_fld_val567(V) end};
field(568) -> {'TradeRequestID'                      , string, false};
field(569) -> {'TradeRequestType'                    , int   , fun(V) -> decode_fld_val569(V) end};
field(570) -> {'PreviouslyReported'                  , bool  , fun(V) -> decode_fld_val570(V) end};
field(571) -> {'TradeReportID'                       , string, false};
field(572) -> {'TradeReportRefID'                    , string, false};
field(573) -> {'MatchStatus'                         , char  , fun(V) -> decode_fld_val573(V) end};
field(574) -> {'MatchType'                           , string, fun(V) -> decode_fld_val574(V) end};
field(575) -> {'OddLot'                              , bool  , fun(V) -> decode_fld_val575(V) end};
field(576) -> {'NoClearingInstructions'              , group , false};
field(577) -> {'ClearingInstruction'                 , int   , fun(V) -> decode_fld_val577(V) end};
field(578) -> {'TradeInputSource'                    , string, false};
field(579) -> {'TradeInputDevice'                    , string, false};
field(580) -> {'NoDates'                             , group , false};
field(581) -> {'AccountType'                         , int   , fun(V) -> decode_fld_val581(V) end};
field(582) -> {'CustOrderCapacity'                   , int   , fun(V) -> decode_fld_val582(V) end};
field(583) -> {'ClOrdLinkID'                         , string, false};
field(584) -> {'MassStatusReqID'                     , string, false};
field(585) -> {'MassStatusReqType'                   , int   , fun(V) -> decode_fld_val585(V) end};
field(586) -> {'OrigOrdModTime'                      , datetm, false};
field(587) -> {'LegSettlType'                        , char  , false};
field(588) -> {'LegSettlDate'                        , string, false};
field(589) -> {'DayBookingInst'                      , char  , fun(V) -> decode_fld_val589(V) end};
field(590) -> {'BookingUnit'                         , char  , fun(V) -> decode_fld_val590(V) end};
field(591) -> {'PreallocMethod'                      , char  , fun(V) -> decode_fld_val591(V) end};
field(592) -> {'UnderlyingCountryOfIssue'            , string, false};
field(593) -> {'UnderlyingStateOrProvinceOfIssue'    , string, false};
field(594) -> {'UnderlyingLocaleOfIssue'             , string, false};
field(595) -> {'UnderlyingInstrRegistry'             , string, false};
field(596) -> {'LegCountryOfIssue'                   , string, false};
field(597) -> {'LegStateOrProvinceOfIssue'           , string, false};
field(598) -> {'LegLocaleOfIssue'                    , string, false};
field(599) -> {'LegInstrRegistry'                    , string, false};
field(600) -> {'LegSymbol'                           , string, false};
field(601) -> {'LegSymbolSfx'                        , string, false};
field(602) -> {'LegSecurityID'                       , string, false};
field(603) -> {'LegSecurityIDSource'                 , string, false};
field(604) -> {'NoLegSecurityAltID'                  , group , false};
field(605) -> {'LegSecurityAltID'                    , string, false};
field(606) -> {'LegSecurityAltIDSource'              , string, false};
field(607) -> {'LegProduct'                          , int   , false};
field(608) -> {'LegCFICode'                          , string, false};
field(609) -> {'LegSecurityType'                     , string, false};
field(610) -> {'LegMaturityMonthYear'                , string, false};
field(611) -> {'LegMaturityDate'                     , string, false};
field(612) -> {'LegStrikePrice'                      , float , false};
field(613) -> {'LegOptAttribute'                     , char  , false};
field(614) -> {'LegContractMultiplier'               , float , false};
field(615) -> {'LegCouponRate'                       , float , false};
field(616) -> {'LegSecurityExchange'                 , string, false};
field(617) -> {'LegIssuer'                           , string, false};
field(618) -> {'EncodedLegIssuerLen'                 , length, false};
field(619) -> {'EncodedLegIssuer'                    , binary, false};
field(620) -> {'LegSecurityDesc'                     , string, false};
field(621) -> {'EncodedLegSecurityDescLen'           , length, false};
field(622) -> {'EncodedLegSecurityDesc'              , binary, false};
field(623) -> {'LegRatioQty'                         , float , false};
field(624) -> {'LegSide'                             , char  , false};
field(625) -> {'TradingSessionSubID'                 , string, false};
field(626) -> {'AllocType'                           , int   , fun(V) -> decode_fld_val626(V) end};
field(627) -> {'NoHops'                              , group , false};
field(628) -> {'HopCompID'                           , string, false};
field(629) -> {'HopSendingTime'                      , datetm, false};
field(630) -> {'HopRefID'                            , int   , false};
field(631) -> {'MidPx'                               , float , false};
field(632) -> {'BidYield'                            , float , false};
field(633) -> {'MidYield'                            , float , false};
field(634) -> {'OfferYield'                          , float , false};
field(635) -> {'ClearingFeeIndicator'                , string, fun(V) -> decode_fld_val635(V) end};
field(636) -> {'WorkingIndicator'                    , bool  , fun(V) -> decode_fld_val636(V) end};
field(637) -> {'LegLastPx'                           , float , false};
field(638) -> {'PriorityIndicator'                   , int   , fun(V) -> decode_fld_val638(V) end};
field(639) -> {'PriceImprovement'                    , float , false};
field(640) -> {'Price2'                              , float , false};
field(641) -> {'LastForwardPoints2'                  , float , false};
field(642) -> {'BidForwardPoints2'                   , float , false};
field(643) -> {'OfferForwardPoints2'                 , float , false};
field(644) -> {'RFQReqID'                            , string, false};
field(645) -> {'MktBidPx'                            , float , false};
field(646) -> {'MktOfferPx'                          , float , false};
field(647) -> {'MinBidSize'                          , float , false};
field(648) -> {'MinOfferSize'                        , float , false};
field(649) -> {'QuoteStatusReqID'                    , string, false};
field(650) -> {'LegalConfirm'                        , bool  , fun(V) -> decode_fld_val650(V) end};
field(651) -> {'UnderlyingLastPx'                    , float , false};
field(652) -> {'UnderlyingLastQty'                   , float , false};
field(654) -> {'LegRefID'                            , string, false};
field(655) -> {'ContraLegRefID'                      , string, false};
field(656) -> {'SettlCurrBidFxRate'                  , float , false};
field(657) -> {'SettlCurrOfferFxRate'                , float , false};
field(658) -> {'QuoteRequestRejectReason'            , int   , fun(V) -> decode_fld_val658(V) end};
field(659) -> {'SideComplianceID'                    , string, false};
field(660) -> {'AcctIDSource'                        , int   , fun(V) -> decode_fld_val660(V) end};
field(661) -> {'AllocAcctIDSource'                   , int   , false};
field(662) -> {'BenchmarkPrice'                      , float , false};
field(663) -> {'BenchmarkPriceType'                  , int   , false};
field(664) -> {'ConfirmID'                           , string, false};
field(665) -> {'ConfirmStatus'                       , int   , fun(V) -> decode_fld_val665(V) end};
field(666) -> {'ConfirmTransType'                    , int   , fun(V) -> decode_fld_val666(V) end};
field(667) -> {'ContractSettlMonth'                  , string, false};
field(668) -> {'DeliveryForm'                        , int   , fun(V) -> decode_fld_val668(V) end};
field(669) -> {'LastParPx'                           , float , false};
field(670) -> {'NoLegAllocs'                         , group , false};
field(671) -> {'LegAllocAccount'                     , string, false};
field(672) -> {'LegIndividualAllocID'                , string, false};
field(673) -> {'LegAllocQty'                         , float , false};
field(674) -> {'LegAllocAcctIDSource'                , string, false};
field(675) -> {'LegSettlCurrency'                    , string, false};
field(676) -> {'LegBenchmarkCurveCurrency'           , string, false};
field(677) -> {'LegBenchmarkCurveName'               , string, false};
field(678) -> {'LegBenchmarkCurvePoint'              , string, false};
field(679) -> {'LegBenchmarkPrice'                   , float , false};
field(680) -> {'LegBenchmarkPriceType'               , int   , false};
field(681) -> {'LegBidPx'                            , float , false};
field(682) -> {'LegIOIQty'                           , string, false};
field(683) -> {'NoLegStipulations'                   , group , false};
field(684) -> {'LegOfferPx'                          , float , false};
field(686) -> {'LegPriceType'                        , int   , false};
field(687) -> {'LegQty'                              , float , false};
field(688) -> {'LegStipulationType'                  , string, false};
field(689) -> {'LegStipulationValue'                 , string, false};
field(690) -> {'LegSwapType'                         , int   , fun(V) -> decode_fld_val690(V) end};
field(691) -> {'Pool'                                , string, false};
field(692) -> {'QuotePriceType'                      , int   , fun(V) -> decode_fld_val692(V) end};
field(693) -> {'QuoteRespID'                         , string, false};
field(694) -> {'QuoteRespType'                       , int   , fun(V) -> decode_fld_val694(V) end};
field(695) -> {'QuoteQualifier'                      , char  , false};
field(696) -> {'YieldRedemptionDate'                 , string, false};
field(697) -> {'YieldRedemptionPrice'                , float , false};
field(698) -> {'YieldRedemptionPriceType'            , int   , false};
field(699) -> {'BenchmarkSecurityID'                 , string, false};
field(700) -> {'ReversalIndicator'                   , bool  , false};
field(701) -> {'YieldCalcDate'                       , string, false};
field(702) -> {'NoPositions'                         , group , false};
field(703) -> {'PosType'                             , string, fun(V) -> decode_fld_val703(V) end};
field(704) -> {'LongQty'                             , float , false};
field(705) -> {'ShortQty'                            , float , false};
field(706) -> {'PosQtyStatus'                        , int   , fun(V) -> decode_fld_val706(V) end};
field(707) -> {'PosAmtType'                          , string, fun(V) -> decode_fld_val707(V) end};
field(708) -> {'PosAmt'                              , float , false};
field(709) -> {'PosTransType'                        , int   , fun(V) -> decode_fld_val709(V) end};
field(710) -> {'PosReqID'                            , string, false};
field(711) -> {'NoUnderlyings'                       , group , false};
field(712) -> {'PosMaintAction'                      , int   , fun(V) -> decode_fld_val712(V) end};
field(713) -> {'OrigPosReqRefID'                     , string, false};
field(714) -> {'PosMaintRptRefID'                    , string, false};
field(715) -> {'ClearingBusinessDate'                , string, false};
field(716) -> {'SettlSessID'                         , string, fun(V) -> decode_fld_val716(V) end};
field(717) -> {'SettlSessSubID'                      , string, false};
field(718) -> {'AdjustmentType'                      , int   , fun(V) -> decode_fld_val718(V) end};
field(719) -> {'ContraryInstructionIndicator'        , bool  , false};
field(720) -> {'PriorSpreadIndicator'                , bool  , false};
field(721) -> {'PosMaintRptID'                       , string, false};
field(722) -> {'PosMaintStatus'                      , int   , fun(V) -> decode_fld_val722(V) end};
field(723) -> {'PosMaintResult'                      , int   , fun(V) -> decode_fld_val723(V) end};
field(724) -> {'PosReqType'                          , int   , fun(V) -> decode_fld_val724(V) end};
field(725) -> {'ResponseTransportType'               , int   , fun(V) -> decode_fld_val725(V) end};
field(726) -> {'ResponseDestination'                 , string, false};
field(727) -> {'TotalNumPosReports'                  , int   , false};
field(728) -> {'PosReqResult'                        , int   , fun(V) -> decode_fld_val728(V) end};
field(729) -> {'PosReqStatus'                        , int   , fun(V) -> decode_fld_val729(V) end};
field(730) -> {'SettlPrice'                          , float , false};
field(731) -> {'SettlPriceType'                      , int   , fun(V) -> decode_fld_val731(V) end};
field(732) -> {'UnderlyingSettlPrice'                , float , false};
field(733) -> {'UnderlyingSettlPriceType'            , int   , false};
field(734) -> {'PriorSettlPrice'                     , float , false};
field(735) -> {'NoQuoteQualifiers'                   , group , false};
field(736) -> {'AllocSettlCurrency'                  , string, false};
field(737) -> {'AllocSettlCurrAmt'                   , float , false};
field(738) -> {'InterestAtMaturity'                  , float , false};
field(739) -> {'LegDatedDate'                        , string, false};
field(740) -> {'LegPool'                             , string, false};
field(741) -> {'AllocInterestAtMaturity'             , float , false};
field(742) -> {'AllocAccruedInterestAmt'             , float , false};
field(743) -> {'DeliveryDate'                        , string, false};
field(744) -> {'AssignmentMethod'                    , char  , fun(V) -> decode_fld_val744(V) end};
field(745) -> {'AssignmentUnit'                      , float , false};
field(746) -> {'OpenInterest'                        , float , false};
field(747) -> {'ExerciseMethod'                      , char  , fun(V) -> decode_fld_val747(V) end};
field(748) -> {'TotNumTradeReports'                  , int   , false};
field(749) -> {'TradeRequestResult'                  , int   , fun(V) -> decode_fld_val749(V) end};
field(750) -> {'TradeRequestStatus'                  , int   , fun(V) -> decode_fld_val750(V) end};
field(751) -> {'TradeReportRejectReason'             , int   , fun(V) -> decode_fld_val751(V) end};
field(752) -> {'SideMultiLegReportingType'           , int   , fun(V) -> decode_fld_val752(V) end};
field(753) -> {'NoPosAmt'                            , group , false};
field(754) -> {'AutoAcceptIndicator'                 , bool  , false};
field(755) -> {'AllocReportID'                       , string, false};
field(756) -> {'NoNested2PartyIDs'                   , group , false};
field(757) -> {'Nested2PartyID'                      , string, false};
field(758) -> {'Nested2PartyIDSource'                , char  , false};
field(759) -> {'Nested2PartyRole'                    , int   , false};
field(760) -> {'Nested2PartySubID'                   , string, false};
field(761) -> {'BenchmarkSecurityIDSource'           , string, false};
field(762) -> {'SecuritySubType'                     , string, false};
field(763) -> {'UnderlyingSecuritySubType'           , string, false};
field(764) -> {'LegSecuritySubType'                  , string, false};
field(765) -> {'AllowableOneSidednessPct'            , float , false};
field(766) -> {'AllowableOneSidednessValue'          , float , false};
field(767) -> {'AllowableOneSidednessCurr'           , string, false};
field(768) -> {'NoTrdRegTimestamps'                  , group , false};
field(769) -> {'TrdRegTimestamp'                     , datetm, false};
field(770) -> {'TrdRegTimestampType'                 , int   , fun(V) -> decode_fld_val770(V) end};
field(771) -> {'TrdRegTimestampOrigin'               , string, false};
field(772) -> {'ConfirmRefID'                        , string, false};
field(773) -> {'ConfirmType'                         , int   , fun(V) -> decode_fld_val773(V) end};
field(774) -> {'ConfirmRejReason'                    , int   , fun(V) -> decode_fld_val774(V) end};
field(775) -> {'BookingType'                         , int   , fun(V) -> decode_fld_val775(V) end};
field(776) -> {'IndividualAllocRejCode'              , int   , false};
field(777) -> {'SettlInstMsgID'                      , string, false};
field(778) -> {'NoSettlInst'                         , group , false};
field(779) -> {'LastUpdateTime'                      , datetm, false};
field(780) -> {'AllocSettlInstType'                  , int   , fun(V) -> decode_fld_val780(V) end};
field(781) -> {'NoSettlPartyIDs'                     , group , false};
field(782) -> {'SettlPartyID'                        , string, false};
field(783) -> {'SettlPartyIDSource'                  , char  , false};
field(784) -> {'SettlPartyRole'                      , int   , false};
field(785) -> {'SettlPartySubID'                     , string, false};
field(786) -> {'SettlPartySubIDType'                 , int   , false};
field(787) -> {'DlvyInstType'                        , char  , fun(V) -> decode_fld_val787(V) end};
field(788) -> {'TerminationType'                     , int   , fun(V) -> decode_fld_val788(V) end};
field(789) -> {'NextExpectedMsgSeqNum'               , int   , false};
field(790) -> {'OrdStatusReqID'                      , string, false};
field(791) -> {'SettlInstReqID'                      , string, false};
field(792) -> {'SettlInstReqRejCode'                 , int   , fun(V) -> decode_fld_val792(V) end};
field(793) -> {'SecondaryAllocID'                    , string, false};
field(794) -> {'AllocReportType'                     , int   , fun(V) -> decode_fld_val794(V) end};
field(795) -> {'AllocReportRefID'                    , string, false};
field(796) -> {'AllocCancReplaceReason'              , int   , fun(V) -> decode_fld_val796(V) end};
field(797) -> {'CopyMsgIndicator'                    , bool  , false};
field(798) -> {'AllocAccountType'                    , int   , fun(V) -> decode_fld_val798(V) end};
field(799) -> {'OrderAvgPx'                          , float , false};
field(800) -> {'OrderBookingQty'                     , float , false};
field(801) -> {'NoSettlPartySubIDs'                  , group , false};
field(802) -> {'NoPartySubIDs'                       , group , false};
field(803) -> {'PartySubIDType'                      , int   , fun(V) -> decode_fld_val803(V) end};
field(804) -> {'NoNestedPartySubIDs'                 , group , false};
field(805) -> {'NestedPartySubIDType'                , int   , false};
field(806) -> {'NoNested2PartySubIDs'                , group , false};
field(807) -> {'Nested2PartySubIDType'               , int   , false};
field(808) -> {'AllocIntermedReqType'                , int   , fun(V) -> decode_fld_val808(V) end};
field(810) -> {'UnderlyingPx'                        , float , false};
field(811) -> {'PriceDelta'                          , float , false};
field(812) -> {'ApplQueueMax'                        , int   , false};
field(813) -> {'ApplQueueDepth'                      , int   , false};
field(814) -> {'ApplQueueResolution'                 , int   , fun(V) -> decode_fld_val814(V) end};
field(815) -> {'ApplQueueAction'                     , int   , fun(V) -> decode_fld_val815(V) end};
field(816) -> {'NoAltMDSource'                       , group , false};
field(817) -> {'AltMDSourceID'                       , string, false};
field(818) -> {'SecondaryTradeReportID'              , string, false};
field(819) -> {'AvgPxIndicator'                      , int   , fun(V) -> decode_fld_val819(V) end};
field(820) -> {'TradeLinkID'                         , string, false};
field(821) -> {'OrderInputDevice'                    , string, false};
field(822) -> {'UnderlyingTradingSessionID'          , string, false};
field(823) -> {'UnderlyingTradingSessionSubID'       , string, false};
field(824) -> {'TradeLegRefID'                       , string, false};
field(825) -> {'ExchangeRule'                        , string, false};
field(826) -> {'TradeAllocIndicator'                 , int   , fun(V) -> decode_fld_val826(V) end};
field(827) -> {'ExpirationCycle'                     , int   , fun(V) -> decode_fld_val827(V) end};
field(828) -> {'TrdType'                             , int   , fun(V) -> decode_fld_val828(V) end};
field(829) -> {'TrdSubType'                          , int   , false};
field(830) -> {'TransferReason'                      , string, false};
field(832) -> {'TotNumAssignmentReports'             , int   , false};
field(833) -> {'AsgnRptID'                           , string, false};
field(834) -> {'ThresholdAmount'                     , float , false};
field(835) -> {'PegMoveType'                         , int   , fun(V) -> decode_fld_val835(V) end};
field(836) -> {'PegOffsetType'                       , int   , fun(V) -> decode_fld_val836(V) end};
field(837) -> {'PegLimitType'                        , int   , fun(V) -> decode_fld_val837(V) end};
field(838) -> {'PegRoundDirection'                   , int   , fun(V) -> decode_fld_val838(V) end};
field(839) -> {'PeggedPrice'                         , float , false};
field(840) -> {'PegScope'                            , int   , fun(V) -> decode_fld_val840(V) end};
field(841) -> {'DiscretionMoveType'                  , int   , fun(V) -> decode_fld_val841(V) end};
field(842) -> {'DiscretionOffsetType'                , int   , fun(V) -> decode_fld_val842(V) end};
field(843) -> {'DiscretionLimitType'                 , int   , fun(V) -> decode_fld_val843(V) end};
field(844) -> {'DiscretionRoundDirection'            , int   , fun(V) -> decode_fld_val844(V) end};
field(845) -> {'DiscretionPrice'                     , float , false};
field(846) -> {'DiscretionScope'                     , int   , fun(V) -> decode_fld_val846(V) end};
field(847) -> {'TargetStrategy'                      , int   , fun(V) -> decode_fld_val847(V) end};
field(848) -> {'TargetStrategyParameters'            , string, false};
field(849) -> {'ParticipationRate'                   , float , false};
field(850) -> {'TargetStrategyPerformance'           , float , false};
field(851) -> {'LastLiquidityInd'                    , int   , fun(V) -> decode_fld_val851(V) end};
field(852) -> {'PublishTrdIndicator'                 , bool  , fun(V) -> decode_fld_val852(V) end};
field(853) -> {'ShortSaleReason'                     , int   , fun(V) -> decode_fld_val853(V) end};
field(854) -> {'QtyType'                             , int   , fun(V) -> decode_fld_val854(V) end};
field(855) -> {'SecondaryTrdType'                    , int   , false};
field(856) -> {'TradeReportType'                     , int   , fun(V) -> decode_fld_val856(V) end};
field(857) -> {'AllocNoOrdersType'                   , int   , fun(V) -> decode_fld_val857(V) end};
field(858) -> {'SharedCommission'                    , float , false};
field(859) -> {'ConfirmReqID'                        , string, false};
field(860) -> {'AvgParPx'                            , float , false};
field(861) -> {'ReportedPx'                          , float , false};
field(862) -> {'NoCapacities'                        , group , false};
field(863) -> {'OrderCapacityQty'                    , float , false};
field(864) -> {'NoEvents'                            , group , false};
field(865) -> {'EventType'                           , int   , fun(V) -> decode_fld_val865(V) end};
field(866) -> {'EventDate'                           , string, false};
field(867) -> {'EventPx'                             , float , false};
field(868) -> {'EventText'                           , string, false};
field(869) -> {'PctAtRisk'                           , float , false};
field(870) -> {'NoInstrAttrib'                       , group , false};
field(871) -> {'InstrAttribType'                     , int   , fun(V) -> decode_fld_val871(V) end};
field(872) -> {'InstrAttribValue'                    , string, false};
field(873) -> {'DatedDate'                           , string, false};
field(874) -> {'InterestAccrualDate'                 , string, false};
field(875) -> {'CPProgram'                           , int   , fun(V) -> decode_fld_val875(V) end};
field(876) -> {'CPRegType'                           , string, false};
field(877) -> {'UnderlyingCPProgram'                 , string, false};
field(878) -> {'UnderlyingCPRegType'                 , string, false};
field(879) -> {'UnderlyingQty'                       , float , false};
field(880) -> {'TrdMatchID'                          , string, false};
field(881) -> {'SecondaryTradeReportRefID'           , string, false};
field(882) -> {'UnderlyingDirtyPrice'                , float , false};
field(883) -> {'UnderlyingEndPrice'                  , float , false};
field(884) -> {'UnderlyingStartValue'                , float , false};
field(885) -> {'UnderlyingCurrentValue'              , float , false};
field(886) -> {'UnderlyingEndValue'                  , float , false};
field(887) -> {'NoUnderlyingStips'                   , group , false};
field(888) -> {'UnderlyingStipType'                  , string, false};
field(889) -> {'UnderlyingStipValue'                 , string, false};
field(890) -> {'MaturityNetMoney'                    , float , false};
field(891) -> {'MiscFeeBasis'                        , int   , fun(V) -> decode_fld_val891(V) end};
field(892) -> {'TotNoAllocs'                         , int   , false};
field(893) -> {'LastFragment'                        , bool  , fun(V) -> decode_fld_val893(V) end};
field(894) -> {'CollReqID'                           , string, false};
field(895) -> {'CollAsgnReason'                      , int   , fun(V) -> decode_fld_val895(V) end};
field(896) -> {'CollInquiryQualifier'                , int   , fun(V) -> decode_fld_val896(V) end};
field(897) -> {'NoTrades'                            , group , false};
field(898) -> {'MarginRatio'                         , float , false};
field(899) -> {'MarginExcess'                        , float , false};
field(900) -> {'TotalNetValue'                       , float , false};
field(901) -> {'CashOutstanding'                     , float , false};
field(902) -> {'CollAsgnID'                          , string, false};
field(903) -> {'CollAsgnTransType'                   , int   , fun(V) -> decode_fld_val903(V) end};
field(904) -> {'CollRespID'                          , string, false};
field(905) -> {'CollAsgnRespType'                    , int   , fun(V) -> decode_fld_val905(V) end};
field(906) -> {'CollAsgnRejectReason'                , int   , fun(V) -> decode_fld_val906(V) end};
field(907) -> {'CollAsgnRefID'                       , string, false};
field(908) -> {'CollRptID'                           , string, false};
field(909) -> {'CollInquiryID'                       , string, false};
field(910) -> {'CollStatus'                          , int   , fun(V) -> decode_fld_val910(V) end};
field(911) -> {'TotNumReports'                       , int   , false};
field(912) -> {'LastRptRequested'                    , bool  , false};
field(913) -> {'AgreementDesc'                       , string, false};
field(914) -> {'AgreementID'                         , string, false};
field(915) -> {'AgreementDate'                       , string, false};
field(916) -> {'StartDate'                           , string, false};
field(917) -> {'EndDate'                             , string, false};
field(918) -> {'AgreementCurrency'                   , string, false};
field(919) -> {'DeliveryType'                        , int   , fun(V) -> decode_fld_val919(V) end};
field(920) -> {'EndAccruedInterestAmt'               , float , false};
field(921) -> {'StartCash'                           , float , false};
field(922) -> {'EndCash'                             , float , false};
field(923) -> {'UserRequestID'                       , string, false};
field(924) -> {'UserRequestType'                     , int   , fun(V) -> decode_fld_val924(V) end};
field(925) -> {'NewPassword'                         , string, false};
field(926) -> {'UserStatus'                          , int   , fun(V) -> decode_fld_val926(V) end};
field(927) -> {'UserStatusText'                      , string, false};
field(928) -> {'StatusValue'                         , int   , fun(V) -> decode_fld_val928(V) end};
field(929) -> {'StatusText'                          , string, false};
field(930) -> {'RefCompID'                           , string, false};
field(931) -> {'RefSubID'                            , string, false};
field(932) -> {'NetworkResponseID'                   , string, false};
field(933) -> {'NetworkRequestID'                    , string, false};
field(934) -> {'LastNetworkResponseID'               , string, false};
field(935) -> {'NetworkRequestType'                  , int   , fun(V) -> decode_fld_val935(V) end};
field(936) -> {'NoCompIDs'                           , group , false};
field(937) -> {'NetworkStatusResponseType'           , int   , fun(V) -> decode_fld_val937(V) end};
field(938) -> {'NoCollInquiryQualifier'              , group , false};
field(939) -> {'TrdRptStatus'                        , int   , fun(V) -> decode_fld_val939(V) end};
field(940) -> {'AffirmStatus'                        , int   , fun(V) -> decode_fld_val940(V) end};
field(941) -> {'UnderlyingStrikeCurrency'            , string, false};
field(942) -> {'LegStrikeCurrency'                   , string, false};
field(943) -> {'TimeBracket'                         , string, false};
field(944) -> {'CollAction'                          , int   , fun(V) -> decode_fld_val944(V) end};
field(945) -> {'CollInquiryStatus'                   , int   , fun(V) -> decode_fld_val945(V) end};
field(946) -> {'CollInquiryResult'                   , int   , fun(V) -> decode_fld_val946(V) end};
field(947) -> {'StrikeCurrency'                      , string, false};
field(948) -> {'NoNested3PartyIDs'                   , group , false};
field(949) -> {'Nested3PartyID'                      , string, false};
field(950) -> {'Nested3PartyIDSource'                , char  , false};
field(951) -> {'Nested3PartyRole'                    , int   , false};
field(952) -> {'NoNested3PartySubIDs'                , group , false};
field(953) -> {'Nested3PartySubID'                   , string, false};
field(954) -> {'Nested3PartySubIDType'               , int   , false};
field(955) -> {'LegContractSettlMonth'               , string, false};
field(956) -> {'LegInterestAccrualDate'              , string, false};
field(_  ) -> false.

field_tag('LastRptRequested'                  ) -> {912, bool  , fun(V) -> try_encode_val   (912, bool  , V) end};
field_tag('SecurityTradingStatus'             ) -> {326, int   , fun(V) -> encode_fld_val326(326, int   , V) end};
field_tag('NoLinesOfText'                     ) -> { 33, group , fun(V) -> try_encode_group (         33, V) end};
field_tag('MDEntrySeller'                     ) -> {289, string, fun(V) -> try_encode_val   (289, string, V) end};
field_tag('NoContAmts'                        ) -> {518, group , fun(V) -> try_encode_group (        518, V) end};
field_tag('SecurityAltIDSource'               ) -> {456, string, fun(V) -> try_encode_val   (456, string, V) end};
field_tag('SideValueInd'                      ) -> {401, int   , fun(V) -> encode_fld_val401(401, int   , V) end};
field_tag('EffectiveTime'                     ) -> {168, datetm, fun(V) -> try_encode_val   (168, datetm, V) end};
field_tag('UnderlyingSecurityID'              ) -> {309, string, fun(V) -> try_encode_val   (309, string, V) end};
field_tag('QuoteID'                           ) -> {117, string, fun(V) -> try_encode_val   (117, string, V) end};
field_tag('SolicitedFlag'                     ) -> {377, bool  , fun(V) -> encode_fld_val377(377, bool  , V) end};
field_tag('TradeInputDevice'                  ) -> {579, string, fun(V) -> try_encode_val   (579, string, V) end};
field_tag('TradeCondition'                    ) -> {277, string, fun(V) -> encode_fld_val277(277, string, V) end};
field_tag('BidDescriptorType'                 ) -> {399, int   , fun(V) -> encode_fld_val399(399, int   , V) end};
field_tag('CrossPercent'                      ) -> {413, float , fun(V) -> try_encode_val   (413, float , V) end};
field_tag('DistribPaymentMethod'              ) -> {477, int   , fun(V) -> encode_fld_val477(477, int   , V) end};
field_tag('UnderlyingFactor'                  ) -> {246, float , fun(V) -> try_encode_val   (246, float , V) end};
field_tag('LongQty'                           ) -> {704, float , fun(V) -> try_encode_val   (704, float , V) end};
field_tag('LegPool'                           ) -> {740, string, fun(V) -> try_encode_val   (740, string, V) end};
field_tag('EncodedLegSecurityDescLen'         ) -> {621, length, fun(V) -> try_encode_val   (621, length, V) end};
field_tag('PctAtRisk'                         ) -> {869, float , fun(V) -> try_encode_val   (869, float , V) end};
field_tag('UserRequestID'                     ) -> {923, string, fun(V) -> try_encode_val   (923, string, V) end};
field_tag('PaymentMethod'                     ) -> {492, int   , fun(V) -> encode_fld_val492(492, int   , V) end};
field_tag('EncodedAllocTextLen'               ) -> {360, length, fun(V) -> try_encode_val   (360, length, V) end};
field_tag('LastParPx'                         ) -> {669, float , fun(V) -> try_encode_val   (669, float , V) end};
field_tag('Commission'                        ) -> { 12, float , fun(V) -> try_encode_val   ( 12, float , V) end};
field_tag('OrderQty2'                         ) -> {192, float , fun(V) -> try_encode_val   (192, float , V) end};
field_tag('UnderlyingSymbol'                  ) -> {311, string, fun(V) -> try_encode_val   (311, string, V) end};
field_tag('NoEvents'                          ) -> {864, group , fun(V) -> try_encode_group (        864, V) end};
field_tag('HaltReasonChar'                    ) -> {327, char  , fun(V) -> encode_fld_val327(327, char  , V) end};
field_tag('StartDate'                         ) -> {916, string, fun(V) -> try_encode_val   (916, string, V) end};
field_tag('BidSpotRate'                       ) -> {188, float , fun(V) -> try_encode_val   (188, float , V) end};
field_tag('MDEntryPx'                         ) -> {270, float , fun(V) -> try_encode_val   (270, float , V) end};
field_tag('NumDaysInterest'                   ) -> {157, int   , fun(V) -> try_encode_val   (157, int   , V) end};
field_tag('LegPositionEffect'                 ) -> {564, char  , fun(V) -> try_encode_val   (564, char  , V) end};
field_tag('SecurityListRequestType'           ) -> {559, int   , fun(V) -> encode_fld_val559(559, int   , V) end};
field_tag('EncodedIssuer'                     ) -> {349, binary, fun(V) -> try_encode_val   (349, binary, V) end};
field_tag('WorkingIndicator'                  ) -> {636, bool  , fun(V) -> encode_fld_val636(636, bool  , V) end};
field_tag('BidPx'                             ) -> {132, float , fun(V) -> try_encode_val   (132, float , V) end};
field_tag('TrdRegTimestampType'               ) -> {770, int   , fun(V) -> encode_fld_val770(770, int   , V) end};
field_tag('RegistID'                          ) -> {513, string, fun(V) -> try_encode_val   (513, string, V) end};
field_tag('EFPTrackingError'                  ) -> {405, float , fun(V) -> try_encode_val   (405, float , V) end};
field_tag('NoOrders'                          ) -> { 73, group , fun(V) -> try_encode_group (         73, V) end};
field_tag('InViewOfCommon'                    ) -> {328, bool  , fun(V) -> encode_fld_val328(328, bool  , V) end};
field_tag('UnderlyingTradingSessionID'        ) -> {822, string, fun(V) -> try_encode_val   (822, string, V) end};
field_tag('GTBookingInst'                     ) -> {427, int   , fun(V) -> encode_fld_val427(427, int   , V) end};
field_tag('SettlSessID'                       ) -> {716, string, fun(V) -> encode_fld_val716(716, string, V) end};
field_tag('TargetStrategyParameters'          ) -> {848, string, fun(V) -> try_encode_val   (848, string, V) end};
field_tag('Adjustment'                        ) -> {334, int   , fun(V) -> encode_fld_val334(334, int   , V) end};
field_tag('QuoteSetValidUntilTime'            ) -> {367, datetm, fun(V) -> try_encode_val   (367, datetm, V) end};
field_tag('WtAverageLiquidity'                ) -> {410, float , fun(V) -> try_encode_val   (410, float , V) end};
field_tag('Price'                             ) -> { 44, float , fun(V) -> try_encode_val   ( 44, float , V) end};
field_tag('BookingRefID'                      ) -> {466, string, fun(V) -> try_encode_val   (466, string, V) end};
field_tag('NoExecs'                           ) -> {124, group , fun(V) -> try_encode_group (        124, V) end};
field_tag('LegSwapType'                       ) -> {690, int   , fun(V) -> encode_fld_val690(690, int   , V) end};
field_tag('DlvyInstType'                      ) -> {787, char  , fun(V) -> encode_fld_val787(787, char  , V) end};
field_tag('PosMaintStatus'                    ) -> {722, int   , fun(V) -> encode_fld_val722(722, int   , V) end};
field_tag('TerminationType'                   ) -> {788, int   , fun(V) -> encode_fld_val788(788, int   , V) end};
field_tag('RepoCollateralSecurityType'        ) -> {239, string, fun(V) -> try_encode_val   (239, string, V) end};
field_tag('StandInstDbName'                   ) -> {170, string, fun(V) -> try_encode_val   (170, string, V) end};
field_tag('StrikeCurrency'                    ) -> {947, string, fun(V) -> try_encode_val   (947, string, V) end};
field_tag('MatchType'                         ) -> {574, string, fun(V) -> encode_fld_val574(574, string, V) end};
field_tag('DiscretionRoundDirection'          ) -> {844, int   , fun(V) -> encode_fld_val844(844, int   , V) end};
field_tag('PartySubID'                        ) -> {523, string, fun(V) -> try_encode_val   (523, string, V) end};
field_tag('NoAffectedOrders'                  ) -> {534, group , fun(V) -> try_encode_group (        534, V) end};
field_tag('IOIID'                             ) -> { 23, string, fun(V) -> try_encode_val   ( 23, string, V) end};
field_tag('Nested3PartyIDSource'              ) -> {950, char  , fun(V) -> try_encode_val   (950, char  , V) end};
field_tag('NoPartySubIDs'                     ) -> {802, group , fun(V) -> try_encode_group (        802, V) end};
field_tag('AllocIntermedReqType'              ) -> {808, int   , fun(V) -> encode_fld_val808(808, int   , V) end};
field_tag('LastCapacity'                      ) -> { 29, char  , fun(V) -> encode_fld_val29 ( 29, char  , V) end};
field_tag('Signature'                         ) -> { 89, binary, fun(V) -> try_encode_val   ( 89, binary, V) end};
field_tag('BookingType'                       ) -> {775, int   , fun(V) -> encode_fld_val775(775, int   , V) end};
field_tag('LegStipulationValue'               ) -> {689, string, fun(V) -> try_encode_val   (689, string, V) end};
field_tag('RegistStatus'                      ) -> {506, char  , fun(V) -> encode_fld_val506(506, char  , V) end};
field_tag('CoveredOrUncovered'                ) -> {203, int   , fun(V) -> encode_fld_val203(203, int   , V) end};
field_tag('LegDatedDate'                      ) -> {739, string, fun(V) -> try_encode_val   (739, string, V) end};
field_tag('CrossID'                           ) -> {548, string, fun(V) -> try_encode_val   (548, string, V) end};
field_tag('LastMsgSeqNumProcessed'            ) -> {369, int   , fun(V) -> try_encode_val   (369, int   , V) end};
field_tag('ContraTradeQty'                    ) -> {437, float , fun(V) -> try_encode_val   (437, float , V) end};
field_tag('NestedPartySubIDType'              ) -> {805, int   , fun(V) -> try_encode_val   (805, int   , V) end};
field_tag('TradeRequestID'                    ) -> {568, string, fun(V) -> try_encode_val   (568, string, V) end};
field_tag('TradeInputSource'                  ) -> {578, string, fun(V) -> try_encode_val   (578, string, V) end};
field_tag('Nested3PartyID'                    ) -> {949, string, fun(V) -> try_encode_val   (949, string, V) end};
field_tag('NumberOfOrders'                    ) -> {346, int   , fun(V) -> try_encode_val   (346, int   , V) end};
field_tag('Urgency'                           ) -> { 61, char  , fun(V) -> encode_fld_val61 ( 61, char  , V) end};
field_tag('CorporateAction'                   ) -> {292, string, fun(V) -> encode_fld_val292(292, string, V) end};
field_tag('NoNested2PartySubIDs'              ) -> {806, group , fun(V) -> try_encode_group (        806, V) end};
field_tag('MaturityNetMoney'                  ) -> {890, float , fun(V) -> try_encode_val   (890, float , V) end};
field_tag('LegStrikeCurrency'                 ) -> {942, string, fun(V) -> try_encode_val   (942, string, V) end};
field_tag('TradeReportRefID'                  ) -> {572, string, fun(V) -> try_encode_val   (572, string, V) end};
field_tag('UnderlyingQty'                     ) -> {879, float , fun(V) -> try_encode_val   (879, float , V) end};
field_tag('LastMkt'                           ) -> { 30, string, fun(V) -> try_encode_val   ( 30, string, V) end};
field_tag('PossDupFlag'                       ) -> { 43, bool  , fun(V) -> encode_fld_val43 ( 43, bool  , V) end};
field_tag('SettlInstTransType'                ) -> {163, char  , fun(V) -> encode_fld_val163(163, char  , V) end};
field_tag('NestedPartyRole'                   ) -> {538, int   , fun(V) -> try_encode_val   (538, int   , V) end};
field_tag('OrdStatus'                         ) -> { 39, char  , fun(V) -> encode_fld_val39 ( 39, char  , V) end};
field_tag('QuoteReqID'                        ) -> {131, string, fun(V) -> try_encode_val   (131, string, V) end};
field_tag('CPProgram'                         ) -> {875, int   , fun(V) -> encode_fld_val875(875, int   , V) end};
field_tag('RefSeqNum'                         ) -> { 45, int   , fun(V) -> try_encode_val   ( 45, int   , V) end};
field_tag('UnderlyingIssueDate'               ) -> {242, string, fun(V) -> try_encode_val   (242, string, V) end};
field_tag('YieldType'                         ) -> {235, string, fun(V) -> encode_fld_val235(235, string, V) end};
field_tag('LegSecurityDesc'                   ) -> {620, string, fun(V) -> try_encode_val   (620, string, V) end};
field_tag('Country'                           ) -> {421, string, fun(V) -> try_encode_val   (421, string, V) end};
field_tag('AsgnRptID'                         ) -> {833, string, fun(V) -> try_encode_val   (833, string, V) end};
field_tag('LegSecurityType'                   ) -> {609, string, fun(V) -> try_encode_val   (609, string, V) end};
field_tag('SecurityID'                        ) -> { 48, string, fun(V) -> try_encode_val   ( 48, string, V) end};
field_tag('AgreementDate'                     ) -> {915, string, fun(V) -> try_encode_val   (915, string, V) end};
field_tag('TradSesOpenTime'                   ) -> {342, datetm, fun(V) -> try_encode_val   (342, datetm, V) end};
field_tag('UnderlyingLastQty'                 ) -> {652, float , fun(V) -> try_encode_val   (652, float , V) end};
field_tag('QuoteCondition'                    ) -> {276, string, fun(V) -> encode_fld_val276(276, string, V) end};
field_tag('UnderlyingSymbolSfx'               ) -> {312, string, fun(V) -> try_encode_val   (312, string, V) end};
field_tag('DeliverToLocationID'               ) -> {145, string, fun(V) -> try_encode_val   (145, string, V) end};
field_tag('UnderlyingRedemptionDate'          ) -> {247, string, fun(V) -> try_encode_val   (247, string, V) end};
field_tag('NoAltMDSource'                     ) -> {816, group , fun(V) -> try_encode_group (        816, V) end};
field_tag('Nested2PartyIDSource'              ) -> {758, char  , fun(V) -> try_encode_val   (758, char  , V) end};
field_tag('OpenCloseSettlFlag'                ) -> {286, string, fun(V) -> encode_fld_val286(286, string, V) end};
field_tag('StandInstDbID'                     ) -> {171, string, fun(V) -> try_encode_val   (171, string, V) end};
field_tag('AllocLinkType'                     ) -> {197, int   , fun(V) -> encode_fld_val197(197, int   , V) end};
field_tag('MaturityDate'                      ) -> {541, string, fun(V) -> try_encode_val   (541, string, V) end};
field_tag('TargetSubID'                       ) -> { 57, string, fun(V) -> try_encode_val   ( 57, string, V) end};
field_tag('SideValue2'                        ) -> {397, float , fun(V) -> try_encode_val   (397, float , V) end};
field_tag('PriceType'                         ) -> {423, int   , fun(V) -> encode_fld_val423(423, int   , V) end};
field_tag('SellVolume'                        ) -> {331, float , fun(V) -> try_encode_val   (331, float , V) end};
field_tag('RegistAcctType'                    ) -> {493, string, fun(V) -> try_encode_val   (493, string, V) end};
field_tag('SettlPriceType'                    ) -> {731, int   , fun(V) -> encode_fld_val731(731, int   , V) end};
field_tag('MassStatusReqType'                 ) -> {585, int   , fun(V) -> encode_fld_val585(585, int   , V) end};
field_tag('TargetLocationID'                  ) -> {143, string, fun(V) -> try_encode_val   (143, string, V) end};
field_tag('NoTradingSessions'                 ) -> {386, group , fun(V) -> try_encode_group (        386, V) end};
field_tag('LiquidityPctLow'                   ) -> {402, float , fun(V) -> try_encode_val   (402, float , V) end};
field_tag('TotalTakedown'                     ) -> {237, float , fun(V) -> try_encode_val   (237, float , V) end};
field_tag('CashDistribAgentCode'              ) -> {499, string, fun(V) -> try_encode_val   (499, string, V) end};
field_tag('BenchmarkCurveName'                ) -> {221, string, fun(V) -> try_encode_val   (221, string, V) end};
field_tag('PaymentRef'                        ) -> {476, string, fun(V) -> try_encode_val   (476, string, V) end};
field_tag('DayAvgPx'                          ) -> {426, float , fun(V) -> try_encode_val   (426, float , V) end};
field_tag('CollAsgnRejectReason'              ) -> {906, int   , fun(V) -> encode_fld_val906(906, int   , V) end};
field_tag('YieldRedemptionDate'               ) -> {696, string, fun(V) -> try_encode_val   (696, string, V) end};
field_tag('ReportToExch'                      ) -> {113, bool  , fun(V) -> encode_fld_val113(113, bool  , V) end};
field_tag('NoNested2PartyIDs'                 ) -> {756, group , fun(V) -> try_encode_group (        756, V) end};
field_tag('IssueDate'                         ) -> {225, string, fun(V) -> try_encode_val   (225, string, V) end};
field_tag('DeliveryDate'                      ) -> {743, string, fun(V) -> try_encode_val   (743, string, V) end};
field_tag('TrdRptStatus'                      ) -> {939, int   , fun(V) -> encode_fld_val939(939, int   , V) end};
field_tag('LegBenchmarkCurveCurrency'         ) -> {676, string, fun(V) -> try_encode_val   (676, string, V) end};
field_tag('MDUpdateType'                      ) -> {265, int   , fun(V) -> encode_fld_val265(265, int   , V) end};
field_tag('PosMaintRptRefID'                  ) -> {714, string, fun(V) -> try_encode_val   (714, string, V) end};
field_tag('TickDirection'                     ) -> {274, char  , fun(V) -> encode_fld_val274(274, char  , V) end};
field_tag('IOIRefID'                          ) -> { 26, string, fun(V) -> try_encode_val   ( 26, string, V) end};
field_tag('ListExecInst'                      ) -> { 69, string, fun(V) -> try_encode_val   ( 69, string, V) end};
field_tag('NoNestedPartyIDs'                  ) -> {539, group , fun(V) -> try_encode_group (        539, V) end};
field_tag('HighPx'                            ) -> {332, float , fun(V) -> try_encode_val   (332, float , V) end};
field_tag('QuotePriceType'                    ) -> {692, int   , fun(V) -> encode_fld_val692(692, int   , V) end};
field_tag('TradeReportID'                     ) -> {571, string, fun(V) -> try_encode_val   (571, string, V) end};
field_tag('NoSides'                           ) -> {552, group , fun(V) -> try_encode_group (        552, V) end};
field_tag('OrderCapacity'                     ) -> {528, char  , fun(V) -> encode_fld_val528(528, char  , V) end};
field_tag('OfferYield'                        ) -> {634, float , fun(V) -> try_encode_val   (634, float , V) end};
field_tag('AllocRejCode'                      ) -> { 88, int   , fun(V) -> encode_fld_val88 ( 88, int   , V) end};
field_tag('OrderCapacityQty'                  ) -> {863, float , fun(V) -> try_encode_val   (863, float , V) end};
field_tag('LegRepoCollateralSecurityType'     ) -> {250, string, fun(V) -> try_encode_val   (250, string, V) end};
field_tag('AffectedOrderID'                   ) -> {535, string, fun(V) -> try_encode_val   (535, string, V) end};
field_tag('TradSesStatusRejReason'            ) -> {567, int   , fun(V) -> encode_fld_val567(567, int   , V) end};
field_tag('LegSecurityID'                     ) -> {602, string, fun(V) -> try_encode_val   (602, string, V) end};
field_tag('AllocPrice'                        ) -> {366, float , fun(V) -> try_encode_val   (366, float , V) end};
field_tag('PosAmt'                            ) -> {708, float , fun(V) -> try_encode_val   (708, float , V) end};
field_tag('OfferForwardPoints'                ) -> {191, float , fun(V) -> try_encode_val   (191, float , V) end};
field_tag('LegSettlDate'                      ) -> {588, string, fun(V) -> try_encode_val   (588, string, V) end};
field_tag('OnBehalfOfLocationID'              ) -> {144, string, fun(V) -> try_encode_val   (144, string, V) end};
field_tag('NoContraBrokers'                   ) -> {382, group , fun(V) -> try_encode_group (        382, V) end};
field_tag('RoundingDirection'                 ) -> {468, char  , fun(V) -> encode_fld_val468(468, char  , V) end};
field_tag('AllocHandlInst'                    ) -> {209, int   , fun(V) -> encode_fld_val209(209, int   , V) end};
field_tag('LegCFICode'                        ) -> {608, string, fun(V) -> try_encode_val   (608, string, V) end};
field_tag('LegBenchmarkPrice'                 ) -> {679, float , fun(V) -> try_encode_val   (679, float , V) end};
field_tag('ParticipationRate'                 ) -> {849, float , fun(V) -> try_encode_val   (849, float , V) end};
field_tag('CFICode'                           ) -> {461, string, fun(V) -> try_encode_val   (461, string, V) end};
field_tag('Price2'                            ) -> {640, float , fun(V) -> try_encode_val   (640, float , V) end};
field_tag('LiquidityIndType'                  ) -> {409, int   , fun(V) -> encode_fld_val409(409, int   , V) end};
field_tag('TrdRegTimestampOrigin'             ) -> {771, string, fun(V) -> try_encode_val   (771, string, V) end};
field_tag('ConfirmStatus'                     ) -> {665, int   , fun(V) -> encode_fld_val665(665, int   , V) end};
field_tag('PaymentRemitterID'                 ) -> {505, string, fun(V) -> try_encode_val   (505, string, V) end};
field_tag('ResetSeqNumFlag'                   ) -> {141, bool  , fun(V) -> encode_fld_val141(141, bool  , V) end};
field_tag('RoutingType'                       ) -> {216, int   , fun(V) -> encode_fld_val216(216, int   , V) end};
field_tag('CashOrderQty'                      ) -> {152, float , fun(V) -> try_encode_val   (152, float , V) end};
field_tag('ConfirmRejReason'                  ) -> {774, int   , fun(V) -> encode_fld_val774(774, int   , V) end};
field_tag('PosQtyStatus'                      ) -> {706, int   , fun(V) -> encode_fld_val706(706, int   , V) end};
field_tag('LegIssuer'                         ) -> {617, string, fun(V) -> try_encode_val   (617, string, V) end};
field_tag('SettlType'                         ) -> { 63, char  , fun(V) -> encode_fld_val63 ( 63, char  , V) end};
field_tag('TrdSubType'                        ) -> {829, int   , fun(V) -> try_encode_val   (829, int   , V) end};
field_tag('SettlInstSource'                   ) -> {165, char  , fun(V) -> encode_fld_val165(165, char  , V) end};
field_tag('LegMaturityMonthYear'              ) -> {610, string, fun(V) -> try_encode_val   (610, string, V) end};
field_tag('CollRespID'                        ) -> {904, string, fun(V) -> try_encode_val   (904, string, V) end};
field_tag('UnderlyingIssuer'                  ) -> {306, string, fun(V) -> try_encode_val   (306, string, V) end};
field_tag('NoTrades'                          ) -> {897, group , fun(V) -> try_encode_group (        897, V) end};
field_tag('CollAction'                        ) -> {944, int   , fun(V) -> encode_fld_val944(944, int   , V) end};
field_tag('SecurityRequestResult'             ) -> {560, int   , fun(V) -> encode_fld_val560(560, int   , V) end};
field_tag('SettlCurrBidFxRate'                ) -> {656, float , fun(V) -> try_encode_val   (656, float , V) end};
field_tag('QuoteRespType'                     ) -> {694, int   , fun(V) -> encode_fld_val694(694, int   , V) end};
field_tag('LegSide'                           ) -> {624, char  , fun(V) -> try_encode_val   (624, char  , V) end};
field_tag('LastNetworkResponseID'             ) -> {934, string, fun(V) -> try_encode_val   (934, string, V) end};
field_tag('ExDestination'                     ) -> {100, string, fun(V) -> try_encode_val   (100, string, V) end};
field_tag('AllocTransType'                    ) -> { 71, char  , fun(V) -> encode_fld_val71 ( 71, char  , V) end};
field_tag('DiscretionPrice'                   ) -> {845, float , fun(V) -> try_encode_val   (845, float , V) end};
field_tag('AffirmStatus'                      ) -> {940, int   , fun(V) -> encode_fld_val940(940, int   , V) end};
field_tag('LegIOIQty'                         ) -> {682, string, fun(V) -> try_encode_val   (682, string, V) end};
field_tag('OfferPx'                           ) -> {133, float , fun(V) -> try_encode_val   (133, float , V) end};
field_tag('AllowableOneSidednessPct'          ) -> {765, float , fun(V) -> try_encode_val   (765, float , V) end};
field_tag('TaxAdvantageType'                  ) -> {495, int   , fun(V) -> encode_fld_val495(495, int   , V) end};
field_tag('MDEntryID'                         ) -> {278, string, fun(V) -> try_encode_val   (278, string, V) end};
field_tag('UnderlyingSecurityIDSource'        ) -> {305, string, fun(V) -> try_encode_val   (305, string, V) end};
field_tag('SettlInstReqID'                    ) -> {791, string, fun(V) -> try_encode_val   (791, string, V) end};
field_tag('UnderlyingLocaleOfIssue'           ) -> {594, string, fun(V) -> try_encode_val   (594, string, V) end};
field_tag('UnderlyingCPRegType'               ) -> {878, string, fun(V) -> try_encode_val   (878, string, V) end};
field_tag('LastPx'                            ) -> { 31, float , fun(V) -> try_encode_val   ( 31, float , V) end};
field_tag('TradeOriginationDate'              ) -> {229, string, fun(V) -> try_encode_val   (229, string, V) end};
field_tag('AssignmentUnit'                    ) -> {745, float , fun(V) -> try_encode_val   (745, float , V) end};
field_tag('AccruedInterestRate'               ) -> {158, float , fun(V) -> try_encode_val   (158, float , V) end};
field_tag('TradeReportType'                   ) -> {856, int   , fun(V) -> encode_fld_val856(856, int   , V) end};
field_tag('CountryOfIssue'                    ) -> {470, string, fun(V) -> try_encode_val   (470, string, V) end};
field_tag('CashMargin'                        ) -> {544, char  , fun(V) -> encode_fld_val544(544, char  , V) end};
field_tag('ListStatusText'                    ) -> {444, string, fun(V) -> try_encode_val   (444, string, V) end};
field_tag('BusinessRejectReason'              ) -> {380, int   , fun(V) -> encode_fld_val380(380, int   , V) end};
field_tag('LegLastPx'                         ) -> {637, float , fun(V) -> try_encode_val   (637, float , V) end};
field_tag('PosReqType'                        ) -> {724, int   , fun(V) -> encode_fld_val724(724, int   , V) end};
field_tag('NoDistribInsts'                    ) -> {510, group , fun(V) -> try_encode_group (        510, V) end};
field_tag('TransferReason'                    ) -> {830, string, fun(V) -> try_encode_val   (830, string, V) end};
field_tag('SecurityType'                      ) -> {167, string, fun(V) -> encode_fld_val167(167, string, V) end};
field_tag('NestedPartySubID'                  ) -> {545, string, fun(V) -> try_encode_val   (545, string, V) end};
field_tag('NetChgPrevDay'                     ) -> {451, float , fun(V) -> try_encode_val   (451, float , V) end};
field_tag('InterestAtMaturity'                ) -> {738, float , fun(V) -> try_encode_val   (738, float , V) end};
field_tag('LegBenchmarkCurveName'             ) -> {677, string, fun(V) -> try_encode_val   (677, string, V) end};
field_tag('EncryptMethod'                     ) -> { 98, int   , fun(V) -> encode_fld_val98 ( 98, int   , V) end};
field_tag('PosReqResult'                      ) -> {728, int   , fun(V) -> encode_fld_val728(728, int   , V) end};
field_tag('ApplQueueResolution'               ) -> {814, int   , fun(V) -> encode_fld_val814(814, int   , V) end};
field_tag('ListName'                          ) -> {392, string, fun(V) -> try_encode_val   (392, string, V) end};
field_tag('RegistRefID'                       ) -> {508, string, fun(V) -> try_encode_val   (508, string, V) end};
field_tag('BasisPxType'                       ) -> {419, char  , fun(V) -> encode_fld_val419(419, char  , V) end};
field_tag('AvgParPx'                          ) -> {860, float , fun(V) -> try_encode_val   (860, float , V) end};
field_tag('OrdStatusReqID'                    ) -> {790, string, fun(V) -> try_encode_val   (790, string, V) end};
field_tag('YieldCalcDate'                     ) -> {701, string, fun(V) -> try_encode_val   (701, string, V) end};
field_tag('ClearingFeeIndicator'              ) -> {635, string, fun(V) -> encode_fld_val635(635, string, V) end};
field_tag('NumBidders'                        ) -> {417, int   , fun(V) -> try_encode_val   (417, int   , V) end};
field_tag('ProcessCode'                       ) -> { 81, char  , fun(V) -> encode_fld_val81 ( 81, char  , V) end};
field_tag('RFQReqID'                          ) -> {644, string, fun(V) -> try_encode_val   (644, string, V) end};
field_tag('NoClearingInstructions'            ) -> {576, group , fun(V) -> try_encode_group (        576, V) end};
field_tag('NoMsgTypes'                        ) -> {384, group , fun(V) -> try_encode_group (        384, V) end};
field_tag('MiscFeeBasis'                      ) -> {891, int   , fun(V) -> encode_fld_val891(891, int   , V) end};
field_tag('SettlInstMsgID'                    ) -> {777, string, fun(V) -> try_encode_val   (777, string, V) end};
field_tag('PriorityIndicator'                 ) -> {638, int   , fun(V) -> encode_fld_val638(638, int   , V) end};
field_tag('EncodedAllocText'                  ) -> {361, binary, fun(V) -> try_encode_val   (361, binary, V) end};
field_tag('DueToRelated'                      ) -> {329, bool  , fun(V) -> encode_fld_val329(329, bool  , V) end};
field_tag('UnderlyingProduct'                 ) -> {462, int   , fun(V) -> try_encode_val   (462, int   , V) end};
field_tag('ClOrdID'                           ) -> { 11, string, fun(V) -> try_encode_val   ( 11, string, V) end};
field_tag('SharedCommission'                  ) -> {858, float , fun(V) -> try_encode_val   (858, float , V) end};
field_tag('UserStatus'                        ) -> {926, int   , fun(V) -> encode_fld_val926(926, int   , V) end};
field_tag('NoDates'                           ) -> {580, group , fun(V) -> try_encode_group (        580, V) end};
field_tag('UnderlyingRepurchaseRate'          ) -> {245, float , fun(V) -> try_encode_val   (245, float , V) end};
field_tag('AgreementDesc'                     ) -> {913, string, fun(V) -> try_encode_val   (913, string, V) end};
field_tag('OrderID'                           ) -> { 37, string, fun(V) -> try_encode_val   ( 37, string, V) end};
field_tag('ContAmtCurr'                       ) -> {521, string, fun(V) -> try_encode_val   (521, string, V) end};
field_tag('AllocAvgPx'                        ) -> {153, float , fun(V) -> try_encode_val   (153, float , V) end};
field_tag('AssignmentMethod'                  ) -> {744, char  , fun(V) -> encode_fld_val744(744, char  , V) end};
field_tag('QuoteQualifier'                    ) -> {695, char  , fun(V) -> try_encode_val   (695, char  , V) end};
field_tag('UnderlyingEndPrice'                ) -> {883, float , fun(V) -> try_encode_val   (883, float , V) end};
field_tag('SettlDate2'                        ) -> {193, string, fun(V) -> try_encode_val   (193, string, V) end};
field_tag('CollAsgnReason'                    ) -> {895, int   , fun(V) -> encode_fld_val895(895, int   , V) end};
field_tag('MDEntryType'                       ) -> {269, char  , fun(V) -> encode_fld_val269(269, char  , V) end};
field_tag('ShortQty'                          ) -> {705, float , fun(V) -> try_encode_val   (705, float , V) end};
field_tag('BodyLength'                        ) -> {  9, length, fun(V) -> try_encode_val   (  9, length, V) end};
field_tag('EncodedLegIssuer'                  ) -> {619, binary, fun(V) -> try_encode_val   (619, binary, V) end};
field_tag('PegLimitType'                      ) -> {837, int   , fun(V) -> encode_fld_val837(837, int   , V) end};
field_tag('QtyType'                           ) -> {854, int   , fun(V) -> encode_fld_val854(854, int   , V) end};
field_tag('AllocAccountType'                  ) -> {798, int   , fun(V) -> encode_fld_val798(798, int   , V) end};
field_tag('NoSettlPartyIDs'                   ) -> {781, group , fun(V) -> try_encode_group (        781, V) end};
field_tag('MultiLegRptTypeReq'                ) -> {563, int   , fun(V) -> encode_fld_val563(563, int   , V) end};
field_tag('AccountType'                       ) -> {581, int   , fun(V) -> encode_fld_val581(581, int   , V) end};
field_tag('TrdRegTimestamp'                   ) -> {769, datetm, fun(V) -> try_encode_val   (769, datetm, V) end};
field_tag('LastQty'                           ) -> { 32, float , fun(V) -> try_encode_val   ( 32, float , V) end};
field_tag('SecurityReqID'                     ) -> {320, string, fun(V) -> try_encode_val   (320, string, V) end};
field_tag('AcctIDSource'                      ) -> {660, int   , fun(V) -> encode_fld_val660(660, int   , V) end};
field_tag('PosReqID'                          ) -> {710, string, fun(V) -> try_encode_val   (710, string, V) end};
field_tag('MsgSeqNum'                         ) -> { 34, int   , fun(V) -> try_encode_val   ( 34, int   , V) end};
field_tag('NoStipulations'                    ) -> {232, group , fun(V) -> try_encode_group (        232, V) end};
field_tag('Concession'                        ) -> {238, float , fun(V) -> try_encode_val   (238, float , V) end};
field_tag('CreditRating'                      ) -> {255, string, fun(V) -> try_encode_val   (255, string, V) end};
field_tag('UnderlyingStrikeCurrency'          ) -> {941, string, fun(V) -> try_encode_val   (941, string, V) end};
field_tag('PriorSpreadIndicator'              ) -> {720, bool  , fun(V) -> try_encode_val   (720, bool  , V) end};
field_tag('MarketDepth'                       ) -> {264, int   , fun(V) -> try_encode_val   (264, int   , V) end};
field_tag('ExecPriceAdjustment'               ) -> {485, float , fun(V) -> try_encode_val   (485, float , V) end};
field_tag('PriorSettlPrice'                   ) -> {734, float , fun(V) -> try_encode_val   (734, float , V) end};
field_tag('TransBkdTime'                      ) -> {483, datetm, fun(V) -> try_encode_val   (483, datetm, V) end};
field_tag('LegSettlCurrency'                  ) -> {675, string, fun(V) -> try_encode_val   (675, string, V) end};
field_tag('IOIQltyInd'                        ) -> { 25, char  , fun(V) -> encode_fld_val25 ( 25, char  , V) end};
field_tag('TradSesReqID'                      ) -> {335, string, fun(V) -> try_encode_val   (335, string, V) end};
field_tag('ConfirmRefID'                      ) -> {772, string, fun(V) -> try_encode_val   (772, string, V) end};
field_tag('RoundingModulus'                   ) -> {469, float , fun(V) -> try_encode_val   (469, float , V) end};
field_tag('ListOrderStatus'                   ) -> {431, int   , fun(V) -> encode_fld_val431(431, int   , V) end};
field_tag('CollInquiryStatus'                 ) -> {945, int   , fun(V) -> encode_fld_val945(945, int   , V) end};
field_tag('RegistTransType'                   ) -> {514, char  , fun(V) -> encode_fld_val514(514, char  , V) end};
field_tag('EncodedLegIssuerLen'               ) -> {618, length, fun(V) -> try_encode_val   (618, length, V) end};
field_tag('IOITransType'                      ) -> { 28, char  , fun(V) -> encode_fld_val28 ( 28, char  , V) end};
field_tag('CommCurrency'                      ) -> {479, string, fun(V) -> try_encode_val   (479, string, V) end};
field_tag('CollReqID'                         ) -> {894, string, fun(V) -> try_encode_val   (894, string, V) end};
field_tag('RefSubID'                          ) -> {931, string, fun(V) -> try_encode_val   (931, string, V) end};
field_tag('BusinessRejectRefID'               ) -> {379, string, fun(V) -> try_encode_val   (379, string, V) end};
field_tag('NoIOIQualifiers'                   ) -> {199, group , fun(V) -> try_encode_group (        199, V) end};
field_tag('AggregatedBook'                    ) -> {266, bool  , fun(V) -> encode_fld_val266(266, bool  , V) end};
field_tag('Password'                          ) -> {554, string, fun(V) -> try_encode_val   (554, string, V) end};
field_tag('LastForwardPoints'                 ) -> {195, float , fun(V) -> try_encode_val   (195, float , V) end};
field_tag('UnderlyingSecurityAltID'           ) -> {458, string, fun(V) -> try_encode_val   (458, string, V) end};
field_tag('BenchmarkCurvePoint'               ) -> {222, string, fun(V) -> try_encode_val   (222, string, V) end};
field_tag('LegSettlType'                      ) -> {587, char  , fun(V) -> try_encode_val   (587, char  , V) end};
field_tag('TotalNetValue'                     ) -> {900, float , fun(V) -> try_encode_val   (900, float , V) end};
field_tag('InvestorCountryOfResidence'        ) -> {475, string, fun(V) -> try_encode_val   (475, string, V) end};
field_tag('DeliveryType'                      ) -> {919, int   , fun(V) -> encode_fld_val919(919, int   , V) end};
field_tag('MDEntryRefID'                      ) -> {280, string, fun(V) -> try_encode_val   (280, string, V) end};
field_tag('CardStartDate'                     ) -> {503, string, fun(V) -> try_encode_val   (503, string, V) end};
field_tag('QuoteRequestRejectReason'          ) -> {658, int   , fun(V) -> encode_fld_val658(658, int   , V) end};
field_tag('TotNumReports'                     ) -> {911, int   , fun(V) -> try_encode_val   (911, int   , V) end};
field_tag('ListExecInstType'                  ) -> {433, char  , fun(V) -> encode_fld_val433(433, char  , V) end};
field_tag('TradSesPreCloseTime'               ) -> {343, datetm, fun(V) -> try_encode_val   (343, datetm, V) end};
field_tag('CxlRejResponseTo'                  ) -> {434, char  , fun(V) -> encode_fld_val434(434, char  , V) end};
field_tag('LegSecuritySubType'                ) -> {764, string, fun(V) -> try_encode_val   (764, string, V) end};
field_tag('ValueOfFutures'                    ) -> {408, float , fun(V) -> try_encode_val   (408, float , V) end};
field_tag('PegOffsetValue'                    ) -> {211, float , fun(V) -> try_encode_val   (211, float , V) end};
field_tag('LiquidityValue'                    ) -> {404, float , fun(V) -> try_encode_val   (404, float , V) end};
field_tag('InstrAttribValue'                  ) -> {872, string, fun(V) -> try_encode_val   (872, string, V) end};
field_tag('MatchStatus'                       ) -> {573, char  , fun(V) -> encode_fld_val573(573, char  , V) end};
field_tag('CardHolderName'                    ) -> {488, string, fun(V) -> try_encode_val   (488, string, V) end};
field_tag('BidForwardPoints2'                 ) -> {642, float , fun(V) -> try_encode_val   (642, float , V) end};
field_tag('NoSecurityAltID'                   ) -> {454, group , fun(V) -> try_encode_group (        454, V) end};
field_tag('AgreementCurrency'                 ) -> {918, string, fun(V) -> try_encode_val   (918, string, V) end};
field_tag('BidType'                           ) -> {394, int   , fun(V) -> encode_fld_val394(394, int   , V) end};
field_tag('NoNestedPartySubIDs'               ) -> {804, group , fun(V) -> try_encode_group (        804, V) end};
field_tag('OptAttribute'                      ) -> {206, char  , fun(V) -> try_encode_val   (206, char  , V) end};
field_tag('NoDlvyInst'                        ) -> { 85, group , fun(V) -> try_encode_group (         85, V) end};
field_tag('AgreementID'                       ) -> {914, string, fun(V) -> try_encode_val   (914, string, V) end};
field_tag('LegSecurityAltIDSource'            ) -> {606, string, fun(V) -> try_encode_val   (606, string, V) end};
field_tag('FundRenewWaiv'                     ) -> {497, char  , fun(V) -> encode_fld_val497(497, char  , V) end};
field_tag('BidDescriptor'                     ) -> {400, string, fun(V) -> try_encode_val   (400, string, V) end};
field_tag('XmlData'                           ) -> {213, binary, fun(V) -> try_encode_val   (213, binary, V) end};
field_tag('EventPx'                           ) -> {867, float , fun(V) -> try_encode_val   (867, float , V) end};
field_tag('SecondaryAllocID'                  ) -> {793, string, fun(V) -> try_encode_val   (793, string, V) end};
field_tag('LegContractMultiplier'             ) -> {614, float , fun(V) -> try_encode_val   (614, float , V) end};
field_tag('SettlPartyID'                      ) -> {782, string, fun(V) -> try_encode_val   (782, string, V) end};
field_tag('MinBidSize'                        ) -> {647, float , fun(V) -> try_encode_val   (647, float , V) end};
field_tag('CrossType'                         ) -> {549, int   , fun(V) -> encode_fld_val549(549, int   , V) end};
field_tag('NoHops'                            ) -> {627, group , fun(V) -> try_encode_group (        627, V) end};
field_tag('IndividualAllocRejCode'            ) -> {776, int   , fun(V) -> try_encode_val   (776, int   , V) end};
field_tag('QuoteRespID'                       ) -> {693, string, fun(V) -> try_encode_val   (693, string, V) end};
field_tag('PartyID'                           ) -> {448, string, fun(V) -> try_encode_val   (448, string, V) end};
field_tag('SecureData'                        ) -> { 91, binary, fun(V) -> try_encode_val   ( 91, binary, V) end};
field_tag('PaymentDate'                       ) -> {504, string, fun(V) -> try_encode_val   (504, string, V) end};
field_tag('AllocSettlInstType'                ) -> {780, int   , fun(V) -> encode_fld_val780(780, int   , V) end};
field_tag('TradeAllocIndicator'               ) -> {826, int   , fun(V) -> encode_fld_val826(826, int   , V) end};
field_tag('CouponPaymentDate'                 ) -> {224, string, fun(V) -> try_encode_val   (224, string, V) end};
field_tag('SettlCurrency'                     ) -> {120, string, fun(V) -> try_encode_val   (120, string, V) end};
field_tag('NewPassword'                       ) -> {925, string, fun(V) -> try_encode_val   (925, string, V) end};
field_tag('BookingUnit'                       ) -> {590, char  , fun(V) -> encode_fld_val590(590, char  , V) end};
field_tag('OpenInterest'                      ) -> {746, float , fun(V) -> try_encode_val   (746, float , V) end};
field_tag('ExDate'                            ) -> {230, string, fun(V) -> try_encode_val   (230, string, V) end};
field_tag('TargetStrategy'                    ) -> {847, int   , fun(V) -> encode_fld_val847(847, int   , V) end};
field_tag('InstrRegistry'                     ) -> {543, string, fun(V) -> try_encode_val   (543, string, V) end};
field_tag('EncodedUnderlyingIssuerLen'        ) -> {362, length, fun(V) -> try_encode_val   (362, length, V) end};
field_tag('SettlDate'                         ) -> { 64, string, fun(V) -> try_encode_val   ( 64, string, V) end};
field_tag('LastSpotRate'                      ) -> {194, float , fun(V) -> try_encode_val   (194, float , V) end};
field_tag('ContraTrader'                      ) -> {337, string, fun(V) -> try_encode_val   (337, string, V) end};
field_tag('PosMaintResult'                    ) -> {723, int   , fun(V) -> encode_fld_val723(723, int   , V) end};
field_tag('UnderlyingCFICode'                 ) -> {463, string, fun(V) -> try_encode_val   (463, string, V) end};
field_tag('UnderlyingSecurityExchange'        ) -> {308, string, fun(V) -> try_encode_val   (308, string, V) end};
field_tag('MDEntryDate'                       ) -> {272, datetm, fun(V) -> try_encode_val   (272, datetm, V) end};
field_tag('NetworkResponseID'                 ) -> {932, string, fun(V) -> try_encode_val   (932, string, V) end};
field_tag('BidRequestTransType'               ) -> {374, char  , fun(V) -> encode_fld_val374(374, char  , V) end};
field_tag('NetworkStatusResponseType'         ) -> {937, int   , fun(V) -> encode_fld_val937(937, int   , V) end};
field_tag('NoRelatedSym'                      ) -> {146, group , fun(V) -> try_encode_group (        146, V) end};
field_tag('TotalAffectedOrders'               ) -> {533, int   , fun(V) -> try_encode_val   (533, int   , V) end};
field_tag('AvgPx'                             ) -> {  6, float , fun(V) -> try_encode_val   (  6, float , V) end};
field_tag('LegInstrRegistry'                  ) -> {599, string, fun(V) -> try_encode_val   (599, string, V) end};
field_tag('SecondaryClOrdID'                  ) -> {526, string, fun(V) -> try_encode_val   (526, string, V) end};
field_tag('OrderPercent'                      ) -> {516, float , fun(V) -> try_encode_val   (516, float , V) end};
field_tag('AltMDSourceID'                     ) -> {817, string, fun(V) -> try_encode_val   (817, string, V) end};
field_tag('ProgRptReqs'                       ) -> {414, int   , fun(V) -> encode_fld_val414(414, int   , V) end};
field_tag('CouponRate'                        ) -> {223, float , fun(V) -> try_encode_val   (223, float , V) end};
field_tag('BidID'                             ) -> {390, string, fun(V) -> try_encode_val   (390, string, V) end};
field_tag('LegPriceType'                      ) -> {686, int   , fun(V) -> try_encode_val   (686, int   , V) end};
field_tag('Scope'                             ) -> {546, string, fun(V) -> encode_fld_val546(546, string, V) end};
field_tag('StatusText'                        ) -> {929, string, fun(V) -> try_encode_val   (929, string, V) end};
field_tag('NoNested3PartyIDs'                 ) -> {948, group , fun(V) -> try_encode_group (        948, V) end};
field_tag('TotalAccruedInterestAmt'           ) -> {540, float , fun(V) -> try_encode_val   (540, float , V) end};
field_tag('Yield'                             ) -> {236, float , fun(V) -> try_encode_val   (236, float , V) end};
field_tag('TradingSessionID'                  ) -> {336, string, fun(V) -> try_encode_val   (336, string, V) end};
field_tag('NoLegAllocs'                       ) -> {670, group , fun(V) -> try_encode_group (        670, V) end};
field_tag('SubscriptionRequestType'           ) -> {263, char  , fun(V) -> encode_fld_val263(263, char  , V) end};
field_tag('LegFactor'                         ) -> {253, float , fun(V) -> try_encode_val   (253, float , V) end};
field_tag('TradSesCloseTime'                  ) -> {344, datetm, fun(V) -> try_encode_val   (344, datetm, V) end};
field_tag('ClientBidID'                       ) -> {391, string, fun(V) -> try_encode_val   (391, string, V) end};
field_tag('RegistRejReasonCode'               ) -> {507, int   , fun(V) -> encode_fld_val507(507, int   , V) end};
field_tag('NoCollInquiryQualifier'            ) -> {938, group , fun(V) -> try_encode_group (        938, V) end};
field_tag('LegBidPx'                          ) -> {681, float , fun(V) -> try_encode_val   (681, float , V) end};
field_tag('Nested2PartySubID'                 ) -> {760, string, fun(V) -> try_encode_val   (760, string, V) end};
field_tag('TradSesMode'                       ) -> {339, int   , fun(V) -> encode_fld_val339(339, int   , V) end};
field_tag('MidPx'                             ) -> {631, float , fun(V) -> try_encode_val   (631, float , V) end};
field_tag('MsgDirection'                      ) -> {385, char  , fun(V) -> encode_fld_val385(385, char  , V) end};
field_tag('NoMDEntries'                       ) -> {268, group , fun(V) -> try_encode_group (        268, V) end};
field_tag('UnderlyingCouponRate'              ) -> {435, float , fun(V) -> try_encode_val   (435, float , V) end};
field_tag('ShortSaleReason'                   ) -> {853, int   , fun(V) -> encode_fld_val853(853, int   , V) end};
field_tag('CollInquiryQualifier'              ) -> {896, int   , fun(V) -> encode_fld_val896(896, int   , V) end};
field_tag('ApplQueueAction'                   ) -> {815, int   , fun(V) -> encode_fld_val815(815, int   , V) end};
field_tag('DiscretionInst'                    ) -> {388, char  , fun(V) -> encode_fld_val388(388, char  , V) end};
field_tag('AdjustmentType'                    ) -> {718, int   , fun(V) -> encode_fld_val718(718, int   , V) end};
field_tag('SessionRejectReason'               ) -> {373, int   , fun(V) -> encode_fld_val373(373, int   , V) end};
field_tag('LegRefID'                          ) -> {654, string, fun(V) -> try_encode_val   (654, string, V) end};
field_tag('RefAllocID'                        ) -> { 72, string, fun(V) -> try_encode_val   ( 72, string, V) end};
field_tag('MinQty'                            ) -> {110, float , fun(V) -> try_encode_val   (110, float , V) end};
field_tag('BenchmarkPriceType'                ) -> {663, int   , fun(V) -> try_encode_val   (663, int   , V) end};
field_tag('TotNoOrders'                       ) -> { 68, int   , fun(V) -> try_encode_val   ( 68, int   , V) end};
field_tag('XmlDataLen'                        ) -> {212, length, fun(V) -> try_encode_val   (212, length, V) end};
field_tag('UnderlyingInstrRegistry'           ) -> {595, string, fun(V) -> try_encode_val   (595, string, V) end};
field_tag('DayCumQty'                         ) -> {425, float , fun(V) -> try_encode_val   (425, float , V) end};
field_tag('MassCancelRequestType'             ) -> {530, char  , fun(V) -> encode_fld_val530(530, char  , V) end};
field_tag('OnBehalfOfSubID'                   ) -> {116, string, fun(V) -> try_encode_val   (116, string, V) end};
field_tag('MultiLegReportingType'             ) -> {442, char  , fun(V) -> encode_fld_val442(442, char  , V) end};
field_tag('MDEntryBuyer'                      ) -> {288, string, fun(V) -> try_encode_val   (288, string, V) end};
field_tag('EncodedListExecInstLen'            ) -> {352, length, fun(V) -> try_encode_val   (352, length, V) end};
field_tag('AllocQty'                          ) -> { 80, float , fun(V) -> try_encode_val   ( 80, float , V) end};
field_tag('LegLocaleOfIssue'                  ) -> {598, string, fun(V) -> try_encode_val   (598, string, V) end};
field_tag('OwnershipType'                     ) -> {517, char  , fun(V) -> encode_fld_val517(517, char  , V) end};
field_tag('OrderQty'                          ) -> { 38, float , fun(V) -> try_encode_val   ( 38, float , V) end};
field_tag('MktOfferPx'                        ) -> {646, float , fun(V) -> try_encode_val   (646, float , V) end};
field_tag('Spread'                            ) -> {218, float , fun(V) -> try_encode_val   (218, float , V) end};
field_tag('MiscFeeAmt'                        ) -> {137, float , fun(V) -> try_encode_val   (137, float , V) end};
field_tag('NoPartyIDs'                        ) -> {453, group , fun(V) -> try_encode_group (        453, V) end};
field_tag('CrossPrioritization'               ) -> {550, int   , fun(V) -> encode_fld_val550(550, int   , V) end};
field_tag('ExpireTime'                        ) -> {126, datetm, fun(V) -> try_encode_val   (126, datetm, V) end};
field_tag('CommType'                          ) -> { 13, char  , fun(V) -> encode_fld_val13 ( 13, char  , V) end};
field_tag('UnderlyingStipType'                ) -> {888, string, fun(V) -> try_encode_val   (888, string, V) end};
field_tag('OrigOrdModTime'                    ) -> {586, datetm, fun(V) -> try_encode_val   (586, datetm, V) end};
field_tag('TimeInForce'                       ) -> { 59, char  , fun(V) -> encode_fld_val59 ( 59, char  , V) end};
field_tag('OrdType'                           ) -> { 40, char  , fun(V) -> encode_fld_val40 ( 40, char  , V) end};
field_tag('CashDistribAgentAcctName'          ) -> {502, string, fun(V) -> try_encode_val   (502, string, V) end};
field_tag('LocationID'                        ) -> {283, string, fun(V) -> try_encode_val   (283, string, V) end};
field_tag('GapFillFlag'                       ) -> {123, bool  , fun(V) -> encode_fld_val123(123, bool  , V) end};
field_tag('EncodedIssuerLen'                  ) -> {348, length, fun(V) -> try_encode_val   (348, length, V) end};
field_tag('TotNoAllocs'                       ) -> {892, int   , fun(V) -> try_encode_val   (892, int   , V) end};
field_tag('CollInquiryResult'                 ) -> {946, int   , fun(V) -> encode_fld_val946(946, int   , V) end};
field_tag('MassStatusReqID'                   ) -> {584, string, fun(V) -> try_encode_val   (584, string, V) end};
field_tag('MktBidPx'                          ) -> {645, float , fun(V) -> try_encode_val   (645, float , V) end};
field_tag('RegistDtls'                        ) -> {509, string, fun(V) -> try_encode_val   (509, string, V) end};
field_tag('PositionEffect'                    ) -> { 77, char  , fun(V) -> encode_fld_val77 ( 77, char  , V) end};
field_tag('EncodedSecurityDesc'               ) -> {351, binary, fun(V) -> try_encode_val   (351, binary, V) end};
field_tag('PeggedPrice'                       ) -> {839, float , fun(V) -> try_encode_val   (839, float , V) end};
field_tag('RawDataLength'                     ) -> { 95, length, fun(V) -> try_encode_val   ( 95, length, V) end};
field_tag('IOINaturalFlag'                    ) -> {130, bool  , fun(V) -> encode_fld_val130(130, bool  , V) end};
field_tag('NoQuoteSets'                       ) -> {296, group , fun(V) -> try_encode_group (        296, V) end};
field_tag('TotNumTradeReports'                ) -> {748, int   , fun(V) -> try_encode_val   (748, int   , V) end};
field_tag('MDEntryPositionNo'                 ) -> {290, int   , fun(V) -> try_encode_val   (290, int   , V) end};
field_tag('OrigClOrdID'                       ) -> { 41, string, fun(V) -> try_encode_val   ( 41, string, V) end};
field_tag('CashDistribCurr'                   ) -> {478, string, fun(V) -> try_encode_val   (478, string, V) end};
field_tag('PegScope'                          ) -> {840, int   , fun(V) -> encode_fld_val840(840, int   , V) end};
field_tag('OwnerType'                         ) -> {522, int   , fun(V) -> encode_fld_val522(522, int   , V) end};
field_tag('DiscretionOffsetValue'             ) -> {389, float , fun(V) -> try_encode_val   (389, float , V) end};
field_tag('SettlInstRefID'                    ) -> {214, string, fun(V) -> try_encode_val   (214, string, V) end};
field_tag('PegOffsetType'                     ) -> {836, int   , fun(V) -> encode_fld_val836(836, int   , V) end};
field_tag('NoAllocs'                          ) -> { 78, group , fun(V) -> try_encode_group (         78, V) end};
field_tag('FairValue'                         ) -> {406, float , fun(V) -> try_encode_val   (406, float , V) end};
field_tag('BenchmarkSecurityID'               ) -> {699, string, fun(V) -> try_encode_val   (699, string, V) end};
field_tag('UnderlyingStateOrProvinceOfIssue'  ) -> {593, string, fun(V) -> try_encode_val   (593, string, V) end};
field_tag('MassCancelResponse'                ) -> {531, char  , fun(V) -> encode_fld_val531(531, char  , V) end};
field_tag('TradingSessionSubID'               ) -> {625, string, fun(V) -> try_encode_val   (625, string, V) end};
field_tag('AllocSettlCurrAmt'                 ) -> {737, float , fun(V) -> try_encode_val   (737, float , V) end};
field_tag('NoRoutingIDs'                      ) -> {215, group , fun(V) -> try_encode_group (        215, V) end};
field_tag('CollAsgnRespType'                  ) -> {905, int   , fun(V) -> encode_fld_val905(905, int   , V) end};
field_tag('EndCash'                           ) -> {922, float , fun(V) -> try_encode_val   (922, float , V) end};
field_tag('CardIssNum'                        ) -> {491, string, fun(V) -> try_encode_val   (491, string, V) end};
field_tag('NumTickets'                        ) -> {395, int   , fun(V) -> try_encode_val   (395, int   , V) end};
field_tag('Currency'                          ) -> { 15, string, fun(V) -> try_encode_val   ( 15, string, V) end};
field_tag('EncodedTextLen'                    ) -> {354, length, fun(V) -> try_encode_val   (354, length, V) end};
field_tag('UnderlyingRepurchaseTerm'          ) -> {244, int   , fun(V) -> try_encode_val   (244, int   , V) end};
field_tag('LegBenchmarkCurvePoint'            ) -> {678, string, fun(V) -> try_encode_val   (678, string, V) end};
field_tag('LegSecurityIDSource'               ) -> {603, string, fun(V) -> try_encode_val   (603, string, V) end};
field_tag('Subject'                           ) -> {147, string, fun(V) -> try_encode_val   (147, string, V) end};
field_tag('CumQty'                            ) -> { 14, float , fun(V) -> try_encode_val   ( 14, float , V) end};
field_tag('AllowableOneSidednessValue'        ) -> {766, float , fun(V) -> try_encode_val   (766, float , V) end};
field_tag('CashDistribAgentAcctNumber'        ) -> {500, string, fun(V) -> try_encode_val   (500, string, V) end};
field_tag('TotNoRelatedSym'                   ) -> {393, int   , fun(V) -> try_encode_val   (393, int   , V) end};
field_tag('UnderlyingTradingSessionSubID'     ) -> {823, string, fun(V) -> try_encode_val   (823, string, V) end};
field_tag('LegCouponPaymentDate'              ) -> {248, string, fun(V) -> try_encode_val   (248, string, V) end};
field_tag('Pool'                              ) -> {691, string, fun(V) -> try_encode_val   (691, string, V) end};
field_tag('CashDistribAgentName'              ) -> {498, string, fun(V) -> try_encode_val   (498, string, V) end};
field_tag('NoLegs'                            ) -> {555, group , fun(V) -> try_encode_group (        555, V) end};
field_tag('HeartBtInt'                        ) -> {108, int   , fun(V) -> try_encode_val   (108, int   , V) end};
field_tag('BenchmarkPrice'                    ) -> {662, float , fun(V) -> try_encode_val   (662, float , V) end};
field_tag('TransactTime'                      ) -> { 60, datetm, fun(V) -> try_encode_val   ( 60, datetm, V) end};
field_tag('AdvId'                             ) -> {  2, string, fun(V) -> try_encode_val   (  2, string, V) end};
field_tag('LocaleOfIssue'                     ) -> {472, string, fun(V) -> try_encode_val   (472, string, V) end};
field_tag('MailingInst'                       ) -> {482, string, fun(V) -> try_encode_val   (482, string, V) end};
field_tag('AllocAcctIDSource'                 ) -> {661, int   , fun(V) -> try_encode_val   (661, int   , V) end};
field_tag('MarginRatio'                       ) -> {898, float , fun(V) -> try_encode_val   (898, float , V) end};
field_tag('CollInquiryID'                     ) -> {909, string, fun(V) -> try_encode_val   (909, string, V) end};
field_tag('LastUpdateTime'                    ) -> {779, datetm, fun(V) -> try_encode_val   (779, datetm, V) end};
field_tag('LastLiquidityInd'                  ) -> {851, int   , fun(V) -> encode_fld_val851(851, int   , V) end};
field_tag('SettlInstID'                       ) -> {162, string, fun(V) -> try_encode_val   (162, string, V) end};
field_tag('OrdRejReason'                      ) -> {103, int   , fun(V) -> encode_fld_val103(103, int   , V) end};
field_tag('QuoteStatusReqID'                  ) -> {649, string, fun(V) -> try_encode_val   (649, string, V) end};
field_tag('TargetStrategyPerformance'         ) -> {850, float , fun(V) -> try_encode_val   (850, float , V) end};
field_tag('LegIssueDate'                      ) -> {249, string, fun(V) -> try_encode_val   (249, string, V) end};
field_tag('CollAsgnTransType'                 ) -> {903, int   , fun(V) -> encode_fld_val903(903, int   , V) end};
field_tag('BeginSeqNo'                        ) -> {  7, int   , fun(V) -> try_encode_val   (  7, int   , V) end};
field_tag('SecurityStatusReqID'               ) -> {324, string, fun(V) -> try_encode_val   (324, string, V) end};
field_tag('TradSesMethod'                     ) -> {338, int   , fun(V) -> encode_fld_val338(338, int   , V) end};
field_tag('ClOrdLinkID'                       ) -> {583, string, fun(V) -> try_encode_val   (583, string, V) end};
field_tag('TradSesStartTime'                  ) -> {341, datetm, fun(V) -> try_encode_val   (341, datetm, V) end};
field_tag('StipulationType'                   ) -> {233, string, fun(V) -> encode_fld_val233(233, string, V) end};
field_tag('PosAmtType'                        ) -> {707, string, fun(V) -> encode_fld_val707(707, string, V) end};
field_tag('PartyRole'                         ) -> {452, int   , fun(V) -> encode_fld_val452(452, int   , V) end};
field_tag('OrigSendingTime'                   ) -> {122, datetm, fun(V) -> try_encode_val   (122, datetm, V) end};
field_tag('PrevClosePx'                       ) -> {140, float , fun(V) -> try_encode_val   (140, float , V) end};
field_tag('RefCompID'                         ) -> {930, string, fun(V) -> try_encode_val   (930, string, V) end};
field_tag('Nested3PartyRole'                  ) -> {951, int   , fun(V) -> try_encode_val   (951, int   , V) end};
field_tag('RptSeq'                            ) -> { 83, int   , fun(V) -> try_encode_val   ( 83, int   , V) end};
field_tag('AvgPxPrecision'                    ) -> { 74, int   , fun(V) -> try_encode_val   ( 74, int   , V) end};
field_tag('MDImplicitDelete'                  ) -> {547, bool  , fun(V) -> encode_fld_val547(547, bool  , V) end};
field_tag('SettlPartyIDSource'                ) -> {783, char  , fun(V) -> try_encode_val   (783, char  , V) end};
field_tag('LegAllocQty'                       ) -> {673, float , fun(V) -> try_encode_val   (673, float , V) end};
field_tag('EncodedText'                       ) -> {355, binary, fun(V) -> try_encode_val   (355, binary, V) end};
field_tag('DistribPercentage'                 ) -> {512, float , fun(V) -> try_encode_val   (512, float , V) end};
field_tag('MiscFeeType'                       ) -> {139, char  , fun(V) -> encode_fld_val139(139, char  , V) end};
field_tag('StopPx'                            ) -> { 99, float , fun(V) -> try_encode_val   ( 99, float , V) end};
field_tag('TestReqID'                         ) -> {112, string, fun(V) -> try_encode_val   (112, string, V) end};
field_tag('AutoAcceptIndicator'               ) -> {754, bool  , fun(V) -> try_encode_val   (754, bool  , V) end};
field_tag('CardNumber'                        ) -> {489, string, fun(V) -> try_encode_val   (489, string, V) end};
field_tag('BidTradeType'                      ) -> {418, char  , fun(V) -> encode_fld_val418(418, char  , V) end};
field_tag('NoLegSecurityAltID'                ) -> {604, group , fun(V) -> try_encode_group (        604, V) end};
field_tag('MDEntrySize'                       ) -> {271, float , fun(V) -> try_encode_val   (271, float , V) end};
field_tag('SecondaryTrdType'                  ) -> {855, int   , fun(V) -> try_encode_val   (855, int   , V) end};
field_tag('EndDate'                           ) -> {917, string, fun(V) -> try_encode_val   (917, string, V) end};
field_tag('AllocLinkID'                       ) -> {196, string, fun(V) -> try_encode_val   (196, string, V) end};
field_tag('UnderlyingSettlPrice'              ) -> {732, float , fun(V) -> try_encode_val   (732, float , V) end};
field_tag('NoBidComponents'                   ) -> {420, group , fun(V) -> try_encode_group (        420, V) end};
field_tag('Account'                           ) -> {  1, string, fun(V) -> try_encode_val   (  1, string, V) end};
field_tag('SecurityExchange'                  ) -> {207, string, fun(V) -> try_encode_val   (207, string, V) end};
field_tag('UnderlyingSecuritySubType'         ) -> {763, string, fun(V) -> try_encode_val   (763, string, V) end};
field_tag('CollStatus'                        ) -> {910, int   , fun(V) -> encode_fld_val910(910, int   , V) end};
field_tag('PossResend'                        ) -> { 97, bool  , fun(V) -> encode_fld_val97 ( 97, bool  , V) end};
field_tag('URLLink'                           ) -> {149, string, fun(V) -> try_encode_val   (149, string, V) end};
field_tag('TradeRequestResult'                ) -> {749, int   , fun(V) -> encode_fld_val749(749, int   , V) end};
field_tag('IOIQualifier'                      ) -> {104, char  , fun(V) -> encode_fld_val104(104, char  , V) end};
field_tag('SettlSessSubID'                    ) -> {717, string, fun(V) -> try_encode_val   (717, string, V) end};
field_tag('Product'                           ) -> {460, int   , fun(V) -> encode_fld_val460(460, int   , V) end};
field_tag('BeginString'                       ) -> {  8, string, fun(V) -> try_encode_val   (  8, string, V) end};
field_tag('BidSize'                           ) -> {134, float , fun(V) -> try_encode_val   (134, float , V) end};
field_tag('UnderlyingMaturityDate'            ) -> {542, string, fun(V) -> try_encode_val   (542, string, V) end};
field_tag('SecondaryOrderID'                  ) -> {198, string, fun(V) -> try_encode_val   (198, string, V) end};
field_tag('StatusValue'                       ) -> {928, int   , fun(V) -> encode_fld_val928(928, int   , V) end};
field_tag('UnsolicitedIndicator'              ) -> {325, bool  , fun(V) -> encode_fld_val325(325, bool  , V) end};
field_tag('AdvRefID'                          ) -> {  3, string, fun(V) -> try_encode_val   (  3, string, V) end};
field_tag('ExecRestatementReason'             ) -> {378, int   , fun(V) -> encode_fld_val378(378, int   , V) end};
field_tag('TradeDate'                         ) -> { 75, string, fun(V) -> try_encode_val   ( 75, string, V) end};
field_tag('NoCapacities'                      ) -> {862, group , fun(V) -> try_encode_group (        862, V) end};
field_tag('SecureDataLen'                     ) -> { 90, length, fun(V) -> try_encode_val   ( 90, length, V) end};
field_tag('EncodedSecurityDescLen'            ) -> {350, length, fun(V) -> try_encode_val   (350, length, V) end};
field_tag('SecondaryTradeReportRefID'         ) -> {881, string, fun(V) -> try_encode_val   (881, string, V) end};
field_tag('LegCouponRate'                     ) -> {615, float , fun(V) -> try_encode_val   (615, float , V) end};
field_tag('AllocNetMoney'                     ) -> {154, float , fun(V) -> try_encode_val   (154, float , V) end};
field_tag('RegistEmail'                       ) -> {511, string, fun(V) -> try_encode_val   (511, string, V) end};
field_tag('LeavesQty'                         ) -> {151, float , fun(V) -> try_encode_val   (151, float , V) end};
field_tag('Text'                              ) -> { 58, string, fun(V) -> try_encode_val   ( 58, string, V) end};
field_tag('OfferSize'                         ) -> {135, float , fun(V) -> try_encode_val   (135, float , V) end};
field_tag('SideMultiLegReportingType'         ) -> {752, int   , fun(V) -> encode_fld_val752(752, int   , V) end};
field_tag('Symbol'                            ) -> { 55, string, fun(V) -> try_encode_val   ( 55, string, V) end};
field_tag('LiquidityNumSecurities'            ) -> {441, int   , fun(V) -> try_encode_val   (441, int   , V) end};
field_tag('MDEntryTime'                       ) -> {273, datetm, fun(V) -> try_encode_val   (273, datetm, V) end};
field_tag('MDEntryOriginator'                 ) -> {282, string, fun(V) -> try_encode_val   (282, string, V) end};
field_tag('OrderInputDevice'                  ) -> {821, string, fun(V) -> try_encode_val   (821, string, V) end};
field_tag('EncodedSubject'                    ) -> {357, binary, fun(V) -> try_encode_val   (357, binary, V) end};
field_tag('Factor'                            ) -> {228, float , fun(V) -> try_encode_val   (228, float , V) end};
field_tag('Nested2PartyID'                    ) -> {757, string, fun(V) -> try_encode_val   (757, string, V) end};
field_tag('ContraryInstructionIndicator'      ) -> {719, bool  , fun(V) -> try_encode_val   (719, bool  , V) end};
field_tag('TradSesEndTime'                    ) -> {345, datetm, fun(V) -> try_encode_val   (345, datetm, V) end};
field_tag('MinTradeVol'                       ) -> {562, float , fun(V) -> try_encode_val   (562, float , V) end};
field_tag('ExpirationCycle'                   ) -> {827, int   , fun(V) -> encode_fld_val827(827, int   , V) end};
field_tag('CollAsgnID'                        ) -> {902, string, fun(V) -> try_encode_val   (902, string, V) end};
field_tag('ContraLegRefID'                    ) -> {655, string, fun(V) -> try_encode_val   (655, string, V) end};
field_tag('EncodedListStatusText'             ) -> {446, binary, fun(V) -> try_encode_val   (446, binary, V) end};
field_tag('HopSendingTime'                    ) -> {629, datetm, fun(V) -> try_encode_val   (629, datetm, V) end};
field_tag('BidForwardPoints'                  ) -> {189, float , fun(V) -> try_encode_val   (189, float , V) end};
field_tag('ExecID'                            ) -> { 17, string, fun(V) -> try_encode_val   ( 17, string, V) end};
field_tag('UnderlyingRepoCollateralSecurityType') -> {243, string, fun(V) -> try_encode_val   (243, string, V) end};
field_tag('ExchangeRule'                      ) -> {825, string, fun(V) -> try_encode_val   (825, string, V) end};
field_tag('SecondaryTradeReportID'            ) -> {818, string, fun(V) -> try_encode_val   (818, string, V) end};
field_tag('ResponseDestination'               ) -> {726, string, fun(V) -> try_encode_val   (726, string, V) end};
field_tag('FinancialStatus'                   ) -> {291, string, fun(V) -> encode_fld_val291(291, string, V) end};
field_tag('ConfirmTransType'                  ) -> {666, int   , fun(V) -> encode_fld_val666(666, int   , V) end};
field_tag('EncodedLegSecurityDesc'            ) -> {622, binary, fun(V) -> try_encode_val   (622, binary, V) end};
field_tag('OrigCrossID'                       ) -> {551, string, fun(V) -> try_encode_val   (551, string, V) end};
field_tag('PosMaintRptID'                     ) -> {721, string, fun(V) -> try_encode_val   (721, string, V) end};
field_tag('LegIndividualAllocID'              ) -> {672, string, fun(V) -> try_encode_val   (672, string, V) end};
field_tag('UnderlyingMaturityMonthYear'       ) -> {313, string, fun(V) -> try_encode_val   (313, string, V) end};
field_tag('MaxMessageSize'                    ) -> {383, length, fun(V) -> try_encode_val   (383, length, V) end};
field_tag('TradSesStatus'                     ) -> {340, int   , fun(V) -> encode_fld_val340(340, int   , V) end};
field_tag('NoQuoteQualifiers'                 ) -> {735, group , fun(V) -> try_encode_group (        735, V) end};
field_tag('UnderlyingStartValue'              ) -> {884, float , fun(V) -> try_encode_val   (884, float , V) end};
field_tag('LegRedemptionDate'                 ) -> {254, string, fun(V) -> try_encode_val   (254, string, V) end};
field_tag('MessageEncoding'                   ) -> {347, string, fun(V) -> encode_fld_val347(347, string, V) end};
field_tag('SecurityIDSource'                  ) -> { 22, string, fun(V) -> encode_fld_val22 ( 22, string, V) end};
field_tag('LegProduct'                        ) -> {607, int   , fun(V) -> try_encode_val   (607, int   , V) end};
field_tag('QuoteType'                         ) -> {537, int   , fun(V) -> encode_fld_val537(537, int   , V) end};
field_tag('ApplQueueDepth'                    ) -> {813, int   , fun(V) -> try_encode_val   (813, int   , V) end};
field_tag('AllocReportRefID'                  ) -> {795, string, fun(V) -> try_encode_val   (795, string, V) end};
field_tag('TradeRequestType'                  ) -> {569, int   , fun(V) -> encode_fld_val569(569, int   , V) end};
field_tag('DateOfBirth'                       ) -> {486, string, fun(V) -> try_encode_val   (486, string, V) end};
field_tag('LegPrice'                          ) -> {566, float , fun(V) -> try_encode_val   (566, float , V) end};
field_tag('LegOptAttribute'                   ) -> {613, char  , fun(V) -> try_encode_val   (613, char  , V) end};
field_tag('AffectedSecondaryOrderID'          ) -> {536, string, fun(V) -> try_encode_val   (536, string, V) end};
field_tag('CollAsgnRefID'                     ) -> {907, string, fun(V) -> try_encode_val   (907, string, V) end};
field_tag('PegMoveType'                       ) -> {835, int   , fun(V) -> encode_fld_val835(835, int   , V) end};
field_tag('LastForwardPoints2'                ) -> {641, float , fun(V) -> try_encode_val   (641, float , V) end};
field_tag('UserRequestType'                   ) -> {924, int   , fun(V) -> encode_fld_val924(924, int   , V) end};
field_tag('LegCurrency'                       ) -> {556, string, fun(V) -> try_encode_val   (556, string, V) end};
field_tag('DeliveryForm'                      ) -> {668, int   , fun(V) -> encode_fld_val668(668, int   , V) end};
field_tag('SecurityRequestType'               ) -> {321, int   , fun(V) -> encode_fld_val321(321, int   , V) end};
field_tag('QuoteSetID'                        ) -> {302, string, fun(V) -> try_encode_val   (302, string, V) end};
field_tag('BuyVolume'                         ) -> {330, float , fun(V) -> try_encode_val   (330, float , V) end};
field_tag('CopyMsgIndicator'                  ) -> {797, bool  , fun(V) -> try_encode_val   (797, bool  , V) end};
field_tag('MassCancelRejectReason'            ) -> {532, char  , fun(V) -> encode_fld_val532(532, char  , V) end};
field_tag('SendingTime'                       ) -> { 52, datetm, fun(V) -> try_encode_val   ( 52, datetm, V) end};
field_tag('RefMsgType'                        ) -> {372, string, fun(V) -> try_encode_val   (372, string, V) end};
field_tag('MaxFloor'                          ) -> {111, float , fun(V) -> try_encode_val   (111, float , V) end};
field_tag('BenchmarkCurveCurrency'            ) -> {220, string, fun(V) -> try_encode_val   (220, string, V) end};
field_tag('TrdType'                           ) -> {828, int   , fun(V) -> encode_fld_val828(828, int   , V) end};
field_tag('DiscretionMoveType'                ) -> {841, int   , fun(V) -> encode_fld_val841(841, int   , V) end};
field_tag('LegRatioQty'                       ) -> {623, float , fun(V) -> try_encode_val   (623, float , V) end};
field_tag('ListStatusType'                    ) -> {429, int   , fun(V) -> encode_fld_val429(429, int   , V) end};
field_tag('ThresholdAmount'                   ) -> {834, float , fun(V) -> try_encode_val   (834, float , V) end};
field_tag('NoTrdRegTimestamps'                ) -> {768, group , fun(V) -> try_encode_group (        768, V) end};
field_tag('LegSecurityAltID'                  ) -> {605, string, fun(V) -> try_encode_val   (605, string, V) end};
field_tag('ComplianceID'                      ) -> {376, string, fun(V) -> try_encode_val   (376, string, V) end};
field_tag('TotalVolumeTraded'                 ) -> {387, float , fun(V) -> try_encode_val   (387, float , V) end};
field_tag('UserStatusText'                    ) -> {927, string, fun(V) -> try_encode_val   (927, string, V) end};
field_tag('PegRoundDirection'                 ) -> {838, int   , fun(V) -> encode_fld_val838(838, int   , V) end};
field_tag('SettlCurrFxRate'                   ) -> {155, float , fun(V) -> try_encode_val   (155, float , V) end};
field_tag('TotalNumPosReports'                ) -> {727, int   , fun(V) -> try_encode_val   (727, int   , V) end};
field_tag('EncodedUnderlyingSecurityDescLen'  ) -> {364, length, fun(V) -> try_encode_val   (364, length, V) end};
field_tag('EncodedHeadlineLen'                ) -> {358, length, fun(V) -> try_encode_val   (358, length, V) end};
field_tag('HopCompID'                         ) -> {628, string, fun(V) -> try_encode_val   (628, string, V) end};
field_tag('OrderRestrictions'                 ) -> {529, string, fun(V) -> encode_fld_val529(529, string, V) end};
field_tag('AllocStatus'                       ) -> { 87, int   , fun(V) -> encode_fld_val87 ( 87, int   , V) end};
field_tag('UnderlyingContractMultiplier'      ) -> {436, float , fun(V) -> try_encode_val   (436, float , V) end};
field_tag('RoutingID'                         ) -> {217, string, fun(V) -> try_encode_val   (217, string, V) end};
field_tag('MinOfferSize'                      ) -> {648, float , fun(V) -> try_encode_val   (648, float , V) end};
field_tag('IndividualAllocID'                 ) -> {467, string, fun(V) -> try_encode_val   (467, string, V) end};
field_tag('UnderlyingCountryOfIssue'          ) -> {592, string, fun(V) -> try_encode_val   (592, string, V) end};
field_tag('AvgPxIndicator'                    ) -> {819, int   , fun(V) -> encode_fld_val819(819, int   , V) end};
field_tag('DeleteReason'                      ) -> {285, char  , fun(V) -> encode_fld_val285(285, char  , V) end};
field_tag('OfferSpotRate'                     ) -> {190, float , fun(V) -> try_encode_val   (190, float , V) end};
field_tag('LastFragment'                      ) -> {893, bool  , fun(V) -> encode_fld_val893(893, bool  , V) end};
field_tag('NoUnderlyingSecurityAltID'         ) -> {457, group , fun(V) -> try_encode_group (        457, V) end};
field_tag('HandlInst'                         ) -> { 21, char  , fun(V) -> encode_fld_val21 ( 21, char  , V) end};
field_tag('OutMainCntryUIndex'                ) -> {412, float , fun(V) -> try_encode_val   (412, float , V) end};
field_tag('RawData'                           ) -> { 96, binary, fun(V) -> try_encode_val   ( 96, binary, V) end};
field_tag('QuoteRequestType'                  ) -> {303, int   , fun(V) -> encode_fld_val303(303, int   , V) end};
field_tag('MaturityMonthYear'                 ) -> {200, string, fun(V) -> try_encode_val   (200, string, V) end};
field_tag('LegCountryOfIssue'                 ) -> {596, string, fun(V) -> try_encode_val   (596, string, V) end};
field_tag('AdvSide'                           ) -> {  4, char  , fun(V) -> encode_fld_val4  (  4, char  , V) end};
field_tag('BasisFeatureDate'                  ) -> {259, string, fun(V) -> try_encode_val   (259, string, V) end};
field_tag('DeliverToCompID'                   ) -> {128, string, fun(V) -> try_encode_val   (128, string, V) end};
field_tag('NoQuoteEntries'                    ) -> {295, group , fun(V) -> try_encode_group (        295, V) end};
field_tag('DayOrderQty'                       ) -> {424, float , fun(V) -> try_encode_val   (424, float , V) end};
field_tag('LegQty'                            ) -> {687, float , fun(V) -> try_encode_val   (687, float , V) end};
field_tag('AccruedInterestAmt'                ) -> {159, float , fun(V) -> try_encode_val   (159, float , V) end};
field_tag('StandInstDbType'                   ) -> {169, int   , fun(V) -> encode_fld_val169(169, int   , V) end};
field_tag('Nested3PartySubID'                 ) -> {953, string, fun(V) -> try_encode_val   (953, string, V) end};
field_tag('CancellationRights'                ) -> {480, char  , fun(V) -> encode_fld_val480(480, char  , V) end};
field_tag('RefTagID'                          ) -> {371, int   , fun(V) -> try_encode_val   (371, int   , V) end};
field_tag('CPRegType'                         ) -> {876, string, fun(V) -> try_encode_val   (876, string, V) end};
field_tag('PublishTrdIndicator'               ) -> {852, bool  , fun(V) -> encode_fld_val852(852, bool  , V) end};
field_tag('OddLot'                            ) -> {575, bool  , fun(V) -> encode_fld_val575(575, bool  , V) end};
field_tag('NoStrikes'                         ) -> {428, group , fun(V) -> try_encode_group (        428, V) end};
field_tag('ContractSettlMonth'                ) -> {667, string, fun(V) -> try_encode_val   (667, string, V) end};
field_tag('MDMkt'                             ) -> {275, string, fun(V) -> try_encode_val   (275, string, V) end};
field_tag('SettlCurrAmt'                      ) -> {119, float , fun(V) -> try_encode_val   (119, float , V) end};
field_tag('TradeLinkID'                       ) -> {820, string, fun(V) -> try_encode_val   (820, string, V) end};
field_tag('Headline'                          ) -> {148, string, fun(V) -> try_encode_val   (148, string, V) end};
field_tag('PosMaintAction'                    ) -> {712, int   , fun(V) -> encode_fld_val712(712, int   , V) end};
field_tag('NoRpts'                            ) -> { 82, int   , fun(V) -> try_encode_val   ( 82, int   , V) end};
field_tag('MailingDtls'                       ) -> {474, string, fun(V) -> try_encode_val   (474, string, V) end};
field_tag('DiscretionOffsetType'              ) -> {842, int   , fun(V) -> encode_fld_val842(842, int   , V) end};
field_tag('LegStrikePrice'                    ) -> {612, float , fun(V) -> try_encode_val   (612, float , V) end};
field_tag('NewSeqNo'                          ) -> { 36, int   , fun(V) -> try_encode_val   ( 36, int   , V) end};
field_tag('ContAmtType'                       ) -> {519, int   , fun(V) -> encode_fld_val519(519, int   , V) end};
field_tag('PosTransType'                      ) -> {709, int   , fun(V) -> encode_fld_val709(709, int   , V) end};
field_tag('AllocCancReplaceReason'            ) -> {796, int   , fun(V) -> encode_fld_val796(796, int   , V) end};
field_tag('IncTaxInd'                         ) -> {416, int   , fun(V) -> encode_fld_val416(416, int   , V) end};
field_tag('LegalConfirm'                      ) -> {650, bool  , fun(V) -> encode_fld_val650(650, bool  , V) end};
field_tag('UnderlyingOptAttribute'            ) -> {317, char  , fun(V) -> try_encode_val   (317, char  , V) end};
field_tag('MidYield'                          ) -> {633, float , fun(V) -> try_encode_val   (633, float , V) end};
field_tag('MiscFeeCurr'                       ) -> {138, string, fun(V) -> try_encode_val   (138, string, V) end};
field_tag('Quantity'                          ) -> { 53, float , fun(V) -> try_encode_val   ( 53, float , V) end};
field_tag('LegStipulationType'                ) -> {688, string, fun(V) -> try_encode_val   (688, string, V) end};
field_tag('LegAllocAcctIDSource'              ) -> {674, string, fun(V) -> try_encode_val   (674, string, V) end};
field_tag('ContAmtValue'                      ) -> {520, float , fun(V) -> try_encode_val   (520, float , V) end};
field_tag('SenderSubID'                       ) -> { 50, string, fun(V) -> try_encode_val   ( 50, string, V) end};
field_tag('CashOutstanding'                   ) -> {901, float , fun(V) -> try_encode_val   (901, float , V) end};
field_tag('TotNoSecurityTypes'                ) -> {557, int   , fun(V) -> try_encode_val   (557, int   , V) end};
field_tag('ListID'                            ) -> { 66, string, fun(V) -> try_encode_val   ( 66, string, V) end};
field_tag('SellerDays'                        ) -> {287, int   , fun(V) -> try_encode_val   (287, int   , V) end};
field_tag('StrikeTime'                        ) -> {443, datetm, fun(V) -> try_encode_val   (443, datetm, V) end};
field_tag('CheckSum'                          ) -> { 10, string, fun(V) -> try_encode_val   ( 10, string, V) end};
field_tag('ExecType'                          ) -> {150, char  , fun(V) -> encode_fld_val150(150, char  , V) end};
field_tag('SecondaryExecID'                   ) -> {527, string, fun(V) -> try_encode_val   (527, string, V) end};
field_tag('DiscretionScope'                   ) -> {846, int   , fun(V) -> encode_fld_val846(846, int   , V) end};
field_tag('ConfirmID'                         ) -> {664, string, fun(V) -> try_encode_val   (664, string, V) end};
field_tag('EventDate'                         ) -> {866, string, fun(V) -> try_encode_val   (866, string, V) end};
field_tag('BasisFeaturePrice'                 ) -> {260, float , fun(V) -> try_encode_val   (260, float , V) end};
field_tag('NoRegistDtls'                      ) -> {473, group , fun(V) -> try_encode_group (        473, V) end};
field_tag('NoCompIDs'                         ) -> {936, group , fun(V) -> try_encode_group (        936, V) end};
field_tag('LegRepurchaseRate'                 ) -> {252, float , fun(V) -> try_encode_val   (252, float , V) end};
field_tag('PosType'                           ) -> {703, string, fun(V) -> encode_fld_val703(703, string, V) end};
field_tag('LegStateOrProvinceOfIssue'         ) -> {597, string, fun(V) -> try_encode_val   (597, string, V) end};
field_tag('TimeBracket'                       ) -> {943, string, fun(V) -> try_encode_val   (943, string, V) end};
field_tag('StartCash'                         ) -> {921, float , fun(V) -> try_encode_val   (921, float , V) end};
field_tag('MsgType'                           ) -> { 35, string, fun(V) -> encode_fld_val35 ( 35, string, V) end};
field_tag('ConfirmReqID'                      ) -> {859, string, fun(V) -> try_encode_val   (859, string, V) end};
field_tag('UnderlyingSecurityType'            ) -> {310, string, fun(V) -> try_encode_val   (310, string, V) end};
field_tag('AllocID'                           ) -> { 70, string, fun(V) -> try_encode_val   ( 70, string, V) end};
field_tag('YieldRedemptionPriceType'          ) -> {698, int   , fun(V) -> try_encode_val   (698, int   , V) end};
field_tag('BidYield'                          ) -> {632, float , fun(V) -> try_encode_val   (632, float , V) end};
field_tag('NetGrossInd'                       ) -> {430, int   , fun(V) -> encode_fld_val430(430, int   , V) end};
field_tag('NoSettlPartySubIDs'                ) -> {801, group , fun(V) -> try_encode_group (        801, V) end};
field_tag('SecurityResponseID'                ) -> {322, string, fun(V) -> try_encode_val   (322, string, V) end};
field_tag('SideComplianceID'                  ) -> {659, string, fun(V) -> try_encode_val   (659, string, V) end};
field_tag('TargetCompID'                      ) -> { 56, string, fun(V) -> try_encode_val   ( 56, string, V) end};
field_tag('SecurityResponseType'              ) -> {323, int   , fun(V) -> encode_fld_val323(323, int   , V) end};
field_tag('AllocText'                         ) -> {161, string, fun(V) -> try_encode_val   (161, string, V) end};
field_tag('EncodedSubjectLen'                 ) -> {356, length, fun(V) -> try_encode_val   (356, length, V) end};
field_tag('PriceImprovement'                  ) -> {639, float , fun(V) -> try_encode_val   (639, float , V) end};
field_tag('Designation'                       ) -> {494, string, fun(V) -> try_encode_val   (494, string, V) end};
field_tag('NoInstrAttrib'                     ) -> {870, group , fun(V) -> try_encode_group (        870, V) end};
field_tag('PreallocMethod'                    ) -> {591, char  , fun(V) -> encode_fld_val591(591, char  , V) end};
field_tag('StateOrProvinceOfIssue'            ) -> {471, string, fun(V) -> try_encode_val   (471, string, V) end};
field_tag('SettlCurrOfferFxRate'              ) -> {657, float , fun(V) -> try_encode_val   (657, float , V) end};
field_tag('UnderlyingStipValue'               ) -> {889, string, fun(V) -> try_encode_val   (889, string, V) end};
field_tag('EncodedUnderlyingSecurityDesc'     ) -> {365, binary, fun(V) -> try_encode_val   (365, binary, V) end};
field_tag('LegInterestAccrualDate'            ) -> {956, string, fun(V) -> try_encode_val   (956, string, V) end};
field_tag('Username'                          ) -> {553, string, fun(V) -> try_encode_val   (553, string, V) end};
field_tag('TotNumAssignmentReports'           ) -> {832, int   , fun(V) -> try_encode_val   (832, int   , V) end};
field_tag('CxlRejReason'                      ) -> {102, int   , fun(V) -> encode_fld_val102(102, int   , V) end};
field_tag('DeskID'                            ) -> {284, string, fun(V) -> try_encode_val   (284, string, V) end};
field_tag('NoNested3PartySubIDs'              ) -> {952, group , fun(V) -> try_encode_group (        952, V) end};
field_tag('InterestAccrualDate'               ) -> {874, string, fun(V) -> try_encode_val   (874, string, V) end};
field_tag('SettlInstReqRejCode'               ) -> {792, int   , fun(V) -> encode_fld_val792(792, int   , V) end};
field_tag('CustOrderCapacity'                 ) -> {582, int   , fun(V) -> encode_fld_val582(582, int   , V) end};
field_tag('ClearingBusinessDate'              ) -> {715, string, fun(V) -> try_encode_val   (715, string, V) end};
field_tag('ReportedPx'                        ) -> {861, float , fun(V) -> try_encode_val   (861, float , V) end};
field_tag('TestMessageIndicator'              ) -> {464, bool  , fun(V) -> encode_fld_val464(464, bool  , V) end};
field_tag('Nested3PartySubIDType'             ) -> {954, int   , fun(V) -> try_encode_val   (954, int   , V) end};
field_tag('DefBidSize'                        ) -> {293, float , fun(V) -> try_encode_val   (293, float , V) end};
field_tag('SideValue1'                        ) -> {396, float , fun(V) -> try_encode_val   (396, float , V) end};
field_tag('ConfirmType'                       ) -> {773, int   , fun(V) -> encode_fld_val773(773, int   , V) end};
field_tag('LegCreditRating'                   ) -> {257, string, fun(V) -> try_encode_val   (257, string, V) end};
field_tag('GrossTradeAmt'                     ) -> {381, float , fun(V) -> try_encode_val   (381, float , V) end};
field_tag('EncodedUnderlyingIssuer'           ) -> {363, binary, fun(V) -> try_encode_val   (363, binary, V) end};
field_tag('ReversalIndicator'                 ) -> {700, bool  , fun(V) -> try_encode_val   (700, bool  , V) end};
field_tag('UnderlyingCurrentValue'            ) -> {885, float , fun(V) -> try_encode_val   (885, float , V) end};
field_tag('SenderLocationID'                  ) -> {142, string, fun(V) -> try_encode_val   (142, string, V) end};
field_tag('ValidUntilTime'                    ) -> { 62, datetm, fun(V) -> try_encode_val   ( 62, datetm, V) end};
field_tag('AllocType'                         ) -> {626, int   , fun(V) -> encode_fld_val626(626, int   , V) end};
field_tag('OfferForwardPoints2'               ) -> {643, float , fun(V) -> try_encode_val   (643, float , V) end};
field_tag('ContraTradeTime'                   ) -> {438, datetm, fun(V) -> try_encode_val   (438, datetm, V) end};
field_tag('NoSettlInst'                       ) -> {778, group , fun(V) -> try_encode_group (        778, V) end};
field_tag('LegBenchmarkPriceType'             ) -> {680, int   , fun(V) -> try_encode_val   (680, int   , V) end};
field_tag('EncodedListExecInst'               ) -> {353, binary, fun(V) -> try_encode_val   (353, binary, V) end};
field_tag('NextExpectedMsgSeqNum'             ) -> {789, int   , fun(V) -> try_encode_val   (789, int   , V) end};
field_tag('OrigPosReqRefID'                   ) -> {713, string, fun(V) -> try_encode_val   (713, string, V) end};
field_tag('MDReqID'                           ) -> {262, string, fun(V) -> try_encode_val   (262, string, V) end};
field_tag('ContraBroker'                      ) -> {375, string, fun(V) -> try_encode_val   (375, string, V) end};
field_tag('NoLegStipulations'                 ) -> {683, group , fun(V) -> try_encode_group (        683, V) end};
field_tag('LegSymbol'                         ) -> {600, string, fun(V) -> try_encode_val   (600, string, V) end};
field_tag('LegRepurchaseTerm'                 ) -> {251, int   , fun(V) -> try_encode_val   (251, int   , V) end};
field_tag('LegSecurityExchange'               ) -> {616, string, fun(V) -> try_encode_val   (616, string, V) end};
field_tag('AllocReportType'                   ) -> {794, int   , fun(V) -> encode_fld_val794(794, int   , V) end};
field_tag('NoUnderlyingStips'                 ) -> {887, group , fun(V) -> try_encode_group (        887, V) end};
field_tag('DKReason'                          ) -> {127, char  , fun(V) -> encode_fld_val127(127, char  , V) end};
field_tag('UnderlyingCPProgram'               ) -> {877, string, fun(V) -> try_encode_val   (877, string, V) end};
field_tag('RedemptionDate'                    ) -> {240, string, fun(V) -> try_encode_val   (240, string, V) end};
field_tag('NoUnderlyings'                     ) -> {711, group , fun(V) -> try_encode_group (        711, V) end};
field_tag('SecurityAltID'                     ) -> {455, string, fun(V) -> try_encode_val   (455, string, V) end};
field_tag('Nested2PartyRole'                  ) -> {759, int   , fun(V) -> try_encode_val   (759, int   , V) end};
field_tag('EventType'                         ) -> {865, int   , fun(V) -> encode_fld_val865(865, int   , V) end};
field_tag('OrderBookingQty'                   ) -> {800, float , fun(V) -> try_encode_val   (800, float , V) end};
field_tag('EncodedHeadline'                   ) -> {359, binary, fun(V) -> try_encode_val   (359, binary, V) end};
field_tag('ClearingInstruction'               ) -> {577, int   , fun(V) -> encode_fld_val577(577, int   , V) end};
field_tag('ExpireDate'                        ) -> {432, string, fun(V) -> try_encode_val   (432, string, V) end};
field_tag('LegOfferPx'                        ) -> {684, float , fun(V) -> try_encode_val   (684, float , V) end};
field_tag('ForexReq'                          ) -> {121, bool  , fun(V) -> encode_fld_val121(121, bool  , V) end};
field_tag('PriceDelta'                        ) -> {811, float , fun(V) -> try_encode_val   (811, float , V) end};
field_tag('SenderCompID'                      ) -> { 49, string, fun(V) -> try_encode_val   ( 49, string, V) end};
field_tag('UnderlyingEndValue'                ) -> {886, float , fun(V) -> try_encode_val   (886, float , V) end};
field_tag('ResponseTransportType'             ) -> {725, int   , fun(V) -> encode_fld_val725(725, int   , V) end};
field_tag('QuoteRejectReason'                 ) -> {300, int   , fun(V) -> encode_fld_val300(300, int   , V) end};
field_tag('CardExpDate'                       ) -> {490, string, fun(V) -> try_encode_val   (490, string, V) end};
field_tag('SettlInstMode'                     ) -> {160, char  , fun(V) -> encode_fld_val160(160, char  , V) end};
field_tag('ExecValuationPoint'                ) -> {515, datetm, fun(V) -> try_encode_val   (515, datetm, V) end};
field_tag('EventText'                         ) -> {868, string, fun(V) -> try_encode_val   (868, string, V) end};
field_tag('UnderlyingPx'                      ) -> {810, float , fun(V) -> try_encode_val   (810, float , V) end};
field_tag('MarginExcess'                      ) -> {899, float , fun(V) -> try_encode_val   (899, float , V) end};
field_tag('IOIQty'                            ) -> { 27, string, fun(V) -> encode_fld_val27 ( 27, string, V) end};
field_tag('MDReqRejReason'                    ) -> {281, char  , fun(V) -> encode_fld_val281(281, char  , V) end};
field_tag('NoPositions'                       ) -> {702, group , fun(V) -> try_encode_group (        702, V) end};
field_tag('AllocReportID'                     ) -> {755, string, fun(V) -> try_encode_val   (755, string, V) end};
field_tag('ExecPriceType'                     ) -> {484, char  , fun(V) -> encode_fld_val484(484, char  , V) end};
field_tag('QuoteEntryID'                      ) -> {299, string, fun(V) -> try_encode_val   (299, string, V) end};
field_tag('CxlQty'                            ) -> { 84, float , fun(V) -> try_encode_val   ( 84, float , V) end};
field_tag('EmailType'                         ) -> { 94, char  , fun(V) -> encode_fld_val94 ( 94, char  , V) end};
field_tag('InstrAttribType'                   ) -> {871, int   , fun(V) -> encode_fld_val871(871, int   , V) end};
field_tag('UnderlyingCreditRating'            ) -> {256, string, fun(V) -> try_encode_val   (256, string, V) end};
field_tag('BenchmarkSecurityIDSource'         ) -> {761, string, fun(V) -> try_encode_val   (761, string, V) end};
field_tag('PutOrCall'                         ) -> {201, int   , fun(V) -> encode_fld_val201(201, int   , V) end};
field_tag('NotifyBrokerOfCredit'              ) -> {208, bool  , fun(V) -> encode_fld_val208(208, bool  , V) end};
field_tag('OrigTime'                          ) -> { 42, datetm, fun(V) -> try_encode_val   ( 42, datetm, V) end};
field_tag('ExecRefID'                         ) -> { 19, string, fun(V) -> try_encode_val   ( 19, string, V) end};
field_tag('LegMaturityDate'                   ) -> {611, string, fun(V) -> try_encode_val   (611, string, V) end};
field_tag('ExchangeForPhysical'               ) -> {411, bool  , fun(V) -> encode_fld_val411(411, bool  , V) end};
field_tag('PartySubIDType'                    ) -> {803, int   , fun(V) -> encode_fld_val803(803, int   , V) end};
field_tag('DayBookingInst'                    ) -> {589, char  , fun(V) -> encode_fld_val589(589, char  , V) end};
field_tag('ListSeqNo'                         ) -> { 67, int   , fun(V) -> try_encode_val   ( 67, int   , V) end};
field_tag('MDUpdateAction'                    ) -> {279, char  , fun(V) -> encode_fld_val279(279, char  , V) end};
field_tag('SignatureLength'                   ) -> { 93, length, fun(V) -> try_encode_val   ( 93, length, V) end};
field_tag('TradeReportRejectReason'           ) -> {751, int   , fun(V) -> encode_fld_val751(751, int   , V) end};
field_tag('StipulationValue'                  ) -> {234, string, fun(V) -> try_encode_val   (234, string, V) end};
field_tag('Nested2PartySubIDType'             ) -> {807, int   , fun(V) -> try_encode_val   (807, int   , V) end};
field_tag('Issuer'                            ) -> {106, string, fun(V) -> try_encode_val   (106, string, V) end};
field_tag('SymbolSfx'                         ) -> { 65, string, fun(V) -> try_encode_val   ( 65, string, V) end};
field_tag('TotNoStrikes'                      ) -> {422, int   , fun(V) -> try_encode_val   (422, int   , V) end};
field_tag('PreviouslyReported'                ) -> {570, bool  , fun(V) -> encode_fld_val570(570, bool  , V) end};
field_tag('UnderlyingSecurityAltIDSource'     ) -> {459, string, fun(V) -> try_encode_val   (459, string, V) end};
field_tag('UnderlyingCurrency'                ) -> {318, string, fun(V) -> try_encode_val   (318, string, V) end};
field_tag('QuoteCancelType'                   ) -> {298, int   , fun(V) -> encode_fld_val298(298, int   , V) end};
field_tag('RoundLot'                          ) -> {561, float , fun(V) -> try_encode_val   (561, float , V) end};
field_tag('NoPosAmt'                          ) -> {753, group , fun(V) -> try_encode_group (        753, V) end};
field_tag('UnderlyingCouponPaymentDate'       ) -> {241, string, fun(V) -> try_encode_val   (241, string, V) end};
field_tag('ApplQueueMax'                      ) -> {812, int   , fun(V) -> try_encode_val   (812, int   , V) end};
field_tag('SettlCurrFxRateCalc'               ) -> {156, char  , fun(V) -> encode_fld_val156(156, char  , V) end};
field_tag('QuoteStatus'                       ) -> {297, int   , fun(V) -> encode_fld_val297(297, int   , V) end};
field_tag('ContractMultiplier'                ) -> {231, float , fun(V) -> try_encode_val   (231, float , V) end};
field_tag('StrikePrice'                       ) -> {202, float , fun(V) -> try_encode_val   (202, float , V) end};
field_tag('EndAccruedInterestAmt'             ) -> {920, float , fun(V) -> try_encode_val   (920, float , V) end};
field_tag('QuoteEntryRejectReason'            ) -> {368, int   , fun(V) -> try_encode_val   (368, int   , V) end};
field_tag('SettlPartySubID'                   ) -> {785, string, fun(V) -> try_encode_val   (785, string, V) end};
field_tag('NetworkRequestID'                  ) -> {933, string, fun(V) -> try_encode_val   (933, string, V) end};
field_tag('QuoteResponseLevel'                ) -> {301, int   , fun(V) -> encode_fld_val301(301, int   , V) end};
field_tag('AllocInterestAtMaturity'           ) -> {741, float , fun(V) -> try_encode_val   (741, float , V) end};
field_tag('DeliverToSubID'                    ) -> {129, string, fun(V) -> try_encode_val   (129, string, V) end};
field_tag('TrdMatchID'                        ) -> {880, string, fun(V) -> try_encode_val   (880, string, V) end};
field_tag('AllowableOneSidednessCurr'         ) -> {767, string, fun(V) -> try_encode_val   (767, string, V) end};
field_tag('LocateReqd'                        ) -> {114, bool  , fun(V) -> encode_fld_val114(114, bool  , V) end};
field_tag('NetMoney'                          ) -> {118, float , fun(V) -> try_encode_val   (118, float , V) end};
field_tag('PosReqStatus'                      ) -> {729, int   , fun(V) -> encode_fld_val729(729, int   , V) end};
field_tag('UnderlyingLastPx'                  ) -> {651, float , fun(V) -> try_encode_val   (651, float , V) end};
field_tag('TradeReportTransType'              ) -> {487, int   , fun(V) -> try_encode_val   (487, int   , V) end};
field_tag('AdvTransType'                      ) -> {  5, string, fun(V) -> encode_fld_val5  (  5, string, V) end};
field_tag('RegistRejReasonText'               ) -> {496, string, fun(V) -> try_encode_val   (496, string, V) end};
field_tag('LegContractSettlMonth'             ) -> {955, string, fun(V) -> try_encode_val   (955, string, V) end};
field_tag('NoBidDescriptors'                  ) -> {398, group , fun(V) -> try_encode_group (        398, V) end};
field_tag('LegCoveredOrUncovered'             ) -> {565, int   , fun(V) -> try_encode_val   (565, int   , V) end};
field_tag('LowPx'                             ) -> {333, float , fun(V) -> try_encode_val   (333, float , V) end};
field_tag('UnderlyingSecurityDesc'            ) -> {307, string, fun(V) -> try_encode_val   (307, string, V) end};
field_tag('LegAllocAccount'                   ) -> {671, string, fun(V) -> try_encode_val   (671, string, V) end};
field_tag('MoneyLaunderingStatus'             ) -> {481, char  , fun(V) -> encode_fld_val481(481, char  , V) end};
field_tag('UnderlyingStrikePrice'             ) -> {316, float , fun(V) -> try_encode_val   (316, float , V) end};
field_tag('SettlPrice'                        ) -> {730, float , fun(V) -> try_encode_val   (730, float , V) end};
field_tag('AllocAccruedInterestAmt'           ) -> {742, float , fun(V) -> try_encode_val   (742, float , V) end};
field_tag('AllocNoOrdersType'                 ) -> {857, int   , fun(V) -> encode_fld_val857(857, int   , V) end};
field_tag('Side'                              ) -> { 54, char  , fun(V) -> encode_fld_val54 ( 54, char  , V) end};
field_tag('OutsideIndexPct'                   ) -> {407, float , fun(V) -> try_encode_val   (407, float , V) end};
field_tag('SecuritySubType'                   ) -> {762, string, fun(V) -> try_encode_val   (762, string, V) end};
field_tag('LiquidityPctHigh'                  ) -> {403, float , fun(V) -> try_encode_val   (403, float , V) end};
field_tag('ExecInst'                          ) -> { 18, string, fun(V) -> encode_fld_val18 ( 18, string, V) end};
field_tag('ProgPeriodInterval'                ) -> {415, int   , fun(V) -> try_encode_val   (415, int   , V) end};
field_tag('NoSecurityTypes'                   ) -> {558, group , fun(V) -> try_encode_group (        558, V) end};
field_tag('TotNoQuoteEntries'                 ) -> {304, int   , fun(V) -> try_encode_val   (304, int   , V) end};
field_tag('MaxShow'                           ) -> {210, float , fun(V) -> try_encode_val   (210, float , V) end};
field_tag('NestedPartyID'                     ) -> {524, string, fun(V) -> try_encode_val   (524, string, V) end};
field_tag('TradeRequestStatus'                ) -> {750, int   , fun(V) -> encode_fld_val750(750, int   , V) end};
field_tag('AllocAccount'                      ) -> { 79, string, fun(V) -> try_encode_val   ( 79, string, V) end};
field_tag('NoMiscFees'                        ) -> {136, group , fun(V) -> try_encode_group (        136, V) end};
field_tag('SettlDeliveryType'                 ) -> {172, int   , fun(V) -> encode_fld_val172(172, int   , V) end};
field_tag('OrderAvgPx'                        ) -> {799, float , fun(V) -> try_encode_val   (799, float , V) end};
field_tag('AllocSettlCurrency'                ) -> {736, string, fun(V) -> try_encode_val   (736, string, V) end};
field_tag('DiscretionLimitType'               ) -> {843, int   , fun(V) -> encode_fld_val843(843, int   , V) end};
field_tag('RepurchaseRate'                    ) -> {227, float , fun(V) -> try_encode_val   (227, float , V) end};
field_tag('EncodedListStatusTextLen'          ) -> {445, length, fun(V) -> try_encode_val   (445, length, V) end};
field_tag('CashDistribPayRef'                 ) -> {501, string, fun(V) -> try_encode_val   (501, string, V) end};
field_tag('OnBehalfOfCompID'                  ) -> {115, string, fun(V) -> try_encode_val   (115, string, V) end};
field_tag('TradedFlatSwitch'                  ) -> {258, bool  , fun(V) -> encode_fld_val258(258, bool  , V) end};
field_tag('NestedPartyIDSource'               ) -> {525, char  , fun(V) -> try_encode_val   (525, char  , V) end};
field_tag('UnderlyingPutOrCall'               ) -> {315, int   , fun(V) -> try_encode_val   (315, int   , V) end};
field_tag('SettlPartyRole'                    ) -> {784, int   , fun(V) -> try_encode_val   (784, int   , V) end};
field_tag('SecurityDesc'                      ) -> {107, string, fun(V) -> try_encode_val   (107, string, V) end};
field_tag('HopRefID'                          ) -> {630, int   , fun(V) -> try_encode_val   (630, int   , V) end};
field_tag('NetworkRequestType'                ) -> {935, int   , fun(V) -> encode_fld_val935(935, int   , V) end};
field_tag('NoMDEntryTypes'                    ) -> {267, group , fun(V) -> try_encode_group (        267, V) end};
field_tag('DefOfferSize'                      ) -> {294, float , fun(V) -> try_encode_val   (294, float , V) end};
field_tag('PartyIDSource'                     ) -> {447, char  , fun(V) -> encode_fld_val447(447, char  , V) end};
field_tag('DatedDate'                         ) -> {873, string, fun(V) -> try_encode_val   (873, string, V) end};
field_tag('LegSymbolSfx'                      ) -> {601, string, fun(V) -> try_encode_val   (601, string, V) end};
field_tag('EmailThreadID'                     ) -> {164, string, fun(V) -> try_encode_val   (164, string, V) end};
field_tag('TradeLegRefID'                     ) -> {824, string, fun(V) -> try_encode_val   (824, string, V) end};
field_tag('RepurchaseTerm'                    ) -> {226, int   , fun(V) -> try_encode_val   (226, int   , V) end};
field_tag('UnderlyingSettlPriceType'          ) -> {733, int   , fun(V) -> try_encode_val   (733, int   , V) end};
field_tag('CollRptID'                         ) -> {908, string, fun(V) -> try_encode_val   (908, string, V) end};
field_tag('YieldRedemptionPrice'              ) -> {697, float , fun(V) -> try_encode_val   (697, float , V) end};
field_tag('SettlPartySubIDType'               ) -> {786, int   , fun(V) -> try_encode_val   (786, int   , V) end};
field_tag('UnderlyingDirtyPrice'              ) -> {882, float , fun(V) -> try_encode_val   (882, float , V) end};
field_tag('EndSeqNo'                          ) -> { 16, int   , fun(V) -> try_encode_val   ( 16, int   , V) end};
field_tag('ExerciseMethod'                    ) -> {747, char  , fun(V) -> encode_fld_val747(747, char  , V) end};
field_tag(_) -> erlang:error(badarg).


decode_fld_val4(Val) ->
  case Val of
    <<"B">> -> 'Buy'  ; %% 0
    <<"S">> -> 'Sell' ; %% 1
    <<"X">> -> 'Cross'; %% 2
    <<"T">> -> 'Trade'; %% 3
    _       -> Val
  end.

encode_fld_val4(ID,_T, 'Buy'  ) -> encode_tagval(ID, <<"B">>);
encode_fld_val4(ID,_T, 'Sell' ) -> encode_tagval(ID, <<"S">>);
encode_fld_val4(ID,_T, 'Cross') -> encode_tagval(ID, <<"X">>);
encode_fld_val4(ID,_T, 'Trade') -> encode_tagval(ID, <<"T">>);
encode_fld_val4(ID, T, V      ) -> try_encode_val(ID, T, V).

decode_fld_val5(Val) ->
  case Val of
    <<"N">> -> 'New'    ; %% 0
    <<"C">> -> 'Cancel' ; %% 1
    <<"R">> -> 'Replace'; %% 2
    _       -> Val
  end.

encode_fld_val5(ID,_T, 'New'    ) -> encode_tagval(ID, <<"N">>);
encode_fld_val5(ID,_T, 'Cancel' ) -> encode_tagval(ID, <<"C">>);
encode_fld_val5(ID,_T, 'Replace') -> encode_tagval(ID, <<"R">>);
encode_fld_val5(ID, T, V        ) -> try_encode_val(ID, T, V).

decode_fld_val13(Val) ->
  case Val of
    <<"1">> -> 'PerUnit'                                        ; %% 0
    <<"2">> -> 'Percentage'                                     ; %% 1
    <<"3">> -> 'Absolute'                                       ; %% 2
    <<"4">> -> '4'                                              ; %% 3
    <<"5">> -> '5'                                              ; %% 4
    <<"6">> -> 'PointsPerBondOrContractSupplyContractmultiplier'; %% 5
    _       -> Val
  end.

encode_fld_val13(ID,_T, 'PerUnit'                                        ) -> encode_tagval(ID, <<"1">>);
encode_fld_val13(ID,_T, 'Percentage'                                     ) -> encode_tagval(ID, <<"2">>);
encode_fld_val13(ID,_T, 'Absolute'                                       ) -> encode_tagval(ID, <<"3">>);
encode_fld_val13(ID,_T, '4'                                              ) -> encode_tagval(ID, <<"4">>);
encode_fld_val13(ID,_T, '5'                                              ) -> encode_tagval(ID, <<"5">>);
encode_fld_val13(ID,_T, 'PointsPerBondOrContractSupplyContractmultiplier') -> encode_tagval(ID, <<"6">>);
encode_fld_val13(ID, T, V                                                ) -> try_encode_val(ID, T, V).

decode_fld_val18(Val) ->
  case Val of
    <<"1">> -> 'NotHeld'                   ; %% 0
    <<"2">> -> 'Work'                      ; %% 1
    <<"3">> -> 'GoAlong'                   ; %% 2
    <<"4">> -> 'OverTheDay'                ; %% 3
    <<"5">> -> 'Held'                      ; %% 4
    <<"6">> -> 'ParticipateDontInitiate'   ; %% 5
    <<"7">> -> 'StrictScale'               ; %% 6
    <<"8">> -> 'TryToScale'                ; %% 7
    <<"9">> -> 'StayOnBidside'             ; %% 8
    <<"0">> -> 'StayOnOfferside'           ; %% 9
    <<"A">> -> 'NoCross'                   ; %% 10
    <<"B">> -> 'OkToCross'                 ; %% 11
    <<"C">> -> 'CallFirst'                 ; %% 12
    <<"D">> -> 'PercentOfVolume'           ; %% 13
    <<"E">> -> 'DoNotIncrease'             ; %% 14
    <<"F">> -> 'DoNotReduce'               ; %% 15
    <<"G">> -> 'AllOrNone'                 ; %% 16
    <<"H">> -> 'ReinstateOnSystemFailure'  ; %% 17
    <<"I">> -> 'InstitutionsOnly'          ; %% 18
    <<"J">> -> 'ReinstateOnTradingHalt'    ; %% 19
    <<"K">> -> 'CancelOnTradingHalt'       ; %% 20
    <<"L">> -> 'LastPeg'                   ; %% 21
    <<"M">> -> 'MidPricePeg'               ; %% 22
    <<"N">> -> 'NonNegotiable'             ; %% 23
    <<"O">> -> 'OpeningPeg'                ; %% 24
    <<"P">> -> 'MarketPeg'                 ; %% 25
    <<"Q">> -> 'CancelOnSystemFailure'     ; %% 26
    <<"R">> -> 'PrimaryPeg'                ; %% 27
    <<"S">> -> 'Suspend'                   ; %% 28
    <<"U">> -> 'CustomerDisplayInstruction'; %% 29
    <<"V">> -> 'Netting'                   ; %% 30
    <<"W">> -> 'PegToVwap'                 ; %% 31
    <<"X">> -> 'TradeAlong'                ; %% 32
    <<"Y">> -> 'TryToStop'                 ; %% 33
    <<"Z">> -> 'CancelIfNotBest'           ; %% 34
    <<"a">> -> 'TrailingStopPeg'           ; %% 35
    <<"b">> -> 'StrictLimit'               ; %% 36
    <<"c">> -> 'IgnorePriceValidityChecks' ; %% 37
    <<"d">> -> 'PegToLimitPrice'           ; %% 38
    <<"e">> -> 'WorkToTargetStrategy'      ; %% 39
    _       -> Val
  end.

encode_fld_val18(ID,_T, 'NotHeld'                   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val18(ID,_T, 'Work'                      ) -> encode_tagval(ID, <<"2">>);
encode_fld_val18(ID,_T, 'GoAlong'                   ) -> encode_tagval(ID, <<"3">>);
encode_fld_val18(ID,_T, 'OverTheDay'                ) -> encode_tagval(ID, <<"4">>);
encode_fld_val18(ID,_T, 'Held'                      ) -> encode_tagval(ID, <<"5">>);
encode_fld_val18(ID,_T, 'ParticipateDontInitiate'   ) -> encode_tagval(ID, <<"6">>);
encode_fld_val18(ID,_T, 'StrictScale'               ) -> encode_tagval(ID, <<"7">>);
encode_fld_val18(ID,_T, 'TryToScale'                ) -> encode_tagval(ID, <<"8">>);
encode_fld_val18(ID,_T, 'StayOnBidside'             ) -> encode_tagval(ID, <<"9">>);
encode_fld_val18(ID,_T, 'StayOnOfferside'           ) -> encode_tagval(ID, <<"0">>);
encode_fld_val18(ID,_T, 'NoCross'                   ) -> encode_tagval(ID, <<"A">>);
encode_fld_val18(ID,_T, 'OkToCross'                 ) -> encode_tagval(ID, <<"B">>);
encode_fld_val18(ID,_T, 'CallFirst'                 ) -> encode_tagval(ID, <<"C">>);
encode_fld_val18(ID,_T, 'PercentOfVolume'           ) -> encode_tagval(ID, <<"D">>);
encode_fld_val18(ID,_T, 'DoNotIncrease'             ) -> encode_tagval(ID, <<"E">>);
encode_fld_val18(ID,_T, 'DoNotReduce'               ) -> encode_tagval(ID, <<"F">>);
encode_fld_val18(ID,_T, 'AllOrNone'                 ) -> encode_tagval(ID, <<"G">>);
encode_fld_val18(ID,_T, 'ReinstateOnSystemFailure'  ) -> encode_tagval(ID, <<"H">>);
encode_fld_val18(ID,_T, 'InstitutionsOnly'          ) -> encode_tagval(ID, <<"I">>);
encode_fld_val18(ID,_T, 'ReinstateOnTradingHalt'    ) -> encode_tagval(ID, <<"J">>);
encode_fld_val18(ID,_T, 'CancelOnTradingHalt'       ) -> encode_tagval(ID, <<"K">>);
encode_fld_val18(ID,_T, 'LastPeg'                   ) -> encode_tagval(ID, <<"L">>);
encode_fld_val18(ID,_T, 'MidPricePeg'               ) -> encode_tagval(ID, <<"M">>);
encode_fld_val18(ID,_T, 'NonNegotiable'             ) -> encode_tagval(ID, <<"N">>);
encode_fld_val18(ID,_T, 'OpeningPeg'                ) -> encode_tagval(ID, <<"O">>);
encode_fld_val18(ID,_T, 'MarketPeg'                 ) -> encode_tagval(ID, <<"P">>);
encode_fld_val18(ID,_T, 'CancelOnSystemFailure'     ) -> encode_tagval(ID, <<"Q">>);
encode_fld_val18(ID,_T, 'PrimaryPeg'                ) -> encode_tagval(ID, <<"R">>);
encode_fld_val18(ID,_T, 'Suspend'                   ) -> encode_tagval(ID, <<"S">>);
encode_fld_val18(ID,_T, 'CustomerDisplayInstruction') -> encode_tagval(ID, <<"U">>);
encode_fld_val18(ID,_T, 'Netting'                   ) -> encode_tagval(ID, <<"V">>);
encode_fld_val18(ID,_T, 'PegToVwap'                 ) -> encode_tagval(ID, <<"W">>);
encode_fld_val18(ID,_T, 'TradeAlong'                ) -> encode_tagval(ID, <<"X">>);
encode_fld_val18(ID,_T, 'TryToStop'                 ) -> encode_tagval(ID, <<"Y">>);
encode_fld_val18(ID,_T, 'CancelIfNotBest'           ) -> encode_tagval(ID, <<"Z">>);
encode_fld_val18(ID,_T, 'TrailingStopPeg'           ) -> encode_tagval(ID, <<"a">>);
encode_fld_val18(ID,_T, 'StrictLimit'               ) -> encode_tagval(ID, <<"b">>);
encode_fld_val18(ID,_T, 'IgnorePriceValidityChecks' ) -> encode_tagval(ID, <<"c">>);
encode_fld_val18(ID,_T, 'PegToLimitPrice'           ) -> encode_tagval(ID, <<"d">>);
encode_fld_val18(ID,_T, 'WorkToTargetStrategy'      ) -> encode_tagval(ID, <<"e">>);
encode_fld_val18(ID, T, V                           ) -> try_encode_val(ID, T, V).

decode_fld_val21(Val) ->
  case Val of
    <<"1">> -> 'AutomatedExecutionOrderPrivateNoBrokerIntervention'; %% 0
    <<"2">> -> 'AutomatedExecutionOrderPublicBrokerInterventionOk' ; %% 1
    <<"3">> -> 'ManualOrderBestExecution'                          ; %% 2
    _       -> Val
  end.

encode_fld_val21(ID,_T, 'AutomatedExecutionOrderPrivateNoBrokerIntervention') -> encode_tagval(ID, <<"1">>);
encode_fld_val21(ID,_T, 'AutomatedExecutionOrderPublicBrokerInterventionOk' ) -> encode_tagval(ID, <<"2">>);
encode_fld_val21(ID,_T, 'ManualOrderBestExecution'                          ) -> encode_tagval(ID, <<"3">>);
encode_fld_val21(ID, T, V                                                   ) -> try_encode_val(ID, T, V).

decode_fld_val22(Val) ->
  case Val of
    <<"1">> -> 'Cusip'                         ; %% 0
    <<"2">> -> 'Sedol'                         ; %% 1
    <<"3">> -> 'Quik'                          ; %% 2
    <<"4">> -> 'IsinNumber'                    ; %% 3
    <<"5">> -> 'RicCode'                       ; %% 4
    <<"6">> -> 'IsoCurrencyCode'               ; %% 5
    <<"7">> -> 'IsoCountryCode'                ; %% 6
    <<"8">> -> 'ExchangeSymbol'                ; %% 7
    <<"9">> -> 'ConsolidatedTapeAssociation'   ; %% 8
    <<"A">> -> 'BloombergSymbol'               ; %% 9
    <<"B">> -> 'Wertpapier'                    ; %% 10
    <<"C">> -> 'Dutch'                         ; %% 11
    <<"D">> -> 'Valoren'                       ; %% 12
    <<"E">> -> 'Sicovam'                       ; %% 13
    <<"F">> -> 'Belgian'                       ; %% 14
    <<"G">> -> 'Common'                        ; %% 15
    <<"H">> -> 'ClearingHouse'                 ; %% 16
    <<"I">> -> 'IsdaFpmlProductSpecification'  ; %% 17
    <<"J">> -> 'OptionsPriceReportingAuthority'; %% 18
    _       -> Val
  end.

encode_fld_val22(ID,_T, 'Cusip'                         ) -> encode_tagval(ID, <<"1">>);
encode_fld_val22(ID,_T, 'Sedol'                         ) -> encode_tagval(ID, <<"2">>);
encode_fld_val22(ID,_T, 'Quik'                          ) -> encode_tagval(ID, <<"3">>);
encode_fld_val22(ID,_T, 'IsinNumber'                    ) -> encode_tagval(ID, <<"4">>);
encode_fld_val22(ID,_T, 'RicCode'                       ) -> encode_tagval(ID, <<"5">>);
encode_fld_val22(ID,_T, 'IsoCurrencyCode'               ) -> encode_tagval(ID, <<"6">>);
encode_fld_val22(ID,_T, 'IsoCountryCode'                ) -> encode_tagval(ID, <<"7">>);
encode_fld_val22(ID,_T, 'ExchangeSymbol'                ) -> encode_tagval(ID, <<"8">>);
encode_fld_val22(ID,_T, 'ConsolidatedTapeAssociation'   ) -> encode_tagval(ID, <<"9">>);
encode_fld_val22(ID,_T, 'BloombergSymbol'               ) -> encode_tagval(ID, <<"A">>);
encode_fld_val22(ID,_T, 'Wertpapier'                    ) -> encode_tagval(ID, <<"B">>);
encode_fld_val22(ID,_T, 'Dutch'                         ) -> encode_tagval(ID, <<"C">>);
encode_fld_val22(ID,_T, 'Valoren'                       ) -> encode_tagval(ID, <<"D">>);
encode_fld_val22(ID,_T, 'Sicovam'                       ) -> encode_tagval(ID, <<"E">>);
encode_fld_val22(ID,_T, 'Belgian'                       ) -> encode_tagval(ID, <<"F">>);
encode_fld_val22(ID,_T, 'Common'                        ) -> encode_tagval(ID, <<"G">>);
encode_fld_val22(ID,_T, 'ClearingHouse'                 ) -> encode_tagval(ID, <<"H">>);
encode_fld_val22(ID,_T, 'IsdaFpmlProductSpecification'  ) -> encode_tagval(ID, <<"I">>);
encode_fld_val22(ID,_T, 'OptionsPriceReportingAuthority') -> encode_tagval(ID, <<"J">>);
encode_fld_val22(ID, T, V                               ) -> try_encode_val(ID, T, V).

decode_fld_val25(Val) ->
  case Val of
    <<"L">> -> 'Low'   ; %% 0
    <<"M">> -> 'Medium'; %% 1
    <<"H">> -> 'High'  ; %% 2
    _       -> Val
  end.

encode_fld_val25(ID,_T, 'Low'   ) -> encode_tagval(ID, <<"L">>);
encode_fld_val25(ID,_T, 'Medium') -> encode_tagval(ID, <<"M">>);
encode_fld_val25(ID,_T, 'High'  ) -> encode_tagval(ID, <<"H">>);
encode_fld_val25(ID, T, V       ) -> try_encode_val(ID, T, V).

decode_fld_val27(Val) ->
  case Val of
    <<"S">> -> 'Small' ; %% 0
    <<"M">> -> 'Medium'; %% 1
    <<"L">> -> 'Large' ; %% 2
    _       -> Val
  end.

encode_fld_val27(ID,_T, 'Small' ) -> encode_tagval(ID, <<"S">>);
encode_fld_val27(ID,_T, 'Medium') -> encode_tagval(ID, <<"M">>);
encode_fld_val27(ID,_T, 'Large' ) -> encode_tagval(ID, <<"L">>);
encode_fld_val27(ID, T, V       ) -> try_encode_val(ID, T, V).

decode_fld_val28(Val) ->
  case Val of
    <<"N">> -> 'New'    ; %% 0
    <<"C">> -> 'Cancel' ; %% 1
    <<"R">> -> 'Replace'; %% 2
    _       -> Val
  end.

encode_fld_val28(ID,_T, 'New'    ) -> encode_tagval(ID, <<"N">>);
encode_fld_val28(ID,_T, 'Cancel' ) -> encode_tagval(ID, <<"C">>);
encode_fld_val28(ID,_T, 'Replace') -> encode_tagval(ID, <<"R">>);
encode_fld_val28(ID, T, V        ) -> try_encode_val(ID, T, V).

decode_fld_val29(Val) ->
  case Val of
    <<"1">> -> 'Agent'           ; %% 0
    <<"2">> -> 'CrossAsAgent'    ; %% 1
    <<"3">> -> 'CrossAsPrincipal'; %% 2
    <<"4">> -> 'Principal'       ; %% 3
    _       -> Val
  end.

encode_fld_val29(ID,_T, 'Agent'           ) -> encode_tagval(ID, <<"1">>);
encode_fld_val29(ID,_T, 'CrossAsAgent'    ) -> encode_tagval(ID, <<"2">>);
encode_fld_val29(ID,_T, 'CrossAsPrincipal') -> encode_tagval(ID, <<"3">>);
encode_fld_val29(ID,_T, 'Principal'       ) -> encode_tagval(ID, <<"4">>);
encode_fld_val29(ID, T, V                 ) -> try_encode_val(ID, T, V).

decode_fld_val35(Val) ->
  case Val of
    <<"0" >> -> 'Heartbeat'                              ; %% 0
    <<"1" >> -> 'Testrequest'                            ; %% 1
    <<"2" >> -> 'Resendrequest'                          ; %% 2
    <<"3" >> -> 'Reject'                                 ; %% 3
    <<"4" >> -> 'Sequencereset'                          ; %% 4
    <<"5" >> -> 'Logout'                                 ; %% 5
    <<"6" >> -> 'Ioi'                                    ; %% 6
    <<"7" >> -> 'Advertisement'                          ; %% 7
    <<"8" >> -> 'Executionreport'                        ; %% 8
    <<"9" >> -> 'Ordercancelreject'                      ; %% 9
    <<"A" >> -> 'Logon'                                  ; %% 10
    <<"B" >> -> 'News'                                   ; %% 11
    <<"C" >> -> 'Email'                                  ; %% 12
    <<"D" >> -> 'Newordersingle'                         ; %% 13
    <<"E" >> -> 'Neworderlist'                           ; %% 14
    <<"F" >> -> 'Ordercancelrequest'                     ; %% 15
    <<"G" >> -> 'Ordercancelreplacerequest'              ; %% 16
    <<"H" >> -> 'Orderstatusrequest'                     ; %% 17
    <<"J" >> -> 'Allocationinstruction'                  ; %% 18
    <<"K" >> -> 'Listcancelrequest'                      ; %% 19
    <<"L" >> -> 'Listexecute'                            ; %% 20
    <<"M" >> -> 'Liststatusrequest'                      ; %% 21
    <<"N" >> -> 'Liststatus'                             ; %% 22
    <<"P" >> -> 'Allocationinstructionack'               ; %% 23
    <<"Q" >> -> 'Dontknowtrade'                          ; %% 24
    <<"R" >> -> 'Quoterequest'                           ; %% 25
    <<"S" >> -> 'Quote'                                  ; %% 26
    <<"T" >> -> 'Settlementinstructions'                 ; %% 27
    <<"V" >> -> 'Marketdatarequest'                      ; %% 28
    <<"W" >> -> 'Marketdatasnapshotfullrefresh'          ; %% 29
    <<"X" >> -> 'Marketdataincrementalrefresh'           ; %% 30
    <<"Y" >> -> 'Marketdatarequestreject'                ; %% 31
    <<"Z" >> -> 'Quotecancel'                            ; %% 32
    <<"a" >> -> 'Quotestatusrequest'                     ; %% 33
    <<"b" >> -> 'Massquoteacknowledgement'               ; %% 34
    <<"c" >> -> 'Securitydefinitionrequest'              ; %% 35
    <<"d" >> -> 'Securitydefinition'                     ; %% 36
    <<"e" >> -> 'Securitystatusrequest'                  ; %% 37
    <<"f" >> -> 'Securitystatus'                         ; %% 38
    <<"g" >> -> 'Tradingsessionstatusrequest'            ; %% 39
    <<"h" >> -> 'Tradingsessionstatus'                   ; %% 40
    <<"i" >> -> 'Massquote'                              ; %% 41
    <<"j" >> -> 'Businessmessagereject'                  ; %% 42
    <<"k" >> -> 'Bidrequest'                             ; %% 43
    <<"l" >> -> 'Bidresponse'                            ; %% 44
    <<"m" >> -> 'Liststrikeprice'                        ; %% 45
    <<"o" >> -> 'Registrationinstructions'               ; %% 46
    <<"p" >> -> 'Registrationinstructionsresponse'       ; %% 47
    <<"q" >> -> 'Ordermasscancelrequest'                 ; %% 48
    <<"r" >> -> 'Ordermasscancelreport'                  ; %% 49
    <<"s" >> -> 'Newordercross'                          ; %% 50
    <<"t" >> -> 'Crossordercancelreplacerequest'         ; %% 51
    <<"u" >> -> 'Crossordercancelrequest'                ; %% 52
    <<"v" >> -> 'Securitytyperequest'                    ; %% 53
    <<"w" >> -> 'Securitytypes'                          ; %% 54
    <<"x" >> -> 'Securitylistrequest'                    ; %% 55
    <<"y" >> -> 'Securitylist'                           ; %% 56
    <<"z" >> -> 'Derivativesecuritylistrequest'          ; %% 57
    <<"AA">> -> 'Derivativesecuritylist'                 ; %% 58
    <<"AB">> -> 'Newordermultileg'                       ; %% 59
    <<"AC">> -> 'Multilegordercancelreplace'             ; %% 60
    <<"AD">> -> 'Tradecapturereportrequest'              ; %% 61
    <<"AE">> -> 'Tradecapturereport'                     ; %% 62
    <<"AF">> -> 'Ordermassstatusrequest'                 ; %% 63
    <<"AG">> -> 'Quoterequestreject'                     ; %% 64
    <<"AH">> -> 'Rfqrequest'                             ; %% 65
    <<"AI">> -> 'Quotestatusreport'                      ; %% 66
    <<"AJ">> -> 'Quoteresponse'                          ; %% 67
    <<"AK">> -> 'Confirmation'                           ; %% 68
    <<"AL">> -> 'Positionmaintenancerequest'             ; %% 69
    <<"AM">> -> 'Positionmaintenancereport'              ; %% 70
    <<"AN">> -> 'Requestforpositions'                    ; %% 71
    <<"AO">> -> 'Requestforpositionsack'                 ; %% 72
    <<"AP">> -> 'Positionreport'                         ; %% 73
    <<"AQ">> -> 'Tradecapturereportrequestack'           ; %% 74
    <<"AR">> -> 'Tradecapturereportack'                  ; %% 75
    <<"AS">> -> 'Allocationreport'                       ; %% 76
    <<"AT">> -> 'Allocationreportack'                    ; %% 77
    <<"AU">> -> 'Confirmationack'                        ; %% 78
    <<"AV">> -> 'Settlementinstructionrequest'           ; %% 79
    <<"AW">> -> 'Assignmentreport'                       ; %% 80
    <<"AX">> -> 'Collateralrequest'                      ; %% 81
    <<"AY">> -> 'Collateralassignment'                   ; %% 82
    <<"AZ">> -> 'Collateralresponse'                     ; %% 83
    <<"BA">> -> 'Collateralreport'                       ; %% 84
    <<"BB">> -> 'Collateralinquiry'                      ; %% 85
    <<"BC">> -> 'Networkcounterpartysystemstatusrequest' ; %% 86
    <<"BD">> -> 'Networkcounterpartysystemstatusresponse'; %% 87
    <<"BE">> -> 'Userrequest'                            ; %% 88
    <<"BF">> -> 'Userresponse'                           ; %% 89
    <<"BG">> -> 'Collateralinquiryack'                   ; %% 90
    <<"BH">> -> 'Confirmationrequest'                    ; %% 91
    _        -> Val
  end.

encode_fld_val35(ID,_T, 'Heartbeat'                              ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val35(ID,_T, 'Testrequest'                            ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val35(ID,_T, 'Resendrequest'                          ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val35(ID,_T, 'Reject'                                 ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val35(ID,_T, 'Sequencereset'                          ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val35(ID,_T, 'Logout'                                 ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val35(ID,_T, 'Ioi'                                    ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val35(ID,_T, 'Advertisement'                          ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val35(ID,_T, 'Executionreport'                        ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val35(ID,_T, 'Ordercancelreject'                      ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val35(ID,_T, 'Logon'                                  ) -> encode_tagval(ID, <<"A" >>);
encode_fld_val35(ID,_T, 'News'                                   ) -> encode_tagval(ID, <<"B" >>);
encode_fld_val35(ID,_T, 'Email'                                  ) -> encode_tagval(ID, <<"C" >>);
encode_fld_val35(ID,_T, 'Newordersingle'                         ) -> encode_tagval(ID, <<"D" >>);
encode_fld_val35(ID,_T, 'Neworderlist'                           ) -> encode_tagval(ID, <<"E" >>);
encode_fld_val35(ID,_T, 'Ordercancelrequest'                     ) -> encode_tagval(ID, <<"F" >>);
encode_fld_val35(ID,_T, 'Ordercancelreplacerequest'              ) -> encode_tagval(ID, <<"G" >>);
encode_fld_val35(ID,_T, 'Orderstatusrequest'                     ) -> encode_tagval(ID, <<"H" >>);
encode_fld_val35(ID,_T, 'Allocationinstruction'                  ) -> encode_tagval(ID, <<"J" >>);
encode_fld_val35(ID,_T, 'Listcancelrequest'                      ) -> encode_tagval(ID, <<"K" >>);
encode_fld_val35(ID,_T, 'Listexecute'                            ) -> encode_tagval(ID, <<"L" >>);
encode_fld_val35(ID,_T, 'Liststatusrequest'                      ) -> encode_tagval(ID, <<"M" >>);
encode_fld_val35(ID,_T, 'Liststatus'                             ) -> encode_tagval(ID, <<"N" >>);
encode_fld_val35(ID,_T, 'Allocationinstructionack'               ) -> encode_tagval(ID, <<"P" >>);
encode_fld_val35(ID,_T, 'Dontknowtrade'                          ) -> encode_tagval(ID, <<"Q" >>);
encode_fld_val35(ID,_T, 'Quoterequest'                           ) -> encode_tagval(ID, <<"R" >>);
encode_fld_val35(ID,_T, 'Quote'                                  ) -> encode_tagval(ID, <<"S" >>);
encode_fld_val35(ID,_T, 'Settlementinstructions'                 ) -> encode_tagval(ID, <<"T" >>);
encode_fld_val35(ID,_T, 'Marketdatarequest'                      ) -> encode_tagval(ID, <<"V" >>);
encode_fld_val35(ID,_T, 'Marketdatasnapshotfullrefresh'          ) -> encode_tagval(ID, <<"W" >>);
encode_fld_val35(ID,_T, 'Marketdataincrementalrefresh'           ) -> encode_tagval(ID, <<"X" >>);
encode_fld_val35(ID,_T, 'Marketdatarequestreject'                ) -> encode_tagval(ID, <<"Y" >>);
encode_fld_val35(ID,_T, 'Quotecancel'                            ) -> encode_tagval(ID, <<"Z" >>);
encode_fld_val35(ID,_T, 'Quotestatusrequest'                     ) -> encode_tagval(ID, <<"a" >>);
encode_fld_val35(ID,_T, 'Massquoteacknowledgement'               ) -> encode_tagval(ID, <<"b" >>);
encode_fld_val35(ID,_T, 'Securitydefinitionrequest'              ) -> encode_tagval(ID, <<"c" >>);
encode_fld_val35(ID,_T, 'Securitydefinition'                     ) -> encode_tagval(ID, <<"d" >>);
encode_fld_val35(ID,_T, 'Securitystatusrequest'                  ) -> encode_tagval(ID, <<"e" >>);
encode_fld_val35(ID,_T, 'Securitystatus'                         ) -> encode_tagval(ID, <<"f" >>);
encode_fld_val35(ID,_T, 'Tradingsessionstatusrequest'            ) -> encode_tagval(ID, <<"g" >>);
encode_fld_val35(ID,_T, 'Tradingsessionstatus'                   ) -> encode_tagval(ID, <<"h" >>);
encode_fld_val35(ID,_T, 'Massquote'                              ) -> encode_tagval(ID, <<"i" >>);
encode_fld_val35(ID,_T, 'Businessmessagereject'                  ) -> encode_tagval(ID, <<"j" >>);
encode_fld_val35(ID,_T, 'Bidrequest'                             ) -> encode_tagval(ID, <<"k" >>);
encode_fld_val35(ID,_T, 'Bidresponse'                            ) -> encode_tagval(ID, <<"l" >>);
encode_fld_val35(ID,_T, 'Liststrikeprice'                        ) -> encode_tagval(ID, <<"m" >>);
encode_fld_val35(ID,_T, 'Registrationinstructions'               ) -> encode_tagval(ID, <<"o" >>);
encode_fld_val35(ID,_T, 'Registrationinstructionsresponse'       ) -> encode_tagval(ID, <<"p" >>);
encode_fld_val35(ID,_T, 'Ordermasscancelrequest'                 ) -> encode_tagval(ID, <<"q" >>);
encode_fld_val35(ID,_T, 'Ordermasscancelreport'                  ) -> encode_tagval(ID, <<"r" >>);
encode_fld_val35(ID,_T, 'Newordercross'                          ) -> encode_tagval(ID, <<"s" >>);
encode_fld_val35(ID,_T, 'Crossordercancelreplacerequest'         ) -> encode_tagval(ID, <<"t" >>);
encode_fld_val35(ID,_T, 'Crossordercancelrequest'                ) -> encode_tagval(ID, <<"u" >>);
encode_fld_val35(ID,_T, 'Securitytyperequest'                    ) -> encode_tagval(ID, <<"v" >>);
encode_fld_val35(ID,_T, 'Securitytypes'                          ) -> encode_tagval(ID, <<"w" >>);
encode_fld_val35(ID,_T, 'Securitylistrequest'                    ) -> encode_tagval(ID, <<"x" >>);
encode_fld_val35(ID,_T, 'Securitylist'                           ) -> encode_tagval(ID, <<"y" >>);
encode_fld_val35(ID,_T, 'Derivativesecuritylistrequest'          ) -> encode_tagval(ID, <<"z" >>);
encode_fld_val35(ID,_T, 'Derivativesecuritylist'                 ) -> encode_tagval(ID, <<"AA">>);
encode_fld_val35(ID,_T, 'Newordermultileg'                       ) -> encode_tagval(ID, <<"AB">>);
encode_fld_val35(ID,_T, 'Multilegordercancelreplace'             ) -> encode_tagval(ID, <<"AC">>);
encode_fld_val35(ID,_T, 'Tradecapturereportrequest'              ) -> encode_tagval(ID, <<"AD">>);
encode_fld_val35(ID,_T, 'Tradecapturereport'                     ) -> encode_tagval(ID, <<"AE">>);
encode_fld_val35(ID,_T, 'Ordermassstatusrequest'                 ) -> encode_tagval(ID, <<"AF">>);
encode_fld_val35(ID,_T, 'Quoterequestreject'                     ) -> encode_tagval(ID, <<"AG">>);
encode_fld_val35(ID,_T, 'Rfqrequest'                             ) -> encode_tagval(ID, <<"AH">>);
encode_fld_val35(ID,_T, 'Quotestatusreport'                      ) -> encode_tagval(ID, <<"AI">>);
encode_fld_val35(ID,_T, 'Quoteresponse'                          ) -> encode_tagval(ID, <<"AJ">>);
encode_fld_val35(ID,_T, 'Confirmation'                           ) -> encode_tagval(ID, <<"AK">>);
encode_fld_val35(ID,_T, 'Positionmaintenancerequest'             ) -> encode_tagval(ID, <<"AL">>);
encode_fld_val35(ID,_T, 'Positionmaintenancereport'              ) -> encode_tagval(ID, <<"AM">>);
encode_fld_val35(ID,_T, 'Requestforpositions'                    ) -> encode_tagval(ID, <<"AN">>);
encode_fld_val35(ID,_T, 'Requestforpositionsack'                 ) -> encode_tagval(ID, <<"AO">>);
encode_fld_val35(ID,_T, 'Positionreport'                         ) -> encode_tagval(ID, <<"AP">>);
encode_fld_val35(ID,_T, 'Tradecapturereportrequestack'           ) -> encode_tagval(ID, <<"AQ">>);
encode_fld_val35(ID,_T, 'Tradecapturereportack'                  ) -> encode_tagval(ID, <<"AR">>);
encode_fld_val35(ID,_T, 'Allocationreport'                       ) -> encode_tagval(ID, <<"AS">>);
encode_fld_val35(ID,_T, 'Allocationreportack'                    ) -> encode_tagval(ID, <<"AT">>);
encode_fld_val35(ID,_T, 'Confirmationack'                        ) -> encode_tagval(ID, <<"AU">>);
encode_fld_val35(ID,_T, 'Settlementinstructionrequest'           ) -> encode_tagval(ID, <<"AV">>);
encode_fld_val35(ID,_T, 'Assignmentreport'                       ) -> encode_tagval(ID, <<"AW">>);
encode_fld_val35(ID,_T, 'Collateralrequest'                      ) -> encode_tagval(ID, <<"AX">>);
encode_fld_val35(ID,_T, 'Collateralassignment'                   ) -> encode_tagval(ID, <<"AY">>);
encode_fld_val35(ID,_T, 'Collateralresponse'                     ) -> encode_tagval(ID, <<"AZ">>);
encode_fld_val35(ID,_T, 'Collateralreport'                       ) -> encode_tagval(ID, <<"BA">>);
encode_fld_val35(ID,_T, 'Collateralinquiry'                      ) -> encode_tagval(ID, <<"BB">>);
encode_fld_val35(ID,_T, 'Networkcounterpartysystemstatusrequest' ) -> encode_tagval(ID, <<"BC">>);
encode_fld_val35(ID,_T, 'Networkcounterpartysystemstatusresponse') -> encode_tagval(ID, <<"BD">>);
encode_fld_val35(ID,_T, 'Userrequest'                            ) -> encode_tagval(ID, <<"BE">>);
encode_fld_val35(ID,_T, 'Userresponse'                           ) -> encode_tagval(ID, <<"BF">>);
encode_fld_val35(ID,_T, 'Collateralinquiryack'                   ) -> encode_tagval(ID, <<"BG">>);
encode_fld_val35(ID,_T, 'Confirmationrequest'                    ) -> encode_tagval(ID, <<"BH">>);
encode_fld_val35(ID, T, V                                        ) -> try_encode_val(ID, T, V).

decode_fld_val39(Val) ->
  case Val of
    <<"0">> -> 'New'               ; %% 0
    <<"1">> -> 'PartiallyFilled'   ; %% 1
    <<"2">> -> 'Filled'            ; %% 2
    <<"3">> -> 'DoneForDay'        ; %% 3
    <<"4">> -> 'Canceled'          ; %% 4
    <<"6">> -> 'PendingCancel'     ; %% 5
    <<"7">> -> 'Stopped'           ; %% 6
    <<"8">> -> 'Rejected'          ; %% 7
    <<"9">> -> 'Suspended'         ; %% 8
    <<"A">> -> 'PendingNew'        ; %% 9
    <<"B">> -> 'Calculated'        ; %% 10
    <<"C">> -> 'Expired'           ; %% 11
    <<"D">> -> 'AcceptedForBidding'; %% 12
    <<"E">> -> 'PendingReplace'    ; %% 13
    _       -> Val
  end.

encode_fld_val39(ID,_T, 'New'               ) -> encode_tagval(ID, <<"0">>);
encode_fld_val39(ID,_T, 'PartiallyFilled'   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val39(ID,_T, 'Filled'            ) -> encode_tagval(ID, <<"2">>);
encode_fld_val39(ID,_T, 'DoneForDay'        ) -> encode_tagval(ID, <<"3">>);
encode_fld_val39(ID,_T, 'Canceled'          ) -> encode_tagval(ID, <<"4">>);
encode_fld_val39(ID,_T, 'PendingCancel'     ) -> encode_tagval(ID, <<"6">>);
encode_fld_val39(ID,_T, 'Stopped'           ) -> encode_tagval(ID, <<"7">>);
encode_fld_val39(ID,_T, 'Rejected'          ) -> encode_tagval(ID, <<"8">>);
encode_fld_val39(ID,_T, 'Suspended'         ) -> encode_tagval(ID, <<"9">>);
encode_fld_val39(ID,_T, 'PendingNew'        ) -> encode_tagval(ID, <<"A">>);
encode_fld_val39(ID,_T, 'Calculated'        ) -> encode_tagval(ID, <<"B">>);
encode_fld_val39(ID,_T, 'Expired'           ) -> encode_tagval(ID, <<"C">>);
encode_fld_val39(ID,_T, 'AcceptedForBidding') -> encode_tagval(ID, <<"D">>);
encode_fld_val39(ID,_T, 'PendingReplace'    ) -> encode_tagval(ID, <<"E">>);
encode_fld_val39(ID, T, V                   ) -> try_encode_val(ID, T, V).

decode_fld_val40(Val) ->
  case Val of
    <<"1">> -> 'Market'                    ; %% 0
    <<"2">> -> 'Limit'                     ; %% 1
    <<"3">> -> 'Stop'                      ; %% 2
    <<"4">> -> 'StopLimit'                 ; %% 3
    <<"6">> -> 'WithOrWithout'             ; %% 4
    <<"7">> -> 'LimitOrBetter'             ; %% 5
    <<"8">> -> 'LimitWithOrWithout'        ; %% 6
    <<"9">> -> 'OnBasis'                   ; %% 7
    <<"D">> -> 'PreviouslyQuoted'          ; %% 8
    <<"E">> -> 'PreviouslyIndicated'       ; %% 9
    <<"G">> -> 'Forex'                     ; %% 10
    <<"I">> -> 'Funari'                    ; %% 11
    <<"J">> -> 'MarketIfTouched'           ; %% 12
    <<"K">> -> 'MarketWithLeftoverAsLimit' ; %% 13
    <<"L">> -> 'PreviousFundValuationPoint'; %% 14
    <<"M">> -> 'NextFundValuationPoint'    ; %% 15
    <<"P">> -> 'Pegged'                    ; %% 16
    _       -> Val
  end.

encode_fld_val40(ID,_T, 'Market'                    ) -> encode_tagval(ID, <<"1">>);
encode_fld_val40(ID,_T, 'Limit'                     ) -> encode_tagval(ID, <<"2">>);
encode_fld_val40(ID,_T, 'Stop'                      ) -> encode_tagval(ID, <<"3">>);
encode_fld_val40(ID,_T, 'StopLimit'                 ) -> encode_tagval(ID, <<"4">>);
encode_fld_val40(ID,_T, 'WithOrWithout'             ) -> encode_tagval(ID, <<"6">>);
encode_fld_val40(ID,_T, 'LimitOrBetter'             ) -> encode_tagval(ID, <<"7">>);
encode_fld_val40(ID,_T, 'LimitWithOrWithout'        ) -> encode_tagval(ID, <<"8">>);
encode_fld_val40(ID,_T, 'OnBasis'                   ) -> encode_tagval(ID, <<"9">>);
encode_fld_val40(ID,_T, 'PreviouslyQuoted'          ) -> encode_tagval(ID, <<"D">>);
encode_fld_val40(ID,_T, 'PreviouslyIndicated'       ) -> encode_tagval(ID, <<"E">>);
encode_fld_val40(ID,_T, 'Forex'                     ) -> encode_tagval(ID, <<"G">>);
encode_fld_val40(ID,_T, 'Funari'                    ) -> encode_tagval(ID, <<"I">>);
encode_fld_val40(ID,_T, 'MarketIfTouched'           ) -> encode_tagval(ID, <<"J">>);
encode_fld_val40(ID,_T, 'MarketWithLeftoverAsLimit' ) -> encode_tagval(ID, <<"K">>);
encode_fld_val40(ID,_T, 'PreviousFundValuationPoint') -> encode_tagval(ID, <<"L">>);
encode_fld_val40(ID,_T, 'NextFundValuationPoint'    ) -> encode_tagval(ID, <<"M">>);
encode_fld_val40(ID,_T, 'Pegged'                    ) -> encode_tagval(ID, <<"P">>);
encode_fld_val40(ID, T, V                           ) -> try_encode_val(ID, T, V).

decode_fld_val43(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val43(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val43(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val43(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val54(Val) ->
  case Val of
    <<"1">> -> 'Buy'             ; %% 0
    <<"2">> -> 'Sell'            ; %% 1
    <<"3">> -> 'BuyMinus'        ; %% 2
    <<"4">> -> 'SellPlus'        ; %% 3
    <<"5">> -> 'SellShort'       ; %% 4
    <<"6">> -> 'SellShortExempt' ; %% 5
    <<"7">> -> 'Undisclosed'     ; %% 6
    <<"8">> -> 'Cross'           ; %% 7
    <<"9">> -> 'CrossShort'      ; %% 8
    <<"A">> -> 'CrossShortExempt'; %% 9
    <<"B">> -> 'AsDefined'       ; %% 10
    <<"C">> -> 'Opposite'        ; %% 11
    <<"D">> -> 'Subscribe'       ; %% 12
    <<"E">> -> 'Redeem'          ; %% 13
    <<"F">> -> 'Lend'            ; %% 14
    <<"G">> -> 'Borrow'          ; %% 15
    _       -> Val
  end.

encode_fld_val54(ID,_T, 'Buy'             ) -> encode_tagval(ID, <<"1">>);
encode_fld_val54(ID,_T, 'Sell'            ) -> encode_tagval(ID, <<"2">>);
encode_fld_val54(ID,_T, 'BuyMinus'        ) -> encode_tagval(ID, <<"3">>);
encode_fld_val54(ID,_T, 'SellPlus'        ) -> encode_tagval(ID, <<"4">>);
encode_fld_val54(ID,_T, 'SellShort'       ) -> encode_tagval(ID, <<"5">>);
encode_fld_val54(ID,_T, 'SellShortExempt' ) -> encode_tagval(ID, <<"6">>);
encode_fld_val54(ID,_T, 'Undisclosed'     ) -> encode_tagval(ID, <<"7">>);
encode_fld_val54(ID,_T, 'Cross'           ) -> encode_tagval(ID, <<"8">>);
encode_fld_val54(ID,_T, 'CrossShort'      ) -> encode_tagval(ID, <<"9">>);
encode_fld_val54(ID,_T, 'CrossShortExempt') -> encode_tagval(ID, <<"A">>);
encode_fld_val54(ID,_T, 'AsDefined'       ) -> encode_tagval(ID, <<"B">>);
encode_fld_val54(ID,_T, 'Opposite'        ) -> encode_tagval(ID, <<"C">>);
encode_fld_val54(ID,_T, 'Subscribe'       ) -> encode_tagval(ID, <<"D">>);
encode_fld_val54(ID,_T, 'Redeem'          ) -> encode_tagval(ID, <<"E">>);
encode_fld_val54(ID,_T, 'Lend'            ) -> encode_tagval(ID, <<"F">>);
encode_fld_val54(ID,_T, 'Borrow'          ) -> encode_tagval(ID, <<"G">>);
encode_fld_val54(ID, T, V                 ) -> try_encode_val(ID, T, V).

decode_fld_val59(Val) ->
  case Val of
    <<"0">> -> 'Day'              ; %% 0
    <<"1">> -> 'GoodTillCancel'   ; %% 1
    <<"2">> -> 'AtTheOpening'     ; %% 2
    <<"3">> -> 'ImmediateOrCancel'; %% 3
    <<"4">> -> 'FillOrKill'       ; %% 4
    <<"5">> -> 'GoodTillCrossing' ; %% 5
    <<"6">> -> 'GoodTillDate'     ; %% 6
    <<"7">> -> 'AtTheClose'       ; %% 7
    _       -> Val
  end.

encode_fld_val59(ID,_T, 'Day'              ) -> encode_tagval(ID, <<"0">>);
encode_fld_val59(ID,_T, 'GoodTillCancel'   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val59(ID,_T, 'AtTheOpening'     ) -> encode_tagval(ID, <<"2">>);
encode_fld_val59(ID,_T, 'ImmediateOrCancel') -> encode_tagval(ID, <<"3">>);
encode_fld_val59(ID,_T, 'FillOrKill'       ) -> encode_tagval(ID, <<"4">>);
encode_fld_val59(ID,_T, 'GoodTillCrossing' ) -> encode_tagval(ID, <<"5">>);
encode_fld_val59(ID,_T, 'GoodTillDate'     ) -> encode_tagval(ID, <<"6">>);
encode_fld_val59(ID,_T, 'AtTheClose'       ) -> encode_tagval(ID, <<"7">>);
encode_fld_val59(ID, T, V                  ) -> try_encode_val(ID, T, V).

decode_fld_val61(Val) ->
  case Val of
    <<"0">> -> 'Normal'    ; %% 0
    <<"1">> -> 'Flash'     ; %% 1
    <<"2">> -> 'Background'; %% 2
    _       -> Val
  end.

encode_fld_val61(ID,_T, 'Normal'    ) -> encode_tagval(ID, <<"0">>);
encode_fld_val61(ID,_T, 'Flash'     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val61(ID,_T, 'Background') -> encode_tagval(ID, <<"2">>);
encode_fld_val61(ID, T, V           ) -> try_encode_val(ID, T, V).

decode_fld_val63(Val) ->
  case Val of
    <<"0">> -> 'Regular'        ; %% 0
    <<"1">> -> 'Cash'           ; %% 1
    <<"2">> -> 'NextDay'        ; %% 2
    <<"3">> -> 'TPlus2'         ; %% 3
    <<"4">> -> 'TPlus3'         ; %% 4
    <<"5">> -> 'TPlus4'         ; %% 5
    <<"6">> -> 'Future'         ; %% 6
    <<"7">> -> 'WhenAndIfIssued'; %% 7
    <<"8">> -> 'SellersOption'  ; %% 8
    <<"9">> -> 'TPlus5'         ; %% 9
    _       -> Val
  end.

encode_fld_val63(ID,_T, 'Regular'        ) -> encode_tagval(ID, <<"0">>);
encode_fld_val63(ID,_T, 'Cash'           ) -> encode_tagval(ID, <<"1">>);
encode_fld_val63(ID,_T, 'NextDay'        ) -> encode_tagval(ID, <<"2">>);
encode_fld_val63(ID,_T, 'TPlus2'         ) -> encode_tagval(ID, <<"3">>);
encode_fld_val63(ID,_T, 'TPlus3'         ) -> encode_tagval(ID, <<"4">>);
encode_fld_val63(ID,_T, 'TPlus4'         ) -> encode_tagval(ID, <<"5">>);
encode_fld_val63(ID,_T, 'Future'         ) -> encode_tagval(ID, <<"6">>);
encode_fld_val63(ID,_T, 'WhenAndIfIssued') -> encode_tagval(ID, <<"7">>);
encode_fld_val63(ID,_T, 'SellersOption'  ) -> encode_tagval(ID, <<"8">>);
encode_fld_val63(ID,_T, 'TPlus5'         ) -> encode_tagval(ID, <<"9">>);
encode_fld_val63(ID, T, V                ) -> try_encode_val(ID, T, V).

decode_fld_val71(Val) ->
  case Val of
    <<"0">> -> 'New'    ; %% 0
    <<"1">> -> 'Replace'; %% 1
    <<"2">> -> 'Cancel' ; %% 2
    _       -> Val
  end.

encode_fld_val71(ID,_T, 'New'    ) -> encode_tagval(ID, <<"0">>);
encode_fld_val71(ID,_T, 'Replace') -> encode_tagval(ID, <<"1">>);
encode_fld_val71(ID,_T, 'Cancel' ) -> encode_tagval(ID, <<"2">>);
encode_fld_val71(ID, T, V        ) -> try_encode_val(ID, T, V).

decode_fld_val77(Val) ->
  case Val of
    <<"O">> -> 'Open'  ; %% 0
    <<"C">> -> 'Close' ; %% 1
    <<"R">> -> 'Rolled'; %% 2
    <<"F">> -> 'Fifo'  ; %% 3
    _       -> Val
  end.

encode_fld_val77(ID,_T, 'Open'  ) -> encode_tagval(ID, <<"O">>);
encode_fld_val77(ID,_T, 'Close' ) -> encode_tagval(ID, <<"C">>);
encode_fld_val77(ID,_T, 'Rolled') -> encode_tagval(ID, <<"R">>);
encode_fld_val77(ID,_T, 'Fifo'  ) -> encode_tagval(ID, <<"F">>);
encode_fld_val77(ID, T, V       ) -> try_encode_val(ID, T, V).

decode_fld_val81(Val) ->
  case Val of
    <<"0">> -> 'Regular'          ; %% 0
    <<"1">> -> 'SoftDollar'       ; %% 1
    <<"2">> -> 'StepIn'           ; %% 2
    <<"3">> -> 'StepOut'          ; %% 3
    <<"4">> -> 'SoftDollarStepIn' ; %% 4
    <<"5">> -> 'SoftDollarStepOut'; %% 5
    <<"6">> -> 'PlanSponsor'      ; %% 6
    _       -> Val
  end.

encode_fld_val81(ID,_T, 'Regular'          ) -> encode_tagval(ID, <<"0">>);
encode_fld_val81(ID,_T, 'SoftDollar'       ) -> encode_tagval(ID, <<"1">>);
encode_fld_val81(ID,_T, 'StepIn'           ) -> encode_tagval(ID, <<"2">>);
encode_fld_val81(ID,_T, 'StepOut'          ) -> encode_tagval(ID, <<"3">>);
encode_fld_val81(ID,_T, 'SoftDollarStepIn' ) -> encode_tagval(ID, <<"4">>);
encode_fld_val81(ID,_T, 'SoftDollarStepOut') -> encode_tagval(ID, <<"5">>);
encode_fld_val81(ID,_T, 'PlanSponsor'      ) -> encode_tagval(ID, <<"6">>);
encode_fld_val81(ID, T, V                  ) -> try_encode_val(ID, T, V).

decode_fld_val87(Val) ->
  case Val of
    <<"0">> -> 'Accepted'              ; %% 0
    <<"1">> -> 'BlockLevelReject'      ; %% 1
    <<"2">> -> 'AccountLevelReject'    ; %% 2
    <<"3">> -> 'Received'              ; %% 3
    <<"4">> -> 'Incomplete'            ; %% 4
    <<"5">> -> 'RejectedByIntermediary'; %% 5
    _       -> Val
  end.

encode_fld_val87(ID,_T, 'Accepted'              ) -> encode_tagval(ID, <<"0">>);
encode_fld_val87(ID,_T, 'BlockLevelReject'      ) -> encode_tagval(ID, <<"1">>);
encode_fld_val87(ID,_T, 'AccountLevelReject'    ) -> encode_tagval(ID, <<"2">>);
encode_fld_val87(ID,_T, 'Received'              ) -> encode_tagval(ID, <<"3">>);
encode_fld_val87(ID,_T, 'Incomplete'            ) -> encode_tagval(ID, <<"4">>);
encode_fld_val87(ID,_T, 'RejectedByIntermediary') -> encode_tagval(ID, <<"5">>);
encode_fld_val87(ID, T, V                       ) -> try_encode_val(ID, T, V).

decode_fld_val88(Val) ->
  case Val of
    <<"0" >> -> 'UnknownAccount'                ; %% 0
    <<"1" >> -> 'IncorrectQuantity'             ; %% 1
    <<"2" >> -> 'IncorrectAveragePrice'         ; %% 2
    <<"3" >> -> 'UnknownExecutingBrokerMnemonic'; %% 3
    <<"4" >> -> 'CommissionDifference'          ; %% 4
    <<"5" >> -> 'UnknownOrderid'                ; %% 5
    <<"6" >> -> 'UnknownListid'                 ; %% 6
    <<"7" >> -> 'Other'                         ; %% 7
    <<"8" >> -> 'IncorrectAllocatedQuantity'    ; %% 8
    <<"9" >> -> 'CalculationDifference'         ; %% 9
    <<"10">> -> 'UnknownOrStaleExecid'          ; %% 10
    <<"11">> -> 'MismatchedDataValue'           ; %% 11
    <<"12">> -> 'UnknownClordid'                ; %% 12
    <<"13">> -> 'WarehouseRequestRejected'      ; %% 13
    _        -> Val
  end.

encode_fld_val88(ID,_T, 'UnknownAccount'                ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val88(ID,_T, 'IncorrectQuantity'             ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val88(ID,_T, 'IncorrectAveragePrice'         ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val88(ID,_T, 'UnknownExecutingBrokerMnemonic') -> encode_tagval(ID, <<"3" >>);
encode_fld_val88(ID,_T, 'CommissionDifference'          ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val88(ID,_T, 'UnknownOrderid'                ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val88(ID,_T, 'UnknownListid'                 ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val88(ID,_T, 'Other'                         ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val88(ID,_T, 'IncorrectAllocatedQuantity'    ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val88(ID,_T, 'CalculationDifference'         ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val88(ID,_T, 'UnknownOrStaleExecid'          ) -> encode_tagval(ID, <<"10">>);
encode_fld_val88(ID,_T, 'MismatchedDataValue'           ) -> encode_tagval(ID, <<"11">>);
encode_fld_val88(ID,_T, 'UnknownClordid'                ) -> encode_tagval(ID, <<"12">>);
encode_fld_val88(ID,_T, 'WarehouseRequestRejected'      ) -> encode_tagval(ID, <<"13">>);
encode_fld_val88(ID, T, V                               ) -> try_encode_val(ID, T, V).

decode_fld_val94(Val) ->
  case Val of
    <<"0">> -> 'New'       ; %% 0
    <<"1">> -> 'Reply'     ; %% 1
    <<"2">> -> 'AdminReply'; %% 2
    _       -> Val
  end.

encode_fld_val94(ID,_T, 'New'       ) -> encode_tagval(ID, <<"0">>);
encode_fld_val94(ID,_T, 'Reply'     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val94(ID,_T, 'AdminReply') -> encode_tagval(ID, <<"2">>);
encode_fld_val94(ID, T, V           ) -> try_encode_val(ID, T, V).

decode_fld_val97(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val97(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val97(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val97(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val98(Val) ->
  case Val of
    <<"0">> -> 'None'     ; %% 0
    <<"1">> -> 'Pkcs'     ; %% 1
    <<"2">> -> 'Des'      ; %% 2
    <<"3">> -> 'PkcsDes'  ; %% 3
    <<"4">> -> 'PgpDes'   ; %% 4
    <<"5">> -> 'PgpDesMd5'; %% 5
    <<"6">> -> 'PemDesMd5'; %% 6
    _       -> Val
  end.

encode_fld_val98(ID,_T, 'None'     ) -> encode_tagval(ID, <<"0">>);
encode_fld_val98(ID,_T, 'Pkcs'     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val98(ID,_T, 'Des'      ) -> encode_tagval(ID, <<"2">>);
encode_fld_val98(ID,_T, 'PkcsDes'  ) -> encode_tagval(ID, <<"3">>);
encode_fld_val98(ID,_T, 'PgpDes'   ) -> encode_tagval(ID, <<"4">>);
encode_fld_val98(ID,_T, 'PgpDesMd5') -> encode_tagval(ID, <<"5">>);
encode_fld_val98(ID,_T, 'PemDesMd5') -> encode_tagval(ID, <<"6">>);
encode_fld_val98(ID, T, V          ) -> try_encode_val(ID, T, V).

decode_fld_val102(Val) ->
  case Val of
    <<"0" >> -> 'TooLateToCancel'                                  ; %% 0
    <<"1" >> -> 'UnknownOrder'                                     ; %% 1
    <<"2" >> -> 'Broker'                                           ; %% 2
    <<"3" >> -> 'OrderAlreadyInPendingCancelOrPendingReplaceStatus'; %% 3
    <<"4" >> -> 'UnableToProcessOrderMassCancelRequest'            ; %% 4
    <<"5" >> -> 'Origordmodtime'                                   ; %% 5
    <<"6" >> -> 'DuplicateClordid'                                 ; %% 6
    <<"99">> -> 'Other'                                            ; %% 7
    _        -> Val
  end.

encode_fld_val102(ID,_T, 'TooLateToCancel'                                  ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val102(ID,_T, 'UnknownOrder'                                     ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val102(ID,_T, 'Broker'                                           ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val102(ID,_T, 'OrderAlreadyInPendingCancelOrPendingReplaceStatus') -> encode_tagval(ID, <<"3" >>);
encode_fld_val102(ID,_T, 'UnableToProcessOrderMassCancelRequest'            ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val102(ID,_T, 'Origordmodtime'                                   ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val102(ID,_T, 'DuplicateClordid'                                 ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val102(ID,_T, 'Other'                                            ) -> encode_tagval(ID, <<"99">>);
encode_fld_val102(ID, T, V                                                  ) -> try_encode_val(ID, T, V).

decode_fld_val103(Val) ->
  case Val of
    <<"0" >> -> 'Broker'                                            ; %% 0
    <<"1" >> -> 'UnknownSymbol'                                     ; %% 1
    <<"2" >> -> 'ExchangeClosed'                                    ; %% 2
    <<"3" >> -> 'OrderExceedsLimit'                                 ; %% 3
    <<"4" >> -> 'TooLateToEnter'                                    ; %% 4
    <<"5" >> -> 'UnknownOrder'                                      ; %% 5
    <<"6" >> -> 'DuplicateOrder'                                    ; %% 6
    <<"7" >> -> 'DuplicateOfAVerballyCommunicatedOrder'             ; %% 7
    <<"8" >> -> 'StaleOrder'                                        ; %% 8
    <<"9" >> -> 'TradeAlongRequired'                                ; %% 9
    <<"10">> -> 'InvalidInvestorId'                                 ; %% 10
    <<"11">> -> 'UnsupportedOrderCharacteristic12SurveillenceOption'; %% 11
    <<"13">> -> 'IncorrectQuantity'                                 ; %% 12
    <<"14">> -> 'IncorrectAllocatedQuantity'                        ; %% 13
    <<"15">> -> 'UnknownAccount'                                    ; %% 14
    <<"99">> -> 'Other'                                             ; %% 15
    _        -> Val
  end.

encode_fld_val103(ID,_T, 'Broker'                                            ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val103(ID,_T, 'UnknownSymbol'                                     ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val103(ID,_T, 'ExchangeClosed'                                    ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val103(ID,_T, 'OrderExceedsLimit'                                 ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val103(ID,_T, 'TooLateToEnter'                                    ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val103(ID,_T, 'UnknownOrder'                                      ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val103(ID,_T, 'DuplicateOrder'                                    ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val103(ID,_T, 'DuplicateOfAVerballyCommunicatedOrder'             ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val103(ID,_T, 'StaleOrder'                                        ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val103(ID,_T, 'TradeAlongRequired'                                ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val103(ID,_T, 'InvalidInvestorId'                                 ) -> encode_tagval(ID, <<"10">>);
encode_fld_val103(ID,_T, 'UnsupportedOrderCharacteristic12SurveillenceOption') -> encode_tagval(ID, <<"11">>);
encode_fld_val103(ID,_T, 'IncorrectQuantity'                                 ) -> encode_tagval(ID, <<"13">>);
encode_fld_val103(ID,_T, 'IncorrectAllocatedQuantity'                        ) -> encode_tagval(ID, <<"14">>);
encode_fld_val103(ID,_T, 'UnknownAccount'                                    ) -> encode_tagval(ID, <<"15">>);
encode_fld_val103(ID,_T, 'Other'                                             ) -> encode_tagval(ID, <<"99">>);
encode_fld_val103(ID, T, V                                                   ) -> try_encode_val(ID, T, V).

decode_fld_val104(Val) ->
  case Val of
    <<"A">> -> 'AllOrNone'          ; %% 0
    <<"B">> -> 'MarketOnClose'      ; %% 1
    <<"C">> -> 'AtTheClose'         ; %% 2
    <<"D">> -> 'Vwap'               ; %% 3
    <<"I">> -> 'InTouchWith'        ; %% 4
    <<"L">> -> 'Limit'              ; %% 5
    <<"M">> -> 'MoreBehind'         ; %% 6
    <<"O">> -> 'AtTheOpen'          ; %% 7
    <<"P">> -> 'TakingAPosition'    ; %% 8
    <<"Q">> -> 'AtTheMarket'        ; %% 9
    <<"R">> -> 'ReadyToTrade'       ; %% 10
    <<"S">> -> 'PortfolioShown'     ; %% 11
    <<"T">> -> 'ThroughTheDay'      ; %% 12
    <<"V">> -> 'Versus'             ; %% 13
    <<"W">> -> 'Indication'         ; %% 14
    <<"X">> -> 'CrossingOpportunity'; %% 15
    <<"Y">> -> 'AtTheMidpoint'      ; %% 16
    <<"Z">> -> 'PreOpen'            ; %% 17
    _       -> Val
  end.

encode_fld_val104(ID,_T, 'AllOrNone'          ) -> encode_tagval(ID, <<"A">>);
encode_fld_val104(ID,_T, 'MarketOnClose'      ) -> encode_tagval(ID, <<"B">>);
encode_fld_val104(ID,_T, 'AtTheClose'         ) -> encode_tagval(ID, <<"C">>);
encode_fld_val104(ID,_T, 'Vwap'               ) -> encode_tagval(ID, <<"D">>);
encode_fld_val104(ID,_T, 'InTouchWith'        ) -> encode_tagval(ID, <<"I">>);
encode_fld_val104(ID,_T, 'Limit'              ) -> encode_tagval(ID, <<"L">>);
encode_fld_val104(ID,_T, 'MoreBehind'         ) -> encode_tagval(ID, <<"M">>);
encode_fld_val104(ID,_T, 'AtTheOpen'          ) -> encode_tagval(ID, <<"O">>);
encode_fld_val104(ID,_T, 'TakingAPosition'    ) -> encode_tagval(ID, <<"P">>);
encode_fld_val104(ID,_T, 'AtTheMarket'        ) -> encode_tagval(ID, <<"Q">>);
encode_fld_val104(ID,_T, 'ReadyToTrade'       ) -> encode_tagval(ID, <<"R">>);
encode_fld_val104(ID,_T, 'PortfolioShown'     ) -> encode_tagval(ID, <<"S">>);
encode_fld_val104(ID,_T, 'ThroughTheDay'      ) -> encode_tagval(ID, <<"T">>);
encode_fld_val104(ID,_T, 'Versus'             ) -> encode_tagval(ID, <<"V">>);
encode_fld_val104(ID,_T, 'Indication'         ) -> encode_tagval(ID, <<"W">>);
encode_fld_val104(ID,_T, 'CrossingOpportunity') -> encode_tagval(ID, <<"X">>);
encode_fld_val104(ID,_T, 'AtTheMidpoint'      ) -> encode_tagval(ID, <<"Y">>);
encode_fld_val104(ID,_T, 'PreOpen'            ) -> encode_tagval(ID, <<"Z">>);
encode_fld_val104(ID, T, V                    ) -> try_encode_val(ID, T, V).

decode_fld_val113(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val113(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val113(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val113(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val114(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val114(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val114(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val114(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val121(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val121(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val121(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val121(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val123(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val123(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val123(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val123(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val127(Val) ->
  case Val of
    <<"A">> -> 'UnknownSymbol'        ; %% 0
    <<"B">> -> 'WrongSide'            ; %% 1
    <<"C">> -> 'QuantityExceedsOrder' ; %% 2
    <<"D">> -> 'NoMatchingOrder'      ; %% 3
    <<"E">> -> 'PriceExceedsLimit'    ; %% 4
    <<"F">> -> 'CalculationDifference'; %% 5
    <<"Z">> -> 'Other'                ; %% 6
    _       -> Val
  end.

encode_fld_val127(ID,_T, 'UnknownSymbol'        ) -> encode_tagval(ID, <<"A">>);
encode_fld_val127(ID,_T, 'WrongSide'            ) -> encode_tagval(ID, <<"B">>);
encode_fld_val127(ID,_T, 'QuantityExceedsOrder' ) -> encode_tagval(ID, <<"C">>);
encode_fld_val127(ID,_T, 'NoMatchingOrder'      ) -> encode_tagval(ID, <<"D">>);
encode_fld_val127(ID,_T, 'PriceExceedsLimit'    ) -> encode_tagval(ID, <<"E">>);
encode_fld_val127(ID,_T, 'CalculationDifference') -> encode_tagval(ID, <<"F">>);
encode_fld_val127(ID,_T, 'Other'                ) -> encode_tagval(ID, <<"Z">>);
encode_fld_val127(ID, T, V                      ) -> try_encode_val(ID, T, V).

decode_fld_val130(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val130(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val130(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val130(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val139(Val) ->
  case Val of
    <<"1" >> -> 'Regulatory'     ; %% 0
    <<"2" >> -> 'Tax'            ; %% 1
    <<"3" >> -> 'LocalCommission'; %% 2
    <<"4" >> -> 'ExchangeFees'   ; %% 3
    <<"5" >> -> 'Stamp'          ; %% 4
    <<"6" >> -> 'Levy'           ; %% 5
    <<"7" >> -> 'Other'          ; %% 6
    <<"8" >> -> 'Markup'         ; %% 7
    <<"9" >> -> 'ConsumptionTax' ; %% 8
    <<"10">> -> 'PerTransaction' ; %% 9
    <<"11">> -> 'Conversion'     ; %% 10
    <<"12">> -> 'Agent'          ; %% 11
    _        -> Val
  end.

encode_fld_val139(ID,_T, 'Regulatory'     ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val139(ID,_T, 'Tax'            ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val139(ID,_T, 'LocalCommission') -> encode_tagval(ID, <<"3" >>);
encode_fld_val139(ID,_T, 'ExchangeFees'   ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val139(ID,_T, 'Stamp'          ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val139(ID,_T, 'Levy'           ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val139(ID,_T, 'Other'          ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val139(ID,_T, 'Markup'         ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val139(ID,_T, 'ConsumptionTax' ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val139(ID,_T, 'PerTransaction' ) -> encode_tagval(ID, <<"10">>);
encode_fld_val139(ID,_T, 'Conversion'     ) -> encode_tagval(ID, <<"11">>);
encode_fld_val139(ID,_T, 'Agent'          ) -> encode_tagval(ID, <<"12">>);
encode_fld_val139(ID, T, V                ) -> try_encode_val(ID, T, V).

decode_fld_val141(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val141(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val141(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val141(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val150(Val) ->
  case Val of
    <<"0">> -> 'New'           ; %% 0
    <<"3">> -> 'DoneForDay'    ; %% 1
    <<"4">> -> 'Canceled'      ; %% 2
    <<"5">> -> 'Replace'       ; %% 3
    <<"6">> -> 'PendingCancel' ; %% 4
    <<"7">> -> 'Stopped'       ; %% 5
    <<"8">> -> 'Rejected'      ; %% 6
    <<"9">> -> 'Suspended'     ; %% 7
    <<"A">> -> 'PendingNew'    ; %% 8
    <<"B">> -> 'Calculated'    ; %% 9
    <<"C">> -> 'Expired'       ; %% 10
    <<"D">> -> 'Restated'      ; %% 11
    <<"E">> -> 'PendingReplace'; %% 12
    <<"F">> -> 'Trade'         ; %% 13
    <<"G">> -> 'TradeCorrect'  ; %% 14
    <<"H">> -> 'TradeCancel'   ; %% 15
    <<"I">> -> 'OrderStatus'   ; %% 16
    _       -> Val
  end.

encode_fld_val150(ID,_T, 'New'           ) -> encode_tagval(ID, <<"0">>);
encode_fld_val150(ID,_T, 'DoneForDay'    ) -> encode_tagval(ID, <<"3">>);
encode_fld_val150(ID,_T, 'Canceled'      ) -> encode_tagval(ID, <<"4">>);
encode_fld_val150(ID,_T, 'Replace'       ) -> encode_tagval(ID, <<"5">>);
encode_fld_val150(ID,_T, 'PendingCancel' ) -> encode_tagval(ID, <<"6">>);
encode_fld_val150(ID,_T, 'Stopped'       ) -> encode_tagval(ID, <<"7">>);
encode_fld_val150(ID,_T, 'Rejected'      ) -> encode_tagval(ID, <<"8">>);
encode_fld_val150(ID,_T, 'Suspended'     ) -> encode_tagval(ID, <<"9">>);
encode_fld_val150(ID,_T, 'PendingNew'    ) -> encode_tagval(ID, <<"A">>);
encode_fld_val150(ID,_T, 'Calculated'    ) -> encode_tagval(ID, <<"B">>);
encode_fld_val150(ID,_T, 'Expired'       ) -> encode_tagval(ID, <<"C">>);
encode_fld_val150(ID,_T, 'Restated'      ) -> encode_tagval(ID, <<"D">>);
encode_fld_val150(ID,_T, 'PendingReplace') -> encode_tagval(ID, <<"E">>);
encode_fld_val150(ID,_T, 'Trade'         ) -> encode_tagval(ID, <<"F">>);
encode_fld_val150(ID,_T, 'TradeCorrect'  ) -> encode_tagval(ID, <<"G">>);
encode_fld_val150(ID,_T, 'TradeCancel'   ) -> encode_tagval(ID, <<"H">>);
encode_fld_val150(ID,_T, 'OrderStatus'   ) -> encode_tagval(ID, <<"I">>);
encode_fld_val150(ID, T, V               ) -> try_encode_val(ID, T, V).

decode_fld_val156(Val) ->
  case Val of
    <<"M">> -> 'Multiply'; %% 0
    <<"D">> -> 'Divide'  ; %% 1
    _       -> Val
  end.

encode_fld_val156(ID,_T, 'Multiply') -> encode_tagval(ID, <<"M">>);
encode_fld_val156(ID,_T, 'Divide'  ) -> encode_tagval(ID, <<"D">>);
encode_fld_val156(ID, T, V         ) -> try_encode_val(ID, T, V).

decode_fld_val160(Val) ->
  case Val of
    <<"1">> -> 'StandingInstructionsProvided'  ; %% 0
    <<"4">> -> 'SpecificOrderForASingleAccount'; %% 1
    <<"5">> -> 'RequestReject'                 ; %% 2
    _       -> Val
  end.

encode_fld_val160(ID,_T, 'StandingInstructionsProvided'  ) -> encode_tagval(ID, <<"1">>);
encode_fld_val160(ID,_T, 'SpecificOrderForASingleAccount') -> encode_tagval(ID, <<"4">>);
encode_fld_val160(ID,_T, 'RequestReject'                 ) -> encode_tagval(ID, <<"5">>);
encode_fld_val160(ID, T, V                               ) -> try_encode_val(ID, T, V).

decode_fld_val163(Val) ->
  case Val of
    <<"N">> -> 'New'    ; %% 0
    <<"C">> -> 'Cancel' ; %% 1
    <<"R">> -> 'Replace'; %% 2
    <<"T">> -> 'Restate'; %% 3
    _       -> Val
  end.

encode_fld_val163(ID,_T, 'New'    ) -> encode_tagval(ID, <<"N">>);
encode_fld_val163(ID,_T, 'Cancel' ) -> encode_tagval(ID, <<"C">>);
encode_fld_val163(ID,_T, 'Replace') -> encode_tagval(ID, <<"R">>);
encode_fld_val163(ID,_T, 'Restate') -> encode_tagval(ID, <<"T">>);
encode_fld_val163(ID, T, V        ) -> try_encode_val(ID, T, V).

decode_fld_val165(Val) ->
  case Val of
    <<"1">> -> 'BrokersInstructions'     ; %% 0
    <<"2">> -> 'InstitutionsInstructions'; %% 1
    <<"3">> -> 'Investor'                ; %% 2
    _       -> Val
  end.

encode_fld_val165(ID,_T, 'BrokersInstructions'     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val165(ID,_T, 'InstitutionsInstructions') -> encode_tagval(ID, <<"2">>);
encode_fld_val165(ID,_T, 'Investor'                ) -> encode_tagval(ID, <<"3">>);
encode_fld_val165(ID, T, V                         ) -> try_encode_val(ID, T, V).

decode_fld_val167(Val) ->
  case Val of
    <<"FUT"      >> -> 'Future'                                  ; %% 0
    <<"OPT"      >> -> 'Option'                                  ; %% 1
    <<"EUSUPRA"  >> -> 'EuroSupranationalCoupons'                ; %% 2
    <<"FAC"      >> -> 'FederalAgencyCoupon'                     ; %% 3
    <<"FADN"     >> -> 'FederalAgencyDiscountNote'               ; %% 4
    <<"PEF"      >> -> 'PrivateExportFunding'                    ; %% 5
    <<"SUPRA"    >> -> 'UsdSupranationalCoupons'                 ; %% 6
    <<"CORP"     >> -> 'CorporateBond'                           ; %% 7
    <<"CPP"      >> -> 'CorporatePrivatePlacement'               ; %% 8
    <<"CB"       >> -> 'ConvertibleBond'                         ; %% 9
    <<"DUAL"     >> -> 'DualCurrency'                            ; %% 10
    <<"EUCORP"   >> -> 'EuroCorporateBond'                       ; %% 11
    <<"XLINKD"   >> -> 'IndexedLinked'                           ; %% 12
    <<"STRUCT"   >> -> 'StructuredNotes'                         ; %% 13
    <<"YANK"     >> -> 'YankeeCorporateBond'                     ; %% 14
    <<"FOR"      >> -> 'ForeignExchangeContract'                 ; %% 15
    <<"CS"       >> -> 'CommonStock'                             ; %% 16
    <<"PS"       >> -> 'PreferredStock'                          ; %% 17
    <<"BRADY"    >> -> 'BradyBond'                               ; %% 18
    <<"EUSOV"    >> -> 'EuroSovereigns'                          ; %% 19
    <<"TBOND"    >> -> 'UsTreasuryBond'                          ; %% 20
    <<"TINT"     >> -> 'InterestStripFromAnyBondOrNote'          ; %% 21
    <<"TIPS"     >> -> 'TreasuryInflationProtectedSecurities'    ; %% 22
    <<"TCAL"     >> -> 'PrincipalStripOfACallableBondOrNote'     ; %% 23
    <<"TPRN"     >> -> 'PrincipalStripFromANonCallableBondOrNote'; %% 24
    <<"UST"      >> -> 'UsTreasuryNoteUst'                       ; %% 25
    <<"USTB"     >> -> 'UsTreasuryBillUstb'                      ; %% 26
    <<"TNOTE"    >> -> 'UsTreasuryNoteTnote'                     ; %% 27
    <<"TBILL"    >> -> 'UsTreasuryBillTbill'                     ; %% 28
    <<"REPO"     >> -> 'Repurchase'                              ; %% 29
    <<"FORWARD"  >> -> 'Forward'                                 ; %% 30
    <<"BUYSELL"  >> -> 'BuySellback'                             ; %% 31
    <<"SECLOAN"  >> -> 'SecuritiesLoan'                          ; %% 32
    <<"SECPLEDGE">> -> 'SecuritiesPledge'                        ; %% 33
    <<"TERM"     >> -> 'TermLoan'                                ; %% 34
    <<"RVLV"     >> -> 'RevolverLoan'                            ; %% 35
    <<"RVLVTRM"  >> -> 'RevolverTermLoan'                        ; %% 36
    <<"BRIDGE"   >> -> 'BridgeLoan'                              ; %% 37
    <<"LOFC"     >> -> 'LetterOfCredit'                          ; %% 38
    <<"SWING"    >> -> 'SwingLineFacility'                       ; %% 39
    <<"DINP"     >> -> 'DebtorInPossession'                      ; %% 40
    <<"DEFLTED"  >> -> 'Defaulted'                               ; %% 41
    <<"WITHDRN"  >> -> 'Withdrawn'                               ; %% 42
    <<"REPLACD"  >> -> 'Replaced'                                ; %% 43
    <<"MATURED"  >> -> 'Matured'                                 ; %% 44
    <<"AMENDED"  >> -> 'AmendedRestated'                         ; %% 45
    <<"RETIRED"  >> -> 'Retired'                                 ; %% 46
    <<"BA"       >> -> 'BankersAcceptance'                       ; %% 47
    <<"BN"       >> -> 'BankNotes'                               ; %% 48
    <<"BOX"      >> -> 'BillOfExchanges'                         ; %% 49
    <<"CD"       >> -> 'CertificateOfDeposit'                    ; %% 50
    <<"CL"       >> -> 'CallLoans'                               ; %% 51
    <<"CP"       >> -> 'CommercialPaper'                         ; %% 52
    <<"DN"       >> -> 'DepositNotes'                            ; %% 53
    <<"EUCD"     >> -> 'EuroCertificateOfDeposit'                ; %% 54
    <<"EUCP"     >> -> 'EuroCommercialPaper'                     ; %% 55
    <<"LQN"      >> -> 'LiquidityNote'                           ; %% 56
    <<"MTN"      >> -> 'MediumTermNotes'                         ; %% 57
    <<"ONITE"    >> -> 'Overnight'                               ; %% 58
    <<"PN"       >> -> 'PromissoryNote'                          ; %% 59
    <<"PZFJ"     >> -> 'PlazosFijos'                             ; %% 60
    <<"STN"      >> -> 'ShortTermLoanNote'                       ; %% 61
    <<"TD"       >> -> 'TimeDeposit'                             ; %% 62
    <<"XCN"      >> -> 'ExtendedCommNote'                        ; %% 63
    <<"YCD"      >> -> 'YankeeCertificateOfDeposit'              ; %% 64
    <<"ABS"      >> -> 'AssetBackedSecurities'                   ; %% 65
    <<"CMBS"     >> -> 'CorpMortgageBackedSecurities'            ; %% 66
    <<"CMO"      >> -> 'CollateralizedMortgageObligation'        ; %% 67
    <<"IET"      >> -> 'IoetteMortgage'                          ; %% 68
    <<"MBS"      >> -> 'MortgageBackedSecurities'                ; %% 69
    <<"MIO"      >> -> 'MortgageInterestOnly'                    ; %% 70
    <<"MPO"      >> -> 'MortgagePrincipalOnly'                   ; %% 71
    <<"MPP"      >> -> 'MortgagePrivatePlacement'                ; %% 72
    <<"MPT"      >> -> 'MiscellaneousPassThrough'                ; %% 73
    <<"PFAND"    >> -> 'Pfandbriefe'                             ; %% 74
    <<"TBA"      >> -> 'ToBeAnnounced'                           ; %% 75
    <<"AN"       >> -> 'OtherAnticipationNotesBanGanEtc'         ; %% 76
    <<"COFO"     >> -> 'CertificateOfObligation'                 ; %% 77
    <<"COFP"     >> -> 'CertificateOfParticipation'              ; %% 78
    <<"GO"       >> -> 'GeneralObligationBonds'                  ; %% 79
    <<"MT"       >> -> 'MandatoryTender'                         ; %% 80
    <<"RAN"      >> -> 'RevenueAnticipationNote'                 ; %% 81
    <<"REV"      >> -> 'RevenueBonds'                            ; %% 82
    <<"SPCLA"    >> -> 'SpecialAssessment'                       ; %% 83
    <<"SPCLO"    >> -> 'SpecialObligation'                       ; %% 84
    <<"SPCLT"    >> -> 'SpecialTax'                              ; %% 85
    <<"TAN"      >> -> 'TaxAnticipationNote'                     ; %% 86
    <<"TAXA"     >> -> 'TaxAllocation'                           ; %% 87
    <<"TECP"     >> -> 'TaxExemptCommercialPaper'                ; %% 88
    <<"TRAN"     >> -> 'TaxRevenueAnticipationNote'              ; %% 89
    <<"VRDN"     >> -> 'VariableRateDemandNote'                  ; %% 90
    <<"WAR"      >> -> 'Warrant'                                 ; %% 91
    <<"MF"       >> -> 'MutualFund'                              ; %% 92
    <<"MLEG"     >> -> 'MultiLegInstrument'                      ; %% 93
    <<"NONE"     >> -> 'NoSecurityType'                          ; %% 94
    _               -> Val
  end.

encode_fld_val167(ID,_T, 'Future'                                  ) -> encode_tagval(ID, <<"FUT"      >>);
encode_fld_val167(ID,_T, 'Option'                                  ) -> encode_tagval(ID, <<"OPT"      >>);
encode_fld_val167(ID,_T, 'EuroSupranationalCoupons'                ) -> encode_tagval(ID, <<"EUSUPRA"  >>);
encode_fld_val167(ID,_T, 'FederalAgencyCoupon'                     ) -> encode_tagval(ID, <<"FAC"      >>);
encode_fld_val167(ID,_T, 'FederalAgencyDiscountNote'               ) -> encode_tagval(ID, <<"FADN"     >>);
encode_fld_val167(ID,_T, 'PrivateExportFunding'                    ) -> encode_tagval(ID, <<"PEF"      >>);
encode_fld_val167(ID,_T, 'UsdSupranationalCoupons'                 ) -> encode_tagval(ID, <<"SUPRA"    >>);
encode_fld_val167(ID,_T, 'CorporateBond'                           ) -> encode_tagval(ID, <<"CORP"     >>);
encode_fld_val167(ID,_T, 'CorporatePrivatePlacement'               ) -> encode_tagval(ID, <<"CPP"      >>);
encode_fld_val167(ID,_T, 'ConvertibleBond'                         ) -> encode_tagval(ID, <<"CB"       >>);
encode_fld_val167(ID,_T, 'DualCurrency'                            ) -> encode_tagval(ID, <<"DUAL"     >>);
encode_fld_val167(ID,_T, 'EuroCorporateBond'                       ) -> encode_tagval(ID, <<"EUCORP"   >>);
encode_fld_val167(ID,_T, 'IndexedLinked'                           ) -> encode_tagval(ID, <<"XLINKD"   >>);
encode_fld_val167(ID,_T, 'StructuredNotes'                         ) -> encode_tagval(ID, <<"STRUCT"   >>);
encode_fld_val167(ID,_T, 'YankeeCorporateBond'                     ) -> encode_tagval(ID, <<"YANK"     >>);
encode_fld_val167(ID,_T, 'ForeignExchangeContract'                 ) -> encode_tagval(ID, <<"FOR"      >>);
encode_fld_val167(ID,_T, 'CommonStock'                             ) -> encode_tagval(ID, <<"CS"       >>);
encode_fld_val167(ID,_T, 'PreferredStock'                          ) -> encode_tagval(ID, <<"PS"       >>);
encode_fld_val167(ID,_T, 'BradyBond'                               ) -> encode_tagval(ID, <<"BRADY"    >>);
encode_fld_val167(ID,_T, 'EuroSovereigns'                          ) -> encode_tagval(ID, <<"EUSOV"    >>);
encode_fld_val167(ID,_T, 'UsTreasuryBond'                          ) -> encode_tagval(ID, <<"TBOND"    >>);
encode_fld_val167(ID,_T, 'InterestStripFromAnyBondOrNote'          ) -> encode_tagval(ID, <<"TINT"     >>);
encode_fld_val167(ID,_T, 'TreasuryInflationProtectedSecurities'    ) -> encode_tagval(ID, <<"TIPS"     >>);
encode_fld_val167(ID,_T, 'PrincipalStripOfACallableBondOrNote'     ) -> encode_tagval(ID, <<"TCAL"     >>);
encode_fld_val167(ID,_T, 'PrincipalStripFromANonCallableBondOrNote') -> encode_tagval(ID, <<"TPRN"     >>);
encode_fld_val167(ID,_T, 'UsTreasuryNoteUst'                       ) -> encode_tagval(ID, <<"UST"      >>);
encode_fld_val167(ID,_T, 'UsTreasuryBillUstb'                      ) -> encode_tagval(ID, <<"USTB"     >>);
encode_fld_val167(ID,_T, 'UsTreasuryNoteTnote'                     ) -> encode_tagval(ID, <<"TNOTE"    >>);
encode_fld_val167(ID,_T, 'UsTreasuryBillTbill'                     ) -> encode_tagval(ID, <<"TBILL"    >>);
encode_fld_val167(ID,_T, 'Repurchase'                              ) -> encode_tagval(ID, <<"REPO"     >>);
encode_fld_val167(ID,_T, 'Forward'                                 ) -> encode_tagval(ID, <<"FORWARD"  >>);
encode_fld_val167(ID,_T, 'BuySellback'                             ) -> encode_tagval(ID, <<"BUYSELL"  >>);
encode_fld_val167(ID,_T, 'SecuritiesLoan'                          ) -> encode_tagval(ID, <<"SECLOAN"  >>);
encode_fld_val167(ID,_T, 'SecuritiesPledge'                        ) -> encode_tagval(ID, <<"SECPLEDGE">>);
encode_fld_val167(ID,_T, 'TermLoan'                                ) -> encode_tagval(ID, <<"TERM"     >>);
encode_fld_val167(ID,_T, 'RevolverLoan'                            ) -> encode_tagval(ID, <<"RVLV"     >>);
encode_fld_val167(ID,_T, 'RevolverTermLoan'                        ) -> encode_tagval(ID, <<"RVLVTRM"  >>);
encode_fld_val167(ID,_T, 'BridgeLoan'                              ) -> encode_tagval(ID, <<"BRIDGE"   >>);
encode_fld_val167(ID,_T, 'LetterOfCredit'                          ) -> encode_tagval(ID, <<"LOFC"     >>);
encode_fld_val167(ID,_T, 'SwingLineFacility'                       ) -> encode_tagval(ID, <<"SWING"    >>);
encode_fld_val167(ID,_T, 'DebtorInPossession'                      ) -> encode_tagval(ID, <<"DINP"     >>);
encode_fld_val167(ID,_T, 'Defaulted'                               ) -> encode_tagval(ID, <<"DEFLTED"  >>);
encode_fld_val167(ID,_T, 'Withdrawn'                               ) -> encode_tagval(ID, <<"WITHDRN"  >>);
encode_fld_val167(ID,_T, 'Replaced'                                ) -> encode_tagval(ID, <<"REPLACD"  >>);
encode_fld_val167(ID,_T, 'Matured'                                 ) -> encode_tagval(ID, <<"MATURED"  >>);
encode_fld_val167(ID,_T, 'AmendedRestated'                         ) -> encode_tagval(ID, <<"AMENDED"  >>);
encode_fld_val167(ID,_T, 'Retired'                                 ) -> encode_tagval(ID, <<"RETIRED"  >>);
encode_fld_val167(ID,_T, 'BankersAcceptance'                       ) -> encode_tagval(ID, <<"BA"       >>);
encode_fld_val167(ID,_T, 'BankNotes'                               ) -> encode_tagval(ID, <<"BN"       >>);
encode_fld_val167(ID,_T, 'BillOfExchanges'                         ) -> encode_tagval(ID, <<"BOX"      >>);
encode_fld_val167(ID,_T, 'CertificateOfDeposit'                    ) -> encode_tagval(ID, <<"CD"       >>);
encode_fld_val167(ID,_T, 'CallLoans'                               ) -> encode_tagval(ID, <<"CL"       >>);
encode_fld_val167(ID,_T, 'CommercialPaper'                         ) -> encode_tagval(ID, <<"CP"       >>);
encode_fld_val167(ID,_T, 'DepositNotes'                            ) -> encode_tagval(ID, <<"DN"       >>);
encode_fld_val167(ID,_T, 'EuroCertificateOfDeposit'                ) -> encode_tagval(ID, <<"EUCD"     >>);
encode_fld_val167(ID,_T, 'EuroCommercialPaper'                     ) -> encode_tagval(ID, <<"EUCP"     >>);
encode_fld_val167(ID,_T, 'LiquidityNote'                           ) -> encode_tagval(ID, <<"LQN"      >>);
encode_fld_val167(ID,_T, 'MediumTermNotes'                         ) -> encode_tagval(ID, <<"MTN"      >>);
encode_fld_val167(ID,_T, 'Overnight'                               ) -> encode_tagval(ID, <<"ONITE"    >>);
encode_fld_val167(ID,_T, 'PromissoryNote'                          ) -> encode_tagval(ID, <<"PN"       >>);
encode_fld_val167(ID,_T, 'PlazosFijos'                             ) -> encode_tagval(ID, <<"PZFJ"     >>);
encode_fld_val167(ID,_T, 'ShortTermLoanNote'                       ) -> encode_tagval(ID, <<"STN"      >>);
encode_fld_val167(ID,_T, 'TimeDeposit'                             ) -> encode_tagval(ID, <<"TD"       >>);
encode_fld_val167(ID,_T, 'ExtendedCommNote'                        ) -> encode_tagval(ID, <<"XCN"      >>);
encode_fld_val167(ID,_T, 'YankeeCertificateOfDeposit'              ) -> encode_tagval(ID, <<"YCD"      >>);
encode_fld_val167(ID,_T, 'AssetBackedSecurities'                   ) -> encode_tagval(ID, <<"ABS"      >>);
encode_fld_val167(ID,_T, 'CorpMortgageBackedSecurities'            ) -> encode_tagval(ID, <<"CMBS"     >>);
encode_fld_val167(ID,_T, 'CollateralizedMortgageObligation'        ) -> encode_tagval(ID, <<"CMO"      >>);
encode_fld_val167(ID,_T, 'IoetteMortgage'                          ) -> encode_tagval(ID, <<"IET"      >>);
encode_fld_val167(ID,_T, 'MortgageBackedSecurities'                ) -> encode_tagval(ID, <<"MBS"      >>);
encode_fld_val167(ID,_T, 'MortgageInterestOnly'                    ) -> encode_tagval(ID, <<"MIO"      >>);
encode_fld_val167(ID,_T, 'MortgagePrincipalOnly'                   ) -> encode_tagval(ID, <<"MPO"      >>);
encode_fld_val167(ID,_T, 'MortgagePrivatePlacement'                ) -> encode_tagval(ID, <<"MPP"      >>);
encode_fld_val167(ID,_T, 'MiscellaneousPassThrough'                ) -> encode_tagval(ID, <<"MPT"      >>);
encode_fld_val167(ID,_T, 'Pfandbriefe'                             ) -> encode_tagval(ID, <<"PFAND"    >>);
encode_fld_val167(ID,_T, 'ToBeAnnounced'                           ) -> encode_tagval(ID, <<"TBA"      >>);
encode_fld_val167(ID,_T, 'OtherAnticipationNotesBanGanEtc'         ) -> encode_tagval(ID, <<"AN"       >>);
encode_fld_val167(ID,_T, 'CertificateOfObligation'                 ) -> encode_tagval(ID, <<"COFO"     >>);
encode_fld_val167(ID,_T, 'CertificateOfParticipation'              ) -> encode_tagval(ID, <<"COFP"     >>);
encode_fld_val167(ID,_T, 'GeneralObligationBonds'                  ) -> encode_tagval(ID, <<"GO"       >>);
encode_fld_val167(ID,_T, 'MandatoryTender'                         ) -> encode_tagval(ID, <<"MT"       >>);
encode_fld_val167(ID,_T, 'RevenueAnticipationNote'                 ) -> encode_tagval(ID, <<"RAN"      >>);
encode_fld_val167(ID,_T, 'RevenueBonds'                            ) -> encode_tagval(ID, <<"REV"      >>);
encode_fld_val167(ID,_T, 'SpecialAssessment'                       ) -> encode_tagval(ID, <<"SPCLA"    >>);
encode_fld_val167(ID,_T, 'SpecialObligation'                       ) -> encode_tagval(ID, <<"SPCLO"    >>);
encode_fld_val167(ID,_T, 'SpecialTax'                              ) -> encode_tagval(ID, <<"SPCLT"    >>);
encode_fld_val167(ID,_T, 'TaxAnticipationNote'                     ) -> encode_tagval(ID, <<"TAN"      >>);
encode_fld_val167(ID,_T, 'TaxAllocation'                           ) -> encode_tagval(ID, <<"TAXA"     >>);
encode_fld_val167(ID,_T, 'TaxExemptCommercialPaper'                ) -> encode_tagval(ID, <<"TECP"     >>);
encode_fld_val167(ID,_T, 'TaxRevenueAnticipationNote'              ) -> encode_tagval(ID, <<"TRAN"     >>);
encode_fld_val167(ID,_T, 'VariableRateDemandNote'                  ) -> encode_tagval(ID, <<"VRDN"     >>);
encode_fld_val167(ID,_T, 'Warrant'                                 ) -> encode_tagval(ID, <<"WAR"      >>);
encode_fld_val167(ID,_T, 'MutualFund'                              ) -> encode_tagval(ID, <<"MF"       >>);
encode_fld_val167(ID,_T, 'MultiLegInstrument'                      ) -> encode_tagval(ID, <<"MLEG"     >>);
encode_fld_val167(ID,_T, 'NoSecurityType'                          ) -> encode_tagval(ID, <<"NONE"     >>);
encode_fld_val167(ID, T, V                                         ) -> try_encode_val(ID, T, V).

decode_fld_val169(Val) ->
  case Val of
    <<"0">> -> 'Other'           ; %% 0
    <<"1">> -> 'DtcSid'          ; %% 1
    <<"2">> -> 'ThomsonAlert'    ; %% 2
    <<"3">> -> 'AGlobalCustodian'; %% 3
    <<"4">> -> 'Accountnet'      ; %% 4
    _       -> Val
  end.

encode_fld_val169(ID,_T, 'Other'           ) -> encode_tagval(ID, <<"0">>);
encode_fld_val169(ID,_T, 'DtcSid'          ) -> encode_tagval(ID, <<"1">>);
encode_fld_val169(ID,_T, 'ThomsonAlert'    ) -> encode_tagval(ID, <<"2">>);
encode_fld_val169(ID,_T, 'AGlobalCustodian') -> encode_tagval(ID, <<"3">>);
encode_fld_val169(ID,_T, 'Accountnet'      ) -> encode_tagval(ID, <<"4">>);
encode_fld_val169(ID, T, V                 ) -> try_encode_val(ID, T, V).

decode_fld_val172(Val) ->
  case Val of
    <<"0">> -> 'VersusPaymentDeliver'; %% 0
    <<"1">> -> 'FreeDeliver'         ; %% 1
    <<"2">> -> 'TriParty'            ; %% 2
    <<"3">> -> 'HoldInCustody'       ; %% 3
    _       -> Val
  end.

encode_fld_val172(ID,_T, 'VersusPaymentDeliver') -> encode_tagval(ID, <<"0">>);
encode_fld_val172(ID,_T, 'FreeDeliver'         ) -> encode_tagval(ID, <<"1">>);
encode_fld_val172(ID,_T, 'TriParty'            ) -> encode_tagval(ID, <<"2">>);
encode_fld_val172(ID,_T, 'HoldInCustody'       ) -> encode_tagval(ID, <<"3">>);
encode_fld_val172(ID, T, V                     ) -> try_encode_val(ID, T, V).

decode_fld_val197(Val) ->
  case Val of
    <<"0">> -> 'FXNetting'; %% 0
    <<"1">> -> 'FXSwap'   ; %% 1
    _       -> Val
  end.

encode_fld_val197(ID,_T, 'FXNetting') -> encode_tagval(ID, <<"0">>);
encode_fld_val197(ID,_T, 'FXSwap'   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val197(ID, T, V          ) -> try_encode_val(ID, T, V).

decode_fld_val201(Val) ->
  case Val of
    <<"0">> -> 'Put' ; %% 0
    <<"1">> -> 'Call'; %% 1
    _       -> Val
  end.

encode_fld_val201(ID,_T, 'Put' ) -> encode_tagval(ID, <<"0">>);
encode_fld_val201(ID,_T, 'Call') -> encode_tagval(ID, <<"1">>);
encode_fld_val201(ID, T, V     ) -> try_encode_val(ID, T, V).

decode_fld_val203(Val) ->
  case Val of
    <<"0">> -> 'Covered'  ; %% 0
    <<"1">> -> 'Uncovered'; %% 1
    _       -> Val
  end.

encode_fld_val203(ID,_T, 'Covered'  ) -> encode_tagval(ID, <<"0">>);
encode_fld_val203(ID,_T, 'Uncovered') -> encode_tagval(ID, <<"1">>);
encode_fld_val203(ID, T, V          ) -> try_encode_val(ID, T, V).

decode_fld_val208(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val208(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val208(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val208(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val209(Val) ->
  case Val of
    <<"1">> -> 'Match'          ; %% 0
    <<"2">> -> 'Forward'        ; %% 1
    <<"3">> -> 'ForwardAndMatch'; %% 2
    _       -> Val
  end.

encode_fld_val209(ID,_T, 'Match'          ) -> encode_tagval(ID, <<"1">>);
encode_fld_val209(ID,_T, 'Forward'        ) -> encode_tagval(ID, <<"2">>);
encode_fld_val209(ID,_T, 'ForwardAndMatch') -> encode_tagval(ID, <<"3">>);
encode_fld_val209(ID, T, V                ) -> try_encode_val(ID, T, V).

decode_fld_val216(Val) ->
  case Val of
    <<"1">> -> 'TargetFirm'; %% 0
    <<"2">> -> 'TargetList'; %% 1
    <<"3">> -> 'BlockFirm' ; %% 2
    <<"4">> -> 'BlockList' ; %% 3
    _       -> Val
  end.

encode_fld_val216(ID,_T, 'TargetFirm') -> encode_tagval(ID, <<"1">>);
encode_fld_val216(ID,_T, 'TargetList') -> encode_tagval(ID, <<"2">>);
encode_fld_val216(ID,_T, 'BlockFirm' ) -> encode_tagval(ID, <<"3">>);
encode_fld_val216(ID,_T, 'BlockList' ) -> encode_tagval(ID, <<"4">>);
encode_fld_val216(ID, T, V           ) -> try_encode_val(ID, T, V).

decode_fld_val233(Val) ->
  case Val of
    <<"AMT"       >> -> 'Amt'                                                                                      ; %% 0
    <<"AUTOREINV" >> -> 'AutoReinvestmentAtRateOrBetter'                                                           ; %% 1
    <<"BANKQUAL"  >> -> 'BankQualified'                                                                            ; %% 2
    <<"BGNCON"    >> -> 'BargainConditionsSee'                                                                     ; %% 3
    <<"COUPON"    >> -> 'CouponRange'                                                                              ; %% 4
    <<"CURRENCY"  >> -> 'IsoCurrencyCode'                                                                          ; %% 5
    <<"CUSTOMDATE">> -> 'CustomStartEndDate'                                                                       ; %% 6
    <<"GEOG"      >> -> 'GeographicsAndRange'                                                                      ; %% 7
    <<"HAIRCUT"   >> -> 'ValuationDiscount'                                                                        ; %% 8
    <<"INSURED"   >> -> 'Insured'                                                                                  ; %% 9
    <<"ISSUE"     >> -> 'YearOrYearMonthOfIssue'                                                                   ; %% 10
    <<"ISSUER"    >> -> 'IssuersTicker'                                                                            ; %% 11
    <<"ISSUESIZE" >> -> 'IssueSizeRange'                                                                           ; %% 12
    <<"LOOKBACK"  >> -> 'LookbackDays'                                                                             ; %% 13
    <<"LOT"       >> -> 'ExplicitLotIdentifier'                                                                    ; %% 14
    <<"LOTVAR"    >> -> 'LotVariance'                                                                              ; %% 15
    <<"MAT"       >> -> 'MaturityYearAndMonth'                                                                     ; %% 16
    <<"MATURITY"  >> -> 'MaturityRange'                                                                            ; %% 17
    <<"MAXSUBS"   >> -> 'MaximumSubstitutions'                                                                     ; %% 18
    <<"MINQTY"    >> -> 'MinimumQuantity'                                                                          ; %% 19
    <<"MININCR"   >> -> 'MinimumIncrement'                                                                         ; %% 20
    <<"MINDNOM"   >> -> 'MinimumDenomination'                                                                      ; %% 21
    <<"PAYFREQ"   >> -> 'PaymentFrequencyCalendar'                                                                 ; %% 22
    <<"PIECES"    >> -> 'NumberOfPieces'                                                                           ; %% 23
    <<"PMAX"      >> -> 'PoolsMaximum'                                                                             ; %% 24
    <<"PPM"       >> -> 'PoolsPerMillion'                                                                          ; %% 25
    <<"PPL"       >> -> 'PoolsPerLot'                                                                              ; %% 26
    <<"PPT"       >> -> 'PoolsPerTrade'                                                                            ; %% 27
    <<"PRICE"     >> -> 'PriceRange'                                                                               ; %% 28
    <<"PRICEFREQ" >> -> 'PricingFrequency'                                                                         ; %% 29
    <<"PROD"      >> -> 'ProductionYear'                                                                           ; %% 30
    <<"PROTECT"   >> -> 'CallProtection'                                                                           ; %% 31
    <<"PURPOSE"   >> -> 'Purpose'                                                                                  ; %% 32
    <<"PXSOURCE"  >> -> 'BenchmarkPriceSource'                                                                     ; %% 33
    <<"RATING"    >> -> 'RatingSourceAndRange'                                                                     ; %% 34
    <<"REDEMPTION">> -> 'TypeOfRedemptionValuesAreNoncallableCallablePrefundedEscrowedtomaturityPutableConvertible'; %% 35
    <<"RESTRICTED">> -> 'Restricted'                                                                               ; %% 36
    <<"SECTOR"    >> -> 'MarketSector'                                                                             ; %% 37
    <<"SECTYPE"   >> -> 'SecuritytypeIncludedOrExcluded'                                                           ; %% 38
    <<"STRUCT"    >> -> 'Structure'                                                                                ; %% 39
    <<"SUBSFREQ"  >> -> 'SubstitutionsFrequency'                                                                   ; %% 40
    <<"SUBSLEFT"  >> -> 'SubstitutionsLeft'                                                                        ; %% 41
    <<"TEXT"      >> -> 'FreeformText'                                                                             ; %% 42
    <<"TRDVAR"    >> -> 'TradeVariance'                                                                            ; %% 43
    <<"WAC"       >> -> 'WeightedAverageCouponvalueInPercent'                                                      ; %% 44
    <<"WAL"       >> -> 'WeightedAverageLifeCouponValueInPercent'                                                  ; %% 45
    <<"WALA"      >> -> 'WeightedAverageLoanAgeValueInMonths'                                                      ; %% 46
    <<"WAM"       >> -> 'WeightedAverageMaturityValueInMonths'                                                     ; %% 47
    <<"WHOLE"     >> -> 'WholePool'                                                                                ; %% 48
    <<"YIELD"     >> -> 'YieldRange'                                                                               ; %% 49
    _                -> Val
  end.

encode_fld_val233(ID,_T, 'Amt'                                                                                      ) -> encode_tagval(ID, <<"AMT"       >>);
encode_fld_val233(ID,_T, 'AutoReinvestmentAtRateOrBetter'                                                           ) -> encode_tagval(ID, <<"AUTOREINV" >>);
encode_fld_val233(ID,_T, 'BankQualified'                                                                            ) -> encode_tagval(ID, <<"BANKQUAL"  >>);
encode_fld_val233(ID,_T, 'BargainConditionsSee'                                                                     ) -> encode_tagval(ID, <<"BGNCON"    >>);
encode_fld_val233(ID,_T, 'CouponRange'                                                                              ) -> encode_tagval(ID, <<"COUPON"    >>);
encode_fld_val233(ID,_T, 'IsoCurrencyCode'                                                                          ) -> encode_tagval(ID, <<"CURRENCY"  >>);
encode_fld_val233(ID,_T, 'CustomStartEndDate'                                                                       ) -> encode_tagval(ID, <<"CUSTOMDATE">>);
encode_fld_val233(ID,_T, 'GeographicsAndRange'                                                                      ) -> encode_tagval(ID, <<"GEOG"      >>);
encode_fld_val233(ID,_T, 'ValuationDiscount'                                                                        ) -> encode_tagval(ID, <<"HAIRCUT"   >>);
encode_fld_val233(ID,_T, 'Insured'                                                                                  ) -> encode_tagval(ID, <<"INSURED"   >>);
encode_fld_val233(ID,_T, 'YearOrYearMonthOfIssue'                                                                   ) -> encode_tagval(ID, <<"ISSUE"     >>);
encode_fld_val233(ID,_T, 'IssuersTicker'                                                                            ) -> encode_tagval(ID, <<"ISSUER"    >>);
encode_fld_val233(ID,_T, 'IssueSizeRange'                                                                           ) -> encode_tagval(ID, <<"ISSUESIZE" >>);
encode_fld_val233(ID,_T, 'LookbackDays'                                                                             ) -> encode_tagval(ID, <<"LOOKBACK"  >>);
encode_fld_val233(ID,_T, 'ExplicitLotIdentifier'                                                                    ) -> encode_tagval(ID, <<"LOT"       >>);
encode_fld_val233(ID,_T, 'LotVariance'                                                                              ) -> encode_tagval(ID, <<"LOTVAR"    >>);
encode_fld_val233(ID,_T, 'MaturityYearAndMonth'                                                                     ) -> encode_tagval(ID, <<"MAT"       >>);
encode_fld_val233(ID,_T, 'MaturityRange'                                                                            ) -> encode_tagval(ID, <<"MATURITY"  >>);
encode_fld_val233(ID,_T, 'MaximumSubstitutions'                                                                     ) -> encode_tagval(ID, <<"MAXSUBS"   >>);
encode_fld_val233(ID,_T, 'MinimumQuantity'                                                                          ) -> encode_tagval(ID, <<"MINQTY"    >>);
encode_fld_val233(ID,_T, 'MinimumIncrement'                                                                         ) -> encode_tagval(ID, <<"MININCR"   >>);
encode_fld_val233(ID,_T, 'MinimumDenomination'                                                                      ) -> encode_tagval(ID, <<"MINDNOM"   >>);
encode_fld_val233(ID,_T, 'PaymentFrequencyCalendar'                                                                 ) -> encode_tagval(ID, <<"PAYFREQ"   >>);
encode_fld_val233(ID,_T, 'NumberOfPieces'                                                                           ) -> encode_tagval(ID, <<"PIECES"    >>);
encode_fld_val233(ID,_T, 'PoolsMaximum'                                                                             ) -> encode_tagval(ID, <<"PMAX"      >>);
encode_fld_val233(ID,_T, 'PoolsPerMillion'                                                                          ) -> encode_tagval(ID, <<"PPM"       >>);
encode_fld_val233(ID,_T, 'PoolsPerLot'                                                                              ) -> encode_tagval(ID, <<"PPL"       >>);
encode_fld_val233(ID,_T, 'PoolsPerTrade'                                                                            ) -> encode_tagval(ID, <<"PPT"       >>);
encode_fld_val233(ID,_T, 'PriceRange'                                                                               ) -> encode_tagval(ID, <<"PRICE"     >>);
encode_fld_val233(ID,_T, 'PricingFrequency'                                                                         ) -> encode_tagval(ID, <<"PRICEFREQ" >>);
encode_fld_val233(ID,_T, 'ProductionYear'                                                                           ) -> encode_tagval(ID, <<"PROD"      >>);
encode_fld_val233(ID,_T, 'CallProtection'                                                                           ) -> encode_tagval(ID, <<"PROTECT"   >>);
encode_fld_val233(ID,_T, 'Purpose'                                                                                  ) -> encode_tagval(ID, <<"PURPOSE"   >>);
encode_fld_val233(ID,_T, 'BenchmarkPriceSource'                                                                     ) -> encode_tagval(ID, <<"PXSOURCE"  >>);
encode_fld_val233(ID,_T, 'RatingSourceAndRange'                                                                     ) -> encode_tagval(ID, <<"RATING"    >>);
encode_fld_val233(ID,_T, 'TypeOfRedemptionValuesAreNoncallableCallablePrefundedEscrowedtomaturityPutableConvertible') -> encode_tagval(ID, <<"REDEMPTION">>);
encode_fld_val233(ID,_T, 'Restricted'                                                                               ) -> encode_tagval(ID, <<"RESTRICTED">>);
encode_fld_val233(ID,_T, 'MarketSector'                                                                             ) -> encode_tagval(ID, <<"SECTOR"    >>);
encode_fld_val233(ID,_T, 'SecuritytypeIncludedOrExcluded'                                                           ) -> encode_tagval(ID, <<"SECTYPE"   >>);
encode_fld_val233(ID,_T, 'Structure'                                                                                ) -> encode_tagval(ID, <<"STRUCT"    >>);
encode_fld_val233(ID,_T, 'SubstitutionsFrequency'                                                                   ) -> encode_tagval(ID, <<"SUBSFREQ"  >>);
encode_fld_val233(ID,_T, 'SubstitutionsLeft'                                                                        ) -> encode_tagval(ID, <<"SUBSLEFT"  >>);
encode_fld_val233(ID,_T, 'FreeformText'                                                                             ) -> encode_tagval(ID, <<"TEXT"      >>);
encode_fld_val233(ID,_T, 'TradeVariance'                                                                            ) -> encode_tagval(ID, <<"TRDVAR"    >>);
encode_fld_val233(ID,_T, 'WeightedAverageCouponvalueInPercent'                                                      ) -> encode_tagval(ID, <<"WAC"       >>);
encode_fld_val233(ID,_T, 'WeightedAverageLifeCouponValueInPercent'                                                  ) -> encode_tagval(ID, <<"WAL"       >>);
encode_fld_val233(ID,_T, 'WeightedAverageLoanAgeValueInMonths'                                                      ) -> encode_tagval(ID, <<"WALA"      >>);
encode_fld_val233(ID,_T, 'WeightedAverageMaturityValueInMonths'                                                     ) -> encode_tagval(ID, <<"WAM"       >>);
encode_fld_val233(ID,_T, 'WholePool'                                                                                ) -> encode_tagval(ID, <<"WHOLE"     >>);
encode_fld_val233(ID,_T, 'YieldRange'                                                                               ) -> encode_tagval(ID, <<"YIELD"     >>);
encode_fld_val233(ID, T, V                                                                                          ) -> try_encode_val(ID, T, V).

decode_fld_val235(Val) ->
  case Val of
    <<"AFTERTAX"      >> -> 'AfterTaxYield'                ; %% 0
    <<"ANNUAL"        >> -> 'AnnualYield'                  ; %% 1
    <<"ATISSUE"       >> -> 'YieldAtIssue'                 ; %% 2
    <<"AVGMATURITY"   >> -> 'YieldToAverageMaturity'       ; %% 3
    <<"BOOK"          >> -> 'BookYield'                    ; %% 4
    <<"CALL"          >> -> 'YieldToNextCall'              ; %% 5
    <<"CHANGE"        >> -> 'YieldChangeSinceClose'        ; %% 6
    <<"CLOSE"         >> -> 'ClosingYield'                 ; %% 7
    <<"COMPOUND"      >> -> 'CompoundYield'                ; %% 8
    <<"CURRENT"       >> -> 'CurrentYield'                 ; %% 9
    <<"GROSS"         >> -> 'TrueGrossYield'               ; %% 10
    <<"GOVTEQUIV"     >> -> 'GovernmentEquivalentYield'    ; %% 11
    <<"INFLATION"     >> -> 'YieldWithInflationAssumption' ; %% 12
    <<"INVERSEFLOATER">> -> 'InverseFloaterBondYield'      ; %% 13
    <<"LASTCLOSE"     >> -> 'MostRecentClosingYield'       ; %% 14
    <<"LASTMONTH"     >> -> 'ClosingYieldMostRecentMonth'  ; %% 15
    <<"LASTQUARTER"   >> -> 'ClosingYieldMostRecentQuarter'; %% 16
    <<"LASTYEAR"      >> -> 'ClosingYieldMostRecentYear'   ; %% 17
    <<"LONGAVGLIFE"   >> -> 'YieldToLongestAverageLife'    ; %% 18
    <<"MARK"          >> -> 'MarkToMarketYield'            ; %% 19
    <<"MATURITY"      >> -> 'YieldToMaturity'              ; %% 20
    <<"NEXTREFUND"    >> -> 'YieldToNextRefund'            ; %% 21
    <<"OPENAVG"       >> -> 'OpenAverageYield'             ; %% 22
    <<"PUT"           >> -> 'YieldToNextPut'               ; %% 23
    <<"PREVCLOSE"     >> -> 'PreviousCloseYield'           ; %% 24
    <<"PROCEEDS"      >> -> 'ProceedsYield'                ; %% 25
    <<"SEMIANNUAL"    >> -> 'SemiAnnualYield'              ; %% 26
    <<"SHORTAVGLIFE"  >> -> 'YieldToShortestAverageLife'   ; %% 27
    <<"SIMPLE"        >> -> 'SimpleYield'                  ; %% 28
    <<"TAXEQUIV"      >> -> 'TaxEquivalentYield'           ; %% 29
    <<"TENDER"        >> -> 'YieldToTenderDate'            ; %% 30
    <<"TRUE"          >> -> 'TrueYield'                    ; %% 31
    <<"VALUE1/32"     >> -> 'YieldValueOf132'              ; %% 32
    <<"WORST"         >> -> 'YieldToWorst'                 ; %% 33
    _                    -> Val
  end.

encode_fld_val235(ID,_T, 'AfterTaxYield'                ) -> encode_tagval(ID, <<"AFTERTAX"      >>);
encode_fld_val235(ID,_T, 'AnnualYield'                  ) -> encode_tagval(ID, <<"ANNUAL"        >>);
encode_fld_val235(ID,_T, 'YieldAtIssue'                 ) -> encode_tagval(ID, <<"ATISSUE"       >>);
encode_fld_val235(ID,_T, 'YieldToAverageMaturity'       ) -> encode_tagval(ID, <<"AVGMATURITY"   >>);
encode_fld_val235(ID,_T, 'BookYield'                    ) -> encode_tagval(ID, <<"BOOK"          >>);
encode_fld_val235(ID,_T, 'YieldToNextCall'              ) -> encode_tagval(ID, <<"CALL"          >>);
encode_fld_val235(ID,_T, 'YieldChangeSinceClose'        ) -> encode_tagval(ID, <<"CHANGE"        >>);
encode_fld_val235(ID,_T, 'ClosingYield'                 ) -> encode_tagval(ID, <<"CLOSE"         >>);
encode_fld_val235(ID,_T, 'CompoundYield'                ) -> encode_tagval(ID, <<"COMPOUND"      >>);
encode_fld_val235(ID,_T, 'CurrentYield'                 ) -> encode_tagval(ID, <<"CURRENT"       >>);
encode_fld_val235(ID,_T, 'TrueGrossYield'               ) -> encode_tagval(ID, <<"GROSS"         >>);
encode_fld_val235(ID,_T, 'GovernmentEquivalentYield'    ) -> encode_tagval(ID, <<"GOVTEQUIV"     >>);
encode_fld_val235(ID,_T, 'YieldWithInflationAssumption' ) -> encode_tagval(ID, <<"INFLATION"     >>);
encode_fld_val235(ID,_T, 'InverseFloaterBondYield'      ) -> encode_tagval(ID, <<"INVERSEFLOATER">>);
encode_fld_val235(ID,_T, 'MostRecentClosingYield'       ) -> encode_tagval(ID, <<"LASTCLOSE"     >>);
encode_fld_val235(ID,_T, 'ClosingYieldMostRecentMonth'  ) -> encode_tagval(ID, <<"LASTMONTH"     >>);
encode_fld_val235(ID,_T, 'ClosingYieldMostRecentQuarter') -> encode_tagval(ID, <<"LASTQUARTER"   >>);
encode_fld_val235(ID,_T, 'ClosingYieldMostRecentYear'   ) -> encode_tagval(ID, <<"LASTYEAR"      >>);
encode_fld_val235(ID,_T, 'YieldToLongestAverageLife'    ) -> encode_tagval(ID, <<"LONGAVGLIFE"   >>);
encode_fld_val235(ID,_T, 'MarkToMarketYield'            ) -> encode_tagval(ID, <<"MARK"          >>);
encode_fld_val235(ID,_T, 'YieldToMaturity'              ) -> encode_tagval(ID, <<"MATURITY"      >>);
encode_fld_val235(ID,_T, 'YieldToNextRefund'            ) -> encode_tagval(ID, <<"NEXTREFUND"    >>);
encode_fld_val235(ID,_T, 'OpenAverageYield'             ) -> encode_tagval(ID, <<"OPENAVG"       >>);
encode_fld_val235(ID,_T, 'YieldToNextPut'               ) -> encode_tagval(ID, <<"PUT"           >>);
encode_fld_val235(ID,_T, 'PreviousCloseYield'           ) -> encode_tagval(ID, <<"PREVCLOSE"     >>);
encode_fld_val235(ID,_T, 'ProceedsYield'                ) -> encode_tagval(ID, <<"PROCEEDS"      >>);
encode_fld_val235(ID,_T, 'SemiAnnualYield'              ) -> encode_tagval(ID, <<"SEMIANNUAL"    >>);
encode_fld_val235(ID,_T, 'YieldToShortestAverageLife'   ) -> encode_tagval(ID, <<"SHORTAVGLIFE"  >>);
encode_fld_val235(ID,_T, 'SimpleYield'                  ) -> encode_tagval(ID, <<"SIMPLE"        >>);
encode_fld_val235(ID,_T, 'TaxEquivalentYield'           ) -> encode_tagval(ID, <<"TAXEQUIV"      >>);
encode_fld_val235(ID,_T, 'YieldToTenderDate'            ) -> encode_tagval(ID, <<"TENDER"        >>);
encode_fld_val235(ID,_T, 'TrueYield'                    ) -> encode_tagval(ID, <<"TRUE"          >>);
encode_fld_val235(ID,_T, 'YieldValueOf132'              ) -> encode_tagval(ID, <<"VALUE1/32"     >>);
encode_fld_val235(ID,_T, 'YieldToWorst'                 ) -> encode_tagval(ID, <<"WORST"         >>);
encode_fld_val235(ID, T, V                              ) -> try_encode_val(ID, T, V).

decode_fld_val258(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val258(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val258(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val258(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val263(Val) ->
  case Val of
    <<"0">> -> 'Snapshot'                                ; %% 0
    <<"1">> -> 'SnapshotPlusUpdates'                     ; %% 1
    <<"2">> -> 'DisablePreviousSnapshotPlusUpdateRequest'; %% 2
    _       -> Val
  end.

encode_fld_val263(ID,_T, 'Snapshot'                                ) -> encode_tagval(ID, <<"0">>);
encode_fld_val263(ID,_T, 'SnapshotPlusUpdates'                     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val263(ID,_T, 'DisablePreviousSnapshotPlusUpdateRequest') -> encode_tagval(ID, <<"2">>);
encode_fld_val263(ID, T, V                                         ) -> try_encode_val(ID, T, V).

decode_fld_val265(Val) ->
  case Val of
    <<"0">> -> 'FullRefresh'       ; %% 0
    <<"1">> -> 'IncrementalRefresh'; %% 1
    _       -> Val
  end.

encode_fld_val265(ID,_T, 'FullRefresh'       ) -> encode_tagval(ID, <<"0">>);
encode_fld_val265(ID,_T, 'IncrementalRefresh') -> encode_tagval(ID, <<"1">>);
encode_fld_val265(ID, T, V                   ) -> try_encode_val(ID, T, V).

decode_fld_val266(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val266(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val266(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val266(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val269(Val) ->
  case Val of
    <<"0">> -> 'Bid'                    ; %% 0
    <<"1">> -> 'Offer'                  ; %% 1
    <<"2">> -> 'Trade'                  ; %% 2
    <<"3">> -> 'IndexValue'             ; %% 3
    <<"4">> -> 'OpeningPrice'           ; %% 4
    <<"5">> -> 'ClosingPrice'           ; %% 5
    <<"6">> -> 'SettlementPrice'        ; %% 6
    <<"7">> -> 'TradingSessionHighPrice'; %% 7
    <<"8">> -> 'TradingSessionLowPrice' ; %% 8
    <<"9">> -> 'TradingSessionVwapPrice'; %% 9
    <<"A">> -> 'Imbalance'              ; %% 10
    <<"B">> -> 'TradeVolume'            ; %% 11
    <<"C">> -> 'OpenInterest'           ; %% 12
    _       -> Val
  end.

encode_fld_val269(ID,_T, 'Bid'                    ) -> encode_tagval(ID, <<"0">>);
encode_fld_val269(ID,_T, 'Offer'                  ) -> encode_tagval(ID, <<"1">>);
encode_fld_val269(ID,_T, 'Trade'                  ) -> encode_tagval(ID, <<"2">>);
encode_fld_val269(ID,_T, 'IndexValue'             ) -> encode_tagval(ID, <<"3">>);
encode_fld_val269(ID,_T, 'OpeningPrice'           ) -> encode_tagval(ID, <<"4">>);
encode_fld_val269(ID,_T, 'ClosingPrice'           ) -> encode_tagval(ID, <<"5">>);
encode_fld_val269(ID,_T, 'SettlementPrice'        ) -> encode_tagval(ID, <<"6">>);
encode_fld_val269(ID,_T, 'TradingSessionHighPrice') -> encode_tagval(ID, <<"7">>);
encode_fld_val269(ID,_T, 'TradingSessionLowPrice' ) -> encode_tagval(ID, <<"8">>);
encode_fld_val269(ID,_T, 'TradingSessionVwapPrice') -> encode_tagval(ID, <<"9">>);
encode_fld_val269(ID,_T, 'Imbalance'              ) -> encode_tagval(ID, <<"A">>);
encode_fld_val269(ID,_T, 'TradeVolume'            ) -> encode_tagval(ID, <<"B">>);
encode_fld_val269(ID,_T, 'OpenInterest'           ) -> encode_tagval(ID, <<"C">>);
encode_fld_val269(ID, T, V                        ) -> try_encode_val(ID, T, V).

decode_fld_val274(Val) ->
  case Val of
    <<"0">> -> 'PlusTick'     ; %% 0
    <<"1">> -> 'ZeroPlusTick' ; %% 1
    <<"2">> -> 'MinusTick'    ; %% 2
    <<"3">> -> 'ZeroMinusTick'; %% 3
    _       -> Val
  end.

encode_fld_val274(ID,_T, 'PlusTick'     ) -> encode_tagval(ID, <<"0">>);
encode_fld_val274(ID,_T, 'ZeroPlusTick' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val274(ID,_T, 'MinusTick'    ) -> encode_tagval(ID, <<"2">>);
encode_fld_val274(ID,_T, 'ZeroMinusTick') -> encode_tagval(ID, <<"3">>);
encode_fld_val274(ID, T, V              ) -> try_encode_val(ID, T, V).

decode_fld_val276(Val) ->
  case Val of
    <<"A">> -> 'Open'            ; %% 0
    <<"B">> -> 'Closed'          ; %% 1
    <<"C">> -> 'ExchangeBest'    ; %% 2
    <<"D">> -> 'ConsolidatedBest'; %% 3
    <<"E">> -> 'Locked'          ; %% 4
    <<"F">> -> 'Crossed'         ; %% 5
    <<"G">> -> 'Depth'           ; %% 6
    <<"H">> -> 'FastTrading'     ; %% 7
    <<"I">> -> 'NonFirm'         ; %% 8
    _       -> Val
  end.

encode_fld_val276(ID,_T, 'Open'            ) -> encode_tagval(ID, <<"A">>);
encode_fld_val276(ID,_T, 'Closed'          ) -> encode_tagval(ID, <<"B">>);
encode_fld_val276(ID,_T, 'ExchangeBest'    ) -> encode_tagval(ID, <<"C">>);
encode_fld_val276(ID,_T, 'ConsolidatedBest') -> encode_tagval(ID, <<"D">>);
encode_fld_val276(ID,_T, 'Locked'          ) -> encode_tagval(ID, <<"E">>);
encode_fld_val276(ID,_T, 'Crossed'         ) -> encode_tagval(ID, <<"F">>);
encode_fld_val276(ID,_T, 'Depth'           ) -> encode_tagval(ID, <<"G">>);
encode_fld_val276(ID,_T, 'FastTrading'     ) -> encode_tagval(ID, <<"H">>);
encode_fld_val276(ID,_T, 'NonFirm'         ) -> encode_tagval(ID, <<"I">>);
encode_fld_val276(ID, T, V                 ) -> try_encode_val(ID, T, V).

decode_fld_val277(Val) ->
  case Val of
    <<"A">> -> 'Cash'                ; %% 0
    <<"B">> -> 'AveragePriceTrade'   ; %% 1
    <<"C">> -> 'CashTrade'           ; %% 2
    <<"D">> -> 'NextDay'             ; %% 3
    <<"E">> -> 'Opening'             ; %% 4
    <<"F">> -> 'IntradayTradeDetail' ; %% 5
    <<"G">> -> 'Rule127Trade'        ; %% 6
    <<"H">> -> 'Rule155Trade'        ; %% 7
    <<"I">> -> 'SoldLast'            ; %% 8
    <<"J">> -> 'NextDayTrade'        ; %% 9
    <<"K">> -> 'Opened'              ; %% 10
    <<"L">> -> 'Seller'              ; %% 11
    <<"M">> -> 'Sold'                ; %% 12
    <<"N">> -> 'StoppedStock'        ; %% 13
    <<"P">> -> 'ImbalanceMoreBuyers' ; %% 14
    <<"Q">> -> 'ImbalanceMoreSellers'; %% 15
    <<"R">> -> 'OpeningPrice'        ; %% 16
    _       -> Val
  end.

encode_fld_val277(ID,_T, 'Cash'                ) -> encode_tagval(ID, <<"A">>);
encode_fld_val277(ID,_T, 'AveragePriceTrade'   ) -> encode_tagval(ID, <<"B">>);
encode_fld_val277(ID,_T, 'CashTrade'           ) -> encode_tagval(ID, <<"C">>);
encode_fld_val277(ID,_T, 'NextDay'             ) -> encode_tagval(ID, <<"D">>);
encode_fld_val277(ID,_T, 'Opening'             ) -> encode_tagval(ID, <<"E">>);
encode_fld_val277(ID,_T, 'IntradayTradeDetail' ) -> encode_tagval(ID, <<"F">>);
encode_fld_val277(ID,_T, 'Rule127Trade'        ) -> encode_tagval(ID, <<"G">>);
encode_fld_val277(ID,_T, 'Rule155Trade'        ) -> encode_tagval(ID, <<"H">>);
encode_fld_val277(ID,_T, 'SoldLast'            ) -> encode_tagval(ID, <<"I">>);
encode_fld_val277(ID,_T, 'NextDayTrade'        ) -> encode_tagval(ID, <<"J">>);
encode_fld_val277(ID,_T, 'Opened'              ) -> encode_tagval(ID, <<"K">>);
encode_fld_val277(ID,_T, 'Seller'              ) -> encode_tagval(ID, <<"L">>);
encode_fld_val277(ID,_T, 'Sold'                ) -> encode_tagval(ID, <<"M">>);
encode_fld_val277(ID,_T, 'StoppedStock'        ) -> encode_tagval(ID, <<"N">>);
encode_fld_val277(ID,_T, 'ImbalanceMoreBuyers' ) -> encode_tagval(ID, <<"P">>);
encode_fld_val277(ID,_T, 'ImbalanceMoreSellers') -> encode_tagval(ID, <<"Q">>);
encode_fld_val277(ID,_T, 'OpeningPrice'        ) -> encode_tagval(ID, <<"R">>);
encode_fld_val277(ID, T, V                     ) -> try_encode_val(ID, T, V).

decode_fld_val279(Val) ->
  case Val of
    <<"0">> -> 'New'   ; %% 0
    <<"1">> -> 'Change'; %% 1
    <<"2">> -> 'Delete'; %% 2
    _       -> Val
  end.

encode_fld_val279(ID,_T, 'New'   ) -> encode_tagval(ID, <<"0">>);
encode_fld_val279(ID,_T, 'Change') -> encode_tagval(ID, <<"1">>);
encode_fld_val279(ID,_T, 'Delete') -> encode_tagval(ID, <<"2">>);
encode_fld_val279(ID, T, V       ) -> try_encode_val(ID, T, V).

decode_fld_val281(Val) ->
  case Val of
    <<"0">> -> 'UnknownSymbol'                     ; %% 0
    <<"1">> -> 'DuplicateMdreqid'                  ; %% 1
    <<"2">> -> 'InsufficientBandwidth'             ; %% 2
    <<"3">> -> 'InsufficientPermissions'           ; %% 3
    <<"4">> -> 'UnsupportedSubscriptionrequesttype'; %% 4
    <<"5">> -> 'UnsupportedMarketdepth'            ; %% 5
    <<"6">> -> 'UnsupportedMdupdatetype'           ; %% 6
    <<"7">> -> 'UnsupportedAggregatedbook'         ; %% 7
    <<"8">> -> 'UnsupportedMdentrytype'            ; %% 8
    <<"9">> -> 'UnsupportedTradingsessionid'       ; %% 9
    <<"A">> -> 'UnsupportedScope'                  ; %% 10
    <<"B">> -> 'UnsupportedOpenclosesettleflag'    ; %% 11
    <<"C">> -> 'UnsupportedMdimplicitdelete'       ; %% 12
    _       -> Val
  end.

encode_fld_val281(ID,_T, 'UnknownSymbol'                     ) -> encode_tagval(ID, <<"0">>);
encode_fld_val281(ID,_T, 'DuplicateMdreqid'                  ) -> encode_tagval(ID, <<"1">>);
encode_fld_val281(ID,_T, 'InsufficientBandwidth'             ) -> encode_tagval(ID, <<"2">>);
encode_fld_val281(ID,_T, 'InsufficientPermissions'           ) -> encode_tagval(ID, <<"3">>);
encode_fld_val281(ID,_T, 'UnsupportedSubscriptionrequesttype') -> encode_tagval(ID, <<"4">>);
encode_fld_val281(ID,_T, 'UnsupportedMarketdepth'            ) -> encode_tagval(ID, <<"5">>);
encode_fld_val281(ID,_T, 'UnsupportedMdupdatetype'           ) -> encode_tagval(ID, <<"6">>);
encode_fld_val281(ID,_T, 'UnsupportedAggregatedbook'         ) -> encode_tagval(ID, <<"7">>);
encode_fld_val281(ID,_T, 'UnsupportedMdentrytype'            ) -> encode_tagval(ID, <<"8">>);
encode_fld_val281(ID,_T, 'UnsupportedTradingsessionid'       ) -> encode_tagval(ID, <<"9">>);
encode_fld_val281(ID,_T, 'UnsupportedScope'                  ) -> encode_tagval(ID, <<"A">>);
encode_fld_val281(ID,_T, 'UnsupportedOpenclosesettleflag'    ) -> encode_tagval(ID, <<"B">>);
encode_fld_val281(ID,_T, 'UnsupportedMdimplicitdelete'       ) -> encode_tagval(ID, <<"C">>);
encode_fld_val281(ID, T, V                                   ) -> try_encode_val(ID, T, V).

decode_fld_val285(Val) ->
  case Val of
    <<"0">> -> 'Cancelation'; %% 0
    <<"1">> -> 'Error'      ; %% 1
    _       -> Val
  end.

encode_fld_val285(ID,_T, 'Cancelation') -> encode_tagval(ID, <<"0">>);
encode_fld_val285(ID,_T, 'Error'      ) -> encode_tagval(ID, <<"1">>);
encode_fld_val285(ID, T, V            ) -> try_encode_val(ID, T, V).

decode_fld_val286(Val) ->
  case Val of
    <<"0">> -> 'DailyOpen'                   ; %% 0
    <<"1">> -> 'SessionOpen'                 ; %% 1
    <<"2">> -> 'DeliverySettlementEntry'     ; %% 2
    <<"3">> -> 'ExpectedEntry'               ; %% 3
    <<"4">> -> 'EntryFromPreviousBusinessDay'; %% 4
    <<"5">> -> 'TheoreticalPriceValue'       ; %% 5
    _       -> Val
  end.

encode_fld_val286(ID,_T, 'DailyOpen'                   ) -> encode_tagval(ID, <<"0">>);
encode_fld_val286(ID,_T, 'SessionOpen'                 ) -> encode_tagval(ID, <<"1">>);
encode_fld_val286(ID,_T, 'DeliverySettlementEntry'     ) -> encode_tagval(ID, <<"2">>);
encode_fld_val286(ID,_T, 'ExpectedEntry'               ) -> encode_tagval(ID, <<"3">>);
encode_fld_val286(ID,_T, 'EntryFromPreviousBusinessDay') -> encode_tagval(ID, <<"4">>);
encode_fld_val286(ID,_T, 'TheoreticalPriceValue'       ) -> encode_tagval(ID, <<"5">>);
encode_fld_val286(ID, T, V                             ) -> try_encode_val(ID, T, V).

decode_fld_val291(Val) ->
  case Val of
    <<"1">> -> 'Bankrupt'        ; %% 0
    <<"2">> -> 'PendingDelisting'; %% 1
    _       -> Val
  end.

encode_fld_val291(ID,_T, 'Bankrupt'        ) -> encode_tagval(ID, <<"1">>);
encode_fld_val291(ID,_T, 'PendingDelisting') -> encode_tagval(ID, <<"2">>);
encode_fld_val291(ID, T, V                 ) -> try_encode_val(ID, T, V).

decode_fld_val292(Val) ->
  case Val of
    <<"A">> -> 'ExDividend'    ; %% 0
    <<"B">> -> 'ExDistribution'; %% 1
    <<"C">> -> 'ExRights'      ; %% 2
    <<"D">> -> 'New'           ; %% 3
    <<"E">> -> 'ExInterest'    ; %% 4
    _       -> Val
  end.

encode_fld_val292(ID,_T, 'ExDividend'    ) -> encode_tagval(ID, <<"A">>);
encode_fld_val292(ID,_T, 'ExDistribution') -> encode_tagval(ID, <<"B">>);
encode_fld_val292(ID,_T, 'ExRights'      ) -> encode_tagval(ID, <<"C">>);
encode_fld_val292(ID,_T, 'New'           ) -> encode_tagval(ID, <<"D">>);
encode_fld_val292(ID,_T, 'ExInterest'    ) -> encode_tagval(ID, <<"E">>);
encode_fld_val292(ID, T, V               ) -> try_encode_val(ID, T, V).

decode_fld_val297(Val) ->
  case Val of
    <<"0" >> -> 'Accepted'                ; %% 0
    <<"1" >> -> 'CanceledForSymbol'       ; %% 1
    <<"2" >> -> 'CanceledForSecurityType' ; %% 2
    <<"3" >> -> 'CanceledForUnderlying'   ; %% 3
    <<"4" >> -> 'CanceledAll'             ; %% 4
    <<"5" >> -> 'Rejected'                ; %% 5
    <<"6" >> -> 'RemovedFromMarket'       ; %% 6
    <<"7" >> -> 'Expired'                 ; %% 7
    <<"8" >> -> 'Query'                   ; %% 8
    <<"9" >> -> 'QuoteNotFound'           ; %% 9
    <<"10">> -> 'Pending'                 ; %% 10
    <<"11">> -> 'Pass'                    ; %% 11
    <<"12">> -> 'LockedMarketWarning'     ; %% 12
    <<"13">> -> 'CrossMarketWarning'      ; %% 13
    <<"14">> -> 'CanceledDueToLockMarket' ; %% 14
    <<"15">> -> 'CanceledDueToCrossMarket'; %% 15
    _        -> Val
  end.

encode_fld_val297(ID,_T, 'Accepted'                ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val297(ID,_T, 'CanceledForSymbol'       ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val297(ID,_T, 'CanceledForSecurityType' ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val297(ID,_T, 'CanceledForUnderlying'   ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val297(ID,_T, 'CanceledAll'             ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val297(ID,_T, 'Rejected'                ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val297(ID,_T, 'RemovedFromMarket'       ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val297(ID,_T, 'Expired'                 ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val297(ID,_T, 'Query'                   ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val297(ID,_T, 'QuoteNotFound'           ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val297(ID,_T, 'Pending'                 ) -> encode_tagval(ID, <<"10">>);
encode_fld_val297(ID,_T, 'Pass'                    ) -> encode_tagval(ID, <<"11">>);
encode_fld_val297(ID,_T, 'LockedMarketWarning'     ) -> encode_tagval(ID, <<"12">>);
encode_fld_val297(ID,_T, 'CrossMarketWarning'      ) -> encode_tagval(ID, <<"13">>);
encode_fld_val297(ID,_T, 'CanceledDueToLockMarket' ) -> encode_tagval(ID, <<"14">>);
encode_fld_val297(ID,_T, 'CanceledDueToCrossMarket') -> encode_tagval(ID, <<"15">>);
encode_fld_val297(ID, T, V                         ) -> try_encode_val(ID, T, V).

decode_fld_val298(Val) ->
  case Val of
    <<"1">> -> 'CancelForSymbol'          ; %% 0
    <<"2">> -> 'CancelForSecurityType'    ; %% 1
    <<"3">> -> 'CancelForUnderlyingSymbol'; %% 2
    <<"4">> -> 'CancelAllQuotes'          ; %% 3
    _       -> Val
  end.

encode_fld_val298(ID,_T, 'CancelForSymbol'          ) -> encode_tagval(ID, <<"1">>);
encode_fld_val298(ID,_T, 'CancelForSecurityType'    ) -> encode_tagval(ID, <<"2">>);
encode_fld_val298(ID,_T, 'CancelForUnderlyingSymbol') -> encode_tagval(ID, <<"3">>);
encode_fld_val298(ID,_T, 'CancelAllQuotes'          ) -> encode_tagval(ID, <<"4">>);
encode_fld_val298(ID, T, V                          ) -> try_encode_val(ID, T, V).

decode_fld_val300(Val) ->
  case Val of
    <<"1" >> -> 'UnknownSymbol'               ; %% 0
    <<"2" >> -> 'Exchange'                    ; %% 1
    <<"3" >> -> 'QuoteRequestExceedsLimit'    ; %% 2
    <<"4" >> -> 'TooLateToEnter'              ; %% 3
    <<"5" >> -> 'UnknownQuote'                ; %% 4
    <<"6" >> -> 'DuplicateQuote'              ; %% 5
    <<"7" >> -> 'InvalidBidAskSpread'         ; %% 6
    <<"8" >> -> 'InvalidPrice'                ; %% 7
    <<"9" >> -> 'NotAuthorizedToQuoteSecurity'; %% 8
    <<"99">> -> 'Other'                       ; %% 9
    _        -> Val
  end.

encode_fld_val300(ID,_T, 'UnknownSymbol'               ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val300(ID,_T, 'Exchange'                    ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val300(ID,_T, 'QuoteRequestExceedsLimit'    ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val300(ID,_T, 'TooLateToEnter'              ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val300(ID,_T, 'UnknownQuote'                ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val300(ID,_T, 'DuplicateQuote'              ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val300(ID,_T, 'InvalidBidAskSpread'         ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val300(ID,_T, 'InvalidPrice'                ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val300(ID,_T, 'NotAuthorizedToQuoteSecurity') -> encode_tagval(ID, <<"9" >>);
encode_fld_val300(ID,_T, 'Other'                       ) -> encode_tagval(ID, <<"99">>);
encode_fld_val300(ID, T, V                             ) -> try_encode_val(ID, T, V).

decode_fld_val301(Val) ->
  case Val of
    <<"0">> -> 'NoAcknowledgement'                       ; %% 0
    <<"1">> -> 'AcknowledgeOnlyNegativeOrErroneousQuotes'; %% 1
    <<"2">> -> 'AcknowledgeEachQuoteMessages'            ; %% 2
    _       -> Val
  end.

encode_fld_val301(ID,_T, 'NoAcknowledgement'                       ) -> encode_tagval(ID, <<"0">>);
encode_fld_val301(ID,_T, 'AcknowledgeOnlyNegativeOrErroneousQuotes') -> encode_tagval(ID, <<"1">>);
encode_fld_val301(ID,_T, 'AcknowledgeEachQuoteMessages'            ) -> encode_tagval(ID, <<"2">>);
encode_fld_val301(ID, T, V                                         ) -> try_encode_val(ID, T, V).

decode_fld_val303(Val) ->
  case Val of
    <<"1">> -> 'Manual'   ; %% 0
    <<"2">> -> 'Automatic'; %% 1
    _       -> Val
  end.

encode_fld_val303(ID,_T, 'Manual'   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val303(ID,_T, 'Automatic') -> encode_tagval(ID, <<"2">>);
encode_fld_val303(ID, T, V          ) -> try_encode_val(ID, T, V).

decode_fld_val321(Val) ->
  case Val of
    <<"0">> -> 'RequestSecurityIdentityAndSpecifications'           ; %% 0
    <<"1">> -> 'RequestSecurityIdentityForTheSpecificationsProvided'; %% 1
    <<"2">> -> 'RequestListSecurityTypes'                           ; %% 2
    <<"3">> -> 'RequestListSecurities'                              ; %% 3
    _       -> Val
  end.

encode_fld_val321(ID,_T, 'RequestSecurityIdentityAndSpecifications'           ) -> encode_tagval(ID, <<"0">>);
encode_fld_val321(ID,_T, 'RequestSecurityIdentityForTheSpecificationsProvided') -> encode_tagval(ID, <<"1">>);
encode_fld_val321(ID,_T, 'RequestListSecurityTypes'                           ) -> encode_tagval(ID, <<"2">>);
encode_fld_val321(ID,_T, 'RequestListSecurities'                              ) -> encode_tagval(ID, <<"3">>);
encode_fld_val321(ID, T, V                                                    ) -> try_encode_val(ID, T, V).

decode_fld_val323(Val) ->
  case Val of
    <<"1">> -> 'AcceptSecurityProposalAsIs'                                ; %% 0
    <<"2">> -> 'AcceptSecurityProposalWithRevisionsAsIndicatedInTheMessage'; %% 1
    <<"5">> -> 'RejectSecurityProposal'                                    ; %% 2
    <<"6">> -> 'CanNotMatchSelectionCriteria'                              ; %% 3
    _       -> Val
  end.

encode_fld_val323(ID,_T, 'AcceptSecurityProposalAsIs'                                ) -> encode_tagval(ID, <<"1">>);
encode_fld_val323(ID,_T, 'AcceptSecurityProposalWithRevisionsAsIndicatedInTheMessage') -> encode_tagval(ID, <<"2">>);
encode_fld_val323(ID,_T, 'RejectSecurityProposal'                                    ) -> encode_tagval(ID, <<"5">>);
encode_fld_val323(ID,_T, 'CanNotMatchSelectionCriteria'                              ) -> encode_tagval(ID, <<"6">>);
encode_fld_val323(ID, T, V                                                           ) -> try_encode_val(ID, T, V).

decode_fld_val325(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val325(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val325(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val325(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val326(Val) ->
  case Val of
    <<"1" >> -> 'OpeningDelay'              ; %% 0
    <<"2" >> -> 'TradingHalt'               ; %% 1
    <<"3" >> -> 'Resume'                    ; %% 2
    <<"4" >> -> 'NoOpenNoResume'            ; %% 3
    <<"5" >> -> 'PriceIndication'           ; %% 4
    <<"6" >> -> 'TradingRangeIndication'    ; %% 5
    <<"7" >> -> 'MarketImbalanceBuy'        ; %% 6
    <<"8" >> -> 'MarketImbalanceSell'       ; %% 7
    <<"9" >> -> 'MarketOnCloseImbalanceBuy' ; %% 8
    <<"10">> -> 'MarketOnCloseImbalanceSell'; %% 9
    <<"12">> -> 'NoMarketImbalance'         ; %% 10
    <<"13">> -> 'NoMarketOnCloseImbalance'  ; %% 11
    <<"14">> -> 'ItsPreOpening'             ; %% 12
    <<"15">> -> 'NewPriceIndication'        ; %% 13
    <<"16">> -> 'TradeDisseminationTime'    ; %% 14
    <<"17">> -> 'ReadyToTrade'              ; %% 15
    <<"18">> -> 'NotAvailableForTrading'    ; %% 16
    <<"19">> -> 'NotTradedOnThisMarket'     ; %% 17
    <<"20">> -> 'UnknownOrInvalid'          ; %% 18
    <<"21">> -> 'PreOpen'                   ; %% 19
    <<"22">> -> 'OpeningRotation'           ; %% 20
    <<"23">> -> 'FastMarket'                ; %% 21
    _        -> Val
  end.

encode_fld_val326(ID,_T, 'OpeningDelay'              ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val326(ID,_T, 'TradingHalt'               ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val326(ID,_T, 'Resume'                    ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val326(ID,_T, 'NoOpenNoResume'            ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val326(ID,_T, 'PriceIndication'           ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val326(ID,_T, 'TradingRangeIndication'    ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val326(ID,_T, 'MarketImbalanceBuy'        ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val326(ID,_T, 'MarketImbalanceSell'       ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val326(ID,_T, 'MarketOnCloseImbalanceBuy' ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val326(ID,_T, 'MarketOnCloseImbalanceSell') -> encode_tagval(ID, <<"10">>);
encode_fld_val326(ID,_T, 'NoMarketImbalance'         ) -> encode_tagval(ID, <<"12">>);
encode_fld_val326(ID,_T, 'NoMarketOnCloseImbalance'  ) -> encode_tagval(ID, <<"13">>);
encode_fld_val326(ID,_T, 'ItsPreOpening'             ) -> encode_tagval(ID, <<"14">>);
encode_fld_val326(ID,_T, 'NewPriceIndication'        ) -> encode_tagval(ID, <<"15">>);
encode_fld_val326(ID,_T, 'TradeDisseminationTime'    ) -> encode_tagval(ID, <<"16">>);
encode_fld_val326(ID,_T, 'ReadyToTrade'              ) -> encode_tagval(ID, <<"17">>);
encode_fld_val326(ID,_T, 'NotAvailableForTrading'    ) -> encode_tagval(ID, <<"18">>);
encode_fld_val326(ID,_T, 'NotTradedOnThisMarket'     ) -> encode_tagval(ID, <<"19">>);
encode_fld_val326(ID,_T, 'UnknownOrInvalid'          ) -> encode_tagval(ID, <<"20">>);
encode_fld_val326(ID,_T, 'PreOpen'                   ) -> encode_tagval(ID, <<"21">>);
encode_fld_val326(ID,_T, 'OpeningRotation'           ) -> encode_tagval(ID, <<"22">>);
encode_fld_val326(ID,_T, 'FastMarket'                ) -> encode_tagval(ID, <<"23">>);
encode_fld_val326(ID, T, V                           ) -> try_encode_val(ID, T, V).

decode_fld_val327(Val) ->
  case Val of
    <<"I">> -> 'OrderImbalance'       ; %% 0
    <<"X">> -> 'EquipmentChangeover'  ; %% 1
    <<"P">> -> 'NewsPending'          ; %% 2
    <<"D">> -> 'NewsDissemination'    ; %% 3
    <<"E">> -> 'OrderInflux'          ; %% 4
    <<"M">> -> 'AdditionalInformation'; %% 5
    _       -> Val
  end.

encode_fld_val327(ID,_T, 'OrderImbalance'       ) -> encode_tagval(ID, <<"I">>);
encode_fld_val327(ID,_T, 'EquipmentChangeover'  ) -> encode_tagval(ID, <<"X">>);
encode_fld_val327(ID,_T, 'NewsPending'          ) -> encode_tagval(ID, <<"P">>);
encode_fld_val327(ID,_T, 'NewsDissemination'    ) -> encode_tagval(ID, <<"D">>);
encode_fld_val327(ID,_T, 'OrderInflux'          ) -> encode_tagval(ID, <<"E">>);
encode_fld_val327(ID,_T, 'AdditionalInformation') -> encode_tagval(ID, <<"M">>);
encode_fld_val327(ID, T, V                      ) -> try_encode_val(ID, T, V).

decode_fld_val328(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val328(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val328(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val328(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val329(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val329(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val329(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val329(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val334(Val) ->
  case Val of
    <<"1">> -> 'Cancel'    ; %% 0
    <<"2">> -> 'Error'     ; %% 1
    <<"3">> -> 'Correction'; %% 2
    _       -> Val
  end.

encode_fld_val334(ID,_T, 'Cancel'    ) -> encode_tagval(ID, <<"1">>);
encode_fld_val334(ID,_T, 'Error'     ) -> encode_tagval(ID, <<"2">>);
encode_fld_val334(ID,_T, 'Correction') -> encode_tagval(ID, <<"3">>);
encode_fld_val334(ID, T, V           ) -> try_encode_val(ID, T, V).

decode_fld_val338(Val) ->
  case Val of
    <<"1">> -> 'Electronic'; %% 0
    <<"2">> -> 'OpenOutcry'; %% 1
    <<"3">> -> 'TwoParty'  ; %% 2
    _       -> Val
  end.

encode_fld_val338(ID,_T, 'Electronic') -> encode_tagval(ID, <<"1">>);
encode_fld_val338(ID,_T, 'OpenOutcry') -> encode_tagval(ID, <<"2">>);
encode_fld_val338(ID,_T, 'TwoParty'  ) -> encode_tagval(ID, <<"3">>);
encode_fld_val338(ID, T, V           ) -> try_encode_val(ID, T, V).

decode_fld_val339(Val) ->
  case Val of
    <<"1">> -> 'Testing'   ; %% 0
    <<"2">> -> 'Simulated' ; %% 1
    <<"3">> -> 'Production'; %% 2
    _       -> Val
  end.

encode_fld_val339(ID,_T, 'Testing'   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val339(ID,_T, 'Simulated' ) -> encode_tagval(ID, <<"2">>);
encode_fld_val339(ID,_T, 'Production') -> encode_tagval(ID, <<"3">>);
encode_fld_val339(ID, T, V           ) -> try_encode_val(ID, T, V).

decode_fld_val340(Val) ->
  case Val of
    <<"0">> -> 'Unknown'        ; %% 0
    <<"1">> -> 'Halted'         ; %% 1
    <<"2">> -> 'Open'           ; %% 2
    <<"3">> -> 'Closed'         ; %% 3
    <<"4">> -> 'PreOpen'        ; %% 4
    <<"5">> -> 'PreClose'       ; %% 5
    <<"6">> -> 'RequestRejected'; %% 6
    _       -> Val
  end.

encode_fld_val340(ID,_T, 'Unknown'        ) -> encode_tagval(ID, <<"0">>);
encode_fld_val340(ID,_T, 'Halted'         ) -> encode_tagval(ID, <<"1">>);
encode_fld_val340(ID,_T, 'Open'           ) -> encode_tagval(ID, <<"2">>);
encode_fld_val340(ID,_T, 'Closed'         ) -> encode_tagval(ID, <<"3">>);
encode_fld_val340(ID,_T, 'PreOpen'        ) -> encode_tagval(ID, <<"4">>);
encode_fld_val340(ID,_T, 'PreClose'       ) -> encode_tagval(ID, <<"5">>);
encode_fld_val340(ID,_T, 'RequestRejected') -> encode_tagval(ID, <<"6">>);
encode_fld_val340(ID, T, V                ) -> try_encode_val(ID, T, V).

decode_fld_val347(Val) ->
  case Val of
    <<"ISO-2022-JP">> -> 'Jis'         ; %% 0
    <<"EUC-JP"     >> -> 'Euc'         ; %% 1
    <<"Shift_JIS"  >> -> 'ForUsingSjis'; %% 2
    <<"UTF-8"      >> -> 'Unicode'     ; %% 3
    _                 -> Val
  end.

encode_fld_val347(ID,_T, 'Jis'         ) -> encode_tagval(ID, <<"ISO-2022-JP">>);
encode_fld_val347(ID,_T, 'Euc'         ) -> encode_tagval(ID, <<"EUC-JP"     >>);
encode_fld_val347(ID,_T, 'ForUsingSjis') -> encode_tagval(ID, <<"Shift_JIS"  >>);
encode_fld_val347(ID,_T, 'Unicode'     ) -> encode_tagval(ID, <<"UTF-8"      >>);
encode_fld_val347(ID, T, V             ) -> try_encode_val(ID, T, V).

decode_fld_val373(Val) ->
  case Val of
    <<"0" >> -> 'InvalidTagNumber'                         ; %% 0
    <<"1" >> -> 'RequiredTagMissing'                       ; %% 1
    <<"2" >> -> 'TagNotDefinedForThisMessageType'          ; %% 2
    <<"3" >> -> 'UndefinedTag'                             ; %% 3
    <<"4" >> -> 'TagSpecifiedWithoutAValue'                ; %% 4
    <<"5" >> -> 'ValueIsIncorrect'                         ; %% 5
    <<"6" >> -> 'IncorrectDataFormatForValue'              ; %% 6
    <<"7" >> -> 'DecryptionProblem'                        ; %% 7
    <<"8" >> -> 'SignatureProblem'                         ; %% 8
    <<"9" >> -> 'CompidProblem'                            ; %% 9
    <<"10">> -> 'SendingtimeAccuracyProblem'               ; %% 10
    <<"11">> -> 'InvalidMsgtype'                           ; %% 11
    <<"12">> -> 'XmlValidationError'                       ; %% 12
    <<"13">> -> 'TagAppearsMoreThanOnce'                   ; %% 13
    <<"14">> -> 'TagSpecifiedOutOfRequiredOrder'           ; %% 14
    <<"15">> -> 'RepeatingGroupFieldsOutOfOrder'           ; %% 15
    <<"16">> -> 'IncorrectNumingroupCountForRepeatingGroup'; %% 16
    <<"17">> -> 'NonDataValueIncludesFieldDelimiter'       ; %% 17
    <<"99">> -> 'Other'                                    ; %% 18
    _        -> Val
  end.

encode_fld_val373(ID,_T, 'InvalidTagNumber'                         ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val373(ID,_T, 'RequiredTagMissing'                       ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val373(ID,_T, 'TagNotDefinedForThisMessageType'          ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val373(ID,_T, 'UndefinedTag'                             ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val373(ID,_T, 'TagSpecifiedWithoutAValue'                ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val373(ID,_T, 'ValueIsIncorrect'                         ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val373(ID,_T, 'IncorrectDataFormatForValue'              ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val373(ID,_T, 'DecryptionProblem'                        ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val373(ID,_T, 'SignatureProblem'                         ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val373(ID,_T, 'CompidProblem'                            ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val373(ID,_T, 'SendingtimeAccuracyProblem'               ) -> encode_tagval(ID, <<"10">>);
encode_fld_val373(ID,_T, 'InvalidMsgtype'                           ) -> encode_tagval(ID, <<"11">>);
encode_fld_val373(ID,_T, 'XmlValidationError'                       ) -> encode_tagval(ID, <<"12">>);
encode_fld_val373(ID,_T, 'TagAppearsMoreThanOnce'                   ) -> encode_tagval(ID, <<"13">>);
encode_fld_val373(ID,_T, 'TagSpecifiedOutOfRequiredOrder'           ) -> encode_tagval(ID, <<"14">>);
encode_fld_val373(ID,_T, 'RepeatingGroupFieldsOutOfOrder'           ) -> encode_tagval(ID, <<"15">>);
encode_fld_val373(ID,_T, 'IncorrectNumingroupCountForRepeatingGroup') -> encode_tagval(ID, <<"16">>);
encode_fld_val373(ID,_T, 'NonDataValueIncludesFieldDelimiter'       ) -> encode_tagval(ID, <<"17">>);
encode_fld_val373(ID,_T, 'Other'                                    ) -> encode_tagval(ID, <<"99">>);
encode_fld_val373(ID, T, V                                          ) -> try_encode_val(ID, T, V).

decode_fld_val374(Val) ->
  case Val of
    <<"N">> -> 'New'   ; %% 0
    <<"C">> -> 'Cancel'; %% 1
    _       -> Val
  end.

encode_fld_val374(ID,_T, 'New'   ) -> encode_tagval(ID, <<"N">>);
encode_fld_val374(ID,_T, 'Cancel') -> encode_tagval(ID, <<"C">>);
encode_fld_val374(ID, T, V       ) -> try_encode_val(ID, T, V).

decode_fld_val377(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val377(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val377(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val377(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val378(Val) ->
  case Val of
    <<"0" >> -> 'GtCorporateAction'       ; %% 0
    <<"1" >> -> 'GtRenewal'               ; %% 1
    <<"2" >> -> 'VerbalChange'            ; %% 2
    <<"3" >> -> 'RepricingOfOrder'        ; %% 3
    <<"4" >> -> 'BrokerOption'            ; %% 4
    <<"5" >> -> 'PartialDeclineOfOrderqty'; %% 5
    <<"6" >> -> 'CancelOnTradingHalt'     ; %% 6
    <<"7" >> -> 'CancelOnSystemFailure'   ; %% 7
    <<"8" >> -> 'Market'                  ; %% 8
    <<"9" >> -> 'CanceledNotBest'         ; %% 9
    <<"10">> -> 'WarehouseRecap'          ; %% 10
    <<"99">> -> 'Other'                   ; %% 11
    _        -> Val
  end.

encode_fld_val378(ID,_T, 'GtCorporateAction'       ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val378(ID,_T, 'GtRenewal'               ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val378(ID,_T, 'VerbalChange'            ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val378(ID,_T, 'RepricingOfOrder'        ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val378(ID,_T, 'BrokerOption'            ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val378(ID,_T, 'PartialDeclineOfOrderqty') -> encode_tagval(ID, <<"5" >>);
encode_fld_val378(ID,_T, 'CancelOnTradingHalt'     ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val378(ID,_T, 'CancelOnSystemFailure'   ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val378(ID,_T, 'Market'                  ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val378(ID,_T, 'CanceledNotBest'         ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val378(ID,_T, 'WarehouseRecap'          ) -> encode_tagval(ID, <<"10">>);
encode_fld_val378(ID,_T, 'Other'                   ) -> encode_tagval(ID, <<"99">>);
encode_fld_val378(ID, T, V                         ) -> try_encode_val(ID, T, V).

decode_fld_val380(Val) ->
  case Val of
    <<"0">> -> 'Other'                              ; %% 0
    <<"1">> -> 'UnkownId'                           ; %% 1
    <<"2">> -> 'UnknownSecurity'                    ; %% 2
    <<"3">> -> 'UnsupportedMessageType'             ; %% 3
    <<"4">> -> 'ApplicationNotAvailable'            ; %% 4
    <<"5">> -> 'ConditionallyRequiredFieldMissing'  ; %% 5
    <<"6">> -> 'NotAuthorized'                      ; %% 6
    <<"7">> -> 'DelivertoFirmNotAvailableAtThisTime'; %% 7
    _       -> Val
  end.

encode_fld_val380(ID,_T, 'Other'                              ) -> encode_tagval(ID, <<"0">>);
encode_fld_val380(ID,_T, 'UnkownId'                           ) -> encode_tagval(ID, <<"1">>);
encode_fld_val380(ID,_T, 'UnknownSecurity'                    ) -> encode_tagval(ID, <<"2">>);
encode_fld_val380(ID,_T, 'UnsupportedMessageType'             ) -> encode_tagval(ID, <<"3">>);
encode_fld_val380(ID,_T, 'ApplicationNotAvailable'            ) -> encode_tagval(ID, <<"4">>);
encode_fld_val380(ID,_T, 'ConditionallyRequiredFieldMissing'  ) -> encode_tagval(ID, <<"5">>);
encode_fld_val380(ID,_T, 'NotAuthorized'                      ) -> encode_tagval(ID, <<"6">>);
encode_fld_val380(ID,_T, 'DelivertoFirmNotAvailableAtThisTime') -> encode_tagval(ID, <<"7">>);
encode_fld_val380(ID, T, V                                    ) -> try_encode_val(ID, T, V).

decode_fld_val385(Val) ->
  case Val of
    <<"S">> -> 'Send'   ; %% 0
    <<"R">> -> 'Receive'; %% 1
    _       -> Val
  end.

encode_fld_val385(ID,_T, 'Send'   ) -> encode_tagval(ID, <<"S">>);
encode_fld_val385(ID,_T, 'Receive') -> encode_tagval(ID, <<"R">>);
encode_fld_val385(ID, T, V        ) -> try_encode_val(ID, T, V).

decode_fld_val388(Val) ->
  case Val of
    <<"0">> -> 'RelatedToDisplayedPrice'   ; %% 0
    <<"1">> -> 'RelatedToMarketPrice'      ; %% 1
    <<"2">> -> 'RelatedToPrimaryPrice'     ; %% 2
    <<"3">> -> 'RelatedToLocalPrimaryPrice'; %% 3
    <<"4">> -> 'RelatedToMidpointPrice'    ; %% 4
    <<"5">> -> 'RelatedToLastTradePrice'   ; %% 5
    <<"6">> -> 'RelatedToVwap'             ; %% 6
    _       -> Val
  end.

encode_fld_val388(ID,_T, 'RelatedToDisplayedPrice'   ) -> encode_tagval(ID, <<"0">>);
encode_fld_val388(ID,_T, 'RelatedToMarketPrice'      ) -> encode_tagval(ID, <<"1">>);
encode_fld_val388(ID,_T, 'RelatedToPrimaryPrice'     ) -> encode_tagval(ID, <<"2">>);
encode_fld_val388(ID,_T, 'RelatedToLocalPrimaryPrice') -> encode_tagval(ID, <<"3">>);
encode_fld_val388(ID,_T, 'RelatedToMidpointPrice'    ) -> encode_tagval(ID, <<"4">>);
encode_fld_val388(ID,_T, 'RelatedToLastTradePrice'   ) -> encode_tagval(ID, <<"5">>);
encode_fld_val388(ID,_T, 'RelatedToVwap'             ) -> encode_tagval(ID, <<"6">>);
encode_fld_val388(ID, T, V                           ) -> try_encode_val(ID, T, V).

decode_fld_val394(Val) ->
  case Val of
    <<"1">> -> 'NonDisclosedStyle'; %% 0
    <<"2">> -> 'DisclosedStyle'   ; %% 1
    <<"3">> -> 'NoBiddingProcess' ; %% 2
    _       -> Val
  end.

encode_fld_val394(ID,_T, 'NonDisclosedStyle') -> encode_tagval(ID, <<"1">>);
encode_fld_val394(ID,_T, 'DisclosedStyle'   ) -> encode_tagval(ID, <<"2">>);
encode_fld_val394(ID,_T, 'NoBiddingProcess' ) -> encode_tagval(ID, <<"3">>);
encode_fld_val394(ID, T, V                  ) -> try_encode_val(ID, T, V).

decode_fld_val399(Val) ->
  case Val of
    <<"1">> -> 'Sector' ; %% 0
    <<"2">> -> 'Country'; %% 1
    <<"3">> -> 'Index'  ; %% 2
    _       -> Val
  end.

encode_fld_val399(ID,_T, 'Sector' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val399(ID,_T, 'Country') -> encode_tagval(ID, <<"2">>);
encode_fld_val399(ID,_T, 'Index'  ) -> encode_tagval(ID, <<"3">>);
encode_fld_val399(ID, T, V        ) -> try_encode_val(ID, T, V).

decode_fld_val401(Val) ->
  case Val of
    <<"1">> -> 'Sidevalue1'; %% 0
    <<"2">> -> 'Sidevalue2'; %% 1
    _       -> Val
  end.

encode_fld_val401(ID,_T, 'Sidevalue1') -> encode_tagval(ID, <<"1">>);
encode_fld_val401(ID,_T, 'Sidevalue2') -> encode_tagval(ID, <<"2">>);
encode_fld_val401(ID, T, V           ) -> try_encode_val(ID, T, V).

decode_fld_val409(Val) ->
  case Val of
    <<"1">> -> '5dayMovingAverage' ; %% 0
    <<"2">> -> '20DayMovingAverage'; %% 1
    <<"3">> -> 'NormalMarketSize'  ; %% 2
    <<"4">> -> 'Other'             ; %% 3
    _       -> Val
  end.

encode_fld_val409(ID,_T, '5dayMovingAverage' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val409(ID,_T, '20DayMovingAverage') -> encode_tagval(ID, <<"2">>);
encode_fld_val409(ID,_T, 'NormalMarketSize'  ) -> encode_tagval(ID, <<"3">>);
encode_fld_val409(ID,_T, 'Other'             ) -> encode_tagval(ID, <<"4">>);
encode_fld_val409(ID, T, V                   ) -> try_encode_val(ID, T, V).

decode_fld_val411(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val411(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val411(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val411(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val414(Val) ->
  case Val of
    <<"1">> -> 'BuysideExplicitlyRequestsStatusUsingStatusrequest'                                      ; %% 0
    <<"2">> -> 'SellsidePeriodicallySendsStatusUsingListstatusPeriodOptionallySpecifiedInProgressperiod'; %% 1
    <<"3">> -> 'RealTimeExecutionReports'                                                               ; %% 2
    _       -> Val
  end.

encode_fld_val414(ID,_T, 'BuysideExplicitlyRequestsStatusUsingStatusrequest'                                      ) -> encode_tagval(ID, <<"1">>);
encode_fld_val414(ID,_T, 'SellsidePeriodicallySendsStatusUsingListstatusPeriodOptionallySpecifiedInProgressperiod') -> encode_tagval(ID, <<"2">>);
encode_fld_val414(ID,_T, 'RealTimeExecutionReports'                                                               ) -> encode_tagval(ID, <<"3">>);
encode_fld_val414(ID, T, V                                                                                        ) -> try_encode_val(ID, T, V).

decode_fld_val416(Val) ->
  case Val of
    <<"1">> -> 'Net'  ; %% 0
    <<"2">> -> 'Gross'; %% 1
    _       -> Val
  end.

encode_fld_val416(ID,_T, 'Net'  ) -> encode_tagval(ID, <<"1">>);
encode_fld_val416(ID,_T, 'Gross') -> encode_tagval(ID, <<"2">>);
encode_fld_val416(ID, T, V      ) -> try_encode_val(ID, T, V).

decode_fld_val418(Val) ->
  case Val of
    <<"R">> -> 'RiskTrade'      ; %% 0
    <<"G">> -> 'VwapGuarantee'  ; %% 1
    <<"A">> -> 'Agency'         ; %% 2
    <<"J">> -> 'GuaranteedClose'; %% 3
    _       -> Val
  end.

encode_fld_val418(ID,_T, 'RiskTrade'      ) -> encode_tagval(ID, <<"R">>);
encode_fld_val418(ID,_T, 'VwapGuarantee'  ) -> encode_tagval(ID, <<"G">>);
encode_fld_val418(ID,_T, 'Agency'         ) -> encode_tagval(ID, <<"A">>);
encode_fld_val418(ID,_T, 'GuaranteedClose') -> encode_tagval(ID, <<"J">>);
encode_fld_val418(ID, T, V                ) -> try_encode_val(ID, T, V).

decode_fld_val419(Val) ->
  case Val of
    <<"2">> -> 'ClosingPriceAtMorningSession'           ; %% 0
    <<"3">> -> 'ClosingPrice'                           ; %% 1
    <<"4">> -> 'CurrentPrice'                           ; %% 2
    <<"5">> -> 'Sq'                                     ; %% 3
    <<"6">> -> 'VwapThroughADay'                        ; %% 4
    <<"7">> -> 'VwapThroughAMorningSession'             ; %% 5
    <<"8">> -> 'VwapThroughAnAfternoonSession'          ; %% 6
    <<"9">> -> 'VwapThroughADayExceptYori'              ; %% 7
    <<"A">> -> 'VwapThroughAMorningSessionExceptYori'   ; %% 8
    <<"B">> -> 'VwapThroughAnAfternoonSessionExceptYori'; %% 9
    <<"C">> -> 'Strike'                                 ; %% 10
    <<"D">> -> 'Open'                                   ; %% 11
    <<"Z">> -> 'Others'                                 ; %% 12
    _       -> Val
  end.

encode_fld_val419(ID,_T, 'ClosingPriceAtMorningSession'           ) -> encode_tagval(ID, <<"2">>);
encode_fld_val419(ID,_T, 'ClosingPrice'                           ) -> encode_tagval(ID, <<"3">>);
encode_fld_val419(ID,_T, 'CurrentPrice'                           ) -> encode_tagval(ID, <<"4">>);
encode_fld_val419(ID,_T, 'Sq'                                     ) -> encode_tagval(ID, <<"5">>);
encode_fld_val419(ID,_T, 'VwapThroughADay'                        ) -> encode_tagval(ID, <<"6">>);
encode_fld_val419(ID,_T, 'VwapThroughAMorningSession'             ) -> encode_tagval(ID, <<"7">>);
encode_fld_val419(ID,_T, 'VwapThroughAnAfternoonSession'          ) -> encode_tagval(ID, <<"8">>);
encode_fld_val419(ID,_T, 'VwapThroughADayExceptYori'              ) -> encode_tagval(ID, <<"9">>);
encode_fld_val419(ID,_T, 'VwapThroughAMorningSessionExceptYori'   ) -> encode_tagval(ID, <<"A">>);
encode_fld_val419(ID,_T, 'VwapThroughAnAfternoonSessionExceptYori') -> encode_tagval(ID, <<"B">>);
encode_fld_val419(ID,_T, 'Strike'                                 ) -> encode_tagval(ID, <<"C">>);
encode_fld_val419(ID,_T, 'Open'                                   ) -> encode_tagval(ID, <<"D">>);
encode_fld_val419(ID,_T, 'Others'                                 ) -> encode_tagval(ID, <<"Z">>);
encode_fld_val419(ID, T, V                                        ) -> try_encode_val(ID, T, V).

decode_fld_val423(Val) ->
  case Val of
    <<"1" >> -> 'Percentage'                      ; %% 0
    <<"2" >> -> 'PerUnit'                         ; %% 1
    <<"3" >> -> 'FixedAmount'                     ; %% 2
    <<"4" >> -> 'DiscountPercentagePointsBelowPar'; %% 3
    <<"5" >> -> 'PremiumPercentagePointsOverPar'  ; %% 4
    <<"6" >> -> 'Spread'                          ; %% 5
    <<"7" >> -> 'TedPrice'                        ; %% 6
    <<"8" >> -> 'TedYield'                        ; %% 7
    <<"9" >> -> 'Yield'                           ; %% 8
    <<"10">> -> 'FixedCabinetTradePrice'          ; %% 9
    <<"11">> -> 'VariableCabinetTradePrice'       ; %% 10
    _        -> Val
  end.

encode_fld_val423(ID,_T, 'Percentage'                      ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val423(ID,_T, 'PerUnit'                         ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val423(ID,_T, 'FixedAmount'                     ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val423(ID,_T, 'DiscountPercentagePointsBelowPar') -> encode_tagval(ID, <<"4" >>);
encode_fld_val423(ID,_T, 'PremiumPercentagePointsOverPar'  ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val423(ID,_T, 'Spread'                          ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val423(ID,_T, 'TedPrice'                        ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val423(ID,_T, 'TedYield'                        ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val423(ID,_T, 'Yield'                           ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val423(ID,_T, 'FixedCabinetTradePrice'          ) -> encode_tagval(ID, <<"10">>);
encode_fld_val423(ID,_T, 'VariableCabinetTradePrice'       ) -> encode_tagval(ID, <<"11">>);
encode_fld_val423(ID, T, V                                 ) -> try_encode_val(ID, T, V).

decode_fld_val427(Val) ->
  case Val of
    <<"0">> -> 'BookOutAllTradesOnDayOfExecution'               ; %% 0
    <<"1">> -> 'AccumulateExecutionsUntilOrderIsFilledOrExpires'; %% 1
    <<"2">> -> 'AccumulateUntilVerballyNotifiedOtherwise'       ; %% 2
    _       -> Val
  end.

encode_fld_val427(ID,_T, 'BookOutAllTradesOnDayOfExecution'               ) -> encode_tagval(ID, <<"0">>);
encode_fld_val427(ID,_T, 'AccumulateExecutionsUntilOrderIsFilledOrExpires') -> encode_tagval(ID, <<"1">>);
encode_fld_val427(ID,_T, 'AccumulateUntilVerballyNotifiedOtherwise'       ) -> encode_tagval(ID, <<"2">>);
encode_fld_val427(ID, T, V                                                ) -> try_encode_val(ID, T, V).

decode_fld_val429(Val) ->
  case Val of
    <<"1">> -> 'Ack'        ; %% 0
    <<"2">> -> 'Response'   ; %% 1
    <<"3">> -> 'Timed'      ; %% 2
    <<"4">> -> 'Execstarted'; %% 3
    <<"5">> -> 'Alldone'    ; %% 4
    <<"6">> -> 'Alert'      ; %% 5
    _       -> Val
  end.

encode_fld_val429(ID,_T, 'Ack'        ) -> encode_tagval(ID, <<"1">>);
encode_fld_val429(ID,_T, 'Response'   ) -> encode_tagval(ID, <<"2">>);
encode_fld_val429(ID,_T, 'Timed'      ) -> encode_tagval(ID, <<"3">>);
encode_fld_val429(ID,_T, 'Execstarted') -> encode_tagval(ID, <<"4">>);
encode_fld_val429(ID,_T, 'Alldone'    ) -> encode_tagval(ID, <<"5">>);
encode_fld_val429(ID,_T, 'Alert'      ) -> encode_tagval(ID, <<"6">>);
encode_fld_val429(ID, T, V            ) -> try_encode_val(ID, T, V).

decode_fld_val430(Val) ->
  case Val of
    <<"1">> -> 'Net'  ; %% 0
    <<"2">> -> 'Gross'; %% 1
    _       -> Val
  end.

encode_fld_val430(ID,_T, 'Net'  ) -> encode_tagval(ID, <<"1">>);
encode_fld_val430(ID,_T, 'Gross') -> encode_tagval(ID, <<"2">>);
encode_fld_val430(ID, T, V      ) -> try_encode_val(ID, T, V).

decode_fld_val431(Val) ->
  case Val of
    <<"1">> -> 'Inbiddingprocess'    ; %% 0
    <<"2">> -> 'Receivedforexecution'; %% 1
    <<"3">> -> 'Executing'           ; %% 2
    <<"4">> -> 'Canceling'           ; %% 3
    <<"5">> -> 'Alert'               ; %% 4
    <<"6">> -> 'AllDone'             ; %% 5
    <<"7">> -> 'Reject'              ; %% 6
    _       -> Val
  end.

encode_fld_val431(ID,_T, 'Inbiddingprocess'    ) -> encode_tagval(ID, <<"1">>);
encode_fld_val431(ID,_T, 'Receivedforexecution') -> encode_tagval(ID, <<"2">>);
encode_fld_val431(ID,_T, 'Executing'           ) -> encode_tagval(ID, <<"3">>);
encode_fld_val431(ID,_T, 'Canceling'           ) -> encode_tagval(ID, <<"4">>);
encode_fld_val431(ID,_T, 'Alert'               ) -> encode_tagval(ID, <<"5">>);
encode_fld_val431(ID,_T, 'AllDone'             ) -> encode_tagval(ID, <<"6">>);
encode_fld_val431(ID,_T, 'Reject'              ) -> encode_tagval(ID, <<"7">>);
encode_fld_val431(ID, T, V                     ) -> try_encode_val(ID, T, V).

decode_fld_val433(Val) ->
  case Val of
    <<"1">> -> 'Immediate'                                  ; %% 0
    <<"2">> -> 'WaitForExecuteInstruction'                  ; %% 1
    <<"3">> -> 'ExchangeSwitchCivOrderSellDriven'           ; %% 2
    <<"4">> -> 'ExchangeSwitchCivOrderBuyDrivenCashTopUp'   ; %% 3
    <<"5">> -> 'ExchangeSwitchCivOrderBuyDrivenCashWithdraw'; %% 4
    _       -> Val
  end.

encode_fld_val433(ID,_T, 'Immediate'                                  ) -> encode_tagval(ID, <<"1">>);
encode_fld_val433(ID,_T, 'WaitForExecuteInstruction'                  ) -> encode_tagval(ID, <<"2">>);
encode_fld_val433(ID,_T, 'ExchangeSwitchCivOrderSellDriven'           ) -> encode_tagval(ID, <<"3">>);
encode_fld_val433(ID,_T, 'ExchangeSwitchCivOrderBuyDrivenCashTopUp'   ) -> encode_tagval(ID, <<"4">>);
encode_fld_val433(ID,_T, 'ExchangeSwitchCivOrderBuyDrivenCashWithdraw') -> encode_tagval(ID, <<"5">>);
encode_fld_val433(ID, T, V                                            ) -> try_encode_val(ID, T, V).

decode_fld_val434(Val) ->
  case Val of
    <<"1">> -> 'OrderCancelRequest'       ; %% 0
    <<"2">> -> 'OrderCancelReplaceRequest'; %% 1
    _       -> Val
  end.

encode_fld_val434(ID,_T, 'OrderCancelRequest'       ) -> encode_tagval(ID, <<"1">>);
encode_fld_val434(ID,_T, 'OrderCancelReplaceRequest') -> encode_tagval(ID, <<"2">>);
encode_fld_val434(ID, T, V                          ) -> try_encode_val(ID, T, V).

decode_fld_val442(Val) ->
  case Val of
    <<"1">> -> 'SingleSecurity'                  ; %% 0
    <<"2">> -> 'IndividualLegOfAMultiLegSecurity'; %% 1
    <<"3">> -> 'MultiLegSecurity'                ; %% 2
    _       -> Val
  end.

encode_fld_val442(ID,_T, 'SingleSecurity'                  ) -> encode_tagval(ID, <<"1">>);
encode_fld_val442(ID,_T, 'IndividualLegOfAMultiLegSecurity') -> encode_tagval(ID, <<"2">>);
encode_fld_val442(ID,_T, 'MultiLegSecurity'                ) -> encode_tagval(ID, <<"3">>);
encode_fld_val442(ID, T, V                                 ) -> try_encode_val(ID, T, V).

decode_fld_val447(Val) ->
  case Val of
    <<"B">> -> 'Bic'                                                                                 ; %% 0
    <<"C">> -> 'GenerallyAcceptedMarketParticipantIdentifier'                                        ; %% 1
    <<"D">> -> 'ProprietaryCustomCode'                                                               ; %% 2
    <<"E">> -> 'IsoCountryCode'                                                                      ; %% 3
    <<"F">> -> 'SettlementEntityLocation'                                                            ; %% 4
    <<"G">> -> 'Mic'                                                                                 ; %% 5
    <<"H">> -> 'CsdParticipantMemberCode'                                                            ; %% 6
    <<"1">> -> 'KoreanInvestorId'                                                                    ; %% 7
    <<"2">> -> 'TaiwaneseQualifiedForeignInvestorIdQfii'                                             ; %% 8
    <<"3">> -> 'TaiwaneseTradingAccount'                                                             ; %% 9
    <<"4">> -> 'MalaysianCentralDepository'                                                          ; %% 10
    <<"5">> -> 'ChineseBShare'                                                                       ; %% 11
    <<"6">> -> 'UkNationalInsuranceOrPensionNumber'                                                  ; %% 12
    <<"7">> -> 'UsSocialSecurityNumber'                                                              ; %% 13
    <<"8">> -> 'UsEmployerIdentificationNumber'                                                      ; %% 14
    <<"9">> -> 'AustralianBusinessNumber'                                                            ; %% 15
    <<"A">> -> 'AustralianTaxFileNumber'                                                             ; %% 16
    <<"I">> -> 'DirectedBrokerThreeCharacterAcronymAsDefinedInIsitcEtcBestPracticeGuidelinesDocument'; %% 17
    _       -> Val
  end.

encode_fld_val447(ID,_T, 'Bic'                                                                                 ) -> encode_tagval(ID, <<"B">>);
encode_fld_val447(ID,_T, 'GenerallyAcceptedMarketParticipantIdentifier'                                        ) -> encode_tagval(ID, <<"C">>);
encode_fld_val447(ID,_T, 'ProprietaryCustomCode'                                                               ) -> encode_tagval(ID, <<"D">>);
encode_fld_val447(ID,_T, 'IsoCountryCode'                                                                      ) -> encode_tagval(ID, <<"E">>);
encode_fld_val447(ID,_T, 'SettlementEntityLocation'                                                            ) -> encode_tagval(ID, <<"F">>);
encode_fld_val447(ID,_T, 'Mic'                                                                                 ) -> encode_tagval(ID, <<"G">>);
encode_fld_val447(ID,_T, 'CsdParticipantMemberCode'                                                            ) -> encode_tagval(ID, <<"H">>);
encode_fld_val447(ID,_T, 'KoreanInvestorId'                                                                    ) -> encode_tagval(ID, <<"1">>);
encode_fld_val447(ID,_T, 'TaiwaneseQualifiedForeignInvestorIdQfii'                                             ) -> encode_tagval(ID, <<"2">>);
encode_fld_val447(ID,_T, 'TaiwaneseTradingAccount'                                                             ) -> encode_tagval(ID, <<"3">>);
encode_fld_val447(ID,_T, 'MalaysianCentralDepository'                                                          ) -> encode_tagval(ID, <<"4">>);
encode_fld_val447(ID,_T, 'ChineseBShare'                                                                       ) -> encode_tagval(ID, <<"5">>);
encode_fld_val447(ID,_T, 'UkNationalInsuranceOrPensionNumber'                                                  ) -> encode_tagval(ID, <<"6">>);
encode_fld_val447(ID,_T, 'UsSocialSecurityNumber'                                                              ) -> encode_tagval(ID, <<"7">>);
encode_fld_val447(ID,_T, 'UsEmployerIdentificationNumber'                                                      ) -> encode_tagval(ID, <<"8">>);
encode_fld_val447(ID,_T, 'AustralianBusinessNumber'                                                            ) -> encode_tagval(ID, <<"9">>);
encode_fld_val447(ID,_T, 'AustralianTaxFileNumber'                                                             ) -> encode_tagval(ID, <<"A">>);
encode_fld_val447(ID,_T, 'DirectedBrokerThreeCharacterAcronymAsDefinedInIsitcEtcBestPracticeGuidelinesDocument') -> encode_tagval(ID, <<"I">>);
encode_fld_val447(ID, T, V                                                                                     ) -> try_encode_val(ID, T, V).

decode_fld_val452(Val) ->
  case Val of
    <<"1" >> -> 'ExecutingFirm'                    ; %% 0
    <<"2" >> -> 'BrokerOfCredit'                   ; %% 1
    <<"3" >> -> 'ClientId'                         ; %% 2
    <<"4" >> -> 'ClearingFirm'                     ; %% 3
    <<"5" >> -> 'InvestorId'                       ; %% 4
    <<"6" >> -> 'IntroducingFirm'                  ; %% 5
    <<"7" >> -> 'EnteringFirm'                     ; %% 6
    <<"8" >> -> 'LocateLendingFirm'                ; %% 7
    <<"9" >> -> 'FundManagerClientId'              ; %% 8
    <<"10">> -> 'SettlementLocation'               ; %% 9
    <<"11">> -> 'OrderOriginationTrader'           ; %% 10
    <<"12">> -> 'ExecutingTrader'                  ; %% 11
    <<"13">> -> 'OrderOriginationFirm'             ; %% 12
    <<"14">> -> 'GiveupClearingFirm'               ; %% 13
    <<"15">> -> 'CorrespondantClearingFirm'        ; %% 14
    <<"16">> -> 'ExecutingSystem'                  ; %% 15
    <<"17">> -> 'ContraFirm'                       ; %% 16
    <<"18">> -> 'ContraClearingFirm'               ; %% 17
    <<"19">> -> 'SponsoringFirm'                   ; %% 18
    <<"20">> -> 'UnderlyingContraFirm'             ; %% 19
    <<"21">> -> 'ClearingOrganization'             ; %% 20
    <<"22">> -> 'Exchange'                         ; %% 21
    <<"24">> -> 'CustomerAccount'                  ; %% 22
    <<"25">> -> 'CorrespondentClearingOrganization'; %% 23
    <<"26">> -> 'CorrespondentBroker'              ; %% 24
    <<"27">> -> 'BuyerSeller'                      ; %% 25
    <<"28">> -> 'Custodian'                        ; %% 26
    <<"29">> -> 'Intermediary'                     ; %% 27
    <<"30">> -> 'Agent'                            ; %% 28
    <<"31">> -> 'SubCustodian'                     ; %% 29
    <<"32">> -> 'Beneficiary'                      ; %% 30
    <<"33">> -> 'InterestedParty'                  ; %% 31
    <<"34">> -> 'RegulatoryBody'                   ; %% 32
    <<"35">> -> 'LiquidityProvider'                ; %% 33
    <<"36">> -> 'EnteringTrader'                   ; %% 34
    <<"37">> -> 'ContraTrader'                     ; %% 35
    <<"38">> -> 'PositionAccount'                  ; %% 36
    _        -> Val
  end.

encode_fld_val452(ID,_T, 'ExecutingFirm'                    ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val452(ID,_T, 'BrokerOfCredit'                   ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val452(ID,_T, 'ClientId'                         ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val452(ID,_T, 'ClearingFirm'                     ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val452(ID,_T, 'InvestorId'                       ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val452(ID,_T, 'IntroducingFirm'                  ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val452(ID,_T, 'EnteringFirm'                     ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val452(ID,_T, 'LocateLendingFirm'                ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val452(ID,_T, 'FundManagerClientId'              ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val452(ID,_T, 'SettlementLocation'               ) -> encode_tagval(ID, <<"10">>);
encode_fld_val452(ID,_T, 'OrderOriginationTrader'           ) -> encode_tagval(ID, <<"11">>);
encode_fld_val452(ID,_T, 'ExecutingTrader'                  ) -> encode_tagval(ID, <<"12">>);
encode_fld_val452(ID,_T, 'OrderOriginationFirm'             ) -> encode_tagval(ID, <<"13">>);
encode_fld_val452(ID,_T, 'GiveupClearingFirm'               ) -> encode_tagval(ID, <<"14">>);
encode_fld_val452(ID,_T, 'CorrespondantClearingFirm'        ) -> encode_tagval(ID, <<"15">>);
encode_fld_val452(ID,_T, 'ExecutingSystem'                  ) -> encode_tagval(ID, <<"16">>);
encode_fld_val452(ID,_T, 'ContraFirm'                       ) -> encode_tagval(ID, <<"17">>);
encode_fld_val452(ID,_T, 'ContraClearingFirm'               ) -> encode_tagval(ID, <<"18">>);
encode_fld_val452(ID,_T, 'SponsoringFirm'                   ) -> encode_tagval(ID, <<"19">>);
encode_fld_val452(ID,_T, 'UnderlyingContraFirm'             ) -> encode_tagval(ID, <<"20">>);
encode_fld_val452(ID,_T, 'ClearingOrganization'             ) -> encode_tagval(ID, <<"21">>);
encode_fld_val452(ID,_T, 'Exchange'                         ) -> encode_tagval(ID, <<"22">>);
encode_fld_val452(ID,_T, 'CustomerAccount'                  ) -> encode_tagval(ID, <<"24">>);
encode_fld_val452(ID,_T, 'CorrespondentClearingOrganization') -> encode_tagval(ID, <<"25">>);
encode_fld_val452(ID,_T, 'CorrespondentBroker'              ) -> encode_tagval(ID, <<"26">>);
encode_fld_val452(ID,_T, 'BuyerSeller'                      ) -> encode_tagval(ID, <<"27">>);
encode_fld_val452(ID,_T, 'Custodian'                        ) -> encode_tagval(ID, <<"28">>);
encode_fld_val452(ID,_T, 'Intermediary'                     ) -> encode_tagval(ID, <<"29">>);
encode_fld_val452(ID,_T, 'Agent'                            ) -> encode_tagval(ID, <<"30">>);
encode_fld_val452(ID,_T, 'SubCustodian'                     ) -> encode_tagval(ID, <<"31">>);
encode_fld_val452(ID,_T, 'Beneficiary'                      ) -> encode_tagval(ID, <<"32">>);
encode_fld_val452(ID,_T, 'InterestedParty'                  ) -> encode_tagval(ID, <<"33">>);
encode_fld_val452(ID,_T, 'RegulatoryBody'                   ) -> encode_tagval(ID, <<"34">>);
encode_fld_val452(ID,_T, 'LiquidityProvider'                ) -> encode_tagval(ID, <<"35">>);
encode_fld_val452(ID,_T, 'EnteringTrader'                   ) -> encode_tagval(ID, <<"36">>);
encode_fld_val452(ID,_T, 'ContraTrader'                     ) -> encode_tagval(ID, <<"37">>);
encode_fld_val452(ID,_T, 'PositionAccount'                  ) -> encode_tagval(ID, <<"38">>);
encode_fld_val452(ID, T, V                                  ) -> try_encode_val(ID, T, V).

decode_fld_val460(Val) ->
  case Val of
    <<"1" >> -> 'Agency'     ; %% 0
    <<"2" >> -> 'Commodity'  ; %% 1
    <<"3" >> -> 'Corporate'  ; %% 2
    <<"4" >> -> 'Currency'   ; %% 3
    <<"5" >> -> 'Equity'     ; %% 4
    <<"6" >> -> 'Government' ; %% 5
    <<"7" >> -> 'Index'      ; %% 6
    <<"8" >> -> 'Loan'       ; %% 7
    <<"9" >> -> 'Moneymarket'; %% 8
    <<"10">> -> 'Mortgage'   ; %% 9
    <<"11">> -> 'Municipal'  ; %% 10
    <<"12">> -> 'Other'      ; %% 11
    <<"13">> -> 'Financing'  ; %% 12
    _        -> Val
  end.

encode_fld_val460(ID,_T, 'Agency'     ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val460(ID,_T, 'Commodity'  ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val460(ID,_T, 'Corporate'  ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val460(ID,_T, 'Currency'   ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val460(ID,_T, 'Equity'     ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val460(ID,_T, 'Government' ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val460(ID,_T, 'Index'      ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val460(ID,_T, 'Loan'       ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val460(ID,_T, 'Moneymarket') -> encode_tagval(ID, <<"9" >>);
encode_fld_val460(ID,_T, 'Mortgage'   ) -> encode_tagval(ID, <<"10">>);
encode_fld_val460(ID,_T, 'Municipal'  ) -> encode_tagval(ID, <<"11">>);
encode_fld_val460(ID,_T, 'Other'      ) -> encode_tagval(ID, <<"12">>);
encode_fld_val460(ID,_T, 'Financing'  ) -> encode_tagval(ID, <<"13">>);
encode_fld_val460(ID, T, V            ) -> try_encode_val(ID, T, V).

decode_fld_val464(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val464(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val464(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val464(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val468(Val) ->
  case Val of
    <<"0">> -> 'RoundToNearest'; %% 0
    <<"1">> -> 'RoundDown'     ; %% 1
    <<"2">> -> 'RoundUp'       ; %% 2
    _       -> Val
  end.

encode_fld_val468(ID,_T, 'RoundToNearest') -> encode_tagval(ID, <<"0">>);
encode_fld_val468(ID,_T, 'RoundDown'     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val468(ID,_T, 'RoundUp'       ) -> encode_tagval(ID, <<"2">>);
encode_fld_val468(ID, T, V               ) -> try_encode_val(ID, T, V).

decode_fld_val477(Val) ->
  case Val of
    <<"1" >> -> 'Crest'                  ; %% 0
    <<"2" >> -> 'Nscc'                   ; %% 1
    <<"3" >> -> 'Euroclear'              ; %% 2
    <<"4" >> -> 'Clearstream'            ; %% 3
    <<"5" >> -> 'Cheque'                 ; %% 4
    <<"6" >> -> 'TelegraphicTransfer'    ; %% 5
    <<"7" >> -> 'Fedwire'                ; %% 6
    <<"8" >> -> 'DirectCredit'           ; %% 7
    <<"9" >> -> 'AchCredit'              ; %% 8
    <<"10">> -> 'Bpay'                   ; %% 9
    <<"11">> -> 'HighValueClearingSystem'; %% 10
    <<"12">> -> 'ReinvestInFund'         ; %% 11
    _        -> Val
  end.

encode_fld_val477(ID,_T, 'Crest'                  ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val477(ID,_T, 'Nscc'                   ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val477(ID,_T, 'Euroclear'              ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val477(ID,_T, 'Clearstream'            ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val477(ID,_T, 'Cheque'                 ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val477(ID,_T, 'TelegraphicTransfer'    ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val477(ID,_T, 'Fedwire'                ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val477(ID,_T, 'DirectCredit'           ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val477(ID,_T, 'AchCredit'              ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val477(ID,_T, 'Bpay'                   ) -> encode_tagval(ID, <<"10">>);
encode_fld_val477(ID,_T, 'HighValueClearingSystem') -> encode_tagval(ID, <<"11">>);
encode_fld_val477(ID,_T, 'ReinvestInFund'         ) -> encode_tagval(ID, <<"12">>);
encode_fld_val477(ID, T, V                        ) -> try_encode_val(ID, T, V).

decode_fld_val480(Val) ->
  case Val of
    <<"Y">> -> 'Yes'              ; %% 0
    <<"N">> -> 'NoExecutionOnly'  ; %% 1
    <<"M">> -> 'NoWaiverAgreement'; %% 2
    <<"O">> -> 'NoInstitutional'  ; %% 3
    _       -> Val
  end.

encode_fld_val480(ID,_T, 'Yes'              ) -> encode_tagval(ID, <<"Y">>);
encode_fld_val480(ID,_T, 'NoExecutionOnly'  ) -> encode_tagval(ID, <<"N">>);
encode_fld_val480(ID,_T, 'NoWaiverAgreement') -> encode_tagval(ID, <<"M">>);
encode_fld_val480(ID,_T, 'NoInstitutional'  ) -> encode_tagval(ID, <<"O">>);
encode_fld_val480(ID, T, V                  ) -> try_encode_val(ID, T, V).

decode_fld_val481(Val) ->
  case Val of
    <<"Y">> -> 'Passed'                                      ; %% 0
    <<"N">> -> 'NotChecked'                                  ; %% 1
    <<"1">> -> 'ExemptBelowTheLimit'                         ; %% 2
    <<"2">> -> 'ExemptClientMoneyTypeExemption'              ; %% 3
    <<"3">> -> 'ExemptAuthorisedCreditOrFinancialInstitution'; %% 4
    _       -> Val
  end.

encode_fld_val481(ID,_T, 'Passed'                                      ) -> encode_tagval(ID, <<"Y">>);
encode_fld_val481(ID,_T, 'NotChecked'                                  ) -> encode_tagval(ID, <<"N">>);
encode_fld_val481(ID,_T, 'ExemptBelowTheLimit'                         ) -> encode_tagval(ID, <<"1">>);
encode_fld_val481(ID,_T, 'ExemptClientMoneyTypeExemption'              ) -> encode_tagval(ID, <<"2">>);
encode_fld_val481(ID,_T, 'ExemptAuthorisedCreditOrFinancialInstitution') -> encode_tagval(ID, <<"3">>);
encode_fld_val481(ID, T, V                                             ) -> try_encode_val(ID, T, V).

decode_fld_val484(Val) ->
  case Val of
    <<"B">> -> 'BidPrice'                         ; %% 0
    <<"C">> -> 'CreationPrice'                    ; %% 1
    <<"D">> -> 'CreationPricePlusAdjustment'      ; %% 2
    <<"E">> -> 'CreationPricePlusAdjustmentAmount'; %% 3
    <<"O">> -> 'OfferPrice'                       ; %% 4
    <<"P">> -> 'OfferPriceMinusAdjustment'        ; %% 5
    <<"Q">> -> 'OfferPriceMinusAdjustmentAmount'  ; %% 6
    <<"S">> -> 'SinglePrice'                      ; %% 7
    _       -> Val
  end.

encode_fld_val484(ID,_T, 'BidPrice'                         ) -> encode_tagval(ID, <<"B">>);
encode_fld_val484(ID,_T, 'CreationPrice'                    ) -> encode_tagval(ID, <<"C">>);
encode_fld_val484(ID,_T, 'CreationPricePlusAdjustment'      ) -> encode_tagval(ID, <<"D">>);
encode_fld_val484(ID,_T, 'CreationPricePlusAdjustmentAmount') -> encode_tagval(ID, <<"E">>);
encode_fld_val484(ID,_T, 'OfferPrice'                       ) -> encode_tagval(ID, <<"O">>);
encode_fld_val484(ID,_T, 'OfferPriceMinusAdjustment'        ) -> encode_tagval(ID, <<"P">>);
encode_fld_val484(ID,_T, 'OfferPriceMinusAdjustmentAmount'  ) -> encode_tagval(ID, <<"Q">>);
encode_fld_val484(ID,_T, 'SinglePrice'                      ) -> encode_tagval(ID, <<"S">>);
encode_fld_val484(ID, T, V                                  ) -> try_encode_val(ID, T, V).

decode_fld_val492(Val) ->
  case Val of
    <<"1" >> -> 'Crest'                  ; %% 0
    <<"2" >> -> 'Nscc'                   ; %% 1
    <<"3" >> -> 'Euroclear'              ; %% 2
    <<"4" >> -> 'Clearstream'            ; %% 3
    <<"5" >> -> 'Cheque'                 ; %% 4
    <<"6" >> -> 'TelegraphicTransfer'    ; %% 5
    <<"7" >> -> 'Fedwire'                ; %% 6
    <<"8" >> -> 'DebitCard'              ; %% 7
    <<"9" >> -> 'DirectDebit'            ; %% 8
    <<"10">> -> 'DirectCredit'           ; %% 9
    <<"11">> -> 'CreditCard'             ; %% 10
    <<"12">> -> 'AchDebit'               ; %% 11
    <<"13">> -> 'AchCredit'              ; %% 12
    <<"14">> -> 'Bpay'                   ; %% 13
    <<"15">> -> 'HighValueClearingSystem'; %% 14
    _        -> Val
  end.

encode_fld_val492(ID,_T, 'Crest'                  ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val492(ID,_T, 'Nscc'                   ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val492(ID,_T, 'Euroclear'              ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val492(ID,_T, 'Clearstream'            ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val492(ID,_T, 'Cheque'                 ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val492(ID,_T, 'TelegraphicTransfer'    ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val492(ID,_T, 'Fedwire'                ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val492(ID,_T, 'DebitCard'              ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val492(ID,_T, 'DirectDebit'            ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val492(ID,_T, 'DirectCredit'           ) -> encode_tagval(ID, <<"10">>);
encode_fld_val492(ID,_T, 'CreditCard'             ) -> encode_tagval(ID, <<"11">>);
encode_fld_val492(ID,_T, 'AchDebit'               ) -> encode_tagval(ID, <<"12">>);
encode_fld_val492(ID,_T, 'AchCredit'              ) -> encode_tagval(ID, <<"13">>);
encode_fld_val492(ID,_T, 'Bpay'                   ) -> encode_tagval(ID, <<"14">>);
encode_fld_val492(ID,_T, 'HighValueClearingSystem') -> encode_tagval(ID, <<"15">>);
encode_fld_val492(ID, T, V                        ) -> try_encode_val(ID, T, V).

decode_fld_val495(Val) ->
  case Val of
    <<"0" >> -> 'NoneNotApplicable'                  ; %% 0
    <<"1" >> -> 'MaxiIsa'                            ; %% 1
    <<"2" >> -> 'Tessa'                              ; %% 2
    <<"3" >> -> 'MiniCashIsa'                        ; %% 3
    <<"4" >> -> 'MiniStocksAndSharesIsa'             ; %% 4
    <<"5" >> -> 'MiniInsuranceIsa'                   ; %% 5
    <<"6" >> -> 'CurrentYearPayment'                 ; %% 6
    <<"7" >> -> 'PriorYearPayment'                   ; %% 7
    <<"8" >> -> 'AssetTransfer'                      ; %% 8
    <<"9" >> -> 'Employee'                           ; %% 9
    <<"10">> -> 'EmployeeCurrentYear'                ; %% 10
    <<"11">> -> 'Employer'                           ; %% 11
    <<"12">> -> 'EmployerCurrentYear'                ; %% 12
    <<"13">> -> 'NonFundPrototypeIra'                ; %% 13
    <<"14">> -> 'NonFundQualifiedPlan'               ; %% 14
    <<"15">> -> 'DefinedContributionPlan'            ; %% 15
    <<"16">> -> 'IndividualRetirementAccount'        ; %% 16
    <<"17">> -> 'IndividualRetirementAccountRollover'; %% 17
    <<"18">> -> 'Keogh'                              ; %% 18
    <<"19">> -> 'ProfitSharingPlan'                  ; %% 19
    <<"20">> -> '401k'                               ; %% 20
    <<"21">> -> 'SelfDirectedIra'                    ; %% 21
    <<"22">> -> '403'                                ; %% 22
    <<"23">> -> '457'                                ; %% 23
    <<"24">> -> 'RothIra24'                          ; %% 24
    <<"25">> -> 'RothIra25'                          ; %% 25
    <<"26">> -> 'RothConversionIra26'                ; %% 26
    <<"27">> -> 'RothConversionIra27'                ; %% 27
    <<"28">> -> 'EducationIra28'                     ; %% 28
    <<"29">> -> 'EducationIra29'                     ; %% 29
    _        -> Val
  end.

encode_fld_val495(ID,_T, 'NoneNotApplicable'                  ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val495(ID,_T, 'MaxiIsa'                            ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val495(ID,_T, 'Tessa'                              ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val495(ID,_T, 'MiniCashIsa'                        ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val495(ID,_T, 'MiniStocksAndSharesIsa'             ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val495(ID,_T, 'MiniInsuranceIsa'                   ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val495(ID,_T, 'CurrentYearPayment'                 ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val495(ID,_T, 'PriorYearPayment'                   ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val495(ID,_T, 'AssetTransfer'                      ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val495(ID,_T, 'Employee'                           ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val495(ID,_T, 'EmployeeCurrentYear'                ) -> encode_tagval(ID, <<"10">>);
encode_fld_val495(ID,_T, 'Employer'                           ) -> encode_tagval(ID, <<"11">>);
encode_fld_val495(ID,_T, 'EmployerCurrentYear'                ) -> encode_tagval(ID, <<"12">>);
encode_fld_val495(ID,_T, 'NonFundPrototypeIra'                ) -> encode_tagval(ID, <<"13">>);
encode_fld_val495(ID,_T, 'NonFundQualifiedPlan'               ) -> encode_tagval(ID, <<"14">>);
encode_fld_val495(ID,_T, 'DefinedContributionPlan'            ) -> encode_tagval(ID, <<"15">>);
encode_fld_val495(ID,_T, 'IndividualRetirementAccount'        ) -> encode_tagval(ID, <<"16">>);
encode_fld_val495(ID,_T, 'IndividualRetirementAccountRollover') -> encode_tagval(ID, <<"17">>);
encode_fld_val495(ID,_T, 'Keogh'                              ) -> encode_tagval(ID, <<"18">>);
encode_fld_val495(ID,_T, 'ProfitSharingPlan'                  ) -> encode_tagval(ID, <<"19">>);
encode_fld_val495(ID,_T, '401k'                               ) -> encode_tagval(ID, <<"20">>);
encode_fld_val495(ID,_T, 'SelfDirectedIra'                    ) -> encode_tagval(ID, <<"21">>);
encode_fld_val495(ID,_T, '403'                                ) -> encode_tagval(ID, <<"22">>);
encode_fld_val495(ID,_T, '457'                                ) -> encode_tagval(ID, <<"23">>);
encode_fld_val495(ID,_T, 'RothIra24'                          ) -> encode_tagval(ID, <<"24">>);
encode_fld_val495(ID,_T, 'RothIra25'                          ) -> encode_tagval(ID, <<"25">>);
encode_fld_val495(ID,_T, 'RothConversionIra26'                ) -> encode_tagval(ID, <<"26">>);
encode_fld_val495(ID,_T, 'RothConversionIra27'                ) -> encode_tagval(ID, <<"27">>);
encode_fld_val495(ID,_T, 'EducationIra28'                     ) -> encode_tagval(ID, <<"28">>);
encode_fld_val495(ID,_T, 'EducationIra29'                     ) -> encode_tagval(ID, <<"29">>);
encode_fld_val495(ID, T, V                                    ) -> try_encode_val(ID, T, V).

decode_fld_val497(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val497(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val497(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val497(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val506(Val) ->
  case Val of
    <<"A">> -> 'Accepted'                                             ; %% 0
    <<"R">> -> 'Rejected'                                             ; %% 1
    <<"H">> -> 'Held'                                                 ; %% 2
    <<"N">> -> 'ReminderIeRegistrationInstructionsAreStillOutstanding'; %% 3
    _       -> Val
  end.

encode_fld_val506(ID,_T, 'Accepted'                                             ) -> encode_tagval(ID, <<"A">>);
encode_fld_val506(ID,_T, 'Rejected'                                             ) -> encode_tagval(ID, <<"R">>);
encode_fld_val506(ID,_T, 'Held'                                                 ) -> encode_tagval(ID, <<"H">>);
encode_fld_val506(ID,_T, 'ReminderIeRegistrationInstructionsAreStillOutstanding') -> encode_tagval(ID, <<"N">>);
encode_fld_val506(ID, T, V                                                      ) -> try_encode_val(ID, T, V).

decode_fld_val507(Val) ->
  case Val of
    <<"1" >> -> 'InvalidUnacceptableAccountType'               ; %% 0
    <<"2" >> -> 'InvalidUnacceptableTaxExemptType'             ; %% 1
    <<"3" >> -> 'InvalidUnacceptableOwnershipType'             ; %% 2
    <<"4" >> -> 'InvalidUnacceptableNoRegDetls'                ; %% 3
    <<"5" >> -> 'InvalidUnacceptableRegSeqNo'                  ; %% 4
    <<"6" >> -> 'InvalidUnacceptableRegDtls'                   ; %% 5
    <<"7" >> -> 'InvalidUnacceptableMailingDtls'               ; %% 6
    <<"8" >> -> 'InvalidUnacceptableMailingInst'               ; %% 7
    <<"9" >> -> 'InvalidUnacceptableInvestorId'                ; %% 8
    <<"10">> -> 'InvalidUnacceptableInvestorIdSource'          ; %% 9
    <<"11">> -> 'InvalidUnacceptableDateOfBirth'               ; %% 10
    <<"12">> -> 'InvalidUnacceptableInvestorCountryOfResidence'; %% 11
    <<"13">> -> 'InvalidUnacceptableNodistribinstns'           ; %% 12
    <<"14">> -> 'InvalidUnacceptableDistribPercentage'         ; %% 13
    <<"15">> -> 'InvalidUnacceptableDistribPaymentMethod'      ; %% 14
    <<"16">> -> 'InvalidUnacceptableCashDistribAgentAcctName'  ; %% 15
    <<"17">> -> 'InvalidUnacceptableCashDistribAgentCode'      ; %% 16
    <<"18">> -> 'InvalidUnacceptableCashDistribAgentAcctNum'   ; %% 17
    <<"99">> -> 'Other'                                        ; %% 18
    _        -> Val
  end.

encode_fld_val507(ID,_T, 'InvalidUnacceptableAccountType'               ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableTaxExemptType'             ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableOwnershipType'             ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableNoRegDetls'                ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableRegSeqNo'                  ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableRegDtls'                   ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableMailingDtls'               ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableMailingInst'               ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableInvestorId'                ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableInvestorIdSource'          ) -> encode_tagval(ID, <<"10">>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableDateOfBirth'               ) -> encode_tagval(ID, <<"11">>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableInvestorCountryOfResidence') -> encode_tagval(ID, <<"12">>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableNodistribinstns'           ) -> encode_tagval(ID, <<"13">>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableDistribPercentage'         ) -> encode_tagval(ID, <<"14">>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableDistribPaymentMethod'      ) -> encode_tagval(ID, <<"15">>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableCashDistribAgentAcctName'  ) -> encode_tagval(ID, <<"16">>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableCashDistribAgentCode'      ) -> encode_tagval(ID, <<"17">>);
encode_fld_val507(ID,_T, 'InvalidUnacceptableCashDistribAgentAcctNum'   ) -> encode_tagval(ID, <<"18">>);
encode_fld_val507(ID,_T, 'Other'                                        ) -> encode_tagval(ID, <<"99">>);
encode_fld_val507(ID, T, V                                              ) -> try_encode_val(ID, T, V).

decode_fld_val514(Val) ->
  case Val of
    <<"0">> -> 'New'    ; %% 0
    <<"1">> -> 'Replace'; %% 1
    <<"2">> -> 'Cancel' ; %% 2
    _       -> Val
  end.

encode_fld_val514(ID,_T, 'New'    ) -> encode_tagval(ID, <<"0">>);
encode_fld_val514(ID,_T, 'Replace') -> encode_tagval(ID, <<"1">>);
encode_fld_val514(ID,_T, 'Cancel' ) -> encode_tagval(ID, <<"2">>);
encode_fld_val514(ID, T, V        ) -> try_encode_val(ID, T, V).

decode_fld_val517(Val) ->
  case Val of
    <<"J">> -> 'JointInvestors' ; %% 0
    <<"T">> -> 'TenantsInCommon'; %% 1
    <<"2">> -> 'JointTrustees'  ; %% 2
    _       -> Val
  end.

encode_fld_val517(ID,_T, 'JointInvestors' ) -> encode_tagval(ID, <<"J">>);
encode_fld_val517(ID,_T, 'TenantsInCommon') -> encode_tagval(ID, <<"T">>);
encode_fld_val517(ID,_T, 'JointTrustees'  ) -> encode_tagval(ID, <<"2">>);
encode_fld_val517(ID, T, V                ) -> try_encode_val(ID, T, V).

decode_fld_val519(Val) ->
  case Val of
    <<"1" >> -> 'CommissionAmount'                  ; %% 0
    <<"2" >> -> 'Commission'                        ; %% 1
    <<"3" >> -> 'InitialChargeAmount'               ; %% 2
    <<"4" >> -> 'InitialCharge'                     ; %% 3
    <<"5" >> -> 'DiscountAmount'                    ; %% 4
    <<"6" >> -> 'Discount'                          ; %% 5
    <<"7" >> -> 'DilutionLevyAmount'                ; %% 6
    <<"8" >> -> 'DilutionLevy'                      ; %% 7
    <<"9" >> -> 'ExitChargeAmount'                  ; %% 8
    <<"10">> -> 'ExitCharge'                        ; %% 9
    <<"11">> -> 'FundBasedRenewalCommission'        ; %% 10
    <<"12">> -> 'ProjectedFundValue'                ; %% 11
    <<"13">> -> 'FundBasedRenewalCommissionAmount13'; %% 12
    <<"14">> -> 'FundBasedRenewalCommissionAmount14'; %% 13
    <<"15">> -> 'NetSettlementAmount'               ; %% 14
    _        -> Val
  end.

encode_fld_val519(ID,_T, 'CommissionAmount'                  ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val519(ID,_T, 'Commission'                        ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val519(ID,_T, 'InitialChargeAmount'               ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val519(ID,_T, 'InitialCharge'                     ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val519(ID,_T, 'DiscountAmount'                    ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val519(ID,_T, 'Discount'                          ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val519(ID,_T, 'DilutionLevyAmount'                ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val519(ID,_T, 'DilutionLevy'                      ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val519(ID,_T, 'ExitChargeAmount'                  ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val519(ID,_T, 'ExitCharge'                        ) -> encode_tagval(ID, <<"10">>);
encode_fld_val519(ID,_T, 'FundBasedRenewalCommission'        ) -> encode_tagval(ID, <<"11">>);
encode_fld_val519(ID,_T, 'ProjectedFundValue'                ) -> encode_tagval(ID, <<"12">>);
encode_fld_val519(ID,_T, 'FundBasedRenewalCommissionAmount13') -> encode_tagval(ID, <<"13">>);
encode_fld_val519(ID,_T, 'FundBasedRenewalCommissionAmount14') -> encode_tagval(ID, <<"14">>);
encode_fld_val519(ID,_T, 'NetSettlementAmount'               ) -> encode_tagval(ID, <<"15">>);
encode_fld_val519(ID, T, V                                   ) -> try_encode_val(ID, T, V).

decode_fld_val522(Val) ->
  case Val of
    <<"1" >> -> 'IndividualInvestor'            ; %% 0
    <<"2" >> -> 'PublicCompany'                 ; %% 1
    <<"3" >> -> 'PrivateCompany'                ; %% 2
    <<"4" >> -> 'IndividualTrustee'             ; %% 3
    <<"5" >> -> 'CompanyTrustee'                ; %% 4
    <<"6" >> -> 'PensionPlan'                   ; %% 5
    <<"7" >> -> 'CustodianUnderGiftsToMinorsAct'; %% 6
    <<"8" >> -> 'Trusts'                        ; %% 7
    <<"9" >> -> 'Fiduciaries'                   ; %% 8
    <<"10">> -> 'NetworkingSubAccount'          ; %% 9
    <<"11">> -> 'NonProfitOrganization'         ; %% 10
    <<"12">> -> 'CorporateBody'                 ; %% 11
    <<"13">> -> 'Nominee'                       ; %% 12
    _        -> Val
  end.

encode_fld_val522(ID,_T, 'IndividualInvestor'            ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val522(ID,_T, 'PublicCompany'                 ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val522(ID,_T, 'PrivateCompany'                ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val522(ID,_T, 'IndividualTrustee'             ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val522(ID,_T, 'CompanyTrustee'                ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val522(ID,_T, 'PensionPlan'                   ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val522(ID,_T, 'CustodianUnderGiftsToMinorsAct') -> encode_tagval(ID, <<"7" >>);
encode_fld_val522(ID,_T, 'Trusts'                        ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val522(ID,_T, 'Fiduciaries'                   ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val522(ID,_T, 'NetworkingSubAccount'          ) -> encode_tagval(ID, <<"10">>);
encode_fld_val522(ID,_T, 'NonProfitOrganization'         ) -> encode_tagval(ID, <<"11">>);
encode_fld_val522(ID,_T, 'CorporateBody'                 ) -> encode_tagval(ID, <<"12">>);
encode_fld_val522(ID,_T, 'Nominee'                       ) -> encode_tagval(ID, <<"13">>);
encode_fld_val522(ID, T, V                               ) -> try_encode_val(ID, T, V).

decode_fld_val528(Val) ->
  case Val of
    <<"A">> -> 'Agency'             ; %% 0
    <<"G">> -> 'Proprietary'        ; %% 1
    <<"I">> -> 'Individual'         ; %% 2
    <<"P">> -> 'Principal'          ; %% 3
    <<"R">> -> 'RisklessPrincipal'  ; %% 4
    <<"W">> -> 'AgentForOtherMember'; %% 5
    _       -> Val
  end.

encode_fld_val528(ID,_T, 'Agency'             ) -> encode_tagval(ID, <<"A">>);
encode_fld_val528(ID,_T, 'Proprietary'        ) -> encode_tagval(ID, <<"G">>);
encode_fld_val528(ID,_T, 'Individual'         ) -> encode_tagval(ID, <<"I">>);
encode_fld_val528(ID,_T, 'Principal'          ) -> encode_tagval(ID, <<"P">>);
encode_fld_val528(ID,_T, 'RisklessPrincipal'  ) -> encode_tagval(ID, <<"R">>);
encode_fld_val528(ID,_T, 'AgentForOtherMember') -> encode_tagval(ID, <<"W">>);
encode_fld_val528(ID, T, V                    ) -> try_encode_val(ID, T, V).

decode_fld_val529(Val) ->
  case Val of
    <<"1">> -> 'ProgramTrade'                                                               ; %% 0
    <<"2">> -> 'IndexArbitrage'                                                             ; %% 1
    <<"3">> -> 'NonIndexArbitrage'                                                          ; %% 2
    <<"4">> -> 'CompetingMarketMaker'                                                       ; %% 3
    <<"5">> -> 'ActingAsMarketMakerOrSpecialistInTheSecurity'                               ; %% 4
    <<"6">> -> 'ActingAsMarketMakerOrSpecialistInTheUnderlyingSecurityOfADerivativeSecurity'; %% 5
    <<"7">> -> 'ForeignEntity'                                                              ; %% 6
    <<"8">> -> 'ExternalMarketParticipant'                                                  ; %% 7
    <<"9">> -> 'ExternalInterConnectedMarketLinkage'                                        ; %% 8
    <<"A">> -> 'RisklessArbitrage'                                                          ; %% 9
    _       -> Val
  end.

encode_fld_val529(ID,_T, 'ProgramTrade'                                                               ) -> encode_tagval(ID, <<"1">>);
encode_fld_val529(ID,_T, 'IndexArbitrage'                                                             ) -> encode_tagval(ID, <<"2">>);
encode_fld_val529(ID,_T, 'NonIndexArbitrage'                                                          ) -> encode_tagval(ID, <<"3">>);
encode_fld_val529(ID,_T, 'CompetingMarketMaker'                                                       ) -> encode_tagval(ID, <<"4">>);
encode_fld_val529(ID,_T, 'ActingAsMarketMakerOrSpecialistInTheSecurity'                               ) -> encode_tagval(ID, <<"5">>);
encode_fld_val529(ID,_T, 'ActingAsMarketMakerOrSpecialistInTheUnderlyingSecurityOfADerivativeSecurity') -> encode_tagval(ID, <<"6">>);
encode_fld_val529(ID,_T, 'ForeignEntity'                                                              ) -> encode_tagval(ID, <<"7">>);
encode_fld_val529(ID,_T, 'ExternalMarketParticipant'                                                  ) -> encode_tagval(ID, <<"8">>);
encode_fld_val529(ID,_T, 'ExternalInterConnectedMarketLinkage'                                        ) -> encode_tagval(ID, <<"9">>);
encode_fld_val529(ID,_T, 'RisklessArbitrage'                                                          ) -> encode_tagval(ID, <<"A">>);
encode_fld_val529(ID, T, V                                                                            ) -> try_encode_val(ID, T, V).

decode_fld_val530(Val) ->
  case Val of
    <<"1">> -> 'CancelOrdersForASecurity'           ; %% 0
    <<"2">> -> 'CancelOrdersForAnUnderlyingSecurity'; %% 1
    <<"3">> -> 'CancelOrdersForAProduct'            ; %% 2
    <<"4">> -> 'CancelOrdersForACficode'            ; %% 3
    <<"5">> -> 'CancelOrdersForASecuritytype'       ; %% 4
    <<"6">> -> 'CancelOrdersForATradingSession'     ; %% 5
    <<"7">> -> 'CancelAllOrders'                    ; %% 6
    _       -> Val
  end.

encode_fld_val530(ID,_T, 'CancelOrdersForASecurity'           ) -> encode_tagval(ID, <<"1">>);
encode_fld_val530(ID,_T, 'CancelOrdersForAnUnderlyingSecurity') -> encode_tagval(ID, <<"2">>);
encode_fld_val530(ID,_T, 'CancelOrdersForAProduct'            ) -> encode_tagval(ID, <<"3">>);
encode_fld_val530(ID,_T, 'CancelOrdersForACficode'            ) -> encode_tagval(ID, <<"4">>);
encode_fld_val530(ID,_T, 'CancelOrdersForASecuritytype'       ) -> encode_tagval(ID, <<"5">>);
encode_fld_val530(ID,_T, 'CancelOrdersForATradingSession'     ) -> encode_tagval(ID, <<"6">>);
encode_fld_val530(ID,_T, 'CancelAllOrders'                    ) -> encode_tagval(ID, <<"7">>);
encode_fld_val530(ID, T, V                                    ) -> try_encode_val(ID, T, V).

decode_fld_val531(Val) ->
  case Val of
    <<"0">> -> 'CancelRequestRejected'              ; %% 0
    <<"1">> -> 'CancelOrdersForASecurity'           ; %% 1
    <<"2">> -> 'CancelOrdersForAnUnderlyingSecurity'; %% 2
    <<"3">> -> 'CancelOrdersForAProduct'            ; %% 3
    <<"4">> -> 'CancelOrdersForACficode'            ; %% 4
    <<"5">> -> 'CancelOrdersForASecuritytype'       ; %% 5
    <<"6">> -> 'CancelOrdersForATradingSession'     ; %% 6
    <<"7">> -> 'CancelAllOrders'                    ; %% 7
    _       -> Val
  end.

encode_fld_val531(ID,_T, 'CancelRequestRejected'              ) -> encode_tagval(ID, <<"0">>);
encode_fld_val531(ID,_T, 'CancelOrdersForASecurity'           ) -> encode_tagval(ID, <<"1">>);
encode_fld_val531(ID,_T, 'CancelOrdersForAnUnderlyingSecurity') -> encode_tagval(ID, <<"2">>);
encode_fld_val531(ID,_T, 'CancelOrdersForAProduct'            ) -> encode_tagval(ID, <<"3">>);
encode_fld_val531(ID,_T, 'CancelOrdersForACficode'            ) -> encode_tagval(ID, <<"4">>);
encode_fld_val531(ID,_T, 'CancelOrdersForASecuritytype'       ) -> encode_tagval(ID, <<"5">>);
encode_fld_val531(ID,_T, 'CancelOrdersForATradingSession'     ) -> encode_tagval(ID, <<"6">>);
encode_fld_val531(ID,_T, 'CancelAllOrders'                    ) -> encode_tagval(ID, <<"7">>);
encode_fld_val531(ID, T, V                                    ) -> try_encode_val(ID, T, V).

decode_fld_val532(Val) ->
  case Val of
    <<"0" >> -> 'MassCancelNotSupported'        ; %% 0
    <<"1" >> -> 'InvalidOrUnknownSecurity'      ; %% 1
    <<"2" >> -> 'InvalidOrUnknownUnderlying'    ; %% 2
    <<"3" >> -> 'InvalidOrUnknownProduct'       ; %% 3
    <<"4" >> -> 'InvalidOrUnknownCficode'       ; %% 4
    <<"5" >> -> 'InvalidOrUnknownSecurityType'  ; %% 5
    <<"6" >> -> 'InvalidOrUnknownTradingSession'; %% 6
    <<"99">> -> 'Other'                         ; %% 7
    _        -> Val
  end.

encode_fld_val532(ID,_T, 'MassCancelNotSupported'        ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val532(ID,_T, 'InvalidOrUnknownSecurity'      ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val532(ID,_T, 'InvalidOrUnknownUnderlying'    ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val532(ID,_T, 'InvalidOrUnknownProduct'       ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val532(ID,_T, 'InvalidOrUnknownCficode'       ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val532(ID,_T, 'InvalidOrUnknownSecurityType'  ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val532(ID,_T, 'InvalidOrUnknownTradingSession') -> encode_tagval(ID, <<"6" >>);
encode_fld_val532(ID,_T, 'Other'                         ) -> encode_tagval(ID, <<"99">>);
encode_fld_val532(ID, T, V                               ) -> try_encode_val(ID, T, V).

decode_fld_val537(Val) ->
  case Val of
    <<"0">> -> 'Indicative'         ; %% 0
    <<"1">> -> 'Tradeable'          ; %% 1
    <<"2">> -> 'RestrictedTradeable'; %% 2
    <<"3">> -> 'Counter'            ; %% 3
    _       -> Val
  end.

encode_fld_val537(ID,_T, 'Indicative'         ) -> encode_tagval(ID, <<"0">>);
encode_fld_val537(ID,_T, 'Tradeable'          ) -> encode_tagval(ID, <<"1">>);
encode_fld_val537(ID,_T, 'RestrictedTradeable') -> encode_tagval(ID, <<"2">>);
encode_fld_val537(ID,_T, 'Counter'            ) -> encode_tagval(ID, <<"3">>);
encode_fld_val537(ID, T, V                    ) -> try_encode_val(ID, T, V).

decode_fld_val544(Val) ->
  case Val of
    <<"1">> -> 'Cash'       ; %% 0
    <<"2">> -> 'MarginOpen' ; %% 1
    <<"3">> -> 'MarginClose'; %% 2
    _       -> Val
  end.

encode_fld_val544(ID,_T, 'Cash'       ) -> encode_tagval(ID, <<"1">>);
encode_fld_val544(ID,_T, 'MarginOpen' ) -> encode_tagval(ID, <<"2">>);
encode_fld_val544(ID,_T, 'MarginClose') -> encode_tagval(ID, <<"3">>);
encode_fld_val544(ID, T, V            ) -> try_encode_val(ID, T, V).

decode_fld_val546(Val) ->
  case Val of
    <<"1">> -> 'Local'   ; %% 0
    <<"2">> -> 'National'; %% 1
    <<"3">> -> 'Global'  ; %% 2
    _       -> Val
  end.

encode_fld_val546(ID,_T, 'Local'   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val546(ID,_T, 'National') -> encode_tagval(ID, <<"2">>);
encode_fld_val546(ID,_T, 'Global'  ) -> encode_tagval(ID, <<"3">>);
encode_fld_val546(ID, T, V         ) -> try_encode_val(ID, T, V).

decode_fld_val547(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val547(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val547(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val547(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val549(Val) ->
  case Val of
    <<"1">> -> 'CrossTradeWhichIsExecutedCompletelyOrNotBothSidesAreTreatedInTheSameMannerThisIsEquivalentToAnAllOrNone'                       ; %% 0
    <<"2">> -> 'CrossTradeWhichIsExecutedPartiallyAndTheRestIsCancelledOneSideIsFullyExecutedTheOtherSideIsPartiallyExecutedWithTheRemainder'  ; %% 1
    <<"3">> -> 'CrossTradeWhichIsPartiallyExecutedWithTheUnfilledPortionsRemainingActiveOneSideOfTheCrossIsFullyExecuted'                      ; %% 2
    <<"4">> -> 'CrossTradeIsExecutedWithExistingOrdersWithTheSamePriceInTheCaseOtherOrdersExistWithTheSamePriceTheQuantityOfTheCrossIsExecuted'; %% 3
    _       -> Val
  end.

encode_fld_val549(ID,_T, 'CrossTradeWhichIsExecutedCompletelyOrNotBothSidesAreTreatedInTheSameMannerThisIsEquivalentToAnAllOrNone'                       ) -> encode_tagval(ID, <<"1">>);
encode_fld_val549(ID,_T, 'CrossTradeWhichIsExecutedPartiallyAndTheRestIsCancelledOneSideIsFullyExecutedTheOtherSideIsPartiallyExecutedWithTheRemainder'  ) -> encode_tagval(ID, <<"2">>);
encode_fld_val549(ID,_T, 'CrossTradeWhichIsPartiallyExecutedWithTheUnfilledPortionsRemainingActiveOneSideOfTheCrossIsFullyExecuted'                      ) -> encode_tagval(ID, <<"3">>);
encode_fld_val549(ID,_T, 'CrossTradeIsExecutedWithExistingOrdersWithTheSamePriceInTheCaseOtherOrdersExistWithTheSamePriceTheQuantityOfTheCrossIsExecuted') -> encode_tagval(ID, <<"4">>);
encode_fld_val549(ID, T, V                                                                                                                               ) -> try_encode_val(ID, T, V).

decode_fld_val550(Val) ->
  case Val of
    <<"0">> -> 'None'                 ; %% 0
    <<"1">> -> 'BuySideIsPrioritized' ; %% 1
    <<"2">> -> 'SellSideIsPrioritized'; %% 2
    _       -> Val
  end.

encode_fld_val550(ID,_T, 'None'                 ) -> encode_tagval(ID, <<"0">>);
encode_fld_val550(ID,_T, 'BuySideIsPrioritized' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val550(ID,_T, 'SellSideIsPrioritized') -> encode_tagval(ID, <<"2">>);
encode_fld_val550(ID, T, V                      ) -> try_encode_val(ID, T, V).

decode_fld_val552(Val) ->
  case Val of
    <<"1">> -> 'OneSide'  ; %% 0
    <<"2">> -> 'BothSides'; %% 1
    _       -> Val
  end.


decode_fld_val559(Val) ->
  case Val of
    <<"0">> -> 'Symbol'                  ; %% 0
    <<"1">> -> 'SecuritytypeAndOrCficode'; %% 1
    <<"2">> -> 'Product'                 ; %% 2
    <<"3">> -> 'Tradingsessionid'        ; %% 3
    <<"4">> -> 'AllSecurities'           ; %% 4
    _       -> Val
  end.

encode_fld_val559(ID,_T, 'Symbol'                  ) -> encode_tagval(ID, <<"0">>);
encode_fld_val559(ID,_T, 'SecuritytypeAndOrCficode') -> encode_tagval(ID, <<"1">>);
encode_fld_val559(ID,_T, 'Product'                 ) -> encode_tagval(ID, <<"2">>);
encode_fld_val559(ID,_T, 'Tradingsessionid'        ) -> encode_tagval(ID, <<"3">>);
encode_fld_val559(ID,_T, 'AllSecurities'           ) -> encode_tagval(ID, <<"4">>);
encode_fld_val559(ID, T, V                         ) -> try_encode_val(ID, T, V).

decode_fld_val560(Val) ->
  case Val of
    <<"0">> -> 'ValidRequest'                                ; %% 0
    <<"1">> -> 'InvalidOrUnsupportedRequest'                 ; %% 1
    <<"2">> -> 'NoInstrumentsFoundThatMatchSelectionCriteria'; %% 2
    <<"3">> -> 'NotAuthorizedToRetrieveInstrumentData'       ; %% 3
    <<"4">> -> 'InstrumentDataTemporarilyUnavailable'        ; %% 4
    <<"5">> -> 'RequestForInstrumentDataNotSupported'        ; %% 5
    _       -> Val
  end.

encode_fld_val560(ID,_T, 'ValidRequest'                                ) -> encode_tagval(ID, <<"0">>);
encode_fld_val560(ID,_T, 'InvalidOrUnsupportedRequest'                 ) -> encode_tagval(ID, <<"1">>);
encode_fld_val560(ID,_T, 'NoInstrumentsFoundThatMatchSelectionCriteria') -> encode_tagval(ID, <<"2">>);
encode_fld_val560(ID,_T, 'NotAuthorizedToRetrieveInstrumentData'       ) -> encode_tagval(ID, <<"3">>);
encode_fld_val560(ID,_T, 'InstrumentDataTemporarilyUnavailable'        ) -> encode_tagval(ID, <<"4">>);
encode_fld_val560(ID,_T, 'RequestForInstrumentDataNotSupported'        ) -> encode_tagval(ID, <<"5">>);
encode_fld_val560(ID, T, V                                             ) -> try_encode_val(ID, T, V).

decode_fld_val563(Val) ->
  case Val of
    <<"0">> -> 'ReportByMulitlegSecurityOnly'                                             ; %% 0
    <<"1">> -> 'ReportByMultilegSecurityAndByInstrumentLegsBelongingToTheMultilegSecurity'; %% 1
    <<"2">> -> 'ReportByInstrumentLegsBelongingToTheMultilegSecurityOnly'                 ; %% 2
    _       -> Val
  end.

encode_fld_val563(ID,_T, 'ReportByMulitlegSecurityOnly'                                             ) -> encode_tagval(ID, <<"0">>);
encode_fld_val563(ID,_T, 'ReportByMultilegSecurityAndByInstrumentLegsBelongingToTheMultilegSecurity') -> encode_tagval(ID, <<"1">>);
encode_fld_val563(ID,_T, 'ReportByInstrumentLegsBelongingToTheMultilegSecurityOnly'                 ) -> encode_tagval(ID, <<"2">>);
encode_fld_val563(ID, T, V                                                                          ) -> try_encode_val(ID, T, V).

decode_fld_val567(Val) ->
  case Val of
    <<"1" >> -> 'UnknownOrInvalidTradingsessionid'; %% 0
    <<"99">> -> 'Other'                           ; %% 1
    _        -> Val
  end.

encode_fld_val567(ID,_T, 'UnknownOrInvalidTradingsessionid') -> encode_tagval(ID, <<"1" >>);
encode_fld_val567(ID,_T, 'Other'                           ) -> encode_tagval(ID, <<"99">>);
encode_fld_val567(ID, T, V                                 ) -> try_encode_val(ID, T, V).

decode_fld_val569(Val) ->
  case Val of
    <<"0">> -> 'AllTrades'                                     ; %% 0
    <<"1">> -> 'MatchedTradesMatchingCriteriaProvidedOnRequest'; %% 1
    <<"2">> -> 'UnmatchedTradesThatMatchCriteria'              ; %% 2
    <<"3">> -> 'UnreportedTradesThatMatchCriteria'             ; %% 3
    <<"4">> -> 'AdvisoriesThatMatchCriteria'                   ; %% 4
    _       -> Val
  end.

encode_fld_val569(ID,_T, 'AllTrades'                                     ) -> encode_tagval(ID, <<"0">>);
encode_fld_val569(ID,_T, 'MatchedTradesMatchingCriteriaProvidedOnRequest') -> encode_tagval(ID, <<"1">>);
encode_fld_val569(ID,_T, 'UnmatchedTradesThatMatchCriteria'              ) -> encode_tagval(ID, <<"2">>);
encode_fld_val569(ID,_T, 'UnreportedTradesThatMatchCriteria'             ) -> encode_tagval(ID, <<"3">>);
encode_fld_val569(ID,_T, 'AdvisoriesThatMatchCriteria'                   ) -> encode_tagval(ID, <<"4">>);
encode_fld_val569(ID, T, V                                               ) -> try_encode_val(ID, T, V).

decode_fld_val570(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val570(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val570(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val570(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val573(Val) ->
  case Val of
    <<"0">> -> 'ComparedMatchedOrAffirmed'      ; %% 0
    <<"1">> -> 'UncomparedUnmatchedOrUnaffirmed'; %% 1
    <<"2">> -> 'AdvisoryOrAlert'                ; %% 2
    _       -> Val
  end.

encode_fld_val573(ID,_T, 'ComparedMatchedOrAffirmed'      ) -> encode_tagval(ID, <<"0">>);
encode_fld_val573(ID,_T, 'UncomparedUnmatchedOrUnaffirmed') -> encode_tagval(ID, <<"1">>);
encode_fld_val573(ID,_T, 'AdvisoryOrAlert'                ) -> encode_tagval(ID, <<"2">>);
encode_fld_val573(ID, T, V                                ) -> try_encode_val(ID, T, V).

decode_fld_val574(Val) ->
  case Val of
    <<"A1">> -> 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusFourBadgesAndExecutionTime'; %% 0
    <<"A2">> -> 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusFourBadges'                ; %% 1
    <<"A3">> -> 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusTwoBadgesAndExecutionTime' ; %% 2
    <<"A4">> -> 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusTwoBadges'                 ; %% 3
    <<"A5">> -> 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusExecutionTime'             ; %% 4
    <<"AQ">> -> 'ComparedRecordsResultingFromStampedAdvisoriesOrSpecialistAcceptsPairOffs'                                    ; %% 5
    <<"S1">> -> 'SummarizedMatchUsingA1ExactMatchCriteriaExceptQuantityIsSummarized'                                          ; %% 6
    <<"S2">> -> 'SummarizedMatchUsingA2ExactMatchCriteriaExceptQuantityIsSummarized'                                          ; %% 7
    <<"S3">> -> 'SummarizedMatchUsingA3ExactMatchCriteriaExceptQuantityIsSummarized'                                          ; %% 8
    <<"S4">> -> 'SummarizedMatchUsingA4ExactMatchCriteriaExceptQuantityIsSummarized'                                          ; %% 9
    <<"S5">> -> 'SummarizedMatchUsingA5ExactMatchCriteriaExceptQuantityIsSummarized'                                          ; %% 10
    <<"M1">> -> 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorMinusBadgesAndTimesActM1Match' ; %% 11
    <<"M2">> -> 'SummarizedMatchMinusBadgesAndTimesActM2Match'                                                                ; %% 12
    <<"MT">> -> 'OcsLockedInNonAct'                                                                                           ; %% 13
    <<"M3">> -> 'ActAcceptedTrade'                                                                                            ; %% 14
    <<"M4">> -> 'ActDefaultTrade'                                                                                             ; %% 15
    <<"M5">> -> 'ActDefaultAfterM2'                                                                                           ; %% 16
    <<"M6">> -> 'ActM6Match'                                                                                                  ; %% 17
    _        -> Val
  end.

encode_fld_val574(ID,_T, 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusFourBadgesAndExecutionTime') -> encode_tagval(ID, <<"A1">>);
encode_fld_val574(ID,_T, 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusFourBadges'                ) -> encode_tagval(ID, <<"A2">>);
encode_fld_val574(ID,_T, 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusTwoBadgesAndExecutionTime' ) -> encode_tagval(ID, <<"A3">>);
encode_fld_val574(ID,_T, 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusTwoBadges'                 ) -> encode_tagval(ID, <<"A4">>);
encode_fld_val574(ID,_T, 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusExecutionTime'             ) -> encode_tagval(ID, <<"A5">>);
encode_fld_val574(ID,_T, 'ComparedRecordsResultingFromStampedAdvisoriesOrSpecialistAcceptsPairOffs'                                    ) -> encode_tagval(ID, <<"AQ">>);
encode_fld_val574(ID,_T, 'SummarizedMatchUsingA1ExactMatchCriteriaExceptQuantityIsSummarized'                                          ) -> encode_tagval(ID, <<"S1">>);
encode_fld_val574(ID,_T, 'SummarizedMatchUsingA2ExactMatchCriteriaExceptQuantityIsSummarized'                                          ) -> encode_tagval(ID, <<"S2">>);
encode_fld_val574(ID,_T, 'SummarizedMatchUsingA3ExactMatchCriteriaExceptQuantityIsSummarized'                                          ) -> encode_tagval(ID, <<"S3">>);
encode_fld_val574(ID,_T, 'SummarizedMatchUsingA4ExactMatchCriteriaExceptQuantityIsSummarized'                                          ) -> encode_tagval(ID, <<"S4">>);
encode_fld_val574(ID,_T, 'SummarizedMatchUsingA5ExactMatchCriteriaExceptQuantityIsSummarized'                                          ) -> encode_tagval(ID, <<"S5">>);
encode_fld_val574(ID,_T, 'ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorMinusBadgesAndTimesActM1Match' ) -> encode_tagval(ID, <<"M1">>);
encode_fld_val574(ID,_T, 'SummarizedMatchMinusBadgesAndTimesActM2Match'                                                                ) -> encode_tagval(ID, <<"M2">>);
encode_fld_val574(ID,_T, 'OcsLockedInNonAct'                                                                                           ) -> encode_tagval(ID, <<"MT">>);
encode_fld_val574(ID,_T, 'ActAcceptedTrade'                                                                                            ) -> encode_tagval(ID, <<"M3">>);
encode_fld_val574(ID,_T, 'ActDefaultTrade'                                                                                             ) -> encode_tagval(ID, <<"M4">>);
encode_fld_val574(ID,_T, 'ActDefaultAfterM2'                                                                                           ) -> encode_tagval(ID, <<"M5">>);
encode_fld_val574(ID,_T, 'ActM6Match'                                                                                                  ) -> encode_tagval(ID, <<"M6">>);
encode_fld_val574(ID, T, V                                                                                                             ) -> try_encode_val(ID, T, V).

decode_fld_val575(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val575(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val575(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val575(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val577(Val) ->
  case Val of
    <<"0" >> -> 'ProcessNormally'                ; %% 0
    <<"1" >> -> 'ExcludeFromAllNetting'          ; %% 1
    <<"2" >> -> 'BilateralNettingOnly'           ; %% 2
    <<"3" >> -> 'ExClearing'                     ; %% 3
    <<"4" >> -> 'SpecialTrade'                   ; %% 4
    <<"5" >> -> 'MultilateralNetting'            ; %% 5
    <<"6" >> -> 'ClearAgainstCentralCounterparty'; %% 6
    <<"7" >> -> 'ExcludeFromCentralCounterparty' ; %% 7
    <<"8" >> -> 'ManualMode'                     ; %% 8
    <<"9" >> -> 'AutomaticPostingMode'           ; %% 9
    <<"10">> -> 'AutomaticGiveUpMode'            ; %% 10
    <<"11">> -> 'QualifiedServiceRepresentative' ; %% 11
    <<"12">> -> 'CustomerTrade'                  ; %% 12
    <<"13">> -> 'SelfClearing'                   ; %% 13
    _        -> Val
  end.

encode_fld_val577(ID,_T, 'ProcessNormally'                ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val577(ID,_T, 'ExcludeFromAllNetting'          ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val577(ID,_T, 'BilateralNettingOnly'           ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val577(ID,_T, 'ExClearing'                     ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val577(ID,_T, 'SpecialTrade'                   ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val577(ID,_T, 'MultilateralNetting'            ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val577(ID,_T, 'ClearAgainstCentralCounterparty') -> encode_tagval(ID, <<"6" >>);
encode_fld_val577(ID,_T, 'ExcludeFromCentralCounterparty' ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val577(ID,_T, 'ManualMode'                     ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val577(ID,_T, 'AutomaticPostingMode'           ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val577(ID,_T, 'AutomaticGiveUpMode'            ) -> encode_tagval(ID, <<"10">>);
encode_fld_val577(ID,_T, 'QualifiedServiceRepresentative' ) -> encode_tagval(ID, <<"11">>);
encode_fld_val577(ID,_T, 'CustomerTrade'                  ) -> encode_tagval(ID, <<"12">>);
encode_fld_val577(ID,_T, 'SelfClearing'                   ) -> encode_tagval(ID, <<"13">>);
encode_fld_val577(ID, T, V                                ) -> try_encode_val(ID, T, V).

decode_fld_val581(Val) ->
  case Val of
    <<"1">> -> 'AccountIsCarriedOnCustomerSideOfBooks'                     ; %% 0
    <<"2">> -> 'AccountIsCarriedOnNonCustomerSideOfBooks'                  ; %% 1
    <<"3">> -> 'HouseTrader'                                               ; %% 2
    <<"4">> -> 'FloorTrader'                                               ; %% 3
    <<"6">> -> 'AccountIsCarriedOnNonCustomerSideOfBooksAndIsCrossMargined'; %% 4
    <<"7">> -> 'AccountIsHouseTraderAndIsCrossMargined'                    ; %% 5
    <<"8">> -> 'JointBackofficeAccount'                                    ; %% 6
    _       -> Val
  end.

encode_fld_val581(ID,_T, 'AccountIsCarriedOnCustomerSideOfBooks'                     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val581(ID,_T, 'AccountIsCarriedOnNonCustomerSideOfBooks'                  ) -> encode_tagval(ID, <<"2">>);
encode_fld_val581(ID,_T, 'HouseTrader'                                               ) -> encode_tagval(ID, <<"3">>);
encode_fld_val581(ID,_T, 'FloorTrader'                                               ) -> encode_tagval(ID, <<"4">>);
encode_fld_val581(ID,_T, 'AccountIsCarriedOnNonCustomerSideOfBooksAndIsCrossMargined') -> encode_tagval(ID, <<"6">>);
encode_fld_val581(ID,_T, 'AccountIsHouseTraderAndIsCrossMargined'                    ) -> encode_tagval(ID, <<"7">>);
encode_fld_val581(ID,_T, 'JointBackofficeAccount'                                    ) -> encode_tagval(ID, <<"8">>);
encode_fld_val581(ID, T, V                                                           ) -> try_encode_val(ID, T, V).

decode_fld_val582(Val) ->
  case Val of
    <<"1">> -> 'MemberTradingForTheirOwnAccount'            ; %% 0
    <<"2">> -> 'ClearingFirmTradingForItsProprietaryAccount'; %% 1
    <<"3">> -> 'MemberTradingForAnotherMember'              ; %% 2
    <<"4">> -> 'AllOther'                                   ; %% 3
    _       -> Val
  end.

encode_fld_val582(ID,_T, 'MemberTradingForTheirOwnAccount'            ) -> encode_tagval(ID, <<"1">>);
encode_fld_val582(ID,_T, 'ClearingFirmTradingForItsProprietaryAccount') -> encode_tagval(ID, <<"2">>);
encode_fld_val582(ID,_T, 'MemberTradingForAnotherMember'              ) -> encode_tagval(ID, <<"3">>);
encode_fld_val582(ID,_T, 'AllOther'                                   ) -> encode_tagval(ID, <<"4">>);
encode_fld_val582(ID, T, V                                            ) -> try_encode_val(ID, T, V).

decode_fld_val585(Val) ->
  case Val of
    <<"1">> -> 'StatusForOrdersForASecurity'           ; %% 0
    <<"2">> -> 'StatusForOrdersForAnUnderlyingSecurity'; %% 1
    <<"3">> -> 'StatusForOrdersForAProduct'            ; %% 2
    <<"4">> -> 'StatusForOrdersForACficode'            ; %% 3
    <<"5">> -> 'StatusForOrdersForASecuritytype'       ; %% 4
    <<"6">> -> 'StatusForOrdersForATradingSession'     ; %% 5
    <<"7">> -> 'StatusForAllOrders'                    ; %% 6
    <<"8">> -> 'StatusForOrdersForAPartyid'            ; %% 7
    _       -> Val
  end.

encode_fld_val585(ID,_T, 'StatusForOrdersForASecurity'           ) -> encode_tagval(ID, <<"1">>);
encode_fld_val585(ID,_T, 'StatusForOrdersForAnUnderlyingSecurity') -> encode_tagval(ID, <<"2">>);
encode_fld_val585(ID,_T, 'StatusForOrdersForAProduct'            ) -> encode_tagval(ID, <<"3">>);
encode_fld_val585(ID,_T, 'StatusForOrdersForACficode'            ) -> encode_tagval(ID, <<"4">>);
encode_fld_val585(ID,_T, 'StatusForOrdersForASecuritytype'       ) -> encode_tagval(ID, <<"5">>);
encode_fld_val585(ID,_T, 'StatusForOrdersForATradingSession'     ) -> encode_tagval(ID, <<"6">>);
encode_fld_val585(ID,_T, 'StatusForAllOrders'                    ) -> encode_tagval(ID, <<"7">>);
encode_fld_val585(ID,_T, 'StatusForOrdersForAPartyid'            ) -> encode_tagval(ID, <<"8">>);
encode_fld_val585(ID, T, V                                       ) -> try_encode_val(ID, T, V).

decode_fld_val589(Val) ->
  case Val of
    <<"0">> -> 'CanTriggerBookingWithoutReferenceToTheOrderInitiator'; %% 0
    <<"1">> -> 'SpeakWithOrderInitiatorBeforeBooking'                ; %% 1
    <<"2">> -> 'Accumulate'                                          ; %% 2
    _       -> Val
  end.

encode_fld_val589(ID,_T, 'CanTriggerBookingWithoutReferenceToTheOrderInitiator') -> encode_tagval(ID, <<"0">>);
encode_fld_val589(ID,_T, 'SpeakWithOrderInitiatorBeforeBooking'                ) -> encode_tagval(ID, <<"1">>);
encode_fld_val589(ID,_T, 'Accumulate'                                          ) -> encode_tagval(ID, <<"2">>);
encode_fld_val589(ID, T, V                                                     ) -> try_encode_val(ID, T, V).

decode_fld_val590(Val) ->
  case Val of
    <<"0">> -> 'EachPartialExecutionIsABookableUnit'                         ; %% 0
    <<"1">> -> 'AggregatePartialExecutionsOnThisOrderAndBookOneTradePerOrder'; %% 1
    <<"2">> -> 'AggregateExecutionsForThisSymbolSideAndSettlementDate'       ; %% 2
    _       -> Val
  end.

encode_fld_val590(ID,_T, 'EachPartialExecutionIsABookableUnit'                         ) -> encode_tagval(ID, <<"0">>);
encode_fld_val590(ID,_T, 'AggregatePartialExecutionsOnThisOrderAndBookOneTradePerOrder') -> encode_tagval(ID, <<"1">>);
encode_fld_val590(ID,_T, 'AggregateExecutionsForThisSymbolSideAndSettlementDate'       ) -> encode_tagval(ID, <<"2">>);
encode_fld_val590(ID, T, V                                                             ) -> try_encode_val(ID, T, V).

decode_fld_val591(Val) ->
  case Val of
    <<"0">> -> 'ProRata'                 ; %% 0
    <<"1">> -> 'DoNotProRataDiscussFirst'; %% 1
    _       -> Val
  end.

encode_fld_val591(ID,_T, 'ProRata'                 ) -> encode_tagval(ID, <<"0">>);
encode_fld_val591(ID,_T, 'DoNotProRataDiscussFirst') -> encode_tagval(ID, <<"1">>);
encode_fld_val591(ID, T, V                         ) -> try_encode_val(ID, T, V).

decode_fld_val626(Val) ->
  case Val of
    <<"1">> -> 'Calculated'           ; %% 0
    <<"2">> -> 'Preliminary'          ; %% 1
    <<"5">> -> 'ReadyToBook'          ; %% 2
    <<"7">> -> 'WarehouseInstruction' ; %% 3
    <<"8">> -> 'RequestToIntermediary'; %% 4
    _       -> Val
  end.

encode_fld_val626(ID,_T, 'Calculated'           ) -> encode_tagval(ID, <<"1">>);
encode_fld_val626(ID,_T, 'Preliminary'          ) -> encode_tagval(ID, <<"2">>);
encode_fld_val626(ID,_T, 'ReadyToBook'          ) -> encode_tagval(ID, <<"5">>);
encode_fld_val626(ID,_T, 'WarehouseInstruction' ) -> encode_tagval(ID, <<"7">>);
encode_fld_val626(ID,_T, 'RequestToIntermediary') -> encode_tagval(ID, <<"8">>);
encode_fld_val626(ID, T, V                      ) -> try_encode_val(ID, T, V).

decode_fld_val635(Val) ->
  case Val of
    <<"B">> -> 'CboeMember'                                                 ; %% 0
    <<"C">> -> 'NonMemberAndCustomer'                                       ; %% 1
    <<"E">> -> 'EquityMemberAndClearingMember'                              ; %% 2
    <<"F">> -> 'FullAndAssociateMemberTradingForOwnAccountAndAsFloorBrokers'; %% 3
    <<"H">> -> '106hAnd106jFirms'                                           ; %% 4
    <<"I">> -> 'GimIdemAndComMembershipInterestHolders'                     ; %% 5
    <<"L">> -> 'LesseeAnd106fEmployees'                                     ; %% 6
    <<"M">> -> 'AllOtherOwnershipTypes'                                     ; %% 7
    <<"1">> -> '1stYearDelegateTradingForHisOwnAccount'                     ; %% 8
    <<"2">> -> '2ndYearDelegateTradingForHisOwnAccount'                     ; %% 9
    <<"3">> -> '3rdYearDelegateTradingForHisOwnAccount'                     ; %% 10
    <<"4">> -> '4thYearDelegateTradingForHisOwnAccount'                     ; %% 11
    <<"5">> -> '5thYearDelegateTradingForHisOwnAccount'                     ; %% 12
    <<"9">> -> '6thYearAndBeyondDelegateTradingForHisOwnAccount'            ; %% 13
    _       -> Val
  end.

encode_fld_val635(ID,_T, 'CboeMember'                                                 ) -> encode_tagval(ID, <<"B">>);
encode_fld_val635(ID,_T, 'NonMemberAndCustomer'                                       ) -> encode_tagval(ID, <<"C">>);
encode_fld_val635(ID,_T, 'EquityMemberAndClearingMember'                              ) -> encode_tagval(ID, <<"E">>);
encode_fld_val635(ID,_T, 'FullAndAssociateMemberTradingForOwnAccountAndAsFloorBrokers') -> encode_tagval(ID, <<"F">>);
encode_fld_val635(ID,_T, '106hAnd106jFirms'                                           ) -> encode_tagval(ID, <<"H">>);
encode_fld_val635(ID,_T, 'GimIdemAndComMembershipInterestHolders'                     ) -> encode_tagval(ID, <<"I">>);
encode_fld_val635(ID,_T, 'LesseeAnd106fEmployees'                                     ) -> encode_tagval(ID, <<"L">>);
encode_fld_val635(ID,_T, 'AllOtherOwnershipTypes'                                     ) -> encode_tagval(ID, <<"M">>);
encode_fld_val635(ID,_T, '1stYearDelegateTradingForHisOwnAccount'                     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val635(ID,_T, '2ndYearDelegateTradingForHisOwnAccount'                     ) -> encode_tagval(ID, <<"2">>);
encode_fld_val635(ID,_T, '3rdYearDelegateTradingForHisOwnAccount'                     ) -> encode_tagval(ID, <<"3">>);
encode_fld_val635(ID,_T, '4thYearDelegateTradingForHisOwnAccount'                     ) -> encode_tagval(ID, <<"4">>);
encode_fld_val635(ID,_T, '5thYearDelegateTradingForHisOwnAccount'                     ) -> encode_tagval(ID, <<"5">>);
encode_fld_val635(ID,_T, '6thYearAndBeyondDelegateTradingForHisOwnAccount'            ) -> encode_tagval(ID, <<"9">>);
encode_fld_val635(ID, T, V                                                            ) -> try_encode_val(ID, T, V).

decode_fld_val636(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val636(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val636(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val636(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val638(Val) ->
  case Val of
    <<"0">> -> 'PriorityUnchanged'                ; %% 0
    <<"1">> -> 'LostPriorityAsResultOfOrderChange'; %% 1
    _       -> Val
  end.

encode_fld_val638(ID,_T, 'PriorityUnchanged'                ) -> encode_tagval(ID, <<"0">>);
encode_fld_val638(ID,_T, 'LostPriorityAsResultOfOrderChange') -> encode_tagval(ID, <<"1">>);
encode_fld_val638(ID, T, V                                  ) -> try_encode_val(ID, T, V).

decode_fld_val650(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val650(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val650(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val650(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val658(Val) ->
  case Val of
    <<"1" >> -> 'UnknownSymbol'              ; %% 0
    <<"2" >> -> 'Exchange'                   ; %% 1
    <<"3" >> -> 'QuoteRequestExceedsLimit'   ; %% 2
    <<"4" >> -> 'TooLateToEnter'             ; %% 3
    <<"5" >> -> 'InvalidPrice'               ; %% 4
    <<"6" >> -> 'NotAuthorizedToRequestQuote'; %% 5
    <<"7" >> -> 'NoMatchForInquiry'          ; %% 6
    <<"8" >> -> 'NoMarketForInstrument'      ; %% 7
    <<"9" >> -> 'NoInventory'                ; %% 8
    <<"10">> -> 'Pass'                       ; %% 9
    <<"99">> -> 'Other'                      ; %% 10
    _        -> Val
  end.

encode_fld_val658(ID,_T, 'UnknownSymbol'              ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val658(ID,_T, 'Exchange'                   ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val658(ID,_T, 'QuoteRequestExceedsLimit'   ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val658(ID,_T, 'TooLateToEnter'             ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val658(ID,_T, 'InvalidPrice'               ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val658(ID,_T, 'NotAuthorizedToRequestQuote') -> encode_tagval(ID, <<"6" >>);
encode_fld_val658(ID,_T, 'NoMatchForInquiry'          ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val658(ID,_T, 'NoMarketForInstrument'      ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val658(ID,_T, 'NoInventory'                ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val658(ID,_T, 'Pass'                       ) -> encode_tagval(ID, <<"10">>);
encode_fld_val658(ID,_T, 'Other'                      ) -> encode_tagval(ID, <<"99">>);
encode_fld_val658(ID, T, V                            ) -> try_encode_val(ID, T, V).

decode_fld_val660(Val) ->
  case Val of
    <<"1" >> -> 'Bic'     ; %% 0
    <<"2" >> -> 'SidCode' ; %% 1
    <<"3" >> -> 'Tfm'     ; %% 2
    <<"4" >> -> 'Omgeo'   ; %% 3
    <<"5" >> -> 'DtccCode'; %% 4
    <<"99">> -> 'Other'   ; %% 5
    _        -> Val
  end.

encode_fld_val660(ID,_T, 'Bic'     ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val660(ID,_T, 'SidCode' ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val660(ID,_T, 'Tfm'     ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val660(ID,_T, 'Omgeo'   ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val660(ID,_T, 'DtccCode') -> encode_tagval(ID, <<"5" >>);
encode_fld_val660(ID,_T, 'Other'   ) -> encode_tagval(ID, <<"99">>);
encode_fld_val660(ID, T, V         ) -> try_encode_val(ID, T, V).

decode_fld_val665(Val) ->
  case Val of
    <<"1">> -> 'Received'                     ; %% 0
    <<"2">> -> 'MismatchedAccount'            ; %% 1
    <<"3">> -> 'MissingSettlementInstructions'; %% 2
    <<"4">> -> 'Confirmed'                    ; %% 3
    <<"5">> -> 'RequestRejected'              ; %% 4
    _       -> Val
  end.

encode_fld_val665(ID,_T, 'Received'                     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val665(ID,_T, 'MismatchedAccount'            ) -> encode_tagval(ID, <<"2">>);
encode_fld_val665(ID,_T, 'MissingSettlementInstructions') -> encode_tagval(ID, <<"3">>);
encode_fld_val665(ID,_T, 'Confirmed'                    ) -> encode_tagval(ID, <<"4">>);
encode_fld_val665(ID,_T, 'RequestRejected'              ) -> encode_tagval(ID, <<"5">>);
encode_fld_val665(ID, T, V                              ) -> try_encode_val(ID, T, V).

decode_fld_val666(Val) ->
  case Val of
    <<"0">> -> 'New'    ; %% 0
    <<"1">> -> 'Replace'; %% 1
    <<"2">> -> 'Cancel' ; %% 2
    _       -> Val
  end.

encode_fld_val666(ID,_T, 'New'    ) -> encode_tagval(ID, <<"0">>);
encode_fld_val666(ID,_T, 'Replace') -> encode_tagval(ID, <<"1">>);
encode_fld_val666(ID,_T, 'Cancel' ) -> encode_tagval(ID, <<"2">>);
encode_fld_val666(ID, T, V        ) -> try_encode_val(ID, T, V).

decode_fld_val668(Val) ->
  case Val of
    <<"1">> -> 'Bookentry'; %% 0
    <<"2">> -> 'Bearer'   ; %% 1
    _       -> Val
  end.

encode_fld_val668(ID,_T, 'Bookentry') -> encode_tagval(ID, <<"1">>);
encode_fld_val668(ID,_T, 'Bearer'   ) -> encode_tagval(ID, <<"2">>);
encode_fld_val668(ID, T, V          ) -> try_encode_val(ID, T, V).

decode_fld_val690(Val) ->
  case Val of
    <<"1">> -> 'ParForPar'       ; %% 0
    <<"2">> -> 'ModifiedDuration'; %% 1
    <<"4">> -> 'Risk'            ; %% 2
    <<"5">> -> 'Proceeds'        ; %% 3
    _       -> Val
  end.

encode_fld_val690(ID,_T, 'ParForPar'       ) -> encode_tagval(ID, <<"1">>);
encode_fld_val690(ID,_T, 'ModifiedDuration') -> encode_tagval(ID, <<"2">>);
encode_fld_val690(ID,_T, 'Risk'            ) -> encode_tagval(ID, <<"4">>);
encode_fld_val690(ID,_T, 'Proceeds'        ) -> encode_tagval(ID, <<"5">>);
encode_fld_val690(ID, T, V                 ) -> try_encode_val(ID, T, V).

decode_fld_val692(Val) ->
  case Val of
    <<"1" >> -> 'Percent'                         ; %% 0
    <<"2" >> -> 'PerShare'                        ; %% 1
    <<"3" >> -> 'FixedAmount'                     ; %% 2
    <<"4" >> -> 'DiscountPercentagePointsBelowPar'; %% 3
    <<"5" >> -> 'PremiumPercentagePointsOverPar'  ; %% 4
    <<"6" >> -> 'BasisPointsRelativeToBenchmark'  ; %% 5
    <<"7" >> -> 'TedPrice'                        ; %% 6
    <<"8" >> -> 'TedYield'                        ; %% 7
    <<"9" >> -> 'YieldSpread'                     ; %% 8
    <<"10">> -> 'Yield'                           ; %% 9
    _        -> Val
  end.

encode_fld_val692(ID,_T, 'Percent'                         ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val692(ID,_T, 'PerShare'                        ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val692(ID,_T, 'FixedAmount'                     ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val692(ID,_T, 'DiscountPercentagePointsBelowPar') -> encode_tagval(ID, <<"4" >>);
encode_fld_val692(ID,_T, 'PremiumPercentagePointsOverPar'  ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val692(ID,_T, 'BasisPointsRelativeToBenchmark'  ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val692(ID,_T, 'TedPrice'                        ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val692(ID,_T, 'TedYield'                        ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val692(ID,_T, 'YieldSpread'                     ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val692(ID,_T, 'Yield'                           ) -> encode_tagval(ID, <<"10">>);
encode_fld_val692(ID, T, V                                 ) -> try_encode_val(ID, T, V).

decode_fld_val694(Val) ->
  case Val of
    <<"1">> -> 'HitLift' ; %% 0
    <<"2">> -> 'Counter' ; %% 1
    <<"3">> -> 'Expired' ; %% 2
    <<"4">> -> 'Cover'   ; %% 3
    <<"5">> -> 'DoneAway'; %% 4
    <<"6">> -> 'Pass'    ; %% 5
    _       -> Val
  end.

encode_fld_val694(ID,_T, 'HitLift' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val694(ID,_T, 'Counter' ) -> encode_tagval(ID, <<"2">>);
encode_fld_val694(ID,_T, 'Expired' ) -> encode_tagval(ID, <<"3">>);
encode_fld_val694(ID,_T, 'Cover'   ) -> encode_tagval(ID, <<"4">>);
encode_fld_val694(ID,_T, 'DoneAway') -> encode_tagval(ID, <<"5">>);
encode_fld_val694(ID,_T, 'Pass'    ) -> encode_tagval(ID, <<"6">>);
encode_fld_val694(ID, T, V         ) -> try_encode_val(ID, T, V).

decode_fld_val703(Val) ->
  case Val of
    <<"TQ" >> -> 'TransactionQuantity'      ; %% 0
    <<"IAS">> -> 'IntraSpreadQty'           ; %% 1
    <<"IES">> -> 'InterSpreadQty'           ; %% 2
    <<"FIN">> -> 'EndOfDayQty'              ; %% 3
    <<"SOD">> -> 'StartOfDayQty'            ; %% 4
    <<"EX" >> -> 'OptionExerciseQty'        ; %% 5
    <<"AS" >> -> 'OptionAssignment'         ; %% 6
    <<"TX" >> -> 'TransactionFromExercise'  ; %% 7
    <<"TA" >> -> 'TransactionFromAssignment'; %% 8
    <<"PIT">> -> 'PitTradeQty'              ; %% 9
    <<"TRF">> -> 'TransferTradeQty'         ; %% 10
    <<"ETR">> -> 'ElectronicTradeQty'       ; %% 11
    <<"ALC">> -> 'AllocationTradeQty'       ; %% 12
    <<"PA" >> -> 'AdjustmentQty'            ; %% 13
    <<"ASF">> -> 'AsOfTradeQty'             ; %% 14
    <<"DLV">> -> 'DeliveryQty'              ; %% 15
    <<"TOT">> -> 'TotalTransactionQty'      ; %% 16
    <<"XM" >> -> 'CrossMarginQty'           ; %% 17
    <<"SPL">> -> 'IntegralSplit'            ; %% 18
    _         -> Val
  end.

encode_fld_val703(ID,_T, 'TransactionQuantity'      ) -> encode_tagval(ID, <<"TQ" >>);
encode_fld_val703(ID,_T, 'IntraSpreadQty'           ) -> encode_tagval(ID, <<"IAS">>);
encode_fld_val703(ID,_T, 'InterSpreadQty'           ) -> encode_tagval(ID, <<"IES">>);
encode_fld_val703(ID,_T, 'EndOfDayQty'              ) -> encode_tagval(ID, <<"FIN">>);
encode_fld_val703(ID,_T, 'StartOfDayQty'            ) -> encode_tagval(ID, <<"SOD">>);
encode_fld_val703(ID,_T, 'OptionExerciseQty'        ) -> encode_tagval(ID, <<"EX" >>);
encode_fld_val703(ID,_T, 'OptionAssignment'         ) -> encode_tagval(ID, <<"AS" >>);
encode_fld_val703(ID,_T, 'TransactionFromExercise'  ) -> encode_tagval(ID, <<"TX" >>);
encode_fld_val703(ID,_T, 'TransactionFromAssignment') -> encode_tagval(ID, <<"TA" >>);
encode_fld_val703(ID,_T, 'PitTradeQty'              ) -> encode_tagval(ID, <<"PIT">>);
encode_fld_val703(ID,_T, 'TransferTradeQty'         ) -> encode_tagval(ID, <<"TRF">>);
encode_fld_val703(ID,_T, 'ElectronicTradeQty'       ) -> encode_tagval(ID, <<"ETR">>);
encode_fld_val703(ID,_T, 'AllocationTradeQty'       ) -> encode_tagval(ID, <<"ALC">>);
encode_fld_val703(ID,_T, 'AdjustmentQty'            ) -> encode_tagval(ID, <<"PA" >>);
encode_fld_val703(ID,_T, 'AsOfTradeQty'             ) -> encode_tagval(ID, <<"ASF">>);
encode_fld_val703(ID,_T, 'DeliveryQty'              ) -> encode_tagval(ID, <<"DLV">>);
encode_fld_val703(ID,_T, 'TotalTransactionQty'      ) -> encode_tagval(ID, <<"TOT">>);
encode_fld_val703(ID,_T, 'CrossMarginQty'           ) -> encode_tagval(ID, <<"XM" >>);
encode_fld_val703(ID,_T, 'IntegralSplit'            ) -> encode_tagval(ID, <<"SPL">>);
encode_fld_val703(ID, T, V                          ) -> try_encode_val(ID, T, V).

decode_fld_val706(Val) ->
  case Val of
    <<"0">> -> 'Submitted'; %% 0
    <<"1">> -> 'Accepted' ; %% 1
    <<"2">> -> 'Rejected' ; %% 2
    _       -> Val
  end.

encode_fld_val706(ID,_T, 'Submitted') -> encode_tagval(ID, <<"0">>);
encode_fld_val706(ID,_T, 'Accepted' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val706(ID,_T, 'Rejected' ) -> encode_tagval(ID, <<"2">>);
encode_fld_val706(ID, T, V          ) -> try_encode_val(ID, T, V).

decode_fld_val707(Val) ->
  case Val of
    <<"FMTM">> -> 'FinalMarkToMarketAmount'      ; %% 0
    <<"IMTM">> -> 'IncrementalMarkToMarketAmount'; %% 1
    <<"TVAR">> -> 'TradeVariationAmount'         ; %% 2
    <<"SMTM">> -> 'StartOfDayMarkToMarketAmount' ; %% 3
    <<"PREM">> -> 'PremiumAmount'                ; %% 4
    <<"CRES">> -> 'CashResidualAmount'           ; %% 5
    <<"CASH">> -> 'CashAmount'                   ; %% 6
    <<"VADJ">> -> 'ValueAdjustedAmount'          ; %% 7
    _          -> Val
  end.

encode_fld_val707(ID,_T, 'FinalMarkToMarketAmount'      ) -> encode_tagval(ID, <<"FMTM">>);
encode_fld_val707(ID,_T, 'IncrementalMarkToMarketAmount') -> encode_tagval(ID, <<"IMTM">>);
encode_fld_val707(ID,_T, 'TradeVariationAmount'         ) -> encode_tagval(ID, <<"TVAR">>);
encode_fld_val707(ID,_T, 'StartOfDayMarkToMarketAmount' ) -> encode_tagval(ID, <<"SMTM">>);
encode_fld_val707(ID,_T, 'PremiumAmount'                ) -> encode_tagval(ID, <<"PREM">>);
encode_fld_val707(ID,_T, 'CashResidualAmount'           ) -> encode_tagval(ID, <<"CRES">>);
encode_fld_val707(ID,_T, 'CashAmount'                   ) -> encode_tagval(ID, <<"CASH">>);
encode_fld_val707(ID,_T, 'ValueAdjustedAmount'          ) -> encode_tagval(ID, <<"VADJ">>);
encode_fld_val707(ID, T, V                              ) -> try_encode_val(ID, T, V).

decode_fld_val709(Val) ->
  case Val of
    <<"1">> -> 'Exercise'                                 ; %% 0
    <<"2">> -> 'DoNotExercise'                            ; %% 1
    <<"3">> -> 'PositionAdjustment'                       ; %% 2
    <<"4">> -> 'PositionChangeSubmissionMarginDisposition'; %% 3
    <<"5">> -> 'Pledge'                                   ; %% 4
    _       -> Val
  end.

encode_fld_val709(ID,_T, 'Exercise'                                 ) -> encode_tagval(ID, <<"1">>);
encode_fld_val709(ID,_T, 'DoNotExercise'                            ) -> encode_tagval(ID, <<"2">>);
encode_fld_val709(ID,_T, 'PositionAdjustment'                       ) -> encode_tagval(ID, <<"3">>);
encode_fld_val709(ID,_T, 'PositionChangeSubmissionMarginDisposition') -> encode_tagval(ID, <<"4">>);
encode_fld_val709(ID,_T, 'Pledge'                                   ) -> encode_tagval(ID, <<"5">>);
encode_fld_val709(ID, T, V                                          ) -> try_encode_val(ID, T, V).

decode_fld_val712(Val) ->
  case Val of
    <<"1">> -> 'NewUsedToIncrementTheOverallTransactionQuantity'                                             ; %% 0
    <<"2">> -> 'ReplaceUsedToOverrideTheOverallTransactionQuantityOrSpecificAddMessagesBasedOnTheReferenceId'; %% 1
    <<"3">> -> 'CancelUsedToRemoveTheOverallTransactionOrSpecificAddMessagesBasedOnReferenceId'              ; %% 2
    _       -> Val
  end.

encode_fld_val712(ID,_T, 'NewUsedToIncrementTheOverallTransactionQuantity'                                             ) -> encode_tagval(ID, <<"1">>);
encode_fld_val712(ID,_T, 'ReplaceUsedToOverrideTheOverallTransactionQuantityOrSpecificAddMessagesBasedOnTheReferenceId') -> encode_tagval(ID, <<"2">>);
encode_fld_val712(ID,_T, 'CancelUsedToRemoveTheOverallTransactionOrSpecificAddMessagesBasedOnReferenceId'              ) -> encode_tagval(ID, <<"3">>);
encode_fld_val712(ID, T, V                                                                                             ) -> try_encode_val(ID, T, V).

decode_fld_val716(Val) ->
  case Val of
    <<"ITD">> -> 'Intraday'              ; %% 0
    <<"RTH">> -> 'RegularTradingHours'   ; %% 1
    <<"ETH">> -> 'ElectronicTradingHours'; %% 2
    _         -> Val
  end.

encode_fld_val716(ID,_T, 'Intraday'              ) -> encode_tagval(ID, <<"ITD">>);
encode_fld_val716(ID,_T, 'RegularTradingHours'   ) -> encode_tagval(ID, <<"RTH">>);
encode_fld_val716(ID,_T, 'ElectronicTradingHours') -> encode_tagval(ID, <<"ETH">>);
encode_fld_val716(ID, T, V                       ) -> try_encode_val(ID, T, V).

decode_fld_val718(Val) ->
  case Val of
    <<"0">> -> 'ProcessRequestAsMarginDisposition'; %% 0
    <<"1">> -> 'DeltaPlus'                        ; %% 1
    <<"2">> -> 'DeltaMinus'                       ; %% 2
    <<"3">> -> 'Final'                            ; %% 3
    _       -> Val
  end.

encode_fld_val718(ID,_T, 'ProcessRequestAsMarginDisposition') -> encode_tagval(ID, <<"0">>);
encode_fld_val718(ID,_T, 'DeltaPlus'                        ) -> encode_tagval(ID, <<"1">>);
encode_fld_val718(ID,_T, 'DeltaMinus'                       ) -> encode_tagval(ID, <<"2">>);
encode_fld_val718(ID,_T, 'Final'                            ) -> encode_tagval(ID, <<"3">>);
encode_fld_val718(ID, T, V                                  ) -> try_encode_val(ID, T, V).

decode_fld_val722(Val) ->
  case Val of
    <<"0">> -> 'Accepted'             ; %% 0
    <<"1">> -> 'AcceptedWithWarnings' ; %% 1
    <<"2">> -> 'Rejected'             ; %% 2
    <<"3">> -> 'Completed'            ; %% 3
    <<"4">> -> 'CompletedWithWarnings'; %% 4
    _       -> Val
  end.

encode_fld_val722(ID,_T, 'Accepted'             ) -> encode_tagval(ID, <<"0">>);
encode_fld_val722(ID,_T, 'AcceptedWithWarnings' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val722(ID,_T, 'Rejected'             ) -> encode_tagval(ID, <<"2">>);
encode_fld_val722(ID,_T, 'Completed'            ) -> encode_tagval(ID, <<"3">>);
encode_fld_val722(ID,_T, 'CompletedWithWarnings') -> encode_tagval(ID, <<"4">>);
encode_fld_val722(ID, T, V                      ) -> try_encode_val(ID, T, V).

decode_fld_val723(Val) ->
  case Val of
    <<"0" >> -> 'SuccessfulCompletion'; %% 0
    <<"1" >> -> 'Rejected'            ; %% 1
    <<"99">> -> 'Other'               ; %% 2
    _        -> Val
  end.

encode_fld_val723(ID,_T, 'SuccessfulCompletion') -> encode_tagval(ID, <<"0" >>);
encode_fld_val723(ID,_T, 'Rejected'            ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val723(ID,_T, 'Other'               ) -> encode_tagval(ID, <<"99">>);
encode_fld_val723(ID, T, V                     ) -> try_encode_val(ID, T, V).

decode_fld_val724(Val) ->
  case Val of
    <<"0">> -> 'Positions'  ; %% 0
    <<"1">> -> 'Trades'     ; %% 1
    <<"2">> -> 'Exercises'  ; %% 2
    <<"3">> -> 'Assignments'; %% 3
    _       -> Val
  end.

encode_fld_val724(ID,_T, 'Positions'  ) -> encode_tagval(ID, <<"0">>);
encode_fld_val724(ID,_T, 'Trades'     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val724(ID,_T, 'Exercises'  ) -> encode_tagval(ID, <<"2">>);
encode_fld_val724(ID,_T, 'Assignments') -> encode_tagval(ID, <<"3">>);
encode_fld_val724(ID, T, V            ) -> try_encode_val(ID, T, V).

decode_fld_val725(Val) ->
  case Val of
    <<"0">> -> 'InbandTransportTheRequestWasSentOver'          ; %% 0
    <<"1">> -> 'OutOfBandPreArrangedOutOfBandDeliveryMechanism'; %% 1
    _       -> Val
  end.

encode_fld_val725(ID,_T, 'InbandTransportTheRequestWasSentOver'          ) -> encode_tagval(ID, <<"0">>);
encode_fld_val725(ID,_T, 'OutOfBandPreArrangedOutOfBandDeliveryMechanism') -> encode_tagval(ID, <<"1">>);
encode_fld_val725(ID, T, V                                               ) -> try_encode_val(ID, T, V).

decode_fld_val728(Val) ->
  case Val of
    <<"0" >> -> 'ValidRequest'                     ; %% 0
    <<"1" >> -> 'InvalidOrUnsupportedRequest'      ; %% 1
    <<"2" >> -> 'NoPositionsFoundThatMatchCriteria'; %% 2
    <<"3" >> -> 'NotAuthorizedToRequestPositions'  ; %% 3
    <<"4" >> -> 'RequestForPositionNotSupported'   ; %% 4
    <<"99">> -> 'Other'                            ; %% 5
    _        -> Val
  end.

encode_fld_val728(ID,_T, 'ValidRequest'                     ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val728(ID,_T, 'InvalidOrUnsupportedRequest'      ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val728(ID,_T, 'NoPositionsFoundThatMatchCriteria') -> encode_tagval(ID, <<"2" >>);
encode_fld_val728(ID,_T, 'NotAuthorizedToRequestPositions'  ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val728(ID,_T, 'RequestForPositionNotSupported'   ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val728(ID,_T, 'Other'                            ) -> encode_tagval(ID, <<"99">>);
encode_fld_val728(ID, T, V                                  ) -> try_encode_val(ID, T, V).

decode_fld_val729(Val) ->
  case Val of
    <<"0">> -> 'Completed'            ; %% 0
    <<"1">> -> 'CompletedWithWarnings'; %% 1
    <<"2">> -> 'Rejected'             ; %% 2
    _       -> Val
  end.

encode_fld_val729(ID,_T, 'Completed'            ) -> encode_tagval(ID, <<"0">>);
encode_fld_val729(ID,_T, 'CompletedWithWarnings') -> encode_tagval(ID, <<"1">>);
encode_fld_val729(ID,_T, 'Rejected'             ) -> encode_tagval(ID, <<"2">>);
encode_fld_val729(ID, T, V                      ) -> try_encode_val(ID, T, V).

decode_fld_val731(Val) ->
  case Val of
    <<"1">> -> 'Final'      ; %% 0
    <<"2">> -> 'Theoretical'; %% 1
    _       -> Val
  end.

encode_fld_val731(ID,_T, 'Final'      ) -> encode_tagval(ID, <<"1">>);
encode_fld_val731(ID,_T, 'Theoretical') -> encode_tagval(ID, <<"2">>);
encode_fld_val731(ID, T, V            ) -> try_encode_val(ID, T, V).

decode_fld_val744(Val) ->
  case Val of
    <<"R">> -> 'Random' ; %% 0
    <<"P">> -> 'Prorata'; %% 1
    _       -> Val
  end.

encode_fld_val744(ID,_T, 'Random' ) -> encode_tagval(ID, <<"R">>);
encode_fld_val744(ID,_T, 'Prorata') -> encode_tagval(ID, <<"P">>);
encode_fld_val744(ID, T, V        ) -> try_encode_val(ID, T, V).

decode_fld_val747(Val) ->
  case Val of
    <<"A">> -> 'Automatic'; %% 0
    <<"M">> -> 'Manual'   ; %% 1
    _       -> Val
  end.

encode_fld_val747(ID,_T, 'Automatic') -> encode_tagval(ID, <<"A">>);
encode_fld_val747(ID,_T, 'Manual'   ) -> encode_tagval(ID, <<"M">>);
encode_fld_val747(ID, T, V          ) -> try_encode_val(ID, T, V).

decode_fld_val749(Val) ->
  case Val of
    <<"0" >> -> 'Successful'                              ; %% 0
    <<"1" >> -> 'InvalidOrUnknownInstrument'              ; %% 1
    <<"2" >> -> 'InvalidTypeOfTradeRequested'             ; %% 2
    <<"3" >> -> 'InvalidParties'                          ; %% 3
    <<"4" >> -> 'InvalidTransportTypeRequested'           ; %% 4
    <<"5" >> -> 'InvalidDestinationRequested'             ; %% 5
    <<"8" >> -> 'TraderequesttypeNotSupported'            ; %% 6
    <<"9" >> -> 'UnauthorizedForTradeCaptureReportRequest'; %% 7
    <<"99">> -> 'Other'                                   ; %% 8
    _        -> Val
  end.

encode_fld_val749(ID,_T, 'Successful'                              ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val749(ID,_T, 'InvalidOrUnknownInstrument'              ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val749(ID,_T, 'InvalidTypeOfTradeRequested'             ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val749(ID,_T, 'InvalidParties'                          ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val749(ID,_T, 'InvalidTransportTypeRequested'           ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val749(ID,_T, 'InvalidDestinationRequested'             ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val749(ID,_T, 'TraderequesttypeNotSupported'            ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val749(ID,_T, 'UnauthorizedForTradeCaptureReportRequest') -> encode_tagval(ID, <<"9" >>);
encode_fld_val749(ID,_T, 'Other'                                   ) -> encode_tagval(ID, <<"99">>);
encode_fld_val749(ID, T, V                                         ) -> try_encode_val(ID, T, V).

decode_fld_val750(Val) ->
  case Val of
    <<"0">> -> 'Accepted' ; %% 0
    <<"1">> -> 'Completed'; %% 1
    <<"2">> -> 'Rejected' ; %% 2
    _       -> Val
  end.

encode_fld_val750(ID,_T, 'Accepted' ) -> encode_tagval(ID, <<"0">>);
encode_fld_val750(ID,_T, 'Completed') -> encode_tagval(ID, <<"1">>);
encode_fld_val750(ID,_T, 'Rejected' ) -> encode_tagval(ID, <<"2">>);
encode_fld_val750(ID, T, V          ) -> try_encode_val(ID, T, V).

decode_fld_val751(Val) ->
  case Val of
    <<"0" >> -> 'Successful'                ; %% 0
    <<"1" >> -> 'InvalidPartyInformation'   ; %% 1
    <<"2" >> -> 'UnknownInstrument'         ; %% 2
    <<"3" >> -> 'UnauthorizedToReportTrades'; %% 3
    <<"4" >> -> 'InvalidTradeType'          ; %% 4
    <<"99">> -> 'Other'                     ; %% 5
    _        -> Val
  end.

encode_fld_val751(ID,_T, 'Successful'                ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val751(ID,_T, 'InvalidPartyInformation'   ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val751(ID,_T, 'UnknownInstrument'         ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val751(ID,_T, 'UnauthorizedToReportTrades') -> encode_tagval(ID, <<"3" >>);
encode_fld_val751(ID,_T, 'InvalidTradeType'          ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val751(ID,_T, 'Other'                     ) -> encode_tagval(ID, <<"99">>);
encode_fld_val751(ID, T, V                           ) -> try_encode_val(ID, T, V).

decode_fld_val752(Val) ->
  case Val of
    <<"1">> -> 'SingleSecurity'                  ; %% 0
    <<"2">> -> 'IndividualLegOfAMultiLegSecurity'; %% 1
    <<"3">> -> 'MultiLegSecurity'                ; %% 2
    _       -> Val
  end.

encode_fld_val752(ID,_T, 'SingleSecurity'                  ) -> encode_tagval(ID, <<"1">>);
encode_fld_val752(ID,_T, 'IndividualLegOfAMultiLegSecurity') -> encode_tagval(ID, <<"2">>);
encode_fld_val752(ID,_T, 'MultiLegSecurity'                ) -> encode_tagval(ID, <<"3">>);
encode_fld_val752(ID, T, V                                 ) -> try_encode_val(ID, T, V).

decode_fld_val770(Val) ->
  case Val of
    <<"1">> -> 'ExecutionTime'  ; %% 0
    <<"2">> -> 'TimeIn'         ; %% 1
    <<"3">> -> 'TimeOut'        ; %% 2
    <<"4">> -> 'BrokerReceipt'  ; %% 3
    <<"5">> -> 'BrokerExecution'; %% 4
    _       -> Val
  end.

encode_fld_val770(ID,_T, 'ExecutionTime'  ) -> encode_tagval(ID, <<"1">>);
encode_fld_val770(ID,_T, 'TimeIn'         ) -> encode_tagval(ID, <<"2">>);
encode_fld_val770(ID,_T, 'TimeOut'        ) -> encode_tagval(ID, <<"3">>);
encode_fld_val770(ID,_T, 'BrokerReceipt'  ) -> encode_tagval(ID, <<"4">>);
encode_fld_val770(ID,_T, 'BrokerExecution') -> encode_tagval(ID, <<"5">>);
encode_fld_val770(ID, T, V                ) -> try_encode_val(ID, T, V).

decode_fld_val773(Val) ->
  case Val of
    <<"1">> -> 'Status'                     ; %% 0
    <<"2">> -> 'Confirmation'               ; %% 1
    <<"3">> -> 'ConfirmationRequestRejected'; %% 2
    _       -> Val
  end.

encode_fld_val773(ID,_T, 'Status'                     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val773(ID,_T, 'Confirmation'               ) -> encode_tagval(ID, <<"2">>);
encode_fld_val773(ID,_T, 'ConfirmationRequestRejected') -> encode_tagval(ID, <<"3">>);
encode_fld_val773(ID, T, V                            ) -> try_encode_val(ID, T, V).

decode_fld_val774(Val) ->
  case Val of
    <<"1" >> -> 'MismatchedAccount'            ; %% 0
    <<"2" >> -> 'MissingSettlementInstructions'; %% 1
    <<"99">> -> 'Other'                        ; %% 2
    _        -> Val
  end.

encode_fld_val774(ID,_T, 'MismatchedAccount'            ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val774(ID,_T, 'MissingSettlementInstructions') -> encode_tagval(ID, <<"2" >>);
encode_fld_val774(ID,_T, 'Other'                        ) -> encode_tagval(ID, <<"99">>);
encode_fld_val774(ID, T, V                              ) -> try_encode_val(ID, T, V).

decode_fld_val775(Val) ->
  case Val of
    <<"0">> -> 'RegularBooking' ; %% 0
    <<"1">> -> 'Cfd'            ; %% 1
    <<"2">> -> 'TotalReturnSwap'; %% 2
    _       -> Val
  end.

encode_fld_val775(ID,_T, 'RegularBooking' ) -> encode_tagval(ID, <<"0">>);
encode_fld_val775(ID,_T, 'Cfd'            ) -> encode_tagval(ID, <<"1">>);
encode_fld_val775(ID,_T, 'TotalReturnSwap') -> encode_tagval(ID, <<"2">>);
encode_fld_val775(ID, T, V                ) -> try_encode_val(ID, T, V).

decode_fld_val780(Val) ->
  case Val of
    <<"0">> -> 'UseDefaultInstructions'      ; %% 0
    <<"1">> -> 'DeriveFromParametersProvided'; %% 1
    <<"2">> -> 'FullDetailsProvided'         ; %% 2
    <<"3">> -> 'SsiDbIdsProvided'            ; %% 3
    <<"4">> -> 'PhoneForInstructions'        ; %% 4
    _       -> Val
  end.

encode_fld_val780(ID,_T, 'UseDefaultInstructions'      ) -> encode_tagval(ID, <<"0">>);
encode_fld_val780(ID,_T, 'DeriveFromParametersProvided') -> encode_tagval(ID, <<"1">>);
encode_fld_val780(ID,_T, 'FullDetailsProvided'         ) -> encode_tagval(ID, <<"2">>);
encode_fld_val780(ID,_T, 'SsiDbIdsProvided'            ) -> encode_tagval(ID, <<"3">>);
encode_fld_val780(ID,_T, 'PhoneForInstructions'        ) -> encode_tagval(ID, <<"4">>);
encode_fld_val780(ID, T, V                             ) -> try_encode_val(ID, T, V).

decode_fld_val787(Val) ->
  case Val of
    <<"S">> -> 'Securities'; %% 0
    <<"C">> -> 'Cash'      ; %% 1
    _       -> Val
  end.

encode_fld_val787(ID,_T, 'Securities') -> encode_tagval(ID, <<"S">>);
encode_fld_val787(ID,_T, 'Cash'      ) -> encode_tagval(ID, <<"C">>);
encode_fld_val787(ID, T, V           ) -> try_encode_val(ID, T, V).

decode_fld_val788(Val) ->
  case Val of
    <<"1">> -> 'Overnight'; %% 0
    <<"2">> -> 'Term'     ; %% 1
    <<"3">> -> 'Flexible' ; %% 2
    <<"4">> -> 'Open'     ; %% 3
    _       -> Val
  end.

encode_fld_val788(ID,_T, 'Overnight') -> encode_tagval(ID, <<"1">>);
encode_fld_val788(ID,_T, 'Term'     ) -> encode_tagval(ID, <<"2">>);
encode_fld_val788(ID,_T, 'Flexible' ) -> encode_tagval(ID, <<"3">>);
encode_fld_val788(ID,_T, 'Open'     ) -> encode_tagval(ID, <<"4">>);
encode_fld_val788(ID, T, V          ) -> try_encode_val(ID, T, V).

decode_fld_val792(Val) ->
  case Val of
    <<"0" >> -> 'UnableToProcessRequest'               ; %% 0
    <<"1" >> -> 'UnknownAccount'                       ; %% 1
    <<"2" >> -> 'NoMatchingSettlementInstructionsFound'; %% 2
    <<"99">> -> 'Other'                                ; %% 3
    _        -> Val
  end.

encode_fld_val792(ID,_T, 'UnableToProcessRequest'               ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val792(ID,_T, 'UnknownAccount'                       ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val792(ID,_T, 'NoMatchingSettlementInstructionsFound') -> encode_tagval(ID, <<"2" >>);
encode_fld_val792(ID,_T, 'Other'                                ) -> encode_tagval(ID, <<"99">>);
encode_fld_val792(ID, T, V                                      ) -> try_encode_val(ID, T, V).

decode_fld_val794(Val) ->
  case Val of
    <<"3">> -> 'SellsideCalculatedUsingPreliminary'  ; %% 0
    <<"4">> -> 'SellsideCalculatedWithoutPreliminary'; %% 1
    <<"5">> -> 'WarehouseRecap'                      ; %% 2
    <<"8">> -> 'RequestToIntermediary'               ; %% 3
    _       -> Val
  end.

encode_fld_val794(ID,_T, 'SellsideCalculatedUsingPreliminary'  ) -> encode_tagval(ID, <<"3">>);
encode_fld_val794(ID,_T, 'SellsideCalculatedWithoutPreliminary') -> encode_tagval(ID, <<"4">>);
encode_fld_val794(ID,_T, 'WarehouseRecap'                      ) -> encode_tagval(ID, <<"5">>);
encode_fld_val794(ID,_T, 'RequestToIntermediary'               ) -> encode_tagval(ID, <<"8">>);
encode_fld_val794(ID, T, V                                     ) -> try_encode_val(ID, T, V).

decode_fld_val796(Val) ->
  case Val of
    <<"1" >> -> 'OriginalDetailsIncompleteIncorrect'; %% 0
    <<"2" >> -> 'ChangeInUnderlyingOrderDetails'    ; %% 1
    <<"99">> -> 'Other'                             ; %% 2
    _        -> Val
  end.

encode_fld_val796(ID,_T, 'OriginalDetailsIncompleteIncorrect') -> encode_tagval(ID, <<"1" >>);
encode_fld_val796(ID,_T, 'ChangeInUnderlyingOrderDetails'    ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val796(ID,_T, 'Other'                             ) -> encode_tagval(ID, <<"99">>);
encode_fld_val796(ID, T, V                                   ) -> try_encode_val(ID, T, V).

decode_fld_val798(Val) ->
  case Val of
    <<"1">> -> 'AccountIsCarriedOnCustomerSideOfBooks'                     ; %% 0
    <<"2">> -> 'AccountIsCarriedOnNonCustomerSideOfBooks'                  ; %% 1
    <<"3">> -> 'HouseTrader'                                               ; %% 2
    <<"4">> -> 'FloorTrader'                                               ; %% 3
    <<"6">> -> 'AccountIsCarriedOnNonCustomerSideOfBooksAndIsCrossMargined'; %% 4
    <<"7">> -> 'AccountIsHouseTraderAndIsCrossMargined'                    ; %% 5
    <<"8">> -> 'JointBackofficeAccount'                                    ; %% 6
    _       -> Val
  end.

encode_fld_val798(ID,_T, 'AccountIsCarriedOnCustomerSideOfBooks'                     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val798(ID,_T, 'AccountIsCarriedOnNonCustomerSideOfBooks'                  ) -> encode_tagval(ID, <<"2">>);
encode_fld_val798(ID,_T, 'HouseTrader'                                               ) -> encode_tagval(ID, <<"3">>);
encode_fld_val798(ID,_T, 'FloorTrader'                                               ) -> encode_tagval(ID, <<"4">>);
encode_fld_val798(ID,_T, 'AccountIsCarriedOnNonCustomerSideOfBooksAndIsCrossMargined') -> encode_tagval(ID, <<"6">>);
encode_fld_val798(ID,_T, 'AccountIsHouseTraderAndIsCrossMargined'                    ) -> encode_tagval(ID, <<"7">>);
encode_fld_val798(ID,_T, 'JointBackofficeAccount'                                    ) -> encode_tagval(ID, <<"8">>);
encode_fld_val798(ID, T, V                                                           ) -> try_encode_val(ID, T, V).

decode_fld_val803(Val) ->
  case Val of
    <<"1" >> -> 'Firm'                    ; %% 0
    <<"2" >> -> 'Person'                  ; %% 1
    <<"3" >> -> 'System'                  ; %% 2
    <<"4" >> -> 'Application'             ; %% 3
    <<"5" >> -> 'FullLegalNameOfFirm'     ; %% 4
    <<"6" >> -> 'PostalAddress'           ; %% 5
    <<"7" >> -> 'PhoneNumber'             ; %% 6
    <<"8" >> -> 'EmailAddress'            ; %% 7
    <<"9" >> -> 'ContactName'             ; %% 8
    <<"10">> -> 'SecuritiesAccountNumber' ; %% 9
    <<"11">> -> 'RegistrationNumber'      ; %% 10
    <<"12">> -> 'RegisteredAddress12'     ; %% 11
    <<"13">> -> 'RegulatoryStatus'        ; %% 12
    <<"14">> -> 'RegistrationName'        ; %% 13
    <<"15">> -> 'CashAccountNumber'       ; %% 14
    <<"16">> -> 'Bic'                     ; %% 15
    <<"17">> -> 'CsdParticipantMemberCode'; %% 16
    <<"18">> -> 'RegisteredAddress18'     ; %% 17
    <<"19">> -> 'FundAccountName'         ; %% 18
    <<"20">> -> 'TelexNumber'             ; %% 19
    <<"21">> -> 'FaxNumber'               ; %% 20
    <<"22">> -> 'SecuritiesAccountName'   ; %% 21
    <<"23">> -> 'CashAccountName'         ; %% 22
    <<"24">> -> 'Department'              ; %% 23
    <<"25">> -> 'Location'                ; %% 24
    <<"26">> -> 'PositionAccountType'     ; %% 25
    _        -> Val
  end.

encode_fld_val803(ID,_T, 'Firm'                    ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val803(ID,_T, 'Person'                  ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val803(ID,_T, 'System'                  ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val803(ID,_T, 'Application'             ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val803(ID,_T, 'FullLegalNameOfFirm'     ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val803(ID,_T, 'PostalAddress'           ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val803(ID,_T, 'PhoneNumber'             ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val803(ID,_T, 'EmailAddress'            ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val803(ID,_T, 'ContactName'             ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val803(ID,_T, 'SecuritiesAccountNumber' ) -> encode_tagval(ID, <<"10">>);
encode_fld_val803(ID,_T, 'RegistrationNumber'      ) -> encode_tagval(ID, <<"11">>);
encode_fld_val803(ID,_T, 'RegisteredAddress12'     ) -> encode_tagval(ID, <<"12">>);
encode_fld_val803(ID,_T, 'RegulatoryStatus'        ) -> encode_tagval(ID, <<"13">>);
encode_fld_val803(ID,_T, 'RegistrationName'        ) -> encode_tagval(ID, <<"14">>);
encode_fld_val803(ID,_T, 'CashAccountNumber'       ) -> encode_tagval(ID, <<"15">>);
encode_fld_val803(ID,_T, 'Bic'                     ) -> encode_tagval(ID, <<"16">>);
encode_fld_val803(ID,_T, 'CsdParticipantMemberCode') -> encode_tagval(ID, <<"17">>);
encode_fld_val803(ID,_T, 'RegisteredAddress18'     ) -> encode_tagval(ID, <<"18">>);
encode_fld_val803(ID,_T, 'FundAccountName'         ) -> encode_tagval(ID, <<"19">>);
encode_fld_val803(ID,_T, 'TelexNumber'             ) -> encode_tagval(ID, <<"20">>);
encode_fld_val803(ID,_T, 'FaxNumber'               ) -> encode_tagval(ID, <<"21">>);
encode_fld_val803(ID,_T, 'SecuritiesAccountName'   ) -> encode_tagval(ID, <<"22">>);
encode_fld_val803(ID,_T, 'CashAccountName'         ) -> encode_tagval(ID, <<"23">>);
encode_fld_val803(ID,_T, 'Department'              ) -> encode_tagval(ID, <<"24">>);
encode_fld_val803(ID,_T, 'Location'                ) -> encode_tagval(ID, <<"25">>);
encode_fld_val803(ID,_T, 'PositionAccountType'     ) -> encode_tagval(ID, <<"26">>);
encode_fld_val803(ID, T, V                         ) -> try_encode_val(ID, T, V).

decode_fld_val808(Val) ->
  case Val of
    <<"1">> -> 'PendingAccept'     ; %% 0
    <<"2">> -> 'PendingRelease'    ; %% 1
    <<"3">> -> 'PendingReversal'   ; %% 2
    <<"4">> -> 'Accept'            ; %% 3
    <<"5">> -> 'BlockLevelReject'  ; %% 4
    <<"6">> -> 'AccountLevelReject'; %% 5
    _       -> Val
  end.

encode_fld_val808(ID,_T, 'PendingAccept'     ) -> encode_tagval(ID, <<"1">>);
encode_fld_val808(ID,_T, 'PendingRelease'    ) -> encode_tagval(ID, <<"2">>);
encode_fld_val808(ID,_T, 'PendingReversal'   ) -> encode_tagval(ID, <<"3">>);
encode_fld_val808(ID,_T, 'Accept'            ) -> encode_tagval(ID, <<"4">>);
encode_fld_val808(ID,_T, 'BlockLevelReject'  ) -> encode_tagval(ID, <<"5">>);
encode_fld_val808(ID,_T, 'AccountLevelReject') -> encode_tagval(ID, <<"6">>);
encode_fld_val808(ID, T, V                   ) -> try_encode_val(ID, T, V).

decode_fld_val814(Val) ->
  case Val of
    <<"0">> -> 'NoActionTaken'; %% 0
    <<"1">> -> 'QueueFlushed' ; %% 1
    <<"2">> -> 'OverlayLast'  ; %% 2
    <<"3">> -> 'EndSession'   ; %% 3
    _       -> Val
  end.

encode_fld_val814(ID,_T, 'NoActionTaken') -> encode_tagval(ID, <<"0">>);
encode_fld_val814(ID,_T, 'QueueFlushed' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val814(ID,_T, 'OverlayLast'  ) -> encode_tagval(ID, <<"2">>);
encode_fld_val814(ID,_T, 'EndSession'   ) -> encode_tagval(ID, <<"3">>);
encode_fld_val814(ID, T, V              ) -> try_encode_val(ID, T, V).

decode_fld_val815(Val) ->
  case Val of
    <<"0">> -> 'NoActionTaken'; %% 0
    <<"1">> -> 'QueueFlushed' ; %% 1
    <<"2">> -> 'OverlayLast'  ; %% 2
    <<"3">> -> 'EndSession'   ; %% 3
    _       -> Val
  end.

encode_fld_val815(ID,_T, 'NoActionTaken') -> encode_tagval(ID, <<"0">>);
encode_fld_val815(ID,_T, 'QueueFlushed' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val815(ID,_T, 'OverlayLast'  ) -> encode_tagval(ID, <<"2">>);
encode_fld_val815(ID,_T, 'EndSession'   ) -> encode_tagval(ID, <<"3">>);
encode_fld_val815(ID, T, V              ) -> try_encode_val(ID, T, V).

decode_fld_val819(Val) ->
  case Val of
    <<"0">> -> 'NoAveragePricing'                                          ; %% 0
    <<"1">> -> 'TradeIsPartOfAnAveragePriceGroupIdentifiedByTheTradelinkid'; %% 1
    <<"2">> -> 'LastTradeInTheAveragePriceGroupIdentifiedByTheTradelinkid' ; %% 2
    _       -> Val
  end.

encode_fld_val819(ID,_T, 'NoAveragePricing'                                          ) -> encode_tagval(ID, <<"0">>);
encode_fld_val819(ID,_T, 'TradeIsPartOfAnAveragePriceGroupIdentifiedByTheTradelinkid') -> encode_tagval(ID, <<"1">>);
encode_fld_val819(ID,_T, 'LastTradeInTheAveragePriceGroupIdentifiedByTheTradelinkid' ) -> encode_tagval(ID, <<"2">>);
encode_fld_val819(ID, T, V                                                           ) -> try_encode_val(ID, T, V).

decode_fld_val826(Val) ->
  case Val of
    <<"0">> -> 'AllocationNotRequired'            ; %% 0
    <<"1">> -> 'AllocationRequired'               ; %% 1
    <<"2">> -> 'UseAllocationProvidedWithTheTrade'; %% 2
    _       -> Val
  end.

encode_fld_val826(ID,_T, 'AllocationNotRequired'            ) -> encode_tagval(ID, <<"0">>);
encode_fld_val826(ID,_T, 'AllocationRequired'               ) -> encode_tagval(ID, <<"1">>);
encode_fld_val826(ID,_T, 'UseAllocationProvidedWithTheTrade') -> encode_tagval(ID, <<"2">>);
encode_fld_val826(ID, T, V                                  ) -> try_encode_val(ID, T, V).

decode_fld_val827(Val) ->
  case Val of
    <<"0">> -> 'ExpireOnTradingSessionClose'; %% 0
    <<"1">> -> 'ExpireOnTradingSessionOpen' ; %% 1
    _       -> Val
  end.

encode_fld_val827(ID,_T, 'ExpireOnTradingSessionClose') -> encode_tagval(ID, <<"0">>);
encode_fld_val827(ID,_T, 'ExpireOnTradingSessionOpen' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val827(ID, T, V                            ) -> try_encode_val(ID, T, V).

decode_fld_val828(Val) ->
  case Val of
    <<"0" >> -> 'RegularTrade'             ; %% 0
    <<"1" >> -> 'BlockTrade'               ; %% 1
    <<"2" >> -> 'Efp'                      ; %% 2
    <<"3" >> -> 'Transfer'                 ; %% 3
    <<"4" >> -> 'LateTrade'                ; %% 4
    <<"5" >> -> 'TTrade'                   ; %% 5
    <<"6" >> -> 'WeightedAveragePriceTrade'; %% 6
    <<"7" >> -> 'BunchedTrade'             ; %% 7
    <<"8" >> -> 'LateBunchedTrade'         ; %% 8
    <<"9" >> -> 'PriorReferencePriceTrade' ; %% 9
    <<"10">> -> 'AfterHoursTrade'          ; %% 10
    _        -> Val
  end.

encode_fld_val828(ID,_T, 'RegularTrade'             ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val828(ID,_T, 'BlockTrade'               ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val828(ID,_T, 'Efp'                      ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val828(ID,_T, 'Transfer'                 ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val828(ID,_T, 'LateTrade'                ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val828(ID,_T, 'TTrade'                   ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val828(ID,_T, 'WeightedAveragePriceTrade') -> encode_tagval(ID, <<"6" >>);
encode_fld_val828(ID,_T, 'BunchedTrade'             ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val828(ID,_T, 'LateBunchedTrade'         ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val828(ID,_T, 'PriorReferencePriceTrade' ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val828(ID,_T, 'AfterHoursTrade'          ) -> encode_tagval(ID, <<"10">>);
encode_fld_val828(ID, T, V                          ) -> try_encode_val(ID, T, V).

decode_fld_val835(Val) ->
  case Val of
    <<"0">> -> 'Floating'; %% 0
    <<"1">> -> 'Fixed'   ; %% 1
    _       -> Val
  end.

encode_fld_val835(ID,_T, 'Floating') -> encode_tagval(ID, <<"0">>);
encode_fld_val835(ID,_T, 'Fixed'   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val835(ID, T, V         ) -> try_encode_val(ID, T, V).

decode_fld_val836(Val) ->
  case Val of
    <<"0">> -> 'Price'      ; %% 0
    <<"1">> -> 'BasisPoints'; %% 1
    <<"2">> -> 'Ticks'      ; %% 2
    <<"3">> -> 'PriceTier'  ; %% 3
    _       -> Val
  end.

encode_fld_val836(ID,_T, 'Price'      ) -> encode_tagval(ID, <<"0">>);
encode_fld_val836(ID,_T, 'BasisPoints') -> encode_tagval(ID, <<"1">>);
encode_fld_val836(ID,_T, 'Ticks'      ) -> encode_tagval(ID, <<"2">>);
encode_fld_val836(ID,_T, 'PriceTier'  ) -> encode_tagval(ID, <<"3">>);
encode_fld_val836(ID, T, V            ) -> try_encode_val(ID, T, V).

decode_fld_val837(Val) ->
  case Val of
    <<"0">> -> 'OrBetter'                                                           ; %% 0
    <<"1">> -> 'StrictLimitIsAStrictLimit'                                          ; %% 1
    <<"2">> -> 'OrWorseForABuyThePegLimitIsAMinimumAndForASellThePegLimitIsAMaximum'; %% 2
    _       -> Val
  end.

encode_fld_val837(ID,_T, 'OrBetter'                                                           ) -> encode_tagval(ID, <<"0">>);
encode_fld_val837(ID,_T, 'StrictLimitIsAStrictLimit'                                          ) -> encode_tagval(ID, <<"1">>);
encode_fld_val837(ID,_T, 'OrWorseForABuyThePegLimitIsAMinimumAndForASellThePegLimitIsAMaximum') -> encode_tagval(ID, <<"2">>);
encode_fld_val837(ID, T, V                                                                    ) -> try_encode_val(ID, T, V).

decode_fld_val838(Val) ->
  case Val of
    <<"1">> -> 'MoreAggressiveOnABuyOrderRoundThePriceUpRoundUpToTheNearestTickOnASellRoundDownToTheNearestTick'; %% 0
    <<"2">> -> 'MorePassiveOnABuyOrderRoundDownToNearestTickOnASellOrderRoundUpToNearestTick'                   ; %% 1
    _       -> Val
  end.

encode_fld_val838(ID,_T, 'MoreAggressiveOnABuyOrderRoundThePriceUpRoundUpToTheNearestTickOnASellRoundDownToTheNearestTick') -> encode_tagval(ID, <<"1">>);
encode_fld_val838(ID,_T, 'MorePassiveOnABuyOrderRoundDownToNearestTickOnASellOrderRoundUpToNearestTick'                   ) -> encode_tagval(ID, <<"2">>);
encode_fld_val838(ID, T, V                                                                                                ) -> try_encode_val(ID, T, V).

decode_fld_val840(Val) ->
  case Val of
    <<"1">> -> 'Local'                 ; %% 0
    <<"2">> -> 'National'              ; %% 1
    <<"3">> -> 'Global'                ; %% 2
    <<"4">> -> 'NationalExcludingLocal'; %% 3
    _       -> Val
  end.

encode_fld_val840(ID,_T, 'Local'                 ) -> encode_tagval(ID, <<"1">>);
encode_fld_val840(ID,_T, 'National'              ) -> encode_tagval(ID, <<"2">>);
encode_fld_val840(ID,_T, 'Global'                ) -> encode_tagval(ID, <<"3">>);
encode_fld_val840(ID,_T, 'NationalExcludingLocal') -> encode_tagval(ID, <<"4">>);
encode_fld_val840(ID, T, V                       ) -> try_encode_val(ID, T, V).

decode_fld_val841(Val) ->
  case Val of
    <<"0">> -> 'Floating'; %% 0
    <<"1">> -> 'Fixed'   ; %% 1
    _       -> Val
  end.

encode_fld_val841(ID,_T, 'Floating') -> encode_tagval(ID, <<"0">>);
encode_fld_val841(ID,_T, 'Fixed'   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val841(ID, T, V         ) -> try_encode_val(ID, T, V).

decode_fld_val842(Val) ->
  case Val of
    <<"0">> -> 'Price'      ; %% 0
    <<"1">> -> 'BasisPoints'; %% 1
    <<"2">> -> 'Ticks'      ; %% 2
    <<"3">> -> 'PriceTier'  ; %% 3
    _       -> Val
  end.

encode_fld_val842(ID,_T, 'Price'      ) -> encode_tagval(ID, <<"0">>);
encode_fld_val842(ID,_T, 'BasisPoints') -> encode_tagval(ID, <<"1">>);
encode_fld_val842(ID,_T, 'Ticks'      ) -> encode_tagval(ID, <<"2">>);
encode_fld_val842(ID,_T, 'PriceTier'  ) -> encode_tagval(ID, <<"3">>);
encode_fld_val842(ID, T, V            ) -> try_encode_val(ID, T, V).

decode_fld_val843(Val) ->
  case Val of
    <<"0">> -> 'OrBetter'                                                                         ; %% 0
    <<"1">> -> 'StrictLimitIsAStrictLimit'                                                        ; %% 1
    <<"2">> -> 'OrWorseForABuyTheDiscretionPriceIsAMinimumAndForASellTheDiscretionPriceIsAMaximum'; %% 2
    _       -> Val
  end.

encode_fld_val843(ID,_T, 'OrBetter'                                                                         ) -> encode_tagval(ID, <<"0">>);
encode_fld_val843(ID,_T, 'StrictLimitIsAStrictLimit'                                                        ) -> encode_tagval(ID, <<"1">>);
encode_fld_val843(ID,_T, 'OrWorseForABuyTheDiscretionPriceIsAMinimumAndForASellTheDiscretionPriceIsAMaximum') -> encode_tagval(ID, <<"2">>);
encode_fld_val843(ID, T, V                                                                                  ) -> try_encode_val(ID, T, V).

decode_fld_val844(Val) ->
  case Val of
    <<"1">> -> 'MoreAggressiveOnABuyOrderRoundThePriceUpRoundUpToTheNearestTickOnASellRoundDownToTheNearestTick'; %% 0
    <<"2">> -> 'MorePassiveOnABuyOrderRoundDownToNearestTickOnASellOrderRoundUpToNearestTick'                   ; %% 1
    _       -> Val
  end.

encode_fld_val844(ID,_T, 'MoreAggressiveOnABuyOrderRoundThePriceUpRoundUpToTheNearestTickOnASellRoundDownToTheNearestTick') -> encode_tagval(ID, <<"1">>);
encode_fld_val844(ID,_T, 'MorePassiveOnABuyOrderRoundDownToNearestTickOnASellOrderRoundUpToNearestTick'                   ) -> encode_tagval(ID, <<"2">>);
encode_fld_val844(ID, T, V                                                                                                ) -> try_encode_val(ID, T, V).

decode_fld_val846(Val) ->
  case Val of
    <<"1">> -> 'Local'                 ; %% 0
    <<"2">> -> 'National'              ; %% 1
    <<"3">> -> 'Global'                ; %% 2
    <<"4">> -> 'NationalExcludingLocal'; %% 3
    _       -> Val
  end.

encode_fld_val846(ID,_T, 'Local'                 ) -> encode_tagval(ID, <<"1">>);
encode_fld_val846(ID,_T, 'National'              ) -> encode_tagval(ID, <<"2">>);
encode_fld_val846(ID,_T, 'Global'                ) -> encode_tagval(ID, <<"3">>);
encode_fld_val846(ID,_T, 'NationalExcludingLocal') -> encode_tagval(ID, <<"4">>);
encode_fld_val846(ID, T, V                       ) -> try_encode_val(ID, T, V).

decode_fld_val847(Val) ->
  case Val of
    <<"1">> -> 'Vwap'                ; %% 0
    <<"2">> -> 'Participate'         ; %% 1
    <<"3">> -> 'MininizeMarketImpact'; %% 2
    _       -> Val
  end.

encode_fld_val847(ID,_T, 'Vwap'                ) -> encode_tagval(ID, <<"1">>);
encode_fld_val847(ID,_T, 'Participate'         ) -> encode_tagval(ID, <<"2">>);
encode_fld_val847(ID,_T, 'MininizeMarketImpact') -> encode_tagval(ID, <<"3">>);
encode_fld_val847(ID, T, V                     ) -> try_encode_val(ID, T, V).

decode_fld_val851(Val) ->
  case Val of
    <<"1">> -> 'AddedLiquidity'    ; %% 0
    <<"2">> -> 'RemovedLiquidity'  ; %% 1
    <<"3">> -> 'LiquidityRoutedOut'; %% 2
    _       -> Val
  end.

encode_fld_val851(ID,_T, 'AddedLiquidity'    ) -> encode_tagval(ID, <<"1">>);
encode_fld_val851(ID,_T, 'RemovedLiquidity'  ) -> encode_tagval(ID, <<"2">>);
encode_fld_val851(ID,_T, 'LiquidityRoutedOut') -> encode_tagval(ID, <<"3">>);
encode_fld_val851(ID, T, V                   ) -> try_encode_val(ID, T, V).

decode_fld_val852(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val852(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val852(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val852(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val853(Val) ->
  case Val of
    <<"0">> -> 'DealerSoldShort'                  ; %% 0
    <<"1">> -> 'DealerSoldShortExempt'            ; %% 1
    <<"2">> -> 'SellingCustomerSoldShort'         ; %% 2
    <<"3">> -> 'SellingCustomerSoldShortExempt'   ; %% 3
    <<"4">> -> 'QualifedServiceRepresentative'    ; %% 4
    <<"5">> -> 'QsrOrAguContraSideSoldShortExempt'; %% 5
    _       -> Val
  end.

encode_fld_val853(ID,_T, 'DealerSoldShort'                  ) -> encode_tagval(ID, <<"0">>);
encode_fld_val853(ID,_T, 'DealerSoldShortExempt'            ) -> encode_tagval(ID, <<"1">>);
encode_fld_val853(ID,_T, 'SellingCustomerSoldShort'         ) -> encode_tagval(ID, <<"2">>);
encode_fld_val853(ID,_T, 'SellingCustomerSoldShortExempt'   ) -> encode_tagval(ID, <<"3">>);
encode_fld_val853(ID,_T, 'QualifedServiceRepresentative'    ) -> encode_tagval(ID, <<"4">>);
encode_fld_val853(ID,_T, 'QsrOrAguContraSideSoldShortExempt') -> encode_tagval(ID, <<"5">>);
encode_fld_val853(ID, T, V                                  ) -> try_encode_val(ID, T, V).

decode_fld_val854(Val) ->
  case Val of
    <<"0">> -> 'Units'    ; %% 0
    <<"1">> -> 'Contracts'; %% 1
    _       -> Val
  end.

encode_fld_val854(ID,_T, 'Units'    ) -> encode_tagval(ID, <<"0">>);
encode_fld_val854(ID,_T, 'Contracts') -> encode_tagval(ID, <<"1">>);
encode_fld_val854(ID, T, V          ) -> try_encode_val(ID, T, V).

decode_fld_val856(Val) ->
  case Val of
    <<"0">> -> 'Submit'            ; %% 0
    <<"1">> -> 'Alleged'           ; %% 1
    <<"2">> -> 'Accept'            ; %% 2
    <<"3">> -> 'Decline'           ; %% 3
    <<"4">> -> 'Addendum'          ; %% 4
    <<"5">> -> 'NoWas'             ; %% 5
    <<"6">> -> 'TradeReportCancel' ; %% 6
    <<"7">> -> 'LockedInTradeBreak'; %% 7
    _       -> Val
  end.

encode_fld_val856(ID,_T, 'Submit'            ) -> encode_tagval(ID, <<"0">>);
encode_fld_val856(ID,_T, 'Alleged'           ) -> encode_tagval(ID, <<"1">>);
encode_fld_val856(ID,_T, 'Accept'            ) -> encode_tagval(ID, <<"2">>);
encode_fld_val856(ID,_T, 'Decline'           ) -> encode_tagval(ID, <<"3">>);
encode_fld_val856(ID,_T, 'Addendum'          ) -> encode_tagval(ID, <<"4">>);
encode_fld_val856(ID,_T, 'NoWas'             ) -> encode_tagval(ID, <<"5">>);
encode_fld_val856(ID,_T, 'TradeReportCancel' ) -> encode_tagval(ID, <<"6">>);
encode_fld_val856(ID,_T, 'LockedInTradeBreak') -> encode_tagval(ID, <<"7">>);
encode_fld_val856(ID, T, V                   ) -> try_encode_val(ID, T, V).

decode_fld_val857(Val) ->
  case Val of
    <<"0">> -> 'NotSpecified'        ; %% 0
    <<"1">> -> 'ExplicitListProvided'; %% 1
    _       -> Val
  end.

encode_fld_val857(ID,_T, 'NotSpecified'        ) -> encode_tagval(ID, <<"0">>);
encode_fld_val857(ID,_T, 'ExplicitListProvided') -> encode_tagval(ID, <<"1">>);
encode_fld_val857(ID, T, V                     ) -> try_encode_val(ID, T, V).

decode_fld_val865(Val) ->
  case Val of
    <<"1" >> -> 'Put'            ; %% 0
    <<"2" >> -> 'Call'           ; %% 1
    <<"3" >> -> 'Tender'         ; %% 2
    <<"4" >> -> 'SinkingFundCall'; %% 3
    <<"99">> -> 'Other'          ; %% 4
    _        -> Val
  end.

encode_fld_val865(ID,_T, 'Put'            ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val865(ID,_T, 'Call'           ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val865(ID,_T, 'Tender'         ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val865(ID,_T, 'SinkingFundCall') -> encode_tagval(ID, <<"4" >>);
encode_fld_val865(ID,_T, 'Other'          ) -> encode_tagval(ID, <<"99">>);
encode_fld_val865(ID, T, V                ) -> try_encode_val(ID, T, V).

decode_fld_val871(Val) ->
  case Val of
    <<"1" >> -> 'Flat'                                                                     ; %% 0
    <<"2" >> -> 'ZeroCoupon'                                                               ; %% 1
    <<"3" >> -> 'InterestBearing'                                                          ; %% 2
    <<"4" >> -> 'NoPeriodicPayments'                                                       ; %% 3
    <<"5" >> -> 'VariableRate'                                                             ; %% 4
    <<"6" >> -> 'LessFeeForPut'                                                            ; %% 5
    <<"7" >> -> 'SteppedCoupon'                                                            ; %% 6
    <<"8" >> -> 'CouponPeriod'                                                             ; %% 7
    <<"9" >> -> 'WhenAndIfIssued'                                                          ; %% 8
    <<"10">> -> 'OriginalIssueDiscount'                                                    ; %% 9
    <<"11">> -> 'CallablePuttable'                                                         ; %% 10
    <<"12">> -> 'EscrowedToMaturity'                                                       ; %% 11
    <<"13">> -> 'EscrowedToRedemptionDateCallableSupplyRedemptionDateInTheInstrattribvalue'; %% 12
    <<"14">> -> 'Prerefunded'                                                              ; %% 13
    <<"15">> -> 'InDefault'                                                                ; %% 14
    <<"16">> -> 'Unrated'                                                                  ; %% 15
    <<"17">> -> 'Taxable'                                                                  ; %% 16
    <<"18">> -> 'Indexed'                                                                  ; %% 17
    <<"19">> -> 'SubjectToAlternativeMinimumTax'                                           ; %% 18
    <<"20">> -> 'OriginalIssueDiscountPriceSupplyPriceInTheInstrattribvalue'               ; %% 19
    <<"21">> -> 'CallableBelowMaturityValue'                                               ; %% 20
    <<"22">> -> 'CallableWithoutNoticeByMailToHolderUnlessRegistered'                      ; %% 21
    <<"99">> -> 'TextSupplyTheTextOfTheAttributeOrDisclaimerInTheInstrattribvalue'         ; %% 22
    _        -> Val
  end.

encode_fld_val871(ID,_T, 'Flat'                                                                     ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val871(ID,_T, 'ZeroCoupon'                                                               ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val871(ID,_T, 'InterestBearing'                                                          ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val871(ID,_T, 'NoPeriodicPayments'                                                       ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val871(ID,_T, 'VariableRate'                                                             ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val871(ID,_T, 'LessFeeForPut'                                                            ) -> encode_tagval(ID, <<"6" >>);
encode_fld_val871(ID,_T, 'SteppedCoupon'                                                            ) -> encode_tagval(ID, <<"7" >>);
encode_fld_val871(ID,_T, 'CouponPeriod'                                                             ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val871(ID,_T, 'WhenAndIfIssued'                                                          ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val871(ID,_T, 'OriginalIssueDiscount'                                                    ) -> encode_tagval(ID, <<"10">>);
encode_fld_val871(ID,_T, 'CallablePuttable'                                                         ) -> encode_tagval(ID, <<"11">>);
encode_fld_val871(ID,_T, 'EscrowedToMaturity'                                                       ) -> encode_tagval(ID, <<"12">>);
encode_fld_val871(ID,_T, 'EscrowedToRedemptionDateCallableSupplyRedemptionDateInTheInstrattribvalue') -> encode_tagval(ID, <<"13">>);
encode_fld_val871(ID,_T, 'Prerefunded'                                                              ) -> encode_tagval(ID, <<"14">>);
encode_fld_val871(ID,_T, 'InDefault'                                                                ) -> encode_tagval(ID, <<"15">>);
encode_fld_val871(ID,_T, 'Unrated'                                                                  ) -> encode_tagval(ID, <<"16">>);
encode_fld_val871(ID,_T, 'Taxable'                                                                  ) -> encode_tagval(ID, <<"17">>);
encode_fld_val871(ID,_T, 'Indexed'                                                                  ) -> encode_tagval(ID, <<"18">>);
encode_fld_val871(ID,_T, 'SubjectToAlternativeMinimumTax'                                           ) -> encode_tagval(ID, <<"19">>);
encode_fld_val871(ID,_T, 'OriginalIssueDiscountPriceSupplyPriceInTheInstrattribvalue'               ) -> encode_tagval(ID, <<"20">>);
encode_fld_val871(ID,_T, 'CallableBelowMaturityValue'                                               ) -> encode_tagval(ID, <<"21">>);
encode_fld_val871(ID,_T, 'CallableWithoutNoticeByMailToHolderUnlessRegistered'                      ) -> encode_tagval(ID, <<"22">>);
encode_fld_val871(ID,_T, 'TextSupplyTheTextOfTheAttributeOrDisclaimerInTheInstrattribvalue'         ) -> encode_tagval(ID, <<"99">>);
encode_fld_val871(ID, T, V                                                                          ) -> try_encode_val(ID, T, V).

decode_fld_val875(Val) ->
  case Val of
    <<"1" >> -> '3'    ; %% 0
    <<"2" >> -> '4'    ; %% 1
    <<"99">> -> 'Other'; %% 2
    _        -> Val
  end.

encode_fld_val875(ID,_T, '3'    ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val875(ID,_T, '4'    ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val875(ID,_T, 'Other') -> encode_tagval(ID, <<"99">>);
encode_fld_val875(ID, T, V      ) -> try_encode_val(ID, T, V).

decode_fld_val891(Val) ->
  case Val of
    <<"0">> -> 'Absolute'  ; %% 0
    <<"1">> -> 'PerUnit'   ; %% 1
    <<"2">> -> 'Percentage'; %% 2
    _       -> Val
  end.

encode_fld_val891(ID,_T, 'Absolute'  ) -> encode_tagval(ID, <<"0">>);
encode_fld_val891(ID,_T, 'PerUnit'   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val891(ID,_T, 'Percentage') -> encode_tagval(ID, <<"2">>);
encode_fld_val891(ID, T, V           ) -> try_encode_val(ID, T, V).

decode_fld_val893(Val) ->
  case Val of
    <<"Y">> -> 'Yes'; %% 0
    <<"N">> -> 'No' ; %% 1
    _       -> Val
  end.

encode_fld_val893(ID,_T, 'Yes') -> encode_tagval(ID, <<"Y">>);
encode_fld_val893(ID,_T, 'No' ) -> encode_tagval(ID, <<"N">>);
encode_fld_val893(ID, T, V    ) -> try_encode_val(ID, T, V).

decode_fld_val895(Val) ->
  case Val of
    <<"0">> -> 'Initial'                ; %% 0
    <<"1">> -> 'Scheduled'              ; %% 1
    <<"2">> -> 'TimeWarning'            ; %% 2
    <<"3">> -> 'MarginDeficiency'       ; %% 3
    <<"4">> -> 'MarginExcess'           ; %% 4
    <<"5">> -> 'ForwardCollateralDemand'; %% 5
    <<"6">> -> 'EventOfDefault'         ; %% 6
    <<"7">> -> 'AdverseTaxEvent'        ; %% 7
    _       -> Val
  end.

encode_fld_val895(ID,_T, 'Initial'                ) -> encode_tagval(ID, <<"0">>);
encode_fld_val895(ID,_T, 'Scheduled'              ) -> encode_tagval(ID, <<"1">>);
encode_fld_val895(ID,_T, 'TimeWarning'            ) -> encode_tagval(ID, <<"2">>);
encode_fld_val895(ID,_T, 'MarginDeficiency'       ) -> encode_tagval(ID, <<"3">>);
encode_fld_val895(ID,_T, 'MarginExcess'           ) -> encode_tagval(ID, <<"4">>);
encode_fld_val895(ID,_T, 'ForwardCollateralDemand') -> encode_tagval(ID, <<"5">>);
encode_fld_val895(ID,_T, 'EventOfDefault'         ) -> encode_tagval(ID, <<"6">>);
encode_fld_val895(ID,_T, 'AdverseTaxEvent'        ) -> encode_tagval(ID, <<"7">>);
encode_fld_val895(ID, T, V                        ) -> try_encode_val(ID, T, V).

decode_fld_val896(Val) ->
  case Val of
    <<"0">> -> 'Tradedate'           ; %% 0
    <<"1">> -> 'GcInstrument'        ; %% 1
    <<"2">> -> 'Collateralinstrument'; %% 2
    <<"3">> -> 'SubstitutionEligible'; %% 3
    <<"4">> -> 'NotAssigned'         ; %% 4
    <<"5">> -> 'PartiallyAssigned'   ; %% 5
    <<"6">> -> 'FullyAssigned'       ; %% 6
    <<"7">> -> 'OutstandingTrades'   ; %% 7
    _       -> Val
  end.

encode_fld_val896(ID,_T, 'Tradedate'           ) -> encode_tagval(ID, <<"0">>);
encode_fld_val896(ID,_T, 'GcInstrument'        ) -> encode_tagval(ID, <<"1">>);
encode_fld_val896(ID,_T, 'Collateralinstrument') -> encode_tagval(ID, <<"2">>);
encode_fld_val896(ID,_T, 'SubstitutionEligible') -> encode_tagval(ID, <<"3">>);
encode_fld_val896(ID,_T, 'NotAssigned'         ) -> encode_tagval(ID, <<"4">>);
encode_fld_val896(ID,_T, 'PartiallyAssigned'   ) -> encode_tagval(ID, <<"5">>);
encode_fld_val896(ID,_T, 'FullyAssigned'       ) -> encode_tagval(ID, <<"6">>);
encode_fld_val896(ID,_T, 'OutstandingTrades'   ) -> encode_tagval(ID, <<"7">>);
encode_fld_val896(ID, T, V                     ) -> try_encode_val(ID, T, V).

decode_fld_val903(Val) ->
  case Val of
    <<"0">> -> 'New'    ; %% 0
    <<"1">> -> 'Replace'; %% 1
    <<"2">> -> 'Cancel' ; %% 2
    <<"3">> -> 'Release'; %% 3
    <<"4">> -> 'Reverse'; %% 4
    _       -> Val
  end.

encode_fld_val903(ID,_T, 'New'    ) -> encode_tagval(ID, <<"0">>);
encode_fld_val903(ID,_T, 'Replace') -> encode_tagval(ID, <<"1">>);
encode_fld_val903(ID,_T, 'Cancel' ) -> encode_tagval(ID, <<"2">>);
encode_fld_val903(ID,_T, 'Release') -> encode_tagval(ID, <<"3">>);
encode_fld_val903(ID,_T, 'Reverse') -> encode_tagval(ID, <<"4">>);
encode_fld_val903(ID, T, V        ) -> try_encode_val(ID, T, V).

decode_fld_val905(Val) ->
  case Val of
    <<"0">> -> 'Received'; %% 0
    <<"1">> -> 'Accepted'; %% 1
    <<"2">> -> 'Declined'; %% 2
    <<"3">> -> 'Rejected'; %% 3
    _       -> Val
  end.

encode_fld_val905(ID,_T, 'Received') -> encode_tagval(ID, <<"0">>);
encode_fld_val905(ID,_T, 'Accepted') -> encode_tagval(ID, <<"1">>);
encode_fld_val905(ID,_T, 'Declined') -> encode_tagval(ID, <<"2">>);
encode_fld_val905(ID,_T, 'Rejected') -> encode_tagval(ID, <<"3">>);
encode_fld_val905(ID, T, V         ) -> try_encode_val(ID, T, V).

decode_fld_val906(Val) ->
  case Val of
    <<"0" >> -> 'UnknownDeal'               ; %% 0
    <<"1" >> -> 'UnknownOrInvalidInstrument'; %% 1
    <<"2" >> -> 'UnauthorizedTransaction'   ; %% 2
    <<"3" >> -> 'InsufficientCollateral'    ; %% 3
    <<"4" >> -> 'InvalidTypeOfCollateral'   ; %% 4
    <<"5" >> -> 'ExcessiveSubstitution'     ; %% 5
    <<"99">> -> 'Other'                     ; %% 6
    _        -> Val
  end.

encode_fld_val906(ID,_T, 'UnknownDeal'               ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val906(ID,_T, 'UnknownOrInvalidInstrument') -> encode_tagval(ID, <<"1" >>);
encode_fld_val906(ID,_T, 'UnauthorizedTransaction'   ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val906(ID,_T, 'InsufficientCollateral'    ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val906(ID,_T, 'InvalidTypeOfCollateral'   ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val906(ID,_T, 'ExcessiveSubstitution'     ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val906(ID,_T, 'Other'                     ) -> encode_tagval(ID, <<"99">>);
encode_fld_val906(ID, T, V                           ) -> try_encode_val(ID, T, V).

decode_fld_val910(Val) ->
  case Val of
    <<"0">> -> 'Unassigned'        ; %% 0
    <<"1">> -> 'PartiallyAssigned' ; %% 1
    <<"2">> -> 'AssignmentProposed'; %% 2
    <<"3">> -> 'Assigned'          ; %% 3
    <<"4">> -> 'Challenged'        ; %% 4
    _       -> Val
  end.

encode_fld_val910(ID,_T, 'Unassigned'        ) -> encode_tagval(ID, <<"0">>);
encode_fld_val910(ID,_T, 'PartiallyAssigned' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val910(ID,_T, 'AssignmentProposed') -> encode_tagval(ID, <<"2">>);
encode_fld_val910(ID,_T, 'Assigned'          ) -> encode_tagval(ID, <<"3">>);
encode_fld_val910(ID,_T, 'Challenged'        ) -> encode_tagval(ID, <<"4">>);
encode_fld_val910(ID, T, V                   ) -> try_encode_val(ID, T, V).

decode_fld_val919(Val) ->
  case Val of
    <<"0">> -> 'VersusPaymentDeliver'; %% 0
    <<"1">> -> 'FreeDeliver'         ; %% 1
    <<"2">> -> 'TriParty'            ; %% 2
    <<"3">> -> 'HoldInCustody'       ; %% 3
    _       -> Val
  end.

encode_fld_val919(ID,_T, 'VersusPaymentDeliver') -> encode_tagval(ID, <<"0">>);
encode_fld_val919(ID,_T, 'FreeDeliver'         ) -> encode_tagval(ID, <<"1">>);
encode_fld_val919(ID,_T, 'TriParty'            ) -> encode_tagval(ID, <<"2">>);
encode_fld_val919(ID,_T, 'HoldInCustody'       ) -> encode_tagval(ID, <<"3">>);
encode_fld_val919(ID, T, V                     ) -> try_encode_val(ID, T, V).

decode_fld_val924(Val) ->
  case Val of
    <<"1">> -> 'Logonuser'                  ; %% 0
    <<"2">> -> 'Logoffuser'                 ; %% 1
    <<"3">> -> 'Changepasswordforuser'      ; %% 2
    <<"4">> -> 'RequestIndividualUserStatus'; %% 3
    _       -> Val
  end.

encode_fld_val924(ID,_T, 'Logonuser'                  ) -> encode_tagval(ID, <<"1">>);
encode_fld_val924(ID,_T, 'Logoffuser'                 ) -> encode_tagval(ID, <<"2">>);
encode_fld_val924(ID,_T, 'Changepasswordforuser'      ) -> encode_tagval(ID, <<"3">>);
encode_fld_val924(ID,_T, 'RequestIndividualUserStatus') -> encode_tagval(ID, <<"4">>);
encode_fld_val924(ID, T, V                            ) -> try_encode_val(ID, T, V).

decode_fld_val926(Val) ->
  case Val of
    <<"1">> -> 'LoggedIn'         ; %% 0
    <<"2">> -> 'NotLoggedIn'      ; %% 1
    <<"3">> -> 'UserNotRecognised'; %% 2
    <<"4">> -> 'PasswordIncorrect'; %% 3
    <<"5">> -> 'PasswordChanged'  ; %% 4
    <<"6">> -> 'Other'            ; %% 5
    _       -> Val
  end.

encode_fld_val926(ID,_T, 'LoggedIn'         ) -> encode_tagval(ID, <<"1">>);
encode_fld_val926(ID,_T, 'NotLoggedIn'      ) -> encode_tagval(ID, <<"2">>);
encode_fld_val926(ID,_T, 'UserNotRecognised') -> encode_tagval(ID, <<"3">>);
encode_fld_val926(ID,_T, 'PasswordIncorrect') -> encode_tagval(ID, <<"4">>);
encode_fld_val926(ID,_T, 'PasswordChanged'  ) -> encode_tagval(ID, <<"5">>);
encode_fld_val926(ID,_T, 'Other'            ) -> encode_tagval(ID, <<"6">>);
encode_fld_val926(ID, T, V                  ) -> try_encode_val(ID, T, V).

decode_fld_val928(Val) ->
  case Val of
    <<"1">> -> 'Connected'                   ; %% 0
    <<"2">> -> 'NotConnectedDownExpectedUp'  ; %% 1
    <<"3">> -> 'NotConnectedDownExpectedDown'; %% 2
    <<"4">> -> 'InProcess'                   ; %% 3
    _       -> Val
  end.

encode_fld_val928(ID,_T, 'Connected'                   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val928(ID,_T, 'NotConnectedDownExpectedUp'  ) -> encode_tagval(ID, <<"2">>);
encode_fld_val928(ID,_T, 'NotConnectedDownExpectedDown') -> encode_tagval(ID, <<"3">>);
encode_fld_val928(ID,_T, 'InProcess'                   ) -> encode_tagval(ID, <<"4">>);
encode_fld_val928(ID, T, V                             ) -> try_encode_val(ID, T, V).

decode_fld_val935(Val) ->
  case Val of
    <<"1">> -> 'Snapshot'                                 ; %% 0
    <<"2">> -> 'Subscribe'                                ; %% 1
    <<"4">> -> 'StopSubscribing'                          ; %% 2
    <<"8">> -> 'LevelOfDetailThenNocompidsBecomesRequired'; %% 3
    _       -> Val
  end.

encode_fld_val935(ID,_T, 'Snapshot'                                 ) -> encode_tagval(ID, <<"1">>);
encode_fld_val935(ID,_T, 'Subscribe'                                ) -> encode_tagval(ID, <<"2">>);
encode_fld_val935(ID,_T, 'StopSubscribing'                          ) -> encode_tagval(ID, <<"4">>);
encode_fld_val935(ID,_T, 'LevelOfDetailThenNocompidsBecomesRequired') -> encode_tagval(ID, <<"8">>);
encode_fld_val935(ID, T, V                                          ) -> try_encode_val(ID, T, V).

decode_fld_val937(Val) ->
  case Val of
    <<"1">> -> 'Full'             ; %% 0
    <<"2">> -> 'IncrementalUpdate'; %% 1
    _       -> Val
  end.

encode_fld_val937(ID,_T, 'Full'             ) -> encode_tagval(ID, <<"1">>);
encode_fld_val937(ID,_T, 'IncrementalUpdate') -> encode_tagval(ID, <<"2">>);
encode_fld_val937(ID, T, V                  ) -> try_encode_val(ID, T, V).

decode_fld_val939(Val) ->
  case Val of
    <<"0">> -> 'Accepted'; %% 0
    <<"1">> -> 'Rejected'; %% 1
    _       -> Val
  end.

encode_fld_val939(ID,_T, 'Accepted') -> encode_tagval(ID, <<"0">>);
encode_fld_val939(ID,_T, 'Rejected') -> encode_tagval(ID, <<"1">>);
encode_fld_val939(ID, T, V         ) -> try_encode_val(ID, T, V).

decode_fld_val940(Val) ->
  case Val of
    <<"1">> -> 'Received'                    ; %% 0
    <<"2">> -> 'ConfirmRejectedIeNotAffirmed'; %% 1
    <<"3">> -> 'Affirmed'                    ; %% 2
    _       -> Val
  end.

encode_fld_val940(ID,_T, 'Received'                    ) -> encode_tagval(ID, <<"1">>);
encode_fld_val940(ID,_T, 'ConfirmRejectedIeNotAffirmed') -> encode_tagval(ID, <<"2">>);
encode_fld_val940(ID,_T, 'Affirmed'                    ) -> encode_tagval(ID, <<"3">>);
encode_fld_val940(ID, T, V                             ) -> try_encode_val(ID, T, V).

decode_fld_val944(Val) ->
  case Val of
    <<"0">> -> 'Retain'; %% 0
    <<"1">> -> 'Add'   ; %% 1
    <<"2">> -> 'Remove'; %% 2
    _       -> Val
  end.

encode_fld_val944(ID,_T, 'Retain') -> encode_tagval(ID, <<"0">>);
encode_fld_val944(ID,_T, 'Add'   ) -> encode_tagval(ID, <<"1">>);
encode_fld_val944(ID,_T, 'Remove') -> encode_tagval(ID, <<"2">>);
encode_fld_val944(ID, T, V       ) -> try_encode_val(ID, T, V).

decode_fld_val945(Val) ->
  case Val of
    <<"0">> -> 'Accepted'             ; %% 0
    <<"1">> -> 'AcceptedWithWarnings' ; %% 1
    <<"2">> -> 'Completed'            ; %% 2
    <<"3">> -> 'CompletedWithWarnings'; %% 3
    <<"4">> -> 'Rejected'             ; %% 4
    _       -> Val
  end.

encode_fld_val945(ID,_T, 'Accepted'             ) -> encode_tagval(ID, <<"0">>);
encode_fld_val945(ID,_T, 'AcceptedWithWarnings' ) -> encode_tagval(ID, <<"1">>);
encode_fld_val945(ID,_T, 'Completed'            ) -> encode_tagval(ID, <<"2">>);
encode_fld_val945(ID,_T, 'CompletedWithWarnings') -> encode_tagval(ID, <<"3">>);
encode_fld_val945(ID,_T, 'Rejected'             ) -> encode_tagval(ID, <<"4">>);
encode_fld_val945(ID, T, V                      ) -> try_encode_val(ID, T, V).

decode_fld_val946(Val) ->
  case Val of
    <<"0" >> -> 'Successful'                           ; %% 0
    <<"1" >> -> 'InvalidOrUnknownInstrument'           ; %% 1
    <<"2" >> -> 'InvalidOrUnknownCollateralType'       ; %% 2
    <<"3" >> -> 'InvalidParties'                       ; %% 3
    <<"4" >> -> 'InvalidTransportTypeRequested'        ; %% 4
    <<"5" >> -> 'InvalidDestinationRequested'          ; %% 5
    <<"6" >> -> 'NoCollateralFoundForTheTradeSpecified'; %% 6
    <<"7" >> -> 'NoCollateralFoundForTheOrderSpecified'; %% 7
    <<"8" >> -> 'CollateralInquiryTypeNotSupported'    ; %% 8
    <<"9" >> -> 'UnauthorizedForCollateralInquiry'     ; %% 9
    <<"99">> -> 'Other'                                ; %% 10
    _        -> Val
  end.

encode_fld_val946(ID,_T, 'Successful'                           ) -> encode_tagval(ID, <<"0" >>);
encode_fld_val946(ID,_T, 'InvalidOrUnknownInstrument'           ) -> encode_tagval(ID, <<"1" >>);
encode_fld_val946(ID,_T, 'InvalidOrUnknownCollateralType'       ) -> encode_tagval(ID, <<"2" >>);
encode_fld_val946(ID,_T, 'InvalidParties'                       ) -> encode_tagval(ID, <<"3" >>);
encode_fld_val946(ID,_T, 'InvalidTransportTypeRequested'        ) -> encode_tagval(ID, <<"4" >>);
encode_fld_val946(ID,_T, 'InvalidDestinationRequested'          ) -> encode_tagval(ID, <<"5" >>);
encode_fld_val946(ID,_T, 'NoCollateralFoundForTheTradeSpecified') -> encode_tagval(ID, <<"6" >>);
encode_fld_val946(ID,_T, 'NoCollateralFoundForTheOrderSpecified') -> encode_tagval(ID, <<"7" >>);
encode_fld_val946(ID,_T, 'CollateralInquiryTypeNotSupported'    ) -> encode_tagval(ID, <<"8" >>);
encode_fld_val946(ID,_T, 'UnauthorizedForCollateralInquiry'     ) -> encode_tagval(ID, <<"9" >>);
encode_fld_val946(ID,_T, 'Other'                                ) -> encode_tagval(ID, <<"99">>);
encode_fld_val946(ID, T, V                                      ) -> try_encode_val(ID, T, V).

try_encode_val(ID, bool,   true)                 -> encode_tagval(ID, <<"Y">>);
try_encode_val(ID, bool,   false)                -> encode_tagval(ID, <<"N">>);
try_encode_val(ID, int,    V) when is_integer(V) -> encode_tagval(ID, integer_to_binary(V));
try_encode_val(ID, length, V) when is_integer(V) -> encode_tagval(ID, integer_to_binary(V));
try_encode_val(ID, char,   V) when is_integer(V), V >= $!, V =< $~ -> encode_tagval(ID, <<V>>);
try_encode_val(ID, string, V) when is_list(V)    -> encode_tagval(ID, list_to_binary(V));
try_encode_val(ID, string, V) when is_binary(V)  -> encode_tagval(ID, V);
try_encode_val(ID, binary, V) when is_binary(V)  -> encode_tagval(ID, V);
try_encode_val(ID, datetm, V) when is_integer(V) -> encode_tagval(ID, fix_nif:encode_timestamp(V));
try_encode_val(ID, datetm, V) when is_binary(V)  -> encode_tagval(ID, V);
try_encode_val(ID, T,      V) -> erlang:error({cannot_encode_val, ID, T, V}).

try_encode_group(ID, [{group, _, _M}|_] = V) when is_map(_M) ->
  N = length(V),
  encode_tagval(ID, [integer_to_binary(N), ?SOH,
                      [encode_field(K,I) || {group, _, M} <- V, {K,I} <- maps:to_list(M)]], false);
try_encode_group(ID, []) ->
  encode_tagval(ID, <<"0">>).

encode_tagval(ID, V) ->
  encode_tagval(ID, V, true).
encode_tagval(ID, V, true) ->
  [integer_to_binary(ID), $=, V, ?SOH];

encode_tagval(ID, V, false) ->
  [integer_to_binary(ID), $=, V].

encode_field(Tag, Val) when is_atom(Tag) ->
  {_Num, _Type, Fun} = field_tag(Tag),
  Fun(Val).

