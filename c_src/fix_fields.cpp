//------------------------------------------------------------------------------
// Author: Serge Aleynikov <saleyn at gmail dot com>
//
// The work is derived from Maxim Lapshin's open source work:
// https://github.com/maxlapshin/fix under the same open source MIT
// licensing terms as the original.
//------------------------------------------------------------------------------
// *** This file is auto-generated, don't modify by hand!!! ***
//------------------------------------------------------------------------------

#include "util.hpp"

namespace {

std::vector<Field> make_all_fields(FixVariant* fvar)
{
  assert(fvar);
  return std::vector<Field>{{
    //--- Tag# 0
    Field{},
    //--- Tag# 1 "Account"
    Field{
      fvar,
      1,
      "Account",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 2 "AdvId"
    Field{
      fvar,
      2,
      "AdvId",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 3 "AdvRefID"
    Field{
      fvar,
      3,
      "AdvRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 4 "AdvSide"
    Field{
      fvar,
      4,
      "AdvSide",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "B", .descr = "Buy"  , .atom = 0}, // 0
        {.value = "S", .descr = "Sell" , .atom = 0}, // 1
        {.value = "X", .descr = "Cross", .atom = 0}, // 2
        {.value = "T", .descr = "Trade", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'B': return f.value_atom(0); // BUY
          case 'S': return f.value_atom(1); // SELL
          case 'X': return f.value_atom(2); // CROSS
          case 'T': return f.value_atom(3); // TRADE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 5 "AdvTransType"
    Field{
      fvar,
      5,
      "AdvTransType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "N", .descr = "New"    , .atom = 0}, // 0
        {.value = "C", .descr = "Cancel" , .atom = 0}, // 1
        {.value = "R", .descr = "Replace", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'N': return f.value_atom(0); // NEW
          case 'C': return f.value_atom(1); // CANCEL
          case 'R': return f.value_atom(2); // REPLACE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 6 "AvgPx"
    Field{
      fvar,
      6,
      "AvgPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 7 "BeginSeqNo"
    Field{
      fvar,
      7,
      "BeginSeqNo",
      FieldType::SEQNUM,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 8 "BeginString"
    Field{
      fvar,
      8,
      "BeginString",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 9 "BodyLength"
    Field{
      fvar,
      9,
      "BodyLength",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 10 "CheckSum"
    Field{
      fvar,
      10,
      "CheckSum",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 11 "ClOrdID"
    Field{
      fvar,
      11,
      "ClOrdID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 12 "Commission"
    Field{
      fvar,
      12,
      "Commission",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 13 "CommType"
    Field{
      fvar,
      13,
      "CommType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "PerUnit"                                        , .atom = 0}, // 0
        {.value = "2", .descr = "Percentage"                                     , .atom = 0}, // 1
        {.value = "3", .descr = "Absolute"                                       , .atom = 0}, // 2
        {.value = "4", .descr = "4"                                              , .atom = 0}, // 3
        {.value = "5", .descr = "5"                                              , .atom = 0}, // 4
        {.value = "6", .descr = "PointsPerBondOrContractSupplyContractmultiplier", .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // PER_UNIT
          case '2': return f.value_atom(1); // PERCENTAGE
          case '3': return f.value_atom(2); // ABSOLUTE
          case '4': return f.value_atom(3); // 4
          case '5': return f.value_atom(4); // 5
          case '6': return f.value_atom(5); // POINTS_PER_BOND_OR_CONTRACT_SUPPLY_CONTRACTMULTIPLIER
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 14 "CumQty"
    Field{
      fvar,
      14,
      "CumQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 15 "Currency"
    Field{
      fvar,
      15,
      "Currency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 16 "EndSeqNo"
    Field{
      fvar,
      16,
      "EndSeqNo",
      FieldType::SEQNUM,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 17 "ExecID"
    Field{
      fvar,
      17,
      "ExecID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 18 "ExecInst"
    Field{
      fvar,
      18,
      "ExecInst",
      FieldType::MULTIPLEVALUESTRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "NotHeld"                   , .atom = 0}, // 0
        {.value = "2", .descr = "Work"                      , .atom = 0}, // 1
        {.value = "3", .descr = "GoAlong"                   , .atom = 0}, // 2
        {.value = "4", .descr = "OverTheDay"                , .atom = 0}, // 3
        {.value = "5", .descr = "Held"                      , .atom = 0}, // 4
        {.value = "6", .descr = "ParticipateDontInitiate"   , .atom = 0}, // 5
        {.value = "7", .descr = "StrictScale"               , .atom = 0}, // 6
        {.value = "8", .descr = "TryToScale"                , .atom = 0}, // 7
        {.value = "9", .descr = "StayOnBidside"             , .atom = 0}, // 8
        {.value = "0", .descr = "StayOnOfferside"           , .atom = 0}, // 9
        {.value = "A", .descr = "NoCross"                   , .atom = 0}, // 10
        {.value = "B", .descr = "OkToCross"                 , .atom = 0}, // 11
        {.value = "C", .descr = "CallFirst"                 , .atom = 0}, // 12
        {.value = "D", .descr = "PercentOfVolume"           , .atom = 0}, // 13
        {.value = "E", .descr = "DoNotIncrease"             , .atom = 0}, // 14
        {.value = "F", .descr = "DoNotReduce"               , .atom = 0}, // 15
        {.value = "G", .descr = "AllOrNone"                 , .atom = 0}, // 16
        {.value = "H", .descr = "ReinstateOnSystemFailure"  , .atom = 0}, // 17
        {.value = "I", .descr = "InstitutionsOnly"          , .atom = 0}, // 18
        {.value = "J", .descr = "ReinstateOnTradingHalt"    , .atom = 0}, // 19
        {.value = "K", .descr = "CancelOnTradingHalt"       , .atom = 0}, // 20
        {.value = "L", .descr = "LastPeg"                   , .atom = 0}, // 21
        {.value = "M", .descr = "MidPricePeg"               , .atom = 0}, // 22
        {.value = "N", .descr = "NonNegotiable"             , .atom = 0}, // 23
        {.value = "O", .descr = "OpeningPeg"                , .atom = 0}, // 24
        {.value = "P", .descr = "MarketPeg"                 , .atom = 0}, // 25
        {.value = "Q", .descr = "CancelOnSystemFailure"     , .atom = 0}, // 26
        {.value = "R", .descr = "PrimaryPeg"                , .atom = 0}, // 27
        {.value = "S", .descr = "Suspend"                   , .atom = 0}, // 28
        {.value = "U", .descr = "CustomerDisplayInstruction", .atom = 0}, // 29
        {.value = "V", .descr = "Netting"                   , .atom = 0}, // 30
        {.value = "W", .descr = "PegToVwap"                 , .atom = 0}, // 31
        {.value = "X", .descr = "TradeAlong"                , .atom = 0}, // 32
        {.value = "Y", .descr = "TryToStop"                 , .atom = 0}, // 33
        {.value = "Z", .descr = "CancelIfNotBest"           , .atom = 0}, // 34
        {.value = "a", .descr = "TrailingStopPeg"           , .atom = 0}, // 35
        {.value = "b", .descr = "StrictLimit"               , .atom = 0}, // 36
        {.value = "c", .descr = "IgnorePriceValidityChecks" , .atom = 0}, // 37
        {.value = "d", .descr = "PegToLimitPrice"           , .atom = 0}, // 38
        {.value = "e", .descr = "WorkToTargetStrategy"      , .atom = 0}, // 39
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // NOT_HELD
          case '2': return f.value_atom(1); // WORK
          case '3': return f.value_atom(2); // GO_ALONG
          case '4': return f.value_atom(3); // OVER_THE_DAY
          case '5': return f.value_atom(4); // HELD
          case '6': return f.value_atom(5); // PARTICIPATE_DONT_INITIATE
          case '7': return f.value_atom(6); // STRICT_SCALE
          case '8': return f.value_atom(7); // TRY_TO_SCALE
          case '9': return f.value_atom(8); // STAY_ON_BIDSIDE
          case '0': return f.value_atom(9); // STAY_ON_OFFERSIDE
          case 'A': return f.value_atom(10); // NO_CROSS
          case 'B': return f.value_atom(11); // OK_TO_CROSS
          case 'C': return f.value_atom(12); // CALL_FIRST
          case 'D': return f.value_atom(13); // PERCENT_OF_VOLUME
          case 'E': return f.value_atom(14); // DO_NOT_INCREASE
          case 'F': return f.value_atom(15); // DO_NOT_REDUCE
          case 'G': return f.value_atom(16); // ALL_OR_NONE
          case 'H': return f.value_atom(17); // REINSTATE_ON_SYSTEM_FAILURE
          case 'I': return f.value_atom(18); // INSTITUTIONS_ONLY
          case 'J': return f.value_atom(19); // REINSTATE_ON_TRADING_HALT
          case 'K': return f.value_atom(20); // CANCEL_ON_TRADING_HALT
          case 'L': return f.value_atom(21); // LAST_PEG
          case 'M': return f.value_atom(22); // MID_PRICE_PEG
          case 'N': return f.value_atom(23); // NON_NEGOTIABLE
          case 'O': return f.value_atom(24); // OPENING_PEG
          case 'P': return f.value_atom(25); // MARKET_PEG
          case 'Q': return f.value_atom(26); // CANCEL_ON_SYSTEM_FAILURE
          case 'R': return f.value_atom(27); // PRIMARY_PEG
          case 'S': return f.value_atom(28); // SUSPEND
          case 'U': return f.value_atom(29); // CUSTOMER_DISPLAY_INSTRUCTION
          case 'V': return f.value_atom(30); // NETTING
          case 'W': return f.value_atom(31); // PEG_TO_VWAP
          case 'X': return f.value_atom(32); // TRADE_ALONG
          case 'Y': return f.value_atom(33); // TRY_TO_STOP
          case 'Z': return f.value_atom(34); // CANCEL_IF_NOT_BEST
          case 'a': return f.value_atom(35); // TRAILING_STOP_PEG
          case 'b': return f.value_atom(36); // STRICT_LIMIT
          case 'c': return f.value_atom(37); // IGNORE_PRICE_VALIDITY_CHECKS
          case 'd': return f.value_atom(38); // PEG_TO_LIMIT_PRICE
          case 'e': return f.value_atom(39); // WORK_TO_TARGET_STRATEGY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 19 "ExecRefID"
    Field{
      fvar,
      19,
      "ExecRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 20
    Field{},
    //--- Tag# 21 "HandlInst"
    Field{
      fvar,
      21,
      "HandlInst",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "AutomatedExecutionOrderPrivateNoBrokerIntervention", .atom = 0}, // 0
        {.value = "2", .descr = "AutomatedExecutionOrderPublicBrokerInterventionOk" , .atom = 0}, // 1
        {.value = "3", .descr = "ManualOrderBestExecution"                          , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // AUTOMATED_EXECUTION_ORDER_PRIVATE_NO_BROKER_INTERVENTION
          case '2': return f.value_atom(1); // AUTOMATED_EXECUTION_ORDER_PUBLIC_BROKER_INTERVENTION_OK
          case '3': return f.value_atom(2); // MANUAL_ORDER_BEST_EXECUTION
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 22 "SecurityIDSource"
    Field{
      fvar,
      22,
      "SecurityIDSource",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Cusip"                         , .atom = 0}, // 0
        {.value = "2", .descr = "Sedol"                         , .atom = 0}, // 1
        {.value = "3", .descr = "Quik"                          , .atom = 0}, // 2
        {.value = "4", .descr = "IsinNumber"                    , .atom = 0}, // 3
        {.value = "5", .descr = "RicCode"                       , .atom = 0}, // 4
        {.value = "6", .descr = "IsoCurrencyCode"               , .atom = 0}, // 5
        {.value = "7", .descr = "IsoCountryCode"                , .atom = 0}, // 6
        {.value = "8", .descr = "ExchangeSymbol"                , .atom = 0}, // 7
        {.value = "9", .descr = "ConsolidatedTapeAssociation"   , .atom = 0}, // 8
        {.value = "A", .descr = "BloombergSymbol"               , .atom = 0}, // 9
        {.value = "B", .descr = "Wertpapier"                    , .atom = 0}, // 10
        {.value = "C", .descr = "Dutch"                         , .atom = 0}, // 11
        {.value = "D", .descr = "Valoren"                       , .atom = 0}, // 12
        {.value = "E", .descr = "Sicovam"                       , .atom = 0}, // 13
        {.value = "F", .descr = "Belgian"                       , .atom = 0}, // 14
        {.value = "G", .descr = "Common"                        , .atom = 0}, // 15
        {.value = "H", .descr = "ClearingHouse"                 , .atom = 0}, // 16
        {.value = "I", .descr = "IsdaFpmlProductSpecification"  , .atom = 0}, // 17
        {.value = "J", .descr = "OptionsPriceReportingAuthority", .atom = 0}, // 18
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // CUSIP
          case '2': return f.value_atom(1); // SEDOL
          case '3': return f.value_atom(2); // QUIK
          case '4': return f.value_atom(3); // ISIN_NUMBER
          case '5': return f.value_atom(4); // RIC_CODE
          case '6': return f.value_atom(5); // ISO_CURRENCY_CODE
          case '7': return f.value_atom(6); // ISO_COUNTRY_CODE
          case '8': return f.value_atom(7); // EXCHANGE_SYMBOL
          case '9': return f.value_atom(8); // CONSOLIDATED_TAPE_ASSOCIATION
          case 'A': return f.value_atom(9); // BLOOMBERG_SYMBOL
          case 'B': return f.value_atom(10); // WERTPAPIER
          case 'C': return f.value_atom(11); // DUTCH
          case 'D': return f.value_atom(12); // VALOREN
          case 'E': return f.value_atom(13); // SICOVAM
          case 'F': return f.value_atom(14); // BELGIAN
          case 'G': return f.value_atom(15); // COMMON
          case 'H': return f.value_atom(16); // CLEARING_HOUSE
          case 'I': return f.value_atom(17); // ISDA_FPML_PRODUCT_SPECIFICATION
          case 'J': return f.value_atom(18); // OPTIONS_PRICE_REPORTING_AUTHORITY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 23 "IOIID"
    Field{
      fvar,
      23,
      "IOIID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 24
    Field{},
    //--- Tag# 25 "IOIQltyInd"
    Field{
      fvar,
      25,
      "IOIQltyInd",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "L", .descr = "Low"   , .atom = 0}, // 0
        {.value = "M", .descr = "Medium", .atom = 0}, // 1
        {.value = "H", .descr = "High"  , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'L': return f.value_atom(0); // LOW
          case 'M': return f.value_atom(1); // MEDIUM
          case 'H': return f.value_atom(2); // HIGH
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 26 "IOIRefID"
    Field{
      fvar,
      26,
      "IOIRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 27 "IOIQty"
    Field{
      fvar,
      27,
      "IOIQty",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "S", .descr = "Small" , .atom = 0}, // 0
        {.value = "M", .descr = "Medium", .atom = 0}, // 1
        {.value = "L", .descr = "Large" , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'S': return f.value_atom(0); // SMALL
          case 'M': return f.value_atom(1); // MEDIUM
          case 'L': return f.value_atom(2); // LARGE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 28 "IOITransType"
    Field{
      fvar,
      28,
      "IOITransType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "N", .descr = "New"    , .atom = 0}, // 0
        {.value = "C", .descr = "Cancel" , .atom = 0}, // 1
        {.value = "R", .descr = "Replace", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'N': return f.value_atom(0); // NEW
          case 'C': return f.value_atom(1); // CANCEL
          case 'R': return f.value_atom(2); // REPLACE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 29 "LastCapacity"
    Field{
      fvar,
      29,
      "LastCapacity",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Agent"           , .atom = 0}, // 0
        {.value = "2", .descr = "CrossAsAgent"    , .atom = 0}, // 1
        {.value = "3", .descr = "CrossAsPrincipal", .atom = 0}, // 2
        {.value = "4", .descr = "Principal"       , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // AGENT
          case '2': return f.value_atom(1); // CROSS_AS_AGENT
          case '3': return f.value_atom(2); // CROSS_AS_PRINCIPAL
          case '4': return f.value_atom(3); // PRINCIPAL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 30 "LastMkt"
    Field{
      fvar,
      30,
      "LastMkt",
      FieldType::EXCHANGE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 31 "LastPx"
    Field{
      fvar,
      31,
      "LastPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 32 "LastQty"
    Field{
      fvar,
      32,
      "LastQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 33 "NoLinesOfText"
    Field{
      fvar,
      33,
      "NoLinesOfText",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoLinesOfText",
      33,
      nullptr
    },
    //--- Tag# 34 "MsgSeqNum"
    Field{
      fvar,
      34,
      "MsgSeqNum",
      FieldType::SEQNUM,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 35 "MsgType"
    Field{
      fvar,
      35,
      "MsgType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "Heartbeat"                              , .atom = 0}, // 0
        {.value = "1" , .descr = "Testrequest"                            , .atom = 0}, // 1
        {.value = "2" , .descr = "Resendrequest"                          , .atom = 0}, // 2
        {.value = "3" , .descr = "Reject"                                 , .atom = 0}, // 3
        {.value = "4" , .descr = "Sequencereset"                          , .atom = 0}, // 4
        {.value = "5" , .descr = "Logout"                                 , .atom = 0}, // 5
        {.value = "6" , .descr = "Ioi"                                    , .atom = 0}, // 6
        {.value = "7" , .descr = "Advertisement"                          , .atom = 0}, // 7
        {.value = "8" , .descr = "Executionreport"                        , .atom = 0}, // 8
        {.value = "9" , .descr = "Ordercancelreject"                      , .atom = 0}, // 9
        {.value = "A" , .descr = "Logon"                                  , .atom = 0}, // 10
        {.value = "B" , .descr = "News"                                   , .atom = 0}, // 11
        {.value = "C" , .descr = "Email"                                  , .atom = 0}, // 12
        {.value = "D" , .descr = "Newordersingle"                         , .atom = 0}, // 13
        {.value = "E" , .descr = "Neworderlist"                           , .atom = 0}, // 14
        {.value = "F" , .descr = "Ordercancelrequest"                     , .atom = 0}, // 15
        {.value = "G" , .descr = "Ordercancelreplacerequest"              , .atom = 0}, // 16
        {.value = "H" , .descr = "Orderstatusrequest"                     , .atom = 0}, // 17
        {.value = "J" , .descr = "Allocationinstruction"                  , .atom = 0}, // 18
        {.value = "K" , .descr = "Listcancelrequest"                      , .atom = 0}, // 19
        {.value = "L" , .descr = "Listexecute"                            , .atom = 0}, // 20
        {.value = "M" , .descr = "Liststatusrequest"                      , .atom = 0}, // 21
        {.value = "N" , .descr = "Liststatus"                             , .atom = 0}, // 22
        {.value = "P" , .descr = "Allocationinstructionack"               , .atom = 0}, // 23
        {.value = "Q" , .descr = "Dontknowtrade"                          , .atom = 0}, // 24
        {.value = "R" , .descr = "Quoterequest"                           , .atom = 0}, // 25
        {.value = "S" , .descr = "Quote"                                  , .atom = 0}, // 26
        {.value = "T" , .descr = "Settlementinstructions"                 , .atom = 0}, // 27
        {.value = "V" , .descr = "Marketdatarequest"                      , .atom = 0}, // 28
        {.value = "W" , .descr = "Marketdatasnapshotfullrefresh"          , .atom = 0}, // 29
        {.value = "X" , .descr = "Marketdataincrementalrefresh"           , .atom = 0}, // 30
        {.value = "Y" , .descr = "Marketdatarequestreject"                , .atom = 0}, // 31
        {.value = "Z" , .descr = "Quotecancel"                            , .atom = 0}, // 32
        {.value = "a" , .descr = "Quotestatusrequest"                     , .atom = 0}, // 33
        {.value = "b" , .descr = "Massquoteacknowledgement"               , .atom = 0}, // 34
        {.value = "c" , .descr = "Securitydefinitionrequest"              , .atom = 0}, // 35
        {.value = "d" , .descr = "Securitydefinition"                     , .atom = 0}, // 36
        {.value = "e" , .descr = "Securitystatusrequest"                  , .atom = 0}, // 37
        {.value = "f" , .descr = "Securitystatus"                         , .atom = 0}, // 38
        {.value = "g" , .descr = "Tradingsessionstatusrequest"            , .atom = 0}, // 39
        {.value = "h" , .descr = "Tradingsessionstatus"                   , .atom = 0}, // 40
        {.value = "i" , .descr = "Massquote"                              , .atom = 0}, // 41
        {.value = "j" , .descr = "Businessmessagereject"                  , .atom = 0}, // 42
        {.value = "k" , .descr = "Bidrequest"                             , .atom = 0}, // 43
        {.value = "l" , .descr = "Bidresponse"                            , .atom = 0}, // 44
        {.value = "m" , .descr = "Liststrikeprice"                        , .atom = 0}, // 45
        {.value = "o" , .descr = "Registrationinstructions"               , .atom = 0}, // 46
        {.value = "p" , .descr = "Registrationinstructionsresponse"       , .atom = 0}, // 47
        {.value = "q" , .descr = "Ordermasscancelrequest"                 , .atom = 0}, // 48
        {.value = "r" , .descr = "Ordermasscancelreport"                  , .atom = 0}, // 49
        {.value = "s" , .descr = "Newordercross"                          , .atom = 0}, // 50
        {.value = "t" , .descr = "Crossordercancelreplacerequest"         , .atom = 0}, // 51
        {.value = "u" , .descr = "Crossordercancelrequest"                , .atom = 0}, // 52
        {.value = "v" , .descr = "Securitytyperequest"                    , .atom = 0}, // 53
        {.value = "w" , .descr = "Securitytypes"                          , .atom = 0}, // 54
        {.value = "x" , .descr = "Securitylistrequest"                    , .atom = 0}, // 55
        {.value = "y" , .descr = "Securitylist"                           , .atom = 0}, // 56
        {.value = "z" , .descr = "Derivativesecuritylistrequest"          , .atom = 0}, // 57
        {.value = "AA", .descr = "Derivativesecuritylist"                 , .atom = 0}, // 58
        {.value = "AB", .descr = "Newordermultileg"                       , .atom = 0}, // 59
        {.value = "AC", .descr = "Multilegordercancelreplace"             , .atom = 0}, // 60
        {.value = "AD", .descr = "Tradecapturereportrequest"              , .atom = 0}, // 61
        {.value = "AE", .descr = "Tradecapturereport"                     , .atom = 0}, // 62
        {.value = "AF", .descr = "Ordermassstatusrequest"                 , .atom = 0}, // 63
        {.value = "AG", .descr = "Quoterequestreject"                     , .atom = 0}, // 64
        {.value = "AH", .descr = "Rfqrequest"                             , .atom = 0}, // 65
        {.value = "AI", .descr = "Quotestatusreport"                      , .atom = 0}, // 66
        {.value = "AJ", .descr = "Quoteresponse"                          , .atom = 0}, // 67
        {.value = "AK", .descr = "Confirmation"                           , .atom = 0}, // 68
        {.value = "AL", .descr = "Positionmaintenancerequest"             , .atom = 0}, // 69
        {.value = "AM", .descr = "Positionmaintenancereport"              , .atom = 0}, // 70
        {.value = "AN", .descr = "Requestforpositions"                    , .atom = 0}, // 71
        {.value = "AO", .descr = "Requestforpositionsack"                 , .atom = 0}, // 72
        {.value = "AP", .descr = "Positionreport"                         , .atom = 0}, // 73
        {.value = "AQ", .descr = "Tradecapturereportrequestack"           , .atom = 0}, // 74
        {.value = "AR", .descr = "Tradecapturereportack"                  , .atom = 0}, // 75
        {.value = "AS", .descr = "Allocationreport"                       , .atom = 0}, // 76
        {.value = "AT", .descr = "Allocationreportack"                    , .atom = 0}, // 77
        {.value = "AU", .descr = "Confirmationack"                        , .atom = 0}, // 78
        {.value = "AV", .descr = "Settlementinstructionrequest"           , .atom = 0}, // 79
        {.value = "AW", .descr = "Assignmentreport"                       , .atom = 0}, // 80
        {.value = "AX", .descr = "Collateralrequest"                      , .atom = 0}, // 81
        {.value = "AY", .descr = "Collateralassignment"                   , .atom = 0}, // 82
        {.value = "AZ", .descr = "Collateralresponse"                     , .atom = 0}, // 83
        {.value = "BA", .descr = "Collateralreport"                       , .atom = 0}, // 84
        {.value = "BB", .descr = "Collateralinquiry"                      , .atom = 0}, // 85
        {.value = "BC", .descr = "Networkcounterpartysystemstatusrequest" , .atom = 0}, // 86
        {.value = "BD", .descr = "Networkcounterpartysystemstatusresponse", .atom = 0}, // 87
        {.value = "BE", .descr = "Userrequest"                            , .atom = 0}, // 88
        {.value = "BF", .descr = "Userresponse"                           , .atom = 0}, // 89
        {.value = "BG", .descr = "Collateralinquiryack"                   , .atom = 0}, // 90
        {.value = "BH", .descr = "Confirmationrequest"                    , .atom = 0}, // 91
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom( 0); // Heartbeat
          case           '1': return f.value_atom( 1); // Testrequest
          case           '2': return f.value_atom( 2); // Resendrequest
          case           '3': return f.value_atom( 3); // Reject
          case           '4': return f.value_atom( 4); // Sequencereset
          case           '5': return f.value_atom( 5); // Logout
          case           '6': return f.value_atom( 6); // Ioi
          case           '7': return f.value_atom( 7); // Advertisement
          case           '8': return f.value_atom( 8); // Executionreport
          case           '9': return f.value_atom( 9); // Ordercancelreject
          case           'A': return f.value_atom(10); // Logon
          case           'B': return f.value_atom(11); // News
          case           'C': return f.value_atom(12); // Email
          case           'D': return f.value_atom(13); // Newordersingle
          case           'E': return f.value_atom(14); // Neworderlist
          case           'F': return f.value_atom(15); // Ordercancelrequest
          case           'G': return f.value_atom(16); // Ordercancelreplacerequest
          case           'H': return f.value_atom(17); // Orderstatusrequest
          case           'J': return f.value_atom(18); // Allocationinstruction
          case           'K': return f.value_atom(19); // Listcancelrequest
          case           'L': return f.value_atom(20); // Listexecute
          case           'M': return f.value_atom(21); // Liststatusrequest
          case           'N': return f.value_atom(22); // Liststatus
          case           'P': return f.value_atom(23); // Allocationinstructionack
          case           'Q': return f.value_atom(24); // Dontknowtrade
          case           'R': return f.value_atom(25); // Quoterequest
          case           'S': return f.value_atom(26); // Quote
          case           'T': return f.value_atom(27); // Settlementinstructions
          case           'V': return f.value_atom(28); // Marketdatarequest
          case           'W': return f.value_atom(29); // Marketdatasnapshotfullrefresh
          case           'X': return f.value_atom(30); // Marketdataincrementalrefresh
          case           'Y': return f.value_atom(31); // Marketdatarequestreject
          case           'Z': return f.value_atom(32); // Quotecancel
          case           'a': return f.value_atom(33); // Quotestatusrequest
          case           'b': return f.value_atom(34); // Massquoteacknowledgement
          case           'c': return f.value_atom(35); // Securitydefinitionrequest
          case           'd': return f.value_atom(36); // Securitydefinition
          case           'e': return f.value_atom(37); // Securitystatusrequest
          case           'f': return f.value_atom(38); // Securitystatus
          case           'g': return f.value_atom(39); // Tradingsessionstatusrequest
          case           'h': return f.value_atom(40); // Tradingsessionstatus
          case           'i': return f.value_atom(41); // Massquote
          case           'j': return f.value_atom(42); // Businessmessagereject
          case           'k': return f.value_atom(43); // Bidrequest
          case           'l': return f.value_atom(44); // Bidresponse
          case           'm': return f.value_atom(45); // Liststrikeprice
          case           'o': return f.value_atom(46); // Registrationinstructions
          case           'p': return f.value_atom(47); // Registrationinstructionsresponse
          case           'q': return f.value_atom(48); // Ordermasscancelrequest
          case           'r': return f.value_atom(49); // Ordermasscancelreport
          case           's': return f.value_atom(50); // Newordercross
          case           't': return f.value_atom(51); // Crossordercancelreplacerequest
          case           'u': return f.value_atom(52); // Crossordercancelrequest
          case           'v': return f.value_atom(53); // Securitytyperequest
          case           'w': return f.value_atom(54); // Securitytypes
          case           'x': return f.value_atom(55); // Securitylistrequest
          case           'y': return f.value_atom(56); // Securitylist
          case           'z': return f.value_atom(57); // Derivativesecuritylistrequest
          case CINT<'A','A'>: return f.value_atom(58); // Derivativesecuritylist
          case CINT<'A','B'>: return f.value_atom(59); // Newordermultileg
          case CINT<'A','C'>: return f.value_atom(60); // Multilegordercancelreplace
          case CINT<'A','D'>: return f.value_atom(61); // Tradecapturereportrequest
          case CINT<'A','E'>: return f.value_atom(62); // Tradecapturereport
          case CINT<'A','F'>: return f.value_atom(63); // Ordermassstatusrequest
          case CINT<'A','G'>: return f.value_atom(64); // Quoterequestreject
          case CINT<'A','H'>: return f.value_atom(65); // Rfqrequest
          case CINT<'A','I'>: return f.value_atom(66); // Quotestatusreport
          case CINT<'A','J'>: return f.value_atom(67); // Quoteresponse
          case CINT<'A','K'>: return f.value_atom(68); // Confirmation
          case CINT<'A','L'>: return f.value_atom(69); // Positionmaintenancerequest
          case CINT<'A','M'>: return f.value_atom(70); // Positionmaintenancereport
          case CINT<'A','N'>: return f.value_atom(71); // Requestforpositions
          case CINT<'A','O'>: return f.value_atom(72); // Requestforpositionsack
          case CINT<'A','P'>: return f.value_atom(73); // Positionreport
          case CINT<'A','Q'>: return f.value_atom(74); // Tradecapturereportrequestack
          case CINT<'A','R'>: return f.value_atom(75); // Tradecapturereportack
          case CINT<'A','S'>: return f.value_atom(76); // Allocationreport
          case CINT<'A','T'>: return f.value_atom(77); // Allocationreportack
          case CINT<'A','U'>: return f.value_atom(78); // Confirmationack
          case CINT<'A','V'>: return f.value_atom(79); // Settlementinstructionrequest
          case CINT<'A','W'>: return f.value_atom(80); // Assignmentreport
          case CINT<'A','X'>: return f.value_atom(81); // Collateralrequest
          case CINT<'A','Y'>: return f.value_atom(82); // Collateralassignment
          case CINT<'A','Z'>: return f.value_atom(83); // Collateralresponse
          case CINT<'B','A'>: return f.value_atom(84); // Collateralreport
          case CINT<'B','B'>: return f.value_atom(85); // Collateralinquiry
          case CINT<'B','C'>: return f.value_atom(86); // Networkcounterpartysystemstatusrequest
          case CINT<'B','D'>: return f.value_atom(87); // Networkcounterpartysystemstatusresponse
          case CINT<'B','E'>: return f.value_atom(88); // Userrequest
          case CINT<'B','F'>: return f.value_atom(89); // Userresponse
          case CINT<'B','G'>: return f.value_atom(90); // Collateralinquiryack
          case CINT<'B','H'>: return f.value_atom(91); // Confirmationrequest
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 36 "NewSeqNo"
    Field{
      fvar,
      36,
      "NewSeqNo",
      FieldType::SEQNUM,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 37 "OrderID"
    Field{
      fvar,
      37,
      "OrderID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 38 "OrderQty"
    Field{
      fvar,
      38,
      "OrderQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 39 "OrdStatus"
    Field{
      fvar,
      39,
      "OrdStatus",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "New"               , .atom = 0}, // 0
        {.value = "1", .descr = "PartiallyFilled"   , .atom = 0}, // 1
        {.value = "2", .descr = "Filled"            , .atom = 0}, // 2
        {.value = "3", .descr = "DoneForDay"        , .atom = 0}, // 3
        {.value = "4", .descr = "Canceled"          , .atom = 0}, // 4
        {.value = "6", .descr = "PendingCancel"     , .atom = 0}, // 5
        {.value = "7", .descr = "Stopped"           , .atom = 0}, // 6
        {.value = "8", .descr = "Rejected"          , .atom = 0}, // 7
        {.value = "9", .descr = "Suspended"         , .atom = 0}, // 8
        {.value = "A", .descr = "PendingNew"        , .atom = 0}, // 9
        {.value = "B", .descr = "Calculated"        , .atom = 0}, // 10
        {.value = "C", .descr = "Expired"           , .atom = 0}, // 11
        {.value = "D", .descr = "AcceptedForBidding", .atom = 0}, // 12
        {.value = "E", .descr = "PendingReplace"    , .atom = 0}, // 13
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NEW
          case '1': return f.value_atom(1); // PARTIALLY_FILLED
          case '2': return f.value_atom(2); // FILLED
          case '3': return f.value_atom(3); // DONE_FOR_DAY
          case '4': return f.value_atom(4); // CANCELED
          case '6': return f.value_atom(5); // PENDING_CANCEL
          case '7': return f.value_atom(6); // STOPPED
          case '8': return f.value_atom(7); // REJECTED
          case '9': return f.value_atom(8); // SUSPENDED
          case 'A': return f.value_atom(9); // PENDING_NEW
          case 'B': return f.value_atom(10); // CALCULATED
          case 'C': return f.value_atom(11); // EXPIRED
          case 'D': return f.value_atom(12); // ACCEPTED_FOR_BIDDING
          case 'E': return f.value_atom(13); // PENDING_REPLACE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 40 "OrdType"
    Field{
      fvar,
      40,
      "OrdType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Market"                    , .atom = 0}, // 0
        {.value = "2", .descr = "Limit"                     , .atom = 0}, // 1
        {.value = "3", .descr = "Stop"                      , .atom = 0}, // 2
        {.value = "4", .descr = "StopLimit"                 , .atom = 0}, // 3
        {.value = "6", .descr = "WithOrWithout"             , .atom = 0}, // 4
        {.value = "7", .descr = "LimitOrBetter"             , .atom = 0}, // 5
        {.value = "8", .descr = "LimitWithOrWithout"        , .atom = 0}, // 6
        {.value = "9", .descr = "OnBasis"                   , .atom = 0}, // 7
        {.value = "D", .descr = "PreviouslyQuoted"          , .atom = 0}, // 8
        {.value = "E", .descr = "PreviouslyIndicated"       , .atom = 0}, // 9
        {.value = "G", .descr = "Forex"                     , .atom = 0}, // 10
        {.value = "I", .descr = "Funari"                    , .atom = 0}, // 11
        {.value = "J", .descr = "MarketIfTouched"           , .atom = 0}, // 12
        {.value = "K", .descr = "MarketWithLeftoverAsLimit" , .atom = 0}, // 13
        {.value = "L", .descr = "PreviousFundValuationPoint", .atom = 0}, // 14
        {.value = "M", .descr = "NextFundValuationPoint"    , .atom = 0}, // 15
        {.value = "P", .descr = "Pegged"                    , .atom = 0}, // 16
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // MARKET
          case '2': return f.value_atom(1); // LIMIT
          case '3': return f.value_atom(2); // STOP
          case '4': return f.value_atom(3); // STOP_LIMIT
          case '6': return f.value_atom(4); // WITH_OR_WITHOUT
          case '7': return f.value_atom(5); // LIMIT_OR_BETTER
          case '8': return f.value_atom(6); // LIMIT_WITH_OR_WITHOUT
          case '9': return f.value_atom(7); // ON_BASIS
          case 'D': return f.value_atom(8); // PREVIOUSLY_QUOTED
          case 'E': return f.value_atom(9); // PREVIOUSLY_INDICATED
          case 'G': return f.value_atom(10); // FOREX
          case 'I': return f.value_atom(11); // FUNARI
          case 'J': return f.value_atom(12); // MARKET_IF_TOUCHED
          case 'K': return f.value_atom(13); // MARKET_WITH_LEFTOVER_AS_LIMIT
          case 'L': return f.value_atom(14); // PREVIOUS_FUND_VALUATION_POINT
          case 'M': return f.value_atom(15); // NEXT_FUND_VALUATION_POINT
          case 'P': return f.value_atom(16); // PEGGED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 41 "OrigClOrdID"
    Field{
      fvar,
      41,
      "OrigClOrdID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 42 "OrigTime"
    Field{
      fvar,
      42,
      "OrigTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 43 "PossDupFlag"
    Field{
      fvar,
      43,
      "PossDupFlag",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 44 "Price"
    Field{
      fvar,
      44,
      "Price",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 45 "RefSeqNum"
    Field{
      fvar,
      45,
      "RefSeqNum",
      FieldType::SEQNUM,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 46
    Field{},
    //--- Tag# 47
    Field{},
    //--- Tag# 48 "SecurityID"
    Field{
      fvar,
      48,
      "SecurityID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 49 "SenderCompID"
    Field{
      fvar,
      49,
      "SenderCompID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 50 "SenderSubID"
    Field{
      fvar,
      50,
      "SenderSubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 51
    Field{},
    //--- Tag# 52 "SendingTime"
    Field{
      fvar,
      52,
      "SendingTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 53 "Quantity"
    Field{
      fvar,
      53,
      "Quantity",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 54 "Side"
    Field{
      fvar,
      54,
      "Side",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Buy"             , .atom = 0}, // 0
        {.value = "2", .descr = "Sell"            , .atom = 0}, // 1
        {.value = "3", .descr = "BuyMinus"        , .atom = 0}, // 2
        {.value = "4", .descr = "SellPlus"        , .atom = 0}, // 3
        {.value = "5", .descr = "SellShort"       , .atom = 0}, // 4
        {.value = "6", .descr = "SellShortExempt" , .atom = 0}, // 5
        {.value = "7", .descr = "Undisclosed"     , .atom = 0}, // 6
        {.value = "8", .descr = "Cross"           , .atom = 0}, // 7
        {.value = "9", .descr = "CrossShort"      , .atom = 0}, // 8
        {.value = "A", .descr = "CrossShortExempt", .atom = 0}, // 9
        {.value = "B", .descr = "AsDefined"       , .atom = 0}, // 10
        {.value = "C", .descr = "Opposite"        , .atom = 0}, // 11
        {.value = "D", .descr = "Subscribe"       , .atom = 0}, // 12
        {.value = "E", .descr = "Redeem"          , .atom = 0}, // 13
        {.value = "F", .descr = "Lend"            , .atom = 0}, // 14
        {.value = "G", .descr = "Borrow"          , .atom = 0}, // 15
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // BUY
          case '2': return f.value_atom(1); // SELL
          case '3': return f.value_atom(2); // BUY_MINUS
          case '4': return f.value_atom(3); // SELL_PLUS
          case '5': return f.value_atom(4); // SELL_SHORT
          case '6': return f.value_atom(5); // SELL_SHORT_EXEMPT
          case '7': return f.value_atom(6); // UNDISCLOSED
          case '8': return f.value_atom(7); // CROSS
          case '9': return f.value_atom(8); // CROSS_SHORT
          case 'A': return f.value_atom(9); // CROSS_SHORT_EXEMPT
          case 'B': return f.value_atom(10); // AS_DEFINED
          case 'C': return f.value_atom(11); // OPPOSITE
          case 'D': return f.value_atom(12); // SUBSCRIBE
          case 'E': return f.value_atom(13); // REDEEM
          case 'F': return f.value_atom(14); // LEND
          case 'G': return f.value_atom(15); // BORROW
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 55 "Symbol"
    Field{
      fvar,
      55,
      "Symbol",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 56 "TargetCompID"
    Field{
      fvar,
      56,
      "TargetCompID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 57 "TargetSubID"
    Field{
      fvar,
      57,
      "TargetSubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 58 "Text"
    Field{
      fvar,
      58,
      "Text",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 59 "TimeInForce"
    Field{
      fvar,
      59,
      "TimeInForce",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Day"              , .atom = 0}, // 0
        {.value = "1", .descr = "GoodTillCancel"   , .atom = 0}, // 1
        {.value = "2", .descr = "AtTheOpening"     , .atom = 0}, // 2
        {.value = "3", .descr = "ImmediateOrCancel", .atom = 0}, // 3
        {.value = "4", .descr = "FillOrKill"       , .atom = 0}, // 4
        {.value = "5", .descr = "GoodTillCrossing" , .atom = 0}, // 5
        {.value = "6", .descr = "GoodTillDate"     , .atom = 0}, // 6
        {.value = "7", .descr = "AtTheClose"       , .atom = 0}, // 7
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // DAY
          case '1': return f.value_atom(1); // GOOD_TILL_CANCEL
          case '2': return f.value_atom(2); // AT_THE_OPENING
          case '3': return f.value_atom(3); // IMMEDIATE_OR_CANCEL
          case '4': return f.value_atom(4); // FILL_OR_KILL
          case '5': return f.value_atom(5); // GOOD_TILL_CROSSING
          case '6': return f.value_atom(6); // GOOD_TILL_DATE
          case '7': return f.value_atom(7); // AT_THE_CLOSE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 60 "TransactTime"
    Field{
      fvar,
      60,
      "TransactTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 61 "Urgency"
    Field{
      fvar,
      61,
      "Urgency",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Normal"    , .atom = 0}, // 0
        {.value = "1", .descr = "Flash"     , .atom = 0}, // 1
        {.value = "2", .descr = "Background", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NORMAL
          case '1': return f.value_atom(1); // FLASH
          case '2': return f.value_atom(2); // BACKGROUND
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 62 "ValidUntilTime"
    Field{
      fvar,
      62,
      "ValidUntilTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 63 "SettlType"
    Field{
      fvar,
      63,
      "SettlType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Regular"        , .atom = 0}, // 0
        {.value = "1", .descr = "Cash"           , .atom = 0}, // 1
        {.value = "2", .descr = "NextDay"        , .atom = 0}, // 2
        {.value = "3", .descr = "TPlus2"         , .atom = 0}, // 3
        {.value = "4", .descr = "TPlus3"         , .atom = 0}, // 4
        {.value = "5", .descr = "TPlus4"         , .atom = 0}, // 5
        {.value = "6", .descr = "Future"         , .atom = 0}, // 6
        {.value = "7", .descr = "WhenAndIfIssued", .atom = 0}, // 7
        {.value = "8", .descr = "SellersOption"  , .atom = 0}, // 8
        {.value = "9", .descr = "TPlus5"         , .atom = 0}, // 9
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // REGULAR
          case '1': return f.value_atom(1); // CASH
          case '2': return f.value_atom(2); // NEXT_DAY
          case '3': return f.value_atom(3); // T_PLUS_2
          case '4': return f.value_atom(4); // T_PLUS_3
          case '5': return f.value_atom(5); // T_PLUS_4
          case '6': return f.value_atom(6); // FUTURE
          case '7': return f.value_atom(7); // WHEN_AND_IF_ISSUED
          case '8': return f.value_atom(8); // SELLERS_OPTION
          case '9': return f.value_atom(9); // T_PLUS_5
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 64 "SettlDate"
    Field{
      fvar,
      64,
      "SettlDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 65 "SymbolSfx"
    Field{
      fvar,
      65,
      "SymbolSfx",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 66 "ListID"
    Field{
      fvar,
      66,
      "ListID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 67 "ListSeqNo"
    Field{
      fvar,
      67,
      "ListSeqNo",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 68 "TotNoOrders"
    Field{
      fvar,
      68,
      "TotNoOrders",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 69 "ListExecInst"
    Field{
      fvar,
      69,
      "ListExecInst",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 70 "AllocID"
    Field{
      fvar,
      70,
      "AllocID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 71 "AllocTransType"
    Field{
      fvar,
      71,
      "AllocTransType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "New"    , .atom = 0}, // 0
        {.value = "1", .descr = "Replace", .atom = 0}, // 1
        {.value = "2", .descr = "Cancel" , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NEW
          case '1': return f.value_atom(1); // REPLACE
          case '2': return f.value_atom(2); // CANCEL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 72 "RefAllocID"
    Field{
      fvar,
      72,
      "RefAllocID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 73 "NoOrders"
    Field{
      fvar,
      73,
      "NoOrders",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoOrders",
      73,
      nullptr
    },
    //--- Tag# 74 "AvgPxPrecision"
    Field{
      fvar,
      74,
      "AvgPxPrecision",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 75 "TradeDate"
    Field{
      fvar,
      75,
      "TradeDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 76
    Field{},
    //--- Tag# 77 "PositionEffect"
    Field{
      fvar,
      77,
      "PositionEffect",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "O", .descr = "Open"  , .atom = 0}, // 0
        {.value = "C", .descr = "Close" , .atom = 0}, // 1
        {.value = "R", .descr = "Rolled", .atom = 0}, // 2
        {.value = "F", .descr = "Fifo"  , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'O': return f.value_atom(0); // OPEN
          case 'C': return f.value_atom(1); // CLOSE
          case 'R': return f.value_atom(2); // ROLLED
          case 'F': return f.value_atom(3); // FIFO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 78 "NoAllocs"
    Field{
      fvar,
      78,
      "NoAllocs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoAllocs",
      78,
      nullptr
    },
    //--- Tag# 79 "AllocAccount"
    Field{
      fvar,
      79,
      "AllocAccount",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 80 "AllocQty"
    Field{
      fvar,
      80,
      "AllocQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 81 "ProcessCode"
    Field{
      fvar,
      81,
      "ProcessCode",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Regular"          , .atom = 0}, // 0
        {.value = "1", .descr = "SoftDollar"       , .atom = 0}, // 1
        {.value = "2", .descr = "StepIn"           , .atom = 0}, // 2
        {.value = "3", .descr = "StepOut"          , .atom = 0}, // 3
        {.value = "4", .descr = "SoftDollarStepIn" , .atom = 0}, // 4
        {.value = "5", .descr = "SoftDollarStepOut", .atom = 0}, // 5
        {.value = "6", .descr = "PlanSponsor"      , .atom = 0}, // 6
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // REGULAR
          case '1': return f.value_atom(1); // SOFT_DOLLAR
          case '2': return f.value_atom(2); // STEP_IN
          case '3': return f.value_atom(3); // STEP_OUT
          case '4': return f.value_atom(4); // SOFT_DOLLAR_STEP_IN
          case '5': return f.value_atom(5); // SOFT_DOLLAR_STEP_OUT
          case '6': return f.value_atom(6); // PLAN_SPONSOR
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 82 "NoRpts"
    Field{
      fvar,
      82,
      "NoRpts",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 83 "RptSeq"
    Field{
      fvar,
      83,
      "RptSeq",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 84 "CxlQty"
    Field{
      fvar,
      84,
      "CxlQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 85 "NoDlvyInst"
    Field{
      fvar,
      85,
      "NoDlvyInst",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoDlvyInst",
      85,
      nullptr
    },
    //--- Tag# 86
    Field{},
    //--- Tag# 87 "AllocStatus"
    Field{
      fvar,
      87,
      "AllocStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Accepted"              , .atom = 0}, // 0
        {.value = "1", .descr = "BlockLevelReject"      , .atom = 0}, // 1
        {.value = "2", .descr = "AccountLevelReject"    , .atom = 0}, // 2
        {.value = "3", .descr = "Received"              , .atom = 0}, // 3
        {.value = "4", .descr = "Incomplete"            , .atom = 0}, // 4
        {.value = "5", .descr = "RejectedByIntermediary", .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // ACCEPTED
          case '1': return f.value_atom(1); // BLOCK_LEVEL_REJECT
          case '2': return f.value_atom(2); // ACCOUNT_LEVEL_REJECT
          case '3': return f.value_atom(3); // RECEIVED
          case '4': return f.value_atom(4); // INCOMPLETE
          case '5': return f.value_atom(5); // REJECTED_BY_INTERMEDIARY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 88 "AllocRejCode"
    Field{
      fvar,
      88,
      "AllocRejCode",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "UnknownAccount"                , .atom = 0}, // 0
        {.value = "1" , .descr = "IncorrectQuantity"             , .atom = 0}, // 1
        {.value = "2" , .descr = "IncorrectAveragePrice"         , .atom = 0}, // 2
        {.value = "3" , .descr = "UnknownExecutingBrokerMnemonic", .atom = 0}, // 3
        {.value = "4" , .descr = "CommissionDifference"          , .atom = 0}, // 4
        {.value = "5" , .descr = "UnknownOrderid"                , .atom = 0}, // 5
        {.value = "6" , .descr = "UnknownListid"                 , .atom = 0}, // 6
        {.value = "7" , .descr = "Other"                         , .atom = 0}, // 7
        {.value = "8" , .descr = "IncorrectAllocatedQuantity"    , .atom = 0}, // 8
        {.value = "9" , .descr = "CalculationDifference"         , .atom = 0}, // 9
        {.value = "10", .descr = "UnknownOrStaleExecid"          , .atom = 0}, // 10
        {.value = "11", .descr = "MismatchedDataValue"           , .atom = 0}, // 11
        {.value = "12", .descr = "UnknownClordid"                , .atom = 0}, // 12
        {.value = "13", .descr = "WarehouseRequestRejected"      , .atom = 0}, // 13
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom( 0); // UnknownAccount
          case           '1': return f.value_atom( 1); // IncorrectQuantity
          case           '2': return f.value_atom( 2); // IncorrectAveragePrice
          case           '3': return f.value_atom( 3); // UnknownExecutingBrokerMnemonic
          case           '4': return f.value_atom( 4); // CommissionDifference
          case           '5': return f.value_atom( 5); // UnknownOrderid
          case           '6': return f.value_atom( 6); // UnknownListid
          case           '7': return f.value_atom( 7); // Other
          case           '8': return f.value_atom( 8); // IncorrectAllocatedQuantity
          case           '9': return f.value_atom( 9); // CalculationDifference
          case CINT<'1','0'>: return f.value_atom(10); // UnknownOrStaleExecid
          case CINT<'1','1'>: return f.value_atom(11); // MismatchedDataValue
          case CINT<'1','2'>: return f.value_atom(12); // UnknownClordid
          case CINT<'1','3'>: return f.value_atom(13); // WarehouseRequestRejected
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 89 "Signature"
    Field{
      fvar,
      89,
      "Signature",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "SignatureLength",
      93,
      nullptr
    },
    //--- Tag# 90 "SecureDataLen"
    Field{
      fvar,
      90,
      "SecureDataLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 91 "SecureData"
    Field{
      fvar,
      91,
      "SecureData",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "SecureDataLen",
      90,
      nullptr
    },
    //--- Tag# 92
    Field{},
    //--- Tag# 93 "SignatureLength"
    Field{
      fvar,
      93,
      "SignatureLength",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 94 "EmailType"
    Field{
      fvar,
      94,
      "EmailType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "New"       , .atom = 0}, // 0
        {.value = "1", .descr = "Reply"     , .atom = 0}, // 1
        {.value = "2", .descr = "AdminReply", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NEW
          case '1': return f.value_atom(1); // REPLY
          case '2': return f.value_atom(2); // ADMIN_REPLY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 95 "RawDataLength"
    Field{
      fvar,
      95,
      "RawDataLength",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 96 "RawData"
    Field{
      fvar,
      96,
      "RawData",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "RawDataLength",
      95,
      nullptr
    },
    //--- Tag# 97 "PossResend"
    Field{
      fvar,
      97,
      "PossResend",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 98 "EncryptMethod"
    Field{
      fvar,
      98,
      "EncryptMethod",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "None"     , .atom = 0}, // 0
        {.value = "1", .descr = "Pkcs"     , .atom = 0}, // 1
        {.value = "2", .descr = "Des"      , .atom = 0}, // 2
        {.value = "3", .descr = "PkcsDes"  , .atom = 0}, // 3
        {.value = "4", .descr = "PgpDes"   , .atom = 0}, // 4
        {.value = "5", .descr = "PgpDesMd5", .atom = 0}, // 5
        {.value = "6", .descr = "PemDesMd5", .atom = 0}, // 6
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NONE
          case '1': return f.value_atom(1); // PKCS
          case '2': return f.value_atom(2); // DES
          case '3': return f.value_atom(3); // PKCS_DES
          case '4': return f.value_atom(4); // PGP_DES
          case '5': return f.value_atom(5); // PGP_DES_MD5
          case '6': return f.value_atom(6); // PEM_DES_MD5
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 99 "StopPx"
    Field{
      fvar,
      99,
      "StopPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 100 "ExDestination"
    Field{
      fvar,
      100,
      "ExDestination",
      FieldType::EXCHANGE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 101
    Field{},
    //--- Tag# 102 "CxlRejReason"
    Field{
      fvar,
      102,
      "CxlRejReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "TooLateToCancel"                                  , .atom = 0}, // 0
        {.value = "1" , .descr = "UnknownOrder"                                     , .atom = 0}, // 1
        {.value = "2" , .descr = "Broker"                                           , .atom = 0}, // 2
        {.value = "3" , .descr = "OrderAlreadyInPendingCancelOrPendingReplaceStatus", .atom = 0}, // 3
        {.value = "4" , .descr = "UnableToProcessOrderMassCancelRequest"            , .atom = 0}, // 4
        {.value = "5" , .descr = "Origordmodtime"                                   , .atom = 0}, // 5
        {.value = "6" , .descr = "DuplicateClordid"                                 , .atom = 0}, // 6
        {.value = "99", .descr = "Other"                                            , .atom = 0}, // 7
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom(0); // TooLateToCancel
          case           '1': return f.value_atom(1); // UnknownOrder
          case           '2': return f.value_atom(2); // Broker
          case           '3': return f.value_atom(3); // OrderAlreadyInPendingCancelOrPendingReplaceStatus
          case           '4': return f.value_atom(4); // UnableToProcessOrderMassCancelRequest
          case           '5': return f.value_atom(5); // Origordmodtime
          case           '6': return f.value_atom(6); // DuplicateClordid
          case CINT<'9','9'>: return f.value_atom(7); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 103 "OrdRejReason"
    Field{
      fvar,
      103,
      "OrdRejReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "Broker"                                            , .atom = 0}, // 0
        {.value = "1" , .descr = "UnknownSymbol"                                     , .atom = 0}, // 1
        {.value = "2" , .descr = "ExchangeClosed"                                    , .atom = 0}, // 2
        {.value = "3" , .descr = "OrderExceedsLimit"                                 , .atom = 0}, // 3
        {.value = "4" , .descr = "TooLateToEnter"                                    , .atom = 0}, // 4
        {.value = "5" , .descr = "UnknownOrder"                                      , .atom = 0}, // 5
        {.value = "6" , .descr = "DuplicateOrder"                                    , .atom = 0}, // 6
        {.value = "7" , .descr = "DuplicateOfAVerballyCommunicatedOrder"             , .atom = 0}, // 7
        {.value = "8" , .descr = "StaleOrder"                                        , .atom = 0}, // 8
        {.value = "9" , .descr = "TradeAlongRequired"                                , .atom = 0}, // 9
        {.value = "10", .descr = "InvalidInvestorId"                                 , .atom = 0}, // 10
        {.value = "11", .descr = "UnsupportedOrderCharacteristic12SurveillenceOption", .atom = 0}, // 11
        {.value = "13", .descr = "IncorrectQuantity"                                 , .atom = 0}, // 12
        {.value = "14", .descr = "IncorrectAllocatedQuantity"                        , .atom = 0}, // 13
        {.value = "15", .descr = "UnknownAccount"                                    , .atom = 0}, // 14
        {.value = "99", .descr = "Other"                                             , .atom = 0}, // 15
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom( 0); // Broker
          case           '1': return f.value_atom( 1); // UnknownSymbol
          case           '2': return f.value_atom( 2); // ExchangeClosed
          case           '3': return f.value_atom( 3); // OrderExceedsLimit
          case           '4': return f.value_atom( 4); // TooLateToEnter
          case           '5': return f.value_atom( 5); // UnknownOrder
          case           '6': return f.value_atom( 6); // DuplicateOrder
          case           '7': return f.value_atom( 7); // DuplicateOfAVerballyCommunicatedOrder
          case           '8': return f.value_atom( 8); // StaleOrder
          case           '9': return f.value_atom( 9); // TradeAlongRequired
          case CINT<'1','0'>: return f.value_atom(10); // InvalidInvestorId
          case CINT<'1','1'>: return f.value_atom(11); // UnsupportedOrderCharacteristic12SurveillenceOption
          case CINT<'1','3'>: return f.value_atom(12); // IncorrectQuantity
          case CINT<'1','4'>: return f.value_atom(13); // IncorrectAllocatedQuantity
          case CINT<'1','5'>: return f.value_atom(14); // UnknownAccount
          case CINT<'9','9'>: return f.value_atom(15); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 104 "IOIQualifier"
    Field{
      fvar,
      104,
      "IOIQualifier",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "A", .descr = "AllOrNone"          , .atom = 0}, // 0
        {.value = "B", .descr = "MarketOnClose"      , .atom = 0}, // 1
        {.value = "C", .descr = "AtTheClose"         , .atom = 0}, // 2
        {.value = "D", .descr = "Vwap"               , .atom = 0}, // 3
        {.value = "I", .descr = "InTouchWith"        , .atom = 0}, // 4
        {.value = "L", .descr = "Limit"              , .atom = 0}, // 5
        {.value = "M", .descr = "MoreBehind"         , .atom = 0}, // 6
        {.value = "O", .descr = "AtTheOpen"          , .atom = 0}, // 7
        {.value = "P", .descr = "TakingAPosition"    , .atom = 0}, // 8
        {.value = "Q", .descr = "AtTheMarket"        , .atom = 0}, // 9
        {.value = "R", .descr = "ReadyToTrade"       , .atom = 0}, // 10
        {.value = "S", .descr = "PortfolioShown"     , .atom = 0}, // 11
        {.value = "T", .descr = "ThroughTheDay"      , .atom = 0}, // 12
        {.value = "V", .descr = "Versus"             , .atom = 0}, // 13
        {.value = "W", .descr = "Indication"         , .atom = 0}, // 14
        {.value = "X", .descr = "CrossingOpportunity", .atom = 0}, // 15
        {.value = "Y", .descr = "AtTheMidpoint"      , .atom = 0}, // 16
        {.value = "Z", .descr = "PreOpen"            , .atom = 0}, // 17
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'A': return f.value_atom(0); // ALL_OR_NONE
          case 'B': return f.value_atom(1); // MARKET_ON_CLOSE
          case 'C': return f.value_atom(2); // AT_THE_CLOSE
          case 'D': return f.value_atom(3); // VWAP
          case 'I': return f.value_atom(4); // IN_TOUCH_WITH
          case 'L': return f.value_atom(5); // LIMIT
          case 'M': return f.value_atom(6); // MORE_BEHIND
          case 'O': return f.value_atom(7); // AT_THE_OPEN
          case 'P': return f.value_atom(8); // TAKING_A_POSITION
          case 'Q': return f.value_atom(9); // AT_THE_MARKET
          case 'R': return f.value_atom(10); // READY_TO_TRADE
          case 'S': return f.value_atom(11); // PORTFOLIO_SHOWN
          case 'T': return f.value_atom(12); // THROUGH_THE_DAY
          case 'V': return f.value_atom(13); // VERSUS
          case 'W': return f.value_atom(14); // INDICATION
          case 'X': return f.value_atom(15); // CROSSING_OPPORTUNITY
          case 'Y': return f.value_atom(16); // AT_THE_MIDPOINT
          case 'Z': return f.value_atom(17); // PRE_OPEN
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 105
    Field{},
    //--- Tag# 106 "Issuer"
    Field{
      fvar,
      106,
      "Issuer",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 107 "SecurityDesc"
    Field{
      fvar,
      107,
      "SecurityDesc",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 108 "HeartBtInt"
    Field{
      fvar,
      108,
      "HeartBtInt",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 109
    Field{},
    //--- Tag# 110 "MinQty"
    Field{
      fvar,
      110,
      "MinQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 111 "MaxFloor"
    Field{
      fvar,
      111,
      "MaxFloor",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 112 "TestReqID"
    Field{
      fvar,
      112,
      "TestReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 113 "ReportToExch"
    Field{
      fvar,
      113,
      "ReportToExch",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 114 "LocateReqd"
    Field{
      fvar,
      114,
      "LocateReqd",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 115 "OnBehalfOfCompID"
    Field{
      fvar,
      115,
      "OnBehalfOfCompID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 116 "OnBehalfOfSubID"
    Field{
      fvar,
      116,
      "OnBehalfOfSubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 117 "QuoteID"
    Field{
      fvar,
      117,
      "QuoteID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 118 "NetMoney"
    Field{
      fvar,
      118,
      "NetMoney",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 119 "SettlCurrAmt"
    Field{
      fvar,
      119,
      "SettlCurrAmt",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 120 "SettlCurrency"
    Field{
      fvar,
      120,
      "SettlCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 121 "ForexReq"
    Field{
      fvar,
      121,
      "ForexReq",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 122 "OrigSendingTime"
    Field{
      fvar,
      122,
      "OrigSendingTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 123 "GapFillFlag"
    Field{
      fvar,
      123,
      "GapFillFlag",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 124 "NoExecs"
    Field{
      fvar,
      124,
      "NoExecs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoExecs",
      124,
      nullptr
    },
    //--- Tag# 125
    Field{},
    //--- Tag# 126 "ExpireTime"
    Field{
      fvar,
      126,
      "ExpireTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 127 "DKReason"
    Field{
      fvar,
      127,
      "DKReason",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "A", .descr = "UnknownSymbol"        , .atom = 0}, // 0
        {.value = "B", .descr = "WrongSide"            , .atom = 0}, // 1
        {.value = "C", .descr = "QuantityExceedsOrder" , .atom = 0}, // 2
        {.value = "D", .descr = "NoMatchingOrder"      , .atom = 0}, // 3
        {.value = "E", .descr = "PriceExceedsLimit"    , .atom = 0}, // 4
        {.value = "F", .descr = "CalculationDifference", .atom = 0}, // 5
        {.value = "Z", .descr = "Other"                , .atom = 0}, // 6
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'A': return f.value_atom(0); // UNKNOWN_SYMBOL
          case 'B': return f.value_atom(1); // WRONG_SIDE
          case 'C': return f.value_atom(2); // QUANTITY_EXCEEDS_ORDER
          case 'D': return f.value_atom(3); // NO_MATCHING_ORDER
          case 'E': return f.value_atom(4); // PRICE_EXCEEDS_LIMIT
          case 'F': return f.value_atom(5); // CALCULATION_DIFFERENCE
          case 'Z': return f.value_atom(6); // OTHER
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 128 "DeliverToCompID"
    Field{
      fvar,
      128,
      "DeliverToCompID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 129 "DeliverToSubID"
    Field{
      fvar,
      129,
      "DeliverToSubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 130 "IOINaturalFlag"
    Field{
      fvar,
      130,
      "IOINaturalFlag",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 131 "QuoteReqID"
    Field{
      fvar,
      131,
      "QuoteReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 132 "BidPx"
    Field{
      fvar,
      132,
      "BidPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 133 "OfferPx"
    Field{
      fvar,
      133,
      "OfferPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 134 "BidSize"
    Field{
      fvar,
      134,
      "BidSize",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 135 "OfferSize"
    Field{
      fvar,
      135,
      "OfferSize",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 136 "NoMiscFees"
    Field{
      fvar,
      136,
      "NoMiscFees",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoMiscFees",
      136,
      nullptr
    },
    //--- Tag# 137 "MiscFeeAmt"
    Field{
      fvar,
      137,
      "MiscFeeAmt",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 138 "MiscFeeCurr"
    Field{
      fvar,
      138,
      "MiscFeeCurr",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 139 "MiscFeeType"
    Field{
      fvar,
      139,
      "MiscFeeType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "Regulatory"     , .atom = 0}, // 0
        {.value = "2" , .descr = "Tax"            , .atom = 0}, // 1
        {.value = "3" , .descr = "LocalCommission", .atom = 0}, // 2
        {.value = "4" , .descr = "ExchangeFees"   , .atom = 0}, // 3
        {.value = "5" , .descr = "Stamp"          , .atom = 0}, // 4
        {.value = "6" , .descr = "Levy"           , .atom = 0}, // 5
        {.value = "7" , .descr = "Other"          , .atom = 0}, // 6
        {.value = "8" , .descr = "Markup"         , .atom = 0}, // 7
        {.value = "9" , .descr = "ConsumptionTax" , .atom = 0}, // 8
        {.value = "10", .descr = "PerTransaction" , .atom = 0}, // 9
        {.value = "11", .descr = "Conversion"     , .atom = 0}, // 10
        {.value = "12", .descr = "Agent"          , .atom = 0}, // 11
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // Regulatory
          case           '2': return f.value_atom( 1); // Tax
          case           '3': return f.value_atom( 2); // LocalCommission
          case           '4': return f.value_atom( 3); // ExchangeFees
          case           '5': return f.value_atom( 4); // Stamp
          case           '6': return f.value_atom( 5); // Levy
          case           '7': return f.value_atom( 6); // Other
          case           '8': return f.value_atom( 7); // Markup
          case           '9': return f.value_atom( 8); // ConsumptionTax
          case CINT<'1','0'>: return f.value_atom( 9); // PerTransaction
          case CINT<'1','1'>: return f.value_atom(10); // Conversion
          case CINT<'1','2'>: return f.value_atom(11); // Agent
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 140 "PrevClosePx"
    Field{
      fvar,
      140,
      "PrevClosePx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 141 "ResetSeqNumFlag"
    Field{
      fvar,
      141,
      "ResetSeqNumFlag",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 142 "SenderLocationID"
    Field{
      fvar,
      142,
      "SenderLocationID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 143 "TargetLocationID"
    Field{
      fvar,
      143,
      "TargetLocationID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 144 "OnBehalfOfLocationID"
    Field{
      fvar,
      144,
      "OnBehalfOfLocationID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 145 "DeliverToLocationID"
    Field{
      fvar,
      145,
      "DeliverToLocationID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 146 "NoRelatedSym"
    Field{
      fvar,
      146,
      "NoRelatedSym",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoRelatedSym",
      146,
      nullptr
    },
    //--- Tag# 147 "Subject"
    Field{
      fvar,
      147,
      "Subject",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 148 "Headline"
    Field{
      fvar,
      148,
      "Headline",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 149 "URLLink"
    Field{
      fvar,
      149,
      "URLLink",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 150 "ExecType"
    Field{
      fvar,
      150,
      "ExecType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "New"           , .atom = 0}, // 0
        {.value = "3", .descr = "DoneForDay"    , .atom = 0}, // 1
        {.value = "4", .descr = "Canceled"      , .atom = 0}, // 2
        {.value = "5", .descr = "Replace"       , .atom = 0}, // 3
        {.value = "6", .descr = "PendingCancel" , .atom = 0}, // 4
        {.value = "7", .descr = "Stopped"       , .atom = 0}, // 5
        {.value = "8", .descr = "Rejected"      , .atom = 0}, // 6
        {.value = "9", .descr = "Suspended"     , .atom = 0}, // 7
        {.value = "A", .descr = "PendingNew"    , .atom = 0}, // 8
        {.value = "B", .descr = "Calculated"    , .atom = 0}, // 9
        {.value = "C", .descr = "Expired"       , .atom = 0}, // 10
        {.value = "D", .descr = "Restated"      , .atom = 0}, // 11
        {.value = "E", .descr = "PendingReplace", .atom = 0}, // 12
        {.value = "F", .descr = "Trade"         , .atom = 0}, // 13
        {.value = "G", .descr = "TradeCorrect"  , .atom = 0}, // 14
        {.value = "H", .descr = "TradeCancel"   , .atom = 0}, // 15
        {.value = "I", .descr = "OrderStatus"   , .atom = 0}, // 16
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NEW
          case '3': return f.value_atom(1); // DONE_FOR_DAY
          case '4': return f.value_atom(2); // CANCELED
          case '5': return f.value_atom(3); // REPLACE
          case '6': return f.value_atom(4); // PENDING_CANCEL
          case '7': return f.value_atom(5); // STOPPED
          case '8': return f.value_atom(6); // REJECTED
          case '9': return f.value_atom(7); // SUSPENDED
          case 'A': return f.value_atom(8); // PENDING_NEW
          case 'B': return f.value_atom(9); // CALCULATED
          case 'C': return f.value_atom(10); // EXPIRED
          case 'D': return f.value_atom(11); // RESTATED
          case 'E': return f.value_atom(12); // PENDING_REPLACE
          case 'F': return f.value_atom(13); // TRADE
          case 'G': return f.value_atom(14); // TRADE_CORRECT
          case 'H': return f.value_atom(15); // TRADE_CANCEL
          case 'I': return f.value_atom(16); // ORDER_STATUS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 151 "LeavesQty"
    Field{
      fvar,
      151,
      "LeavesQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 152 "CashOrderQty"
    Field{
      fvar,
      152,
      "CashOrderQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 153 "AllocAvgPx"
    Field{
      fvar,
      153,
      "AllocAvgPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 154 "AllocNetMoney"
    Field{
      fvar,
      154,
      "AllocNetMoney",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 155 "SettlCurrFxRate"
    Field{
      fvar,
      155,
      "SettlCurrFxRate",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 156 "SettlCurrFxRateCalc"
    Field{
      fvar,
      156,
      "SettlCurrFxRateCalc",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "M", .descr = "Multiply", .atom = 0}, // 0
        {.value = "D", .descr = "Divide"  , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'M': return f.value_atom(0); // MULTIPLY
          case 'D': return f.value_atom(1); // DIVIDE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 157 "NumDaysInterest"
    Field{
      fvar,
      157,
      "NumDaysInterest",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 158 "AccruedInterestRate"
    Field{
      fvar,
      158,
      "AccruedInterestRate",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 159 "AccruedInterestAmt"
    Field{
      fvar,
      159,
      "AccruedInterestAmt",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 160 "SettlInstMode"
    Field{
      fvar,
      160,
      "SettlInstMode",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "StandingInstructionsProvided"  , .atom = 0}, // 0
        {.value = "4", .descr = "SpecificOrderForASingleAccount", .atom = 0}, // 1
        {.value = "5", .descr = "RequestReject"                 , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // STANDING_INSTRUCTIONS_PROVIDED
          case '4': return f.value_atom(1); // SPECIFIC_ORDER_FOR_A_SINGLE_ACCOUNT
          case '5': return f.value_atom(2); // REQUEST_REJECT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 161 "AllocText"
    Field{
      fvar,
      161,
      "AllocText",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 162 "SettlInstID"
    Field{
      fvar,
      162,
      "SettlInstID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 163 "SettlInstTransType"
    Field{
      fvar,
      163,
      "SettlInstTransType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "N", .descr = "New"    , .atom = 0}, // 0
        {.value = "C", .descr = "Cancel" , .atom = 0}, // 1
        {.value = "R", .descr = "Replace", .atom = 0}, // 2
        {.value = "T", .descr = "Restate", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'N': return f.value_atom(0); // NEW
          case 'C': return f.value_atom(1); // CANCEL
          case 'R': return f.value_atom(2); // REPLACE
          case 'T': return f.value_atom(3); // RESTATE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 164 "EmailThreadID"
    Field{
      fvar,
      164,
      "EmailThreadID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 165 "SettlInstSource"
    Field{
      fvar,
      165,
      "SettlInstSource",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "BrokersInstructions"     , .atom = 0}, // 0
        {.value = "2", .descr = "InstitutionsInstructions", .atom = 0}, // 1
        {.value = "3", .descr = "Investor"                , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // BROKERS_INSTRUCTIONS
          case '2': return f.value_atom(1); // INSTITUTIONS_INSTRUCTIONS
          case '3': return f.value_atom(2); // INVESTOR
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 166
    Field{},
    //--- Tag# 167 "SecurityType"
    Field{
      fvar,
      167,
      "SecurityType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "FUT"      , .descr = "Future"                                  , .atom = 0}, // 0
        {.value = "OPT"      , .descr = "Option"                                  , .atom = 0}, // 1
        {.value = "EUSUPRA"  , .descr = "EuroSupranationalCoupons"                , .atom = 0}, // 2
        {.value = "FAC"      , .descr = "FederalAgencyCoupon"                     , .atom = 0}, // 3
        {.value = "FADN"     , .descr = "FederalAgencyDiscountNote"               , .atom = 0}, // 4
        {.value = "PEF"      , .descr = "PrivateExportFunding"                    , .atom = 0}, // 5
        {.value = "SUPRA"    , .descr = "UsdSupranationalCoupons"                 , .atom = 0}, // 6
        {.value = "CORP"     , .descr = "CorporateBond"                           , .atom = 0}, // 7
        {.value = "CPP"      , .descr = "CorporatePrivatePlacement"               , .atom = 0}, // 8
        {.value = "CB"       , .descr = "ConvertibleBond"                         , .atom = 0}, // 9
        {.value = "DUAL"     , .descr = "DualCurrency"                            , .atom = 0}, // 10
        {.value = "EUCORP"   , .descr = "EuroCorporateBond"                       , .atom = 0}, // 11
        {.value = "XLINKD"   , .descr = "IndexedLinked"                           , .atom = 0}, // 12
        {.value = "STRUCT"   , .descr = "StructuredNotes"                         , .atom = 0}, // 13
        {.value = "YANK"     , .descr = "YankeeCorporateBond"                     , .atom = 0}, // 14
        {.value = "FOR"      , .descr = "ForeignExchangeContract"                 , .atom = 0}, // 15
        {.value = "CS"       , .descr = "CommonStock"                             , .atom = 0}, // 16
        {.value = "PS"       , .descr = "PreferredStock"                          , .atom = 0}, // 17
        {.value = "BRADY"    , .descr = "BradyBond"                               , .atom = 0}, // 18
        {.value = "EUSOV"    , .descr = "EuroSovereigns"                          , .atom = 0}, // 19
        {.value = "TBOND"    , .descr = "UsTreasuryBond"                          , .atom = 0}, // 20
        {.value = "TINT"     , .descr = "InterestStripFromAnyBondOrNote"          , .atom = 0}, // 21
        {.value = "TIPS"     , .descr = "TreasuryInflationProtectedSecurities"    , .atom = 0}, // 22
        {.value = "TCAL"     , .descr = "PrincipalStripOfACallableBondOrNote"     , .atom = 0}, // 23
        {.value = "TPRN"     , .descr = "PrincipalStripFromANonCallableBondOrNote", .atom = 0}, // 24
        {.value = "UST"      , .descr = "UsTreasuryNoteUst"                       , .atom = 0}, // 25
        {.value = "USTB"     , .descr = "UsTreasuryBillUstb"                      , .atom = 0}, // 26
        {.value = "TNOTE"    , .descr = "UsTreasuryNoteTnote"                     , .atom = 0}, // 27
        {.value = "TBILL"    , .descr = "UsTreasuryBillTbill"                     , .atom = 0}, // 28
        {.value = "REPO"     , .descr = "Repurchase"                              , .atom = 0}, // 29
        {.value = "FORWARD"  , .descr = "Forward"                                 , .atom = 0}, // 30
        {.value = "BUYSELL"  , .descr = "BuySellback"                             , .atom = 0}, // 31
        {.value = "SECLOAN"  , .descr = "SecuritiesLoan"                          , .atom = 0}, // 32
        {.value = "SECPLEDGE", .descr = "SecuritiesPledge"                        , .atom = 0}, // 33
        {.value = "TERM"     , .descr = "TermLoan"                                , .atom = 0}, // 34
        {.value = "RVLV"     , .descr = "RevolverLoan"                            , .atom = 0}, // 35
        {.value = "RVLVTRM"  , .descr = "RevolverTermLoan"                        , .atom = 0}, // 36
        {.value = "BRIDGE"   , .descr = "BridgeLoan"                              , .atom = 0}, // 37
        {.value = "LOFC"     , .descr = "LetterOfCredit"                          , .atom = 0}, // 38
        {.value = "SWING"    , .descr = "SwingLineFacility"                       , .atom = 0}, // 39
        {.value = "DINP"     , .descr = "DebtorInPossession"                      , .atom = 0}, // 40
        {.value = "DEFLTED"  , .descr = "Defaulted"                               , .atom = 0}, // 41
        {.value = "WITHDRN"  , .descr = "Withdrawn"                               , .atom = 0}, // 42
        {.value = "REPLACD"  , .descr = "Replaced"                                , .atom = 0}, // 43
        {.value = "MATURED"  , .descr = "Matured"                                 , .atom = 0}, // 44
        {.value = "AMENDED"  , .descr = "AmendedRestated"                         , .atom = 0}, // 45
        {.value = "RETIRED"  , .descr = "Retired"                                 , .atom = 0}, // 46
        {.value = "BA"       , .descr = "BankersAcceptance"                       , .atom = 0}, // 47
        {.value = "BN"       , .descr = "BankNotes"                               , .atom = 0}, // 48
        {.value = "BOX"      , .descr = "BillOfExchanges"                         , .atom = 0}, // 49
        {.value = "CD"       , .descr = "CertificateOfDeposit"                    , .atom = 0}, // 50
        {.value = "CL"       , .descr = "CallLoans"                               , .atom = 0}, // 51
        {.value = "CP"       , .descr = "CommercialPaper"                         , .atom = 0}, // 52
        {.value = "DN"       , .descr = "DepositNotes"                            , .atom = 0}, // 53
        {.value = "EUCD"     , .descr = "EuroCertificateOfDeposit"                , .atom = 0}, // 54
        {.value = "EUCP"     , .descr = "EuroCommercialPaper"                     , .atom = 0}, // 55
        {.value = "LQN"      , .descr = "LiquidityNote"                           , .atom = 0}, // 56
        {.value = "MTN"      , .descr = "MediumTermNotes"                         , .atom = 0}, // 57
        {.value = "ONITE"    , .descr = "Overnight"                               , .atom = 0}, // 58
        {.value = "PN"       , .descr = "PromissoryNote"                          , .atom = 0}, // 59
        {.value = "PZFJ"     , .descr = "PlazosFijos"                             , .atom = 0}, // 60
        {.value = "STN"      , .descr = "ShortTermLoanNote"                       , .atom = 0}, // 61
        {.value = "TD"       , .descr = "TimeDeposit"                             , .atom = 0}, // 62
        {.value = "XCN"      , .descr = "ExtendedCommNote"                        , .atom = 0}, // 63
        {.value = "YCD"      , .descr = "YankeeCertificateOfDeposit"              , .atom = 0}, // 64
        {.value = "ABS"      , .descr = "AssetBackedSecurities"                   , .atom = 0}, // 65
        {.value = "CMBS"     , .descr = "CorpMortgageBackedSecurities"            , .atom = 0}, // 66
        {.value = "CMO"      , .descr = "CollateralizedMortgageObligation"        , .atom = 0}, // 67
        {.value = "IET"      , .descr = "IoetteMortgage"                          , .atom = 0}, // 68
        {.value = "MBS"      , .descr = "MortgageBackedSecurities"                , .atom = 0}, // 69
        {.value = "MIO"      , .descr = "MortgageInterestOnly"                    , .atom = 0}, // 70
        {.value = "MPO"      , .descr = "MortgagePrincipalOnly"                   , .atom = 0}, // 71
        {.value = "MPP"      , .descr = "MortgagePrivatePlacement"                , .atom = 0}, // 72
        {.value = "MPT"      , .descr = "MiscellaneousPassThrough"                , .atom = 0}, // 73
        {.value = "PFAND"    , .descr = "Pfandbriefe"                             , .atom = 0}, // 74
        {.value = "TBA"      , .descr = "ToBeAnnounced"                           , .atom = 0}, // 75
        {.value = "AN"       , .descr = "OtherAnticipationNotesBanGanEtc"         , .atom = 0}, // 76
        {.value = "COFO"     , .descr = "CertificateOfObligation"                 , .atom = 0}, // 77
        {.value = "COFP"     , .descr = "CertificateOfParticipation"              , .atom = 0}, // 78
        {.value = "GO"       , .descr = "GeneralObligationBonds"                  , .atom = 0}, // 79
        {.value = "MT"       , .descr = "MandatoryTender"                         , .atom = 0}, // 80
        {.value = "RAN"      , .descr = "RevenueAnticipationNote"                 , .atom = 0}, // 81
        {.value = "REV"      , .descr = "RevenueBonds"                            , .atom = 0}, // 82
        {.value = "SPCLA"    , .descr = "SpecialAssessment"                       , .atom = 0}, // 83
        {.value = "SPCLO"    , .descr = "SpecialObligation"                       , .atom = 0}, // 84
        {.value = "SPCLT"    , .descr = "SpecialTax"                              , .atom = 0}, // 85
        {.value = "TAN"      , .descr = "TaxAnticipationNote"                     , .atom = 0}, // 86
        {.value = "TAXA"     , .descr = "TaxAllocation"                           , .atom = 0}, // 87
        {.value = "TECP"     , .descr = "TaxExemptCommercialPaper"                , .atom = 0}, // 88
        {.value = "TRAN"     , .descr = "TaxRevenueAnticipationNote"              , .atom = 0}, // 89
        {.value = "VRDN"     , .descr = "VariableRateDemandNote"                  , .atom = 0}, // 90
        {.value = "WAR"      , .descr = "Warrant"                                 , .atom = 0}, // 91
        {.value = "MF"       , .descr = "MutualFund"                              , .atom = 0}, // 92
        {.value = "MLEG"     , .descr = "MultiLegInstrument"                      , .atom = 0}, // 93
        {.value = "NONE"     , .descr = "NoSecurityType"                          , .atom = 0}, // 94
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto   fc = f.value(std::string_view(code, len));
        return fc ? fc->get_atom(env) : am_undefined;
      },
    },
    //--- Tag# 168 "EffectiveTime"
    Field{
      fvar,
      168,
      "EffectiveTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 169 "StandInstDbType"
    Field{
      fvar,
      169,
      "StandInstDbType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Other"           , .atom = 0}, // 0
        {.value = "1", .descr = "DtcSid"          , .atom = 0}, // 1
        {.value = "2", .descr = "ThomsonAlert"    , .atom = 0}, // 2
        {.value = "3", .descr = "AGlobalCustodian", .atom = 0}, // 3
        {.value = "4", .descr = "Accountnet"      , .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // OTHER
          case '1': return f.value_atom(1); // DTC_SID
          case '2': return f.value_atom(2); // THOMSON_ALERT
          case '3': return f.value_atom(3); // A_GLOBAL_CUSTODIAN
          case '4': return f.value_atom(4); // ACCOUNTNET
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 170 "StandInstDbName"
    Field{
      fvar,
      170,
      "StandInstDbName",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 171 "StandInstDbID"
    Field{
      fvar,
      171,
      "StandInstDbID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 172 "SettlDeliveryType"
    Field{
      fvar,
      172,
      "SettlDeliveryType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "VersusPaymentDeliver", .atom = 0}, // 0
        {.value = "1", .descr = "FreeDeliver"         , .atom = 0}, // 1
        {.value = "2", .descr = "TriParty"            , .atom = 0}, // 2
        {.value = "3", .descr = "HoldInCustody"       , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // VERSUS_PAYMENT_DELIVER
          case '1': return f.value_atom(1); // FREE_DELIVER
          case '2': return f.value_atom(2); // TRI_PARTY
          case '3': return f.value_atom(3); // HOLD_IN_CUSTODY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 173
    Field{},
    //--- Tag# 174
    Field{},
    //--- Tag# 175
    Field{},
    //--- Tag# 176
    Field{},
    //--- Tag# 177
    Field{},
    //--- Tag# 178
    Field{},
    //--- Tag# 179
    Field{},
    //--- Tag# 180
    Field{},
    //--- Tag# 181
    Field{},
    //--- Tag# 182
    Field{},
    //--- Tag# 183
    Field{},
    //--- Tag# 184
    Field{},
    //--- Tag# 185
    Field{},
    //--- Tag# 186
    Field{},
    //--- Tag# 187
    Field{},
    //--- Tag# 188 "BidSpotRate"
    Field{
      fvar,
      188,
      "BidSpotRate",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 189 "BidForwardPoints"
    Field{
      fvar,
      189,
      "BidForwardPoints",
      FieldType::PRICEOFFSET,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 190 "OfferSpotRate"
    Field{
      fvar,
      190,
      "OfferSpotRate",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 191 "OfferForwardPoints"
    Field{
      fvar,
      191,
      "OfferForwardPoints",
      FieldType::PRICEOFFSET,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 192 "OrderQty2"
    Field{
      fvar,
      192,
      "OrderQty2",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 193 "SettlDate2"
    Field{
      fvar,
      193,
      "SettlDate2",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 194 "LastSpotRate"
    Field{
      fvar,
      194,
      "LastSpotRate",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 195 "LastForwardPoints"
    Field{
      fvar,
      195,
      "LastForwardPoints",
      FieldType::PRICEOFFSET,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 196 "AllocLinkID"
    Field{
      fvar,
      196,
      "AllocLinkID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 197 "AllocLinkType"
    Field{
      fvar,
      197,
      "AllocLinkType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "FXNetting", .atom = 0}, // 0
        {.value = "1", .descr = "FXSwap"   , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // F_X_NETTING
          case '1': return f.value_atom(1); // F_X_SWAP
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 198 "SecondaryOrderID"
    Field{
      fvar,
      198,
      "SecondaryOrderID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 199 "NoIOIQualifiers"
    Field{
      fvar,
      199,
      "NoIOIQualifiers",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoIOIQualifiers",
      199,
      nullptr
    },
    //--- Tag# 200 "MaturityMonthYear"
    Field{
      fvar,
      200,
      "MaturityMonthYear",
      FieldType::MONTHYEAR,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 201 "PutOrCall"
    Field{
      fvar,
      201,
      "PutOrCall",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Put" , .atom = 0}, // 0
        {.value = "1", .descr = "Call", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // PUT
          case '1': return f.value_atom(1); // CALL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 202 "StrikePrice"
    Field{
      fvar,
      202,
      "StrikePrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 203 "CoveredOrUncovered"
    Field{
      fvar,
      203,
      "CoveredOrUncovered",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Covered"  , .atom = 0}, // 0
        {.value = "1", .descr = "Uncovered", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // COVERED
          case '1': return f.value_atom(1); // UNCOVERED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 204
    Field{},
    //--- Tag# 205
    Field{},
    //--- Tag# 206 "OptAttribute"
    Field{
      fvar,
      206,
      "OptAttribute",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 207 "SecurityExchange"
    Field{
      fvar,
      207,
      "SecurityExchange",
      FieldType::EXCHANGE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 208 "NotifyBrokerOfCredit"
    Field{
      fvar,
      208,
      "NotifyBrokerOfCredit",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 209 "AllocHandlInst"
    Field{
      fvar,
      209,
      "AllocHandlInst",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Match"          , .atom = 0}, // 0
        {.value = "2", .descr = "Forward"        , .atom = 0}, // 1
        {.value = "3", .descr = "ForwardAndMatch", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // MATCH
          case '2': return f.value_atom(1); // FORWARD
          case '3': return f.value_atom(2); // FORWARD_AND_MATCH
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 210 "MaxShow"
    Field{
      fvar,
      210,
      "MaxShow",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 211 "PegOffsetValue"
    Field{
      fvar,
      211,
      "PegOffsetValue",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 212 "XmlDataLen"
    Field{
      fvar,
      212,
      "XmlDataLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 213 "XmlData"
    Field{
      fvar,
      213,
      "XmlData",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "XmlDataLen",
      212,
      nullptr
    },
    //--- Tag# 214 "SettlInstRefID"
    Field{
      fvar,
      214,
      "SettlInstRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 215 "NoRoutingIDs"
    Field{
      fvar,
      215,
      "NoRoutingIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoRoutingIDs",
      215,
      nullptr
    },
    //--- Tag# 216 "RoutingType"
    Field{
      fvar,
      216,
      "RoutingType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "TargetFirm", .atom = 0}, // 0
        {.value = "2", .descr = "TargetList", .atom = 0}, // 1
        {.value = "3", .descr = "BlockFirm" , .atom = 0}, // 2
        {.value = "4", .descr = "BlockList" , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // TARGET_FIRM
          case '2': return f.value_atom(1); // TARGET_LIST
          case '3': return f.value_atom(2); // BLOCK_FIRM
          case '4': return f.value_atom(3); // BLOCK_LIST
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 217 "RoutingID"
    Field{
      fvar,
      217,
      "RoutingID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 218 "Spread"
    Field{
      fvar,
      218,
      "Spread",
      FieldType::PRICEOFFSET,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 219
    Field{},
    //--- Tag# 220 "BenchmarkCurveCurrency"
    Field{
      fvar,
      220,
      "BenchmarkCurveCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 221 "BenchmarkCurveName"
    Field{
      fvar,
      221,
      "BenchmarkCurveName",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 222 "BenchmarkCurvePoint"
    Field{
      fvar,
      222,
      "BenchmarkCurvePoint",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 223 "CouponRate"
    Field{
      fvar,
      223,
      "CouponRate",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 224 "CouponPaymentDate"
    Field{
      fvar,
      224,
      "CouponPaymentDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 225 "IssueDate"
    Field{
      fvar,
      225,
      "IssueDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 226 "RepurchaseTerm"
    Field{
      fvar,
      226,
      "RepurchaseTerm",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 227 "RepurchaseRate"
    Field{
      fvar,
      227,
      "RepurchaseRate",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 228 "Factor"
    Field{
      fvar,
      228,
      "Factor",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 229 "TradeOriginationDate"
    Field{
      fvar,
      229,
      "TradeOriginationDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 230 "ExDate"
    Field{
      fvar,
      230,
      "ExDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 231 "ContractMultiplier"
    Field{
      fvar,
      231,
      "ContractMultiplier",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 232 "NoStipulations"
    Field{
      fvar,
      232,
      "NoStipulations",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoStipulations",
      232,
      nullptr
    },
    //--- Tag# 233 "StipulationType"
    Field{
      fvar,
      233,
      "StipulationType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "AMT"       , .descr = "Amt"                                                                                      , .atom = 0}, // 0
        {.value = "AUTOREINV" , .descr = "AutoReinvestmentAtRateOrBetter"                                                           , .atom = 0}, // 1
        {.value = "BANKQUAL"  , .descr = "BankQualified"                                                                            , .atom = 0}, // 2
        {.value = "BGNCON"    , .descr = "BargainConditionsSee"                                                                     , .atom = 0}, // 3
        {.value = "COUPON"    , .descr = "CouponRange"                                                                              , .atom = 0}, // 4
        {.value = "CURRENCY"  , .descr = "IsoCurrencyCode"                                                                          , .atom = 0}, // 5
        {.value = "CUSTOMDATE", .descr = "CustomStartEndDate"                                                                       , .atom = 0}, // 6
        {.value = "GEOG"      , .descr = "GeographicsAndRange"                                                                      , .atom = 0}, // 7
        {.value = "HAIRCUT"   , .descr = "ValuationDiscount"                                                                        , .atom = 0}, // 8
        {.value = "INSURED"   , .descr = "Insured"                                                                                  , .atom = 0}, // 9
        {.value = "ISSUE"     , .descr = "YearOrYearMonthOfIssue"                                                                   , .atom = 0}, // 10
        {.value = "ISSUER"    , .descr = "IssuersTicker"                                                                            , .atom = 0}, // 11
        {.value = "ISSUESIZE" , .descr = "IssueSizeRange"                                                                           , .atom = 0}, // 12
        {.value = "LOOKBACK"  , .descr = "LookbackDays"                                                                             , .atom = 0}, // 13
        {.value = "LOT"       , .descr = "ExplicitLotIdentifier"                                                                    , .atom = 0}, // 14
        {.value = "LOTVAR"    , .descr = "LotVariance"                                                                              , .atom = 0}, // 15
        {.value = "MAT"       , .descr = "MaturityYearAndMonth"                                                                     , .atom = 0}, // 16
        {.value = "MATURITY"  , .descr = "MaturityRange"                                                                            , .atom = 0}, // 17
        {.value = "MAXSUBS"   , .descr = "MaximumSubstitutions"                                                                     , .atom = 0}, // 18
        {.value = "MINQTY"    , .descr = "MinimumQuantity"                                                                          , .atom = 0}, // 19
        {.value = "MININCR"   , .descr = "MinimumIncrement"                                                                         , .atom = 0}, // 20
        {.value = "MINDNOM"   , .descr = "MinimumDenomination"                                                                      , .atom = 0}, // 21
        {.value = "PAYFREQ"   , .descr = "PaymentFrequencyCalendar"                                                                 , .atom = 0}, // 22
        {.value = "PIECES"    , .descr = "NumberOfPieces"                                                                           , .atom = 0}, // 23
        {.value = "PMAX"      , .descr = "PoolsMaximum"                                                                             , .atom = 0}, // 24
        {.value = "PPM"       , .descr = "PoolsPerMillion"                                                                          , .atom = 0}, // 25
        {.value = "PPL"       , .descr = "PoolsPerLot"                                                                              , .atom = 0}, // 26
        {.value = "PPT"       , .descr = "PoolsPerTrade"                                                                            , .atom = 0}, // 27
        {.value = "PRICE"     , .descr = "PriceRange"                                                                               , .atom = 0}, // 28
        {.value = "PRICEFREQ" , .descr = "PricingFrequency"                                                                         , .atom = 0}, // 29
        {.value = "PROD"      , .descr = "ProductionYear"                                                                           , .atom = 0}, // 30
        {.value = "PROTECT"   , .descr = "CallProtection"                                                                           , .atom = 0}, // 31
        {.value = "PURPOSE"   , .descr = "Purpose"                                                                                  , .atom = 0}, // 32
        {.value = "PXSOURCE"  , .descr = "BenchmarkPriceSource"                                                                     , .atom = 0}, // 33
        {.value = "RATING"    , .descr = "RatingSourceAndRange"                                                                     , .atom = 0}, // 34
        {.value = "REDEMPTION", .descr = "TypeOfRedemptionValuesAreNoncallableCallablePrefundedEscrowedtomaturityPutableConvertible", .atom = 0}, // 35
        {.value = "RESTRICTED", .descr = "Restricted"                                                                               , .atom = 0}, // 36
        {.value = "SECTOR"    , .descr = "MarketSector"                                                                             , .atom = 0}, // 37
        {.value = "SECTYPE"   , .descr = "SecuritytypeIncludedOrExcluded"                                                           , .atom = 0}, // 38
        {.value = "STRUCT"    , .descr = "Structure"                                                                                , .atom = 0}, // 39
        {.value = "SUBSFREQ"  , .descr = "SubstitutionsFrequency"                                                                   , .atom = 0}, // 40
        {.value = "SUBSLEFT"  , .descr = "SubstitutionsLeft"                                                                        , .atom = 0}, // 41
        {.value = "TEXT"      , .descr = "FreeformText"                                                                             , .atom = 0}, // 42
        {.value = "TRDVAR"    , .descr = "TradeVariance"                                                                            , .atom = 0}, // 43
        {.value = "WAC"       , .descr = "WeightedAverageCouponvalueInPercent"                                                      , .atom = 0}, // 44
        {.value = "WAL"       , .descr = "WeightedAverageLifeCouponValueInPercent"                                                  , .atom = 0}, // 45
        {.value = "WALA"      , .descr = "WeightedAverageLoanAgeValueInMonths"                                                      , .atom = 0}, // 46
        {.value = "WAM"       , .descr = "WeightedAverageMaturityValueInMonths"                                                     , .atom = 0}, // 47
        {.value = "WHOLE"     , .descr = "WholePool"                                                                                , .atom = 0}, // 48
        {.value = "YIELD"     , .descr = "YieldRange"                                                                               , .atom = 0}, // 49
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto   fc = f.value(std::string_view(code, len));
        return fc ? fc->get_atom(env) : am_undefined;
      },
    },
    //--- Tag# 234 "StipulationValue"
    Field{
      fvar,
      234,
      "StipulationValue",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 235 "YieldType"
    Field{
      fvar,
      235,
      "YieldType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "AFTERTAX"      , .descr = "AfterTaxYield"                , .atom = 0}, // 0
        {.value = "ANNUAL"        , .descr = "AnnualYield"                  , .atom = 0}, // 1
        {.value = "ATISSUE"       , .descr = "YieldAtIssue"                 , .atom = 0}, // 2
        {.value = "AVGMATURITY"   , .descr = "YieldToAverageMaturity"       , .atom = 0}, // 3
        {.value = "BOOK"          , .descr = "BookYield"                    , .atom = 0}, // 4
        {.value = "CALL"          , .descr = "YieldToNextCall"              , .atom = 0}, // 5
        {.value = "CHANGE"        , .descr = "YieldChangeSinceClose"        , .atom = 0}, // 6
        {.value = "CLOSE"         , .descr = "ClosingYield"                 , .atom = 0}, // 7
        {.value = "COMPOUND"      , .descr = "CompoundYield"                , .atom = 0}, // 8
        {.value = "CURRENT"       , .descr = "CurrentYield"                 , .atom = 0}, // 9
        {.value = "GROSS"         , .descr = "TrueGrossYield"               , .atom = 0}, // 10
        {.value = "GOVTEQUIV"     , .descr = "GovernmentEquivalentYield"    , .atom = 0}, // 11
        {.value = "INFLATION"     , .descr = "YieldWithInflationAssumption" , .atom = 0}, // 12
        {.value = "INVERSEFLOATER", .descr = "InverseFloaterBondYield"      , .atom = 0}, // 13
        {.value = "LASTCLOSE"     , .descr = "MostRecentClosingYield"       , .atom = 0}, // 14
        {.value = "LASTMONTH"     , .descr = "ClosingYieldMostRecentMonth"  , .atom = 0}, // 15
        {.value = "LASTQUARTER"   , .descr = "ClosingYieldMostRecentQuarter", .atom = 0}, // 16
        {.value = "LASTYEAR"      , .descr = "ClosingYieldMostRecentYear"   , .atom = 0}, // 17
        {.value = "LONGAVGLIFE"   , .descr = "YieldToLongestAverageLife"    , .atom = 0}, // 18
        {.value = "MARK"          , .descr = "MarkToMarketYield"            , .atom = 0}, // 19
        {.value = "MATURITY"      , .descr = "YieldToMaturity"              , .atom = 0}, // 20
        {.value = "NEXTREFUND"    , .descr = "YieldToNextRefund"            , .atom = 0}, // 21
        {.value = "OPENAVG"       , .descr = "OpenAverageYield"             , .atom = 0}, // 22
        {.value = "PUT"           , .descr = "YieldToNextPut"               , .atom = 0}, // 23
        {.value = "PREVCLOSE"     , .descr = "PreviousCloseYield"           , .atom = 0}, // 24
        {.value = "PROCEEDS"      , .descr = "ProceedsYield"                , .atom = 0}, // 25
        {.value = "SEMIANNUAL"    , .descr = "SemiAnnualYield"              , .atom = 0}, // 26
        {.value = "SHORTAVGLIFE"  , .descr = "YieldToShortestAverageLife"   , .atom = 0}, // 27
        {.value = "SIMPLE"        , .descr = "SimpleYield"                  , .atom = 0}, // 28
        {.value = "TAXEQUIV"      , .descr = "TaxEquivalentYield"           , .atom = 0}, // 29
        {.value = "TENDER"        , .descr = "YieldToTenderDate"            , .atom = 0}, // 30
        {.value = "TRUE"          , .descr = "TrueYield"                    , .atom = 0}, // 31
        {.value = "VALUE1/32"     , .descr = "YieldValueOf132"              , .atom = 0}, // 32
        {.value = "WORST"         , .descr = "YieldToWorst"                 , .atom = 0}, // 33
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto   fc = f.value(std::string_view(code, len));
        return fc ? fc->get_atom(env) : am_undefined;
      },
    },
    //--- Tag# 236 "Yield"
    Field{
      fvar,
      236,
      "Yield",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 237 "TotalTakedown"
    Field{
      fvar,
      237,
      "TotalTakedown",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 238 "Concession"
    Field{
      fvar,
      238,
      "Concession",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 239 "RepoCollateralSecurityType"
    Field{
      fvar,
      239,
      "RepoCollateralSecurityType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 240 "RedemptionDate"
    Field{
      fvar,
      240,
      "RedemptionDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 241 "UnderlyingCouponPaymentDate"
    Field{
      fvar,
      241,
      "UnderlyingCouponPaymentDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 242 "UnderlyingIssueDate"
    Field{
      fvar,
      242,
      "UnderlyingIssueDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 243 "UnderlyingRepoCollateralSecurityType"
    Field{
      fvar,
      243,
      "UnderlyingRepoCollateralSecurityType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 244 "UnderlyingRepurchaseTerm"
    Field{
      fvar,
      244,
      "UnderlyingRepurchaseTerm",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 245 "UnderlyingRepurchaseRate"
    Field{
      fvar,
      245,
      "UnderlyingRepurchaseRate",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 246 "UnderlyingFactor"
    Field{
      fvar,
      246,
      "UnderlyingFactor",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 247 "UnderlyingRedemptionDate"
    Field{
      fvar,
      247,
      "UnderlyingRedemptionDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 248 "LegCouponPaymentDate"
    Field{
      fvar,
      248,
      "LegCouponPaymentDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 249 "LegIssueDate"
    Field{
      fvar,
      249,
      "LegIssueDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 250 "LegRepoCollateralSecurityType"
    Field{
      fvar,
      250,
      "LegRepoCollateralSecurityType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 251 "LegRepurchaseTerm"
    Field{
      fvar,
      251,
      "LegRepurchaseTerm",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 252 "LegRepurchaseRate"
    Field{
      fvar,
      252,
      "LegRepurchaseRate",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 253 "LegFactor"
    Field{
      fvar,
      253,
      "LegFactor",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 254 "LegRedemptionDate"
    Field{
      fvar,
      254,
      "LegRedemptionDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 255 "CreditRating"
    Field{
      fvar,
      255,
      "CreditRating",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 256 "UnderlyingCreditRating"
    Field{
      fvar,
      256,
      "UnderlyingCreditRating",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 257 "LegCreditRating"
    Field{
      fvar,
      257,
      "LegCreditRating",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 258 "TradedFlatSwitch"
    Field{
      fvar,
      258,
      "TradedFlatSwitch",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 259 "BasisFeatureDate"
    Field{
      fvar,
      259,
      "BasisFeatureDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 260 "BasisFeaturePrice"
    Field{
      fvar,
      260,
      "BasisFeaturePrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 261
    Field{},
    //--- Tag# 262 "MDReqID"
    Field{
      fvar,
      262,
      "MDReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 263 "SubscriptionRequestType"
    Field{
      fvar,
      263,
      "SubscriptionRequestType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Snapshot"                                , .atom = 0}, // 0
        {.value = "1", .descr = "SnapshotPlusUpdates"                     , .atom = 0}, // 1
        {.value = "2", .descr = "DisablePreviousSnapshotPlusUpdateRequest", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // SNAPSHOT
          case '1': return f.value_atom(1); // SNAPSHOT_PLUS_UPDATES
          case '2': return f.value_atom(2); // DISABLE_PREVIOUS_SNAPSHOT_PLUS_UPDATE_REQUEST
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 264 "MarketDepth"
    Field{
      fvar,
      264,
      "MarketDepth",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 265 "MDUpdateType"
    Field{
      fvar,
      265,
      "MDUpdateType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "FullRefresh"       , .atom = 0}, // 0
        {.value = "1", .descr = "IncrementalRefresh", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // FULL_REFRESH
          case '1': return f.value_atom(1); // INCREMENTAL_REFRESH
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 266 "AggregatedBook"
    Field{
      fvar,
      266,
      "AggregatedBook",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 267 "NoMDEntryTypes"
    Field{
      fvar,
      267,
      "NoMDEntryTypes",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoMDEntryTypes",
      267,
      nullptr
    },
    //--- Tag# 268 "NoMDEntries"
    Field{
      fvar,
      268,
      "NoMDEntries",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoMDEntries",
      268,
      nullptr
    },
    //--- Tag# 269 "MDEntryType"
    Field{
      fvar,
      269,
      "MDEntryType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Bid"                    , .atom = 0}, // 0
        {.value = "1", .descr = "Offer"                  , .atom = 0}, // 1
        {.value = "2", .descr = "Trade"                  , .atom = 0}, // 2
        {.value = "3", .descr = "IndexValue"             , .atom = 0}, // 3
        {.value = "4", .descr = "OpeningPrice"           , .atom = 0}, // 4
        {.value = "5", .descr = "ClosingPrice"           , .atom = 0}, // 5
        {.value = "6", .descr = "SettlementPrice"        , .atom = 0}, // 6
        {.value = "7", .descr = "TradingSessionHighPrice", .atom = 0}, // 7
        {.value = "8", .descr = "TradingSessionLowPrice" , .atom = 0}, // 8
        {.value = "9", .descr = "TradingSessionVwapPrice", .atom = 0}, // 9
        {.value = "A", .descr = "Imbalance"              , .atom = 0}, // 10
        {.value = "B", .descr = "TradeVolume"            , .atom = 0}, // 11
        {.value = "C", .descr = "OpenInterest"           , .atom = 0}, // 12
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // BID
          case '1': return f.value_atom(1); // OFFER
          case '2': return f.value_atom(2); // TRADE
          case '3': return f.value_atom(3); // INDEX_VALUE
          case '4': return f.value_atom(4); // OPENING_PRICE
          case '5': return f.value_atom(5); // CLOSING_PRICE
          case '6': return f.value_atom(6); // SETTLEMENT_PRICE
          case '7': return f.value_atom(7); // TRADING_SESSION_HIGH_PRICE
          case '8': return f.value_atom(8); // TRADING_SESSION_LOW_PRICE
          case '9': return f.value_atom(9); // TRADING_SESSION_VWAP_PRICE
          case 'A': return f.value_atom(10); // IMBALANCE
          case 'B': return f.value_atom(11); // TRADE_VOLUME
          case 'C': return f.value_atom(12); // OPEN_INTEREST
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 270 "MDEntryPx"
    Field{
      fvar,
      270,
      "MDEntryPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 271 "MDEntrySize"
    Field{
      fvar,
      271,
      "MDEntrySize",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 272 "MDEntryDate"
    Field{
      fvar,
      272,
      "MDEntryDate",
      FieldType::UTCDATEONLY,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 273 "MDEntryTime"
    Field{
      fvar,
      273,
      "MDEntryTime",
      FieldType::UTCTIMEONLY,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 274 "TickDirection"
    Field{
      fvar,
      274,
      "TickDirection",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "PlusTick"     , .atom = 0}, // 0
        {.value = "1", .descr = "ZeroPlusTick" , .atom = 0}, // 1
        {.value = "2", .descr = "MinusTick"    , .atom = 0}, // 2
        {.value = "3", .descr = "ZeroMinusTick", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // PLUS_TICK
          case '1': return f.value_atom(1); // ZERO_PLUS_TICK
          case '2': return f.value_atom(2); // MINUS_TICK
          case '3': return f.value_atom(3); // ZERO_MINUS_TICK
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 275 "MDMkt"
    Field{
      fvar,
      275,
      "MDMkt",
      FieldType::EXCHANGE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 276 "QuoteCondition"
    Field{
      fvar,
      276,
      "QuoteCondition",
      FieldType::MULTIPLEVALUESTRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "A", .descr = "Open"            , .atom = 0}, // 0
        {.value = "B", .descr = "Closed"          , .atom = 0}, // 1
        {.value = "C", .descr = "ExchangeBest"    , .atom = 0}, // 2
        {.value = "D", .descr = "ConsolidatedBest", .atom = 0}, // 3
        {.value = "E", .descr = "Locked"          , .atom = 0}, // 4
        {.value = "F", .descr = "Crossed"         , .atom = 0}, // 5
        {.value = "G", .descr = "Depth"           , .atom = 0}, // 6
        {.value = "H", .descr = "FastTrading"     , .atom = 0}, // 7
        {.value = "I", .descr = "NonFirm"         , .atom = 0}, // 8
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'A': return f.value_atom(0); // OPEN
          case 'B': return f.value_atom(1); // CLOSED
          case 'C': return f.value_atom(2); // EXCHANGE_BEST
          case 'D': return f.value_atom(3); // CONSOLIDATED_BEST
          case 'E': return f.value_atom(4); // LOCKED
          case 'F': return f.value_atom(5); // CROSSED
          case 'G': return f.value_atom(6); // DEPTH
          case 'H': return f.value_atom(7); // FAST_TRADING
          case 'I': return f.value_atom(8); // NON_FIRM
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 277 "TradeCondition"
    Field{
      fvar,
      277,
      "TradeCondition",
      FieldType::MULTIPLEVALUESTRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "A", .descr = "Cash"                , .atom = 0}, // 0
        {.value = "B", .descr = "AveragePriceTrade"   , .atom = 0}, // 1
        {.value = "C", .descr = "CashTrade"           , .atom = 0}, // 2
        {.value = "D", .descr = "NextDay"             , .atom = 0}, // 3
        {.value = "E", .descr = "Opening"             , .atom = 0}, // 4
        {.value = "F", .descr = "IntradayTradeDetail" , .atom = 0}, // 5
        {.value = "G", .descr = "Rule127Trade"        , .atom = 0}, // 6
        {.value = "H", .descr = "Rule155Trade"        , .atom = 0}, // 7
        {.value = "I", .descr = "SoldLast"            , .atom = 0}, // 8
        {.value = "J", .descr = "NextDayTrade"        , .atom = 0}, // 9
        {.value = "K", .descr = "Opened"              , .atom = 0}, // 10
        {.value = "L", .descr = "Seller"              , .atom = 0}, // 11
        {.value = "M", .descr = "Sold"                , .atom = 0}, // 12
        {.value = "N", .descr = "StoppedStock"        , .atom = 0}, // 13
        {.value = "P", .descr = "ImbalanceMoreBuyers" , .atom = 0}, // 14
        {.value = "Q", .descr = "ImbalanceMoreSellers", .atom = 0}, // 15
        {.value = "R", .descr = "OpeningPrice"        , .atom = 0}, // 16
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'A': return f.value_atom(0); // CASH
          case 'B': return f.value_atom(1); // AVERAGE_PRICE_TRADE
          case 'C': return f.value_atom(2); // CASH_TRADE
          case 'D': return f.value_atom(3); // NEXT_DAY
          case 'E': return f.value_atom(4); // OPENING
          case 'F': return f.value_atom(5); // INTRADAY_TRADE_DETAIL
          case 'G': return f.value_atom(6); // RULE_127_TRADE
          case 'H': return f.value_atom(7); // RULE_155_TRADE
          case 'I': return f.value_atom(8); // SOLD_LAST
          case 'J': return f.value_atom(9); // NEXT_DAY_TRADE
          case 'K': return f.value_atom(10); // OPENED
          case 'L': return f.value_atom(11); // SELLER
          case 'M': return f.value_atom(12); // SOLD
          case 'N': return f.value_atom(13); // STOPPED_STOCK
          case 'P': return f.value_atom(14); // IMBALANCE_MORE_BUYERS
          case 'Q': return f.value_atom(15); // IMBALANCE_MORE_SELLERS
          case 'R': return f.value_atom(16); // OPENING_PRICE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 278 "MDEntryID"
    Field{
      fvar,
      278,
      "MDEntryID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 279 "MDUpdateAction"
    Field{
      fvar,
      279,
      "MDUpdateAction",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "New"   , .atom = 0}, // 0
        {.value = "1", .descr = "Change", .atom = 0}, // 1
        {.value = "2", .descr = "Delete", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NEW
          case '1': return f.value_atom(1); // CHANGE
          case '2': return f.value_atom(2); // DELETE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 280 "MDEntryRefID"
    Field{
      fvar,
      280,
      "MDEntryRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 281 "MDReqRejReason"
    Field{
      fvar,
      281,
      "MDReqRejReason",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "UnknownSymbol"                     , .atom = 0}, // 0
        {.value = "1", .descr = "DuplicateMdreqid"                  , .atom = 0}, // 1
        {.value = "2", .descr = "InsufficientBandwidth"             , .atom = 0}, // 2
        {.value = "3", .descr = "InsufficientPermissions"           , .atom = 0}, // 3
        {.value = "4", .descr = "UnsupportedSubscriptionrequesttype", .atom = 0}, // 4
        {.value = "5", .descr = "UnsupportedMarketdepth"            , .atom = 0}, // 5
        {.value = "6", .descr = "UnsupportedMdupdatetype"           , .atom = 0}, // 6
        {.value = "7", .descr = "UnsupportedAggregatedbook"         , .atom = 0}, // 7
        {.value = "8", .descr = "UnsupportedMdentrytype"            , .atom = 0}, // 8
        {.value = "9", .descr = "UnsupportedTradingsessionid"       , .atom = 0}, // 9
        {.value = "A", .descr = "UnsupportedScope"                  , .atom = 0}, // 10
        {.value = "B", .descr = "UnsupportedOpenclosesettleflag"    , .atom = 0}, // 11
        {.value = "C", .descr = "UnsupportedMdimplicitdelete"       , .atom = 0}, // 12
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // UNKNOWN_SYMBOL
          case '1': return f.value_atom(1); // DUPLICATE_MDREQID
          case '2': return f.value_atom(2); // INSUFFICIENT_BANDWIDTH
          case '3': return f.value_atom(3); // INSUFFICIENT_PERMISSIONS
          case '4': return f.value_atom(4); // UNSUPPORTED_SUBSCRIPTIONREQUESTTYPE
          case '5': return f.value_atom(5); // UNSUPPORTED_MARKETDEPTH
          case '6': return f.value_atom(6); // UNSUPPORTED_MDUPDATETYPE
          case '7': return f.value_atom(7); // UNSUPPORTED_AGGREGATEDBOOK
          case '8': return f.value_atom(8); // UNSUPPORTED_MDENTRYTYPE
          case '9': return f.value_atom(9); // UNSUPPORTED_TRADINGSESSIONID
          case 'A': return f.value_atom(10); // UNSUPPORTED_SCOPE
          case 'B': return f.value_atom(11); // UNSUPPORTED_OPENCLOSESETTLEFLAG
          case 'C': return f.value_atom(12); // UNSUPPORTED_MDIMPLICITDELETE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 282 "MDEntryOriginator"
    Field{
      fvar,
      282,
      "MDEntryOriginator",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 283 "LocationID"
    Field{
      fvar,
      283,
      "LocationID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 284 "DeskID"
    Field{
      fvar,
      284,
      "DeskID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 285 "DeleteReason"
    Field{
      fvar,
      285,
      "DeleteReason",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Cancelation", .atom = 0}, // 0
        {.value = "1", .descr = "Error"      , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // CANCELATION
          case '1': return f.value_atom(1); // ERROR
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 286 "OpenCloseSettlFlag"
    Field{
      fvar,
      286,
      "OpenCloseSettlFlag",
      FieldType::MULTIPLEVALUESTRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "DailyOpen"                   , .atom = 0}, // 0
        {.value = "1", .descr = "SessionOpen"                 , .atom = 0}, // 1
        {.value = "2", .descr = "DeliverySettlementEntry"     , .atom = 0}, // 2
        {.value = "3", .descr = "ExpectedEntry"               , .atom = 0}, // 3
        {.value = "4", .descr = "EntryFromPreviousBusinessDay", .atom = 0}, // 4
        {.value = "5", .descr = "TheoreticalPriceValue"       , .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // DAILY_OPEN
          case '1': return f.value_atom(1); // SESSION_OPEN
          case '2': return f.value_atom(2); // DELIVERY_SETTLEMENT_ENTRY
          case '3': return f.value_atom(3); // EXPECTED_ENTRY
          case '4': return f.value_atom(4); // ENTRY_FROM_PREVIOUS_BUSINESS_DAY
          case '5': return f.value_atom(5); // THEORETICAL_PRICE_VALUE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 287 "SellerDays"
    Field{
      fvar,
      287,
      "SellerDays",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 288 "MDEntryBuyer"
    Field{
      fvar,
      288,
      "MDEntryBuyer",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 289 "MDEntrySeller"
    Field{
      fvar,
      289,
      "MDEntrySeller",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 290 "MDEntryPositionNo"
    Field{
      fvar,
      290,
      "MDEntryPositionNo",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 291 "FinancialStatus"
    Field{
      fvar,
      291,
      "FinancialStatus",
      FieldType::MULTIPLEVALUESTRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Bankrupt"        , .atom = 0}, // 0
        {.value = "2", .descr = "PendingDelisting", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // BANKRUPT
          case '2': return f.value_atom(1); // PENDING_DELISTING
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 292 "CorporateAction"
    Field{
      fvar,
      292,
      "CorporateAction",
      FieldType::MULTIPLEVALUESTRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "A", .descr = "ExDividend"    , .atom = 0}, // 0
        {.value = "B", .descr = "ExDistribution", .atom = 0}, // 1
        {.value = "C", .descr = "ExRights"      , .atom = 0}, // 2
        {.value = "D", .descr = "New"           , .atom = 0}, // 3
        {.value = "E", .descr = "ExInterest"    , .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'A': return f.value_atom(0); // EX_DIVIDEND
          case 'B': return f.value_atom(1); // EX_DISTRIBUTION
          case 'C': return f.value_atom(2); // EX_RIGHTS
          case 'D': return f.value_atom(3); // NEW
          case 'E': return f.value_atom(4); // EX_INTEREST
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 293 "DefBidSize"
    Field{
      fvar,
      293,
      "DefBidSize",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 294 "DefOfferSize"
    Field{
      fvar,
      294,
      "DefOfferSize",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 295 "NoQuoteEntries"
    Field{
      fvar,
      295,
      "NoQuoteEntries",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoQuoteEntries",
      295,
      nullptr
    },
    //--- Tag# 296 "NoQuoteSets"
    Field{
      fvar,
      296,
      "NoQuoteSets",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoQuoteSets",
      296,
      nullptr
    },
    //--- Tag# 297 "QuoteStatus"
    Field{
      fvar,
      297,
      "QuoteStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "Accepted"                , .atom = 0}, // 0
        {.value = "1" , .descr = "CanceledForSymbol"       , .atom = 0}, // 1
        {.value = "2" , .descr = "CanceledForSecurityType" , .atom = 0}, // 2
        {.value = "3" , .descr = "CanceledForUnderlying"   , .atom = 0}, // 3
        {.value = "4" , .descr = "CanceledAll"             , .atom = 0}, // 4
        {.value = "5" , .descr = "Rejected"                , .atom = 0}, // 5
        {.value = "6" , .descr = "RemovedFromMarket"       , .atom = 0}, // 6
        {.value = "7" , .descr = "Expired"                 , .atom = 0}, // 7
        {.value = "8" , .descr = "Query"                   , .atom = 0}, // 8
        {.value = "9" , .descr = "QuoteNotFound"           , .atom = 0}, // 9
        {.value = "10", .descr = "Pending"                 , .atom = 0}, // 10
        {.value = "11", .descr = "Pass"                    , .atom = 0}, // 11
        {.value = "12", .descr = "LockedMarketWarning"     , .atom = 0}, // 12
        {.value = "13", .descr = "CrossMarketWarning"      , .atom = 0}, // 13
        {.value = "14", .descr = "CanceledDueToLockMarket" , .atom = 0}, // 14
        {.value = "15", .descr = "CanceledDueToCrossMarket", .atom = 0}, // 15
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom( 0); // Accepted
          case           '1': return f.value_atom( 1); // CanceledForSymbol
          case           '2': return f.value_atom( 2); // CanceledForSecurityType
          case           '3': return f.value_atom( 3); // CanceledForUnderlying
          case           '4': return f.value_atom( 4); // CanceledAll
          case           '5': return f.value_atom( 5); // Rejected
          case           '6': return f.value_atom( 6); // RemovedFromMarket
          case           '7': return f.value_atom( 7); // Expired
          case           '8': return f.value_atom( 8); // Query
          case           '9': return f.value_atom( 9); // QuoteNotFound
          case CINT<'1','0'>: return f.value_atom(10); // Pending
          case CINT<'1','1'>: return f.value_atom(11); // Pass
          case CINT<'1','2'>: return f.value_atom(12); // LockedMarketWarning
          case CINT<'1','3'>: return f.value_atom(13); // CrossMarketWarning
          case CINT<'1','4'>: return f.value_atom(14); // CanceledDueToLockMarket
          case CINT<'1','5'>: return f.value_atom(15); // CanceledDueToCrossMarket
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 298 "QuoteCancelType"
    Field{
      fvar,
      298,
      "QuoteCancelType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "CancelForSymbol"          , .atom = 0}, // 0
        {.value = "2", .descr = "CancelForSecurityType"    , .atom = 0}, // 1
        {.value = "3", .descr = "CancelForUnderlyingSymbol", .atom = 0}, // 2
        {.value = "4", .descr = "CancelAllQuotes"          , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // CANCEL_FOR_SYMBOL
          case '2': return f.value_atom(1); // CANCEL_FOR_SECURITY_TYPE
          case '3': return f.value_atom(2); // CANCEL_FOR_UNDERLYING_SYMBOL
          case '4': return f.value_atom(3); // CANCEL_ALL_QUOTES
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 299 "QuoteEntryID"
    Field{
      fvar,
      299,
      "QuoteEntryID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 300 "QuoteRejectReason"
    Field{
      fvar,
      300,
      "QuoteRejectReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "UnknownSymbol"               , .atom = 0}, // 0
        {.value = "2" , .descr = "Exchange"                    , .atom = 0}, // 1
        {.value = "3" , .descr = "QuoteRequestExceedsLimit"    , .atom = 0}, // 2
        {.value = "4" , .descr = "TooLateToEnter"              , .atom = 0}, // 3
        {.value = "5" , .descr = "UnknownQuote"                , .atom = 0}, // 4
        {.value = "6" , .descr = "DuplicateQuote"              , .atom = 0}, // 5
        {.value = "7" , .descr = "InvalidBidAskSpread"         , .atom = 0}, // 6
        {.value = "8" , .descr = "InvalidPrice"                , .atom = 0}, // 7
        {.value = "9" , .descr = "NotAuthorizedToQuoteSecurity", .atom = 0}, // 8
        {.value = "99", .descr = "Other"                       , .atom = 0}, // 9
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // UnknownSymbol
          case           '2': return f.value_atom( 1); // Exchange
          case           '3': return f.value_atom( 2); // QuoteRequestExceedsLimit
          case           '4': return f.value_atom( 3); // TooLateToEnter
          case           '5': return f.value_atom( 4); // UnknownQuote
          case           '6': return f.value_atom( 5); // DuplicateQuote
          case           '7': return f.value_atom( 6); // InvalidBidAskSpread
          case           '8': return f.value_atom( 7); // InvalidPrice
          case           '9': return f.value_atom( 8); // NotAuthorizedToQuoteSecurity
          case CINT<'9','9'>: return f.value_atom( 9); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 301 "QuoteResponseLevel"
    Field{
      fvar,
      301,
      "QuoteResponseLevel",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "NoAcknowledgement"                       , .atom = 0}, // 0
        {.value = "1", .descr = "AcknowledgeOnlyNegativeOrErroneousQuotes", .atom = 0}, // 1
        {.value = "2", .descr = "AcknowledgeEachQuoteMessages"            , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NO_ACKNOWLEDGEMENT
          case '1': return f.value_atom(1); // ACKNOWLEDGE_ONLY_NEGATIVE_OR_ERRONEOUS_QUOTES
          case '2': return f.value_atom(2); // ACKNOWLEDGE_EACH_QUOTE_MESSAGES
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 302 "QuoteSetID"
    Field{
      fvar,
      302,
      "QuoteSetID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 303 "QuoteRequestType"
    Field{
      fvar,
      303,
      "QuoteRequestType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Manual"   , .atom = 0}, // 0
        {.value = "2", .descr = "Automatic", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // MANUAL
          case '2': return f.value_atom(1); // AUTOMATIC
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 304 "TotNoQuoteEntries"
    Field{
      fvar,
      304,
      "TotNoQuoteEntries",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 305 "UnderlyingSecurityIDSource"
    Field{
      fvar,
      305,
      "UnderlyingSecurityIDSource",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 306 "UnderlyingIssuer"
    Field{
      fvar,
      306,
      "UnderlyingIssuer",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 307 "UnderlyingSecurityDesc"
    Field{
      fvar,
      307,
      "UnderlyingSecurityDesc",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 308 "UnderlyingSecurityExchange"
    Field{
      fvar,
      308,
      "UnderlyingSecurityExchange",
      FieldType::EXCHANGE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 309 "UnderlyingSecurityID"
    Field{
      fvar,
      309,
      "UnderlyingSecurityID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 310 "UnderlyingSecurityType"
    Field{
      fvar,
      310,
      "UnderlyingSecurityType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 311 "UnderlyingSymbol"
    Field{
      fvar,
      311,
      "UnderlyingSymbol",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 312 "UnderlyingSymbolSfx"
    Field{
      fvar,
      312,
      "UnderlyingSymbolSfx",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 313 "UnderlyingMaturityMonthYear"
    Field{
      fvar,
      313,
      "UnderlyingMaturityMonthYear",
      FieldType::MONTHYEAR,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 314
    Field{},
    //--- Tag# 315 "UnderlyingPutOrCall"
    Field{
      fvar,
      315,
      "UnderlyingPutOrCall",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 316 "UnderlyingStrikePrice"
    Field{
      fvar,
      316,
      "UnderlyingStrikePrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 317 "UnderlyingOptAttribute"
    Field{
      fvar,
      317,
      "UnderlyingOptAttribute",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 318 "UnderlyingCurrency"
    Field{
      fvar,
      318,
      "UnderlyingCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 319
    Field{},
    //--- Tag# 320 "SecurityReqID"
    Field{
      fvar,
      320,
      "SecurityReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 321 "SecurityRequestType"
    Field{
      fvar,
      321,
      "SecurityRequestType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "RequestSecurityIdentityAndSpecifications"           , .atom = 0}, // 0
        {.value = "1", .descr = "RequestSecurityIdentityForTheSpecificationsProvided", .atom = 0}, // 1
        {.value = "2", .descr = "RequestListSecurityTypes"                           , .atom = 0}, // 2
        {.value = "3", .descr = "RequestListSecurities"                              , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // REQUEST_SECURITY_IDENTITY_AND_SPECIFICATIONS
          case '1': return f.value_atom(1); // REQUEST_SECURITY_IDENTITY_FOR_THE_SPECIFICATIONS_PROVIDED
          case '2': return f.value_atom(2); // REQUEST_LIST_SECURITY_TYPES
          case '3': return f.value_atom(3); // REQUEST_LIST_SECURITIES
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 322 "SecurityResponseID"
    Field{
      fvar,
      322,
      "SecurityResponseID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 323 "SecurityResponseType"
    Field{
      fvar,
      323,
      "SecurityResponseType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "AcceptSecurityProposalAsIs"                                , .atom = 0}, // 0
        {.value = "2", .descr = "AcceptSecurityProposalWithRevisionsAsIndicatedInTheMessage", .atom = 0}, // 1
        {.value = "5", .descr = "RejectSecurityProposal"                                    , .atom = 0}, // 2
        {.value = "6", .descr = "CanNotMatchSelectionCriteria"                              , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // ACCEPT_SECURITY_PROPOSAL_AS_IS
          case '2': return f.value_atom(1); // ACCEPT_SECURITY_PROPOSAL_WITH_REVISIONS_AS_INDICATED_IN_THE_MESSAGE
          case '5': return f.value_atom(2); // REJECT_SECURITY_PROPOSAL
          case '6': return f.value_atom(3); // CAN_NOT_MATCH_SELECTION_CRITERIA
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 324 "SecurityStatusReqID"
    Field{
      fvar,
      324,
      "SecurityStatusReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 325 "UnsolicitedIndicator"
    Field{
      fvar,
      325,
      "UnsolicitedIndicator",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 326 "SecurityTradingStatus"
    Field{
      fvar,
      326,
      "SecurityTradingStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "OpeningDelay"              , .atom = 0}, // 0
        {.value = "2" , .descr = "TradingHalt"               , .atom = 0}, // 1
        {.value = "3" , .descr = "Resume"                    , .atom = 0}, // 2
        {.value = "4" , .descr = "NoOpenNoResume"            , .atom = 0}, // 3
        {.value = "5" , .descr = "PriceIndication"           , .atom = 0}, // 4
        {.value = "6" , .descr = "TradingRangeIndication"    , .atom = 0}, // 5
        {.value = "7" , .descr = "MarketImbalanceBuy"        , .atom = 0}, // 6
        {.value = "8" , .descr = "MarketImbalanceSell"       , .atom = 0}, // 7
        {.value = "9" , .descr = "MarketOnCloseImbalanceBuy" , .atom = 0}, // 8
        {.value = "10", .descr = "MarketOnCloseImbalanceSell", .atom = 0}, // 9
        {.value = "12", .descr = "NoMarketImbalance"         , .atom = 0}, // 10
        {.value = "13", .descr = "NoMarketOnCloseImbalance"  , .atom = 0}, // 11
        {.value = "14", .descr = "ItsPreOpening"             , .atom = 0}, // 12
        {.value = "15", .descr = "NewPriceIndication"        , .atom = 0}, // 13
        {.value = "16", .descr = "TradeDisseminationTime"    , .atom = 0}, // 14
        {.value = "17", .descr = "ReadyToTrade"              , .atom = 0}, // 15
        {.value = "18", .descr = "NotAvailableForTrading"    , .atom = 0}, // 16
        {.value = "19", .descr = "NotTradedOnThisMarket"     , .atom = 0}, // 17
        {.value = "20", .descr = "UnknownOrInvalid"          , .atom = 0}, // 18
        {.value = "21", .descr = "PreOpen"                   , .atom = 0}, // 19
        {.value = "22", .descr = "OpeningRotation"           , .atom = 0}, // 20
        {.value = "23", .descr = "FastMarket"                , .atom = 0}, // 21
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // OpeningDelay
          case           '2': return f.value_atom( 1); // TradingHalt
          case           '3': return f.value_atom( 2); // Resume
          case           '4': return f.value_atom( 3); // NoOpenNoResume
          case           '5': return f.value_atom( 4); // PriceIndication
          case           '6': return f.value_atom( 5); // TradingRangeIndication
          case           '7': return f.value_atom( 6); // MarketImbalanceBuy
          case           '8': return f.value_atom( 7); // MarketImbalanceSell
          case           '9': return f.value_atom( 8); // MarketOnCloseImbalanceBuy
          case CINT<'1','0'>: return f.value_atom( 9); // MarketOnCloseImbalanceSell
          case CINT<'1','2'>: return f.value_atom(10); // NoMarketImbalance
          case CINT<'1','3'>: return f.value_atom(11); // NoMarketOnCloseImbalance
          case CINT<'1','4'>: return f.value_atom(12); // ItsPreOpening
          case CINT<'1','5'>: return f.value_atom(13); // NewPriceIndication
          case CINT<'1','6'>: return f.value_atom(14); // TradeDisseminationTime
          case CINT<'1','7'>: return f.value_atom(15); // ReadyToTrade
          case CINT<'1','8'>: return f.value_atom(16); // NotAvailableForTrading
          case CINT<'1','9'>: return f.value_atom(17); // NotTradedOnThisMarket
          case CINT<'2','0'>: return f.value_atom(18); // UnknownOrInvalid
          case CINT<'2','1'>: return f.value_atom(19); // PreOpen
          case CINT<'2','2'>: return f.value_atom(20); // OpeningRotation
          case CINT<'2','3'>: return f.value_atom(21); // FastMarket
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 327 "HaltReasonChar"
    Field{
      fvar,
      327,
      "HaltReasonChar",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "I", .descr = "OrderImbalance"       , .atom = 0}, // 0
        {.value = "X", .descr = "EquipmentChangeover"  , .atom = 0}, // 1
        {.value = "P", .descr = "NewsPending"          , .atom = 0}, // 2
        {.value = "D", .descr = "NewsDissemination"    , .atom = 0}, // 3
        {.value = "E", .descr = "OrderInflux"          , .atom = 0}, // 4
        {.value = "M", .descr = "AdditionalInformation", .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'I': return f.value_atom(0); // ORDER_IMBALANCE
          case 'X': return f.value_atom(1); // EQUIPMENT_CHANGEOVER
          case 'P': return f.value_atom(2); // NEWS_PENDING
          case 'D': return f.value_atom(3); // NEWS_DISSEMINATION
          case 'E': return f.value_atom(4); // ORDER_INFLUX
          case 'M': return f.value_atom(5); // ADDITIONAL_INFORMATION
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 328 "InViewOfCommon"
    Field{
      fvar,
      328,
      "InViewOfCommon",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 329 "DueToRelated"
    Field{
      fvar,
      329,
      "DueToRelated",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 330 "BuyVolume"
    Field{
      fvar,
      330,
      "BuyVolume",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 331 "SellVolume"
    Field{
      fvar,
      331,
      "SellVolume",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 332 "HighPx"
    Field{
      fvar,
      332,
      "HighPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 333 "LowPx"
    Field{
      fvar,
      333,
      "LowPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 334 "Adjustment"
    Field{
      fvar,
      334,
      "Adjustment",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Cancel"    , .atom = 0}, // 0
        {.value = "2", .descr = "Error"     , .atom = 0}, // 1
        {.value = "3", .descr = "Correction", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // CANCEL
          case '2': return f.value_atom(1); // ERROR
          case '3': return f.value_atom(2); // CORRECTION
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 335 "TradSesReqID"
    Field{
      fvar,
      335,
      "TradSesReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 336 "TradingSessionID"
    Field{
      fvar,
      336,
      "TradingSessionID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 337 "ContraTrader"
    Field{
      fvar,
      337,
      "ContraTrader",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 338 "TradSesMethod"
    Field{
      fvar,
      338,
      "TradSesMethod",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Electronic", .atom = 0}, // 0
        {.value = "2", .descr = "OpenOutcry", .atom = 0}, // 1
        {.value = "3", .descr = "TwoParty"  , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // ELECTRONIC
          case '2': return f.value_atom(1); // OPEN_OUTCRY
          case '3': return f.value_atom(2); // TWO_PARTY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 339 "TradSesMode"
    Field{
      fvar,
      339,
      "TradSesMode",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Testing"   , .atom = 0}, // 0
        {.value = "2", .descr = "Simulated" , .atom = 0}, // 1
        {.value = "3", .descr = "Production", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // TESTING
          case '2': return f.value_atom(1); // SIMULATED
          case '3': return f.value_atom(2); // PRODUCTION
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 340 "TradSesStatus"
    Field{
      fvar,
      340,
      "TradSesStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Unknown"        , .atom = 0}, // 0
        {.value = "1", .descr = "Halted"         , .atom = 0}, // 1
        {.value = "2", .descr = "Open"           , .atom = 0}, // 2
        {.value = "3", .descr = "Closed"         , .atom = 0}, // 3
        {.value = "4", .descr = "PreOpen"        , .atom = 0}, // 4
        {.value = "5", .descr = "PreClose"       , .atom = 0}, // 5
        {.value = "6", .descr = "RequestRejected", .atom = 0}, // 6
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // UNKNOWN
          case '1': return f.value_atom(1); // HALTED
          case '2': return f.value_atom(2); // OPEN
          case '3': return f.value_atom(3); // CLOSED
          case '4': return f.value_atom(4); // PRE_OPEN
          case '5': return f.value_atom(5); // PRE_CLOSE
          case '6': return f.value_atom(6); // REQUEST_REJECTED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 341 "TradSesStartTime"
    Field{
      fvar,
      341,
      "TradSesStartTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 342 "TradSesOpenTime"
    Field{
      fvar,
      342,
      "TradSesOpenTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 343 "TradSesPreCloseTime"
    Field{
      fvar,
      343,
      "TradSesPreCloseTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 344 "TradSesCloseTime"
    Field{
      fvar,
      344,
      "TradSesCloseTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 345 "TradSesEndTime"
    Field{
      fvar,
      345,
      "TradSesEndTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 346 "NumberOfOrders"
    Field{
      fvar,
      346,
      "NumberOfOrders",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 347 "MessageEncoding"
    Field{
      fvar,
      347,
      "MessageEncoding",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "ISO-2022-JP", .descr = "Jis"         , .atom = 0}, // 0
        {.value = "EUC-JP"     , .descr = "Euc"         , .atom = 0}, // 1
        {.value = "Shift_JIS"  , .descr = "ForUsingSjis", .atom = 0}, // 2
        {.value = "UTF-8"      , .descr = "Unicode"     , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto   fc = f.value(std::string_view(code, len));
        return fc ? fc->get_atom(env) : am_undefined;
      },
    },
    //--- Tag# 348 "EncodedIssuerLen"
    Field{
      fvar,
      348,
      "EncodedIssuerLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 349 "EncodedIssuer"
    Field{
      fvar,
      349,
      "EncodedIssuer",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedIssuerLen",
      348,
      nullptr
    },
    //--- Tag# 350 "EncodedSecurityDescLen"
    Field{
      fvar,
      350,
      "EncodedSecurityDescLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 351 "EncodedSecurityDesc"
    Field{
      fvar,
      351,
      "EncodedSecurityDesc",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedSecurityDescLen",
      350,
      nullptr
    },
    //--- Tag# 352 "EncodedListExecInstLen"
    Field{
      fvar,
      352,
      "EncodedListExecInstLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 353 "EncodedListExecInst"
    Field{
      fvar,
      353,
      "EncodedListExecInst",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedListExecInstLen",
      352,
      nullptr
    },
    //--- Tag# 354 "EncodedTextLen"
    Field{
      fvar,
      354,
      "EncodedTextLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 355 "EncodedText"
    Field{
      fvar,
      355,
      "EncodedText",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedTextLen",
      354,
      nullptr
    },
    //--- Tag# 356 "EncodedSubjectLen"
    Field{
      fvar,
      356,
      "EncodedSubjectLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 357 "EncodedSubject"
    Field{
      fvar,
      357,
      "EncodedSubject",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedSubjectLen",
      356,
      nullptr
    },
    //--- Tag# 358 "EncodedHeadlineLen"
    Field{
      fvar,
      358,
      "EncodedHeadlineLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 359 "EncodedHeadline"
    Field{
      fvar,
      359,
      "EncodedHeadline",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedHeadlineLen",
      358,
      nullptr
    },
    //--- Tag# 360 "EncodedAllocTextLen"
    Field{
      fvar,
      360,
      "EncodedAllocTextLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 361 "EncodedAllocText"
    Field{
      fvar,
      361,
      "EncodedAllocText",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedAllocTextLen",
      360,
      nullptr
    },
    //--- Tag# 362 "EncodedUnderlyingIssuerLen"
    Field{
      fvar,
      362,
      "EncodedUnderlyingIssuerLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 363 "EncodedUnderlyingIssuer"
    Field{
      fvar,
      363,
      "EncodedUnderlyingIssuer",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedUnderlyingIssuerLen",
      362,
      nullptr
    },
    //--- Tag# 364 "EncodedUnderlyingSecurityDescLen"
    Field{
      fvar,
      364,
      "EncodedUnderlyingSecurityDescLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 365 "EncodedUnderlyingSecurityDesc"
    Field{
      fvar,
      365,
      "EncodedUnderlyingSecurityDesc",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedUnderlyingSecurityDescLen",
      364,
      nullptr
    },
    //--- Tag# 366 "AllocPrice"
    Field{
      fvar,
      366,
      "AllocPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 367 "QuoteSetValidUntilTime"
    Field{
      fvar,
      367,
      "QuoteSetValidUntilTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 368 "QuoteEntryRejectReason"
    Field{
      fvar,
      368,
      "QuoteEntryRejectReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 369 "LastMsgSeqNumProcessed"
    Field{
      fvar,
      369,
      "LastMsgSeqNumProcessed",
      FieldType::SEQNUM,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 370
    Field{},
    //--- Tag# 371 "RefTagID"
    Field{
      fvar,
      371,
      "RefTagID",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 372 "RefMsgType"
    Field{
      fvar,
      372,
      "RefMsgType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 373 "SessionRejectReason"
    Field{
      fvar,
      373,
      "SessionRejectReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "InvalidTagNumber"                         , .atom = 0}, // 0
        {.value = "1" , .descr = "RequiredTagMissing"                       , .atom = 0}, // 1
        {.value = "2" , .descr = "TagNotDefinedForThisMessageType"          , .atom = 0}, // 2
        {.value = "3" , .descr = "UndefinedTag"                             , .atom = 0}, // 3
        {.value = "4" , .descr = "TagSpecifiedWithoutAValue"                , .atom = 0}, // 4
        {.value = "5" , .descr = "ValueIsIncorrect"                         , .atom = 0}, // 5
        {.value = "6" , .descr = "IncorrectDataFormatForValue"              , .atom = 0}, // 6
        {.value = "7" , .descr = "DecryptionProblem"                        , .atom = 0}, // 7
        {.value = "8" , .descr = "SignatureProblem"                         , .atom = 0}, // 8
        {.value = "9" , .descr = "CompidProblem"                            , .atom = 0}, // 9
        {.value = "10", .descr = "SendingtimeAccuracyProblem"               , .atom = 0}, // 10
        {.value = "11", .descr = "InvalidMsgtype"                           , .atom = 0}, // 11
        {.value = "12", .descr = "XmlValidationError"                       , .atom = 0}, // 12
        {.value = "13", .descr = "TagAppearsMoreThanOnce"                   , .atom = 0}, // 13
        {.value = "14", .descr = "TagSpecifiedOutOfRequiredOrder"           , .atom = 0}, // 14
        {.value = "15", .descr = "RepeatingGroupFieldsOutOfOrder"           , .atom = 0}, // 15
        {.value = "16", .descr = "IncorrectNumingroupCountForRepeatingGroup", .atom = 0}, // 16
        {.value = "17", .descr = "NonDataValueIncludesFieldDelimiter"       , .atom = 0}, // 17
        {.value = "99", .descr = "Other"                                    , .atom = 0}, // 18
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom( 0); // InvalidTagNumber
          case           '1': return f.value_atom( 1); // RequiredTagMissing
          case           '2': return f.value_atom( 2); // TagNotDefinedForThisMessageType
          case           '3': return f.value_atom( 3); // UndefinedTag
          case           '4': return f.value_atom( 4); // TagSpecifiedWithoutAValue
          case           '5': return f.value_atom( 5); // ValueIsIncorrect
          case           '6': return f.value_atom( 6); // IncorrectDataFormatForValue
          case           '7': return f.value_atom( 7); // DecryptionProblem
          case           '8': return f.value_atom( 8); // SignatureProblem
          case           '9': return f.value_atom( 9); // CompidProblem
          case CINT<'1','0'>: return f.value_atom(10); // SendingtimeAccuracyProblem
          case CINT<'1','1'>: return f.value_atom(11); // InvalidMsgtype
          case CINT<'1','2'>: return f.value_atom(12); // XmlValidationError
          case CINT<'1','3'>: return f.value_atom(13); // TagAppearsMoreThanOnce
          case CINT<'1','4'>: return f.value_atom(14); // TagSpecifiedOutOfRequiredOrder
          case CINT<'1','5'>: return f.value_atom(15); // RepeatingGroupFieldsOutOfOrder
          case CINT<'1','6'>: return f.value_atom(16); // IncorrectNumingroupCountForRepeatingGroup
          case CINT<'1','7'>: return f.value_atom(17); // NonDataValueIncludesFieldDelimiter
          case CINT<'9','9'>: return f.value_atom(18); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 374 "BidRequestTransType"
    Field{
      fvar,
      374,
      "BidRequestTransType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "N", .descr = "New"   , .atom = 0}, // 0
        {.value = "C", .descr = "Cancel", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'N': return f.value_atom(0); // NEW
          case 'C': return f.value_atom(1); // CANCEL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 375 "ContraBroker"
    Field{
      fvar,
      375,
      "ContraBroker",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 376 "ComplianceID"
    Field{
      fvar,
      376,
      "ComplianceID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 377 "SolicitedFlag"
    Field{
      fvar,
      377,
      "SolicitedFlag",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 378 "ExecRestatementReason"
    Field{
      fvar,
      378,
      "ExecRestatementReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "GtCorporateAction"       , .atom = 0}, // 0
        {.value = "1" , .descr = "GtRenewal"               , .atom = 0}, // 1
        {.value = "2" , .descr = "VerbalChange"            , .atom = 0}, // 2
        {.value = "3" , .descr = "RepricingOfOrder"        , .atom = 0}, // 3
        {.value = "4" , .descr = "BrokerOption"            , .atom = 0}, // 4
        {.value = "5" , .descr = "PartialDeclineOfOrderqty", .atom = 0}, // 5
        {.value = "6" , .descr = "CancelOnTradingHalt"     , .atom = 0}, // 6
        {.value = "7" , .descr = "CancelOnSystemFailure"   , .atom = 0}, // 7
        {.value = "8" , .descr = "Market"                  , .atom = 0}, // 8
        {.value = "9" , .descr = "CanceledNotBest"         , .atom = 0}, // 9
        {.value = "10", .descr = "WarehouseRecap"          , .atom = 0}, // 10
        {.value = "99", .descr = "Other"                   , .atom = 0}, // 11
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom( 0); // GtCorporateAction
          case           '1': return f.value_atom( 1); // GtRenewal
          case           '2': return f.value_atom( 2); // VerbalChange
          case           '3': return f.value_atom( 3); // RepricingOfOrder
          case           '4': return f.value_atom( 4); // BrokerOption
          case           '5': return f.value_atom( 5); // PartialDeclineOfOrderqty
          case           '6': return f.value_atom( 6); // CancelOnTradingHalt
          case           '7': return f.value_atom( 7); // CancelOnSystemFailure
          case           '8': return f.value_atom( 8); // Market
          case           '9': return f.value_atom( 9); // CanceledNotBest
          case CINT<'1','0'>: return f.value_atom(10); // WarehouseRecap
          case CINT<'9','9'>: return f.value_atom(11); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 379 "BusinessRejectRefID"
    Field{
      fvar,
      379,
      "BusinessRejectRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 380 "BusinessRejectReason"
    Field{
      fvar,
      380,
      "BusinessRejectReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Other"                              , .atom = 0}, // 0
        {.value = "1", .descr = "UnkownId"                           , .atom = 0}, // 1
        {.value = "2", .descr = "UnknownSecurity"                    , .atom = 0}, // 2
        {.value = "3", .descr = "UnsupportedMessageType"             , .atom = 0}, // 3
        {.value = "4", .descr = "ApplicationNotAvailable"            , .atom = 0}, // 4
        {.value = "5", .descr = "ConditionallyRequiredFieldMissing"  , .atom = 0}, // 5
        {.value = "6", .descr = "NotAuthorized"                      , .atom = 0}, // 6
        {.value = "7", .descr = "DelivertoFirmNotAvailableAtThisTime", .atom = 0}, // 7
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // OTHER
          case '1': return f.value_atom(1); // UNKOWN_ID
          case '2': return f.value_atom(2); // UNKNOWN_SECURITY
          case '3': return f.value_atom(3); // UNSUPPORTED_MESSAGE_TYPE
          case '4': return f.value_atom(4); // APPLICATION_NOT_AVAILABLE
          case '5': return f.value_atom(5); // CONDITIONALLY_REQUIRED_FIELD_MISSING
          case '6': return f.value_atom(6); // NOT_AUTHORIZED
          case '7': return f.value_atom(7); // DELIVERTO_FIRM_NOT_AVAILABLE_AT_THIS_TIME
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 381 "GrossTradeAmt"
    Field{
      fvar,
      381,
      "GrossTradeAmt",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 382 "NoContraBrokers"
    Field{
      fvar,
      382,
      "NoContraBrokers",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoContraBrokers",
      382,
      nullptr
    },
    //--- Tag# 383 "MaxMessageSize"
    Field{
      fvar,
      383,
      "MaxMessageSize",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 384 "NoMsgTypes"
    Field{
      fvar,
      384,
      "NoMsgTypes",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoMsgTypes",
      384,
      nullptr
    },
    //--- Tag# 385 "MsgDirection"
    Field{
      fvar,
      385,
      "MsgDirection",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "S", .descr = "Send"   , .atom = 0}, // 0
        {.value = "R", .descr = "Receive", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'S': return f.value_atom(0); // SEND
          case 'R': return f.value_atom(1); // RECEIVE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 386 "NoTradingSessions"
    Field{
      fvar,
      386,
      "NoTradingSessions",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoTradingSessions",
      386,
      nullptr
    },
    //--- Tag# 387 "TotalVolumeTraded"
    Field{
      fvar,
      387,
      "TotalVolumeTraded",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 388 "DiscretionInst"
    Field{
      fvar,
      388,
      "DiscretionInst",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "RelatedToDisplayedPrice"   , .atom = 0}, // 0
        {.value = "1", .descr = "RelatedToMarketPrice"      , .atom = 0}, // 1
        {.value = "2", .descr = "RelatedToPrimaryPrice"     , .atom = 0}, // 2
        {.value = "3", .descr = "RelatedToLocalPrimaryPrice", .atom = 0}, // 3
        {.value = "4", .descr = "RelatedToMidpointPrice"    , .atom = 0}, // 4
        {.value = "5", .descr = "RelatedToLastTradePrice"   , .atom = 0}, // 5
        {.value = "6", .descr = "RelatedToVwap"             , .atom = 0}, // 6
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // RELATED_TO_DISPLAYED_PRICE
          case '1': return f.value_atom(1); // RELATED_TO_MARKET_PRICE
          case '2': return f.value_atom(2); // RELATED_TO_PRIMARY_PRICE
          case '3': return f.value_atom(3); // RELATED_TO_LOCAL_PRIMARY_PRICE
          case '4': return f.value_atom(4); // RELATED_TO_MIDPOINT_PRICE
          case '5': return f.value_atom(5); // RELATED_TO_LAST_TRADE_PRICE
          case '6': return f.value_atom(6); // RELATED_TO_VWAP
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 389 "DiscretionOffsetValue"
    Field{
      fvar,
      389,
      "DiscretionOffsetValue",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 390 "BidID"
    Field{
      fvar,
      390,
      "BidID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 391 "ClientBidID"
    Field{
      fvar,
      391,
      "ClientBidID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 392 "ListName"
    Field{
      fvar,
      392,
      "ListName",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 393 "TotNoRelatedSym"
    Field{
      fvar,
      393,
      "TotNoRelatedSym",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 394 "BidType"
    Field{
      fvar,
      394,
      "BidType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "NonDisclosedStyle", .atom = 0}, // 0
        {.value = "2", .descr = "DisclosedStyle"   , .atom = 0}, // 1
        {.value = "3", .descr = "NoBiddingProcess" , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // NON_DISCLOSED_STYLE
          case '2': return f.value_atom(1); // DISCLOSED_STYLE
          case '3': return f.value_atom(2); // NO_BIDDING_PROCESS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 395 "NumTickets"
    Field{
      fvar,
      395,
      "NumTickets",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 396 "SideValue1"
    Field{
      fvar,
      396,
      "SideValue1",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 397 "SideValue2"
    Field{
      fvar,
      397,
      "SideValue2",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 398 "NoBidDescriptors"
    Field{
      fvar,
      398,
      "NoBidDescriptors",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoBidDescriptors",
      398,
      nullptr
    },
    //--- Tag# 399 "BidDescriptorType"
    Field{
      fvar,
      399,
      "BidDescriptorType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Sector" , .atom = 0}, // 0
        {.value = "2", .descr = "Country", .atom = 0}, // 1
        {.value = "3", .descr = "Index"  , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // SECTOR
          case '2': return f.value_atom(1); // COUNTRY
          case '3': return f.value_atom(2); // INDEX
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 400 "BidDescriptor"
    Field{
      fvar,
      400,
      "BidDescriptor",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 401 "SideValueInd"
    Field{
      fvar,
      401,
      "SideValueInd",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Sidevalue1", .atom = 0}, // 0
        {.value = "2", .descr = "Sidevalue2", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // SIDEVALUE1
          case '2': return f.value_atom(1); // SIDEVALUE_2
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 402 "LiquidityPctLow"
    Field{
      fvar,
      402,
      "LiquidityPctLow",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 403 "LiquidityPctHigh"
    Field{
      fvar,
      403,
      "LiquidityPctHigh",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 404 "LiquidityValue"
    Field{
      fvar,
      404,
      "LiquidityValue",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 405 "EFPTrackingError"
    Field{
      fvar,
      405,
      "EFPTrackingError",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 406 "FairValue"
    Field{
      fvar,
      406,
      "FairValue",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 407 "OutsideIndexPct"
    Field{
      fvar,
      407,
      "OutsideIndexPct",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 408 "ValueOfFutures"
    Field{
      fvar,
      408,
      "ValueOfFutures",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 409 "LiquidityIndType"
    Field{
      fvar,
      409,
      "LiquidityIndType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "5dayMovingAverage" , .atom = 0}, // 0
        {.value = "2", .descr = "20DayMovingAverage", .atom = 0}, // 1
        {.value = "3", .descr = "NormalMarketSize"  , .atom = 0}, // 2
        {.value = "4", .descr = "Other"             , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // 5DAY_MOVING_AVERAGE
          case '2': return f.value_atom(1); // 20_DAY_MOVING_AVERAGE
          case '3': return f.value_atom(2); // NORMAL_MARKET_SIZE
          case '4': return f.value_atom(3); // OTHER
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 410 "WtAverageLiquidity"
    Field{
      fvar,
      410,
      "WtAverageLiquidity",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 411 "ExchangeForPhysical"
    Field{
      fvar,
      411,
      "ExchangeForPhysical",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 412 "OutMainCntryUIndex"
    Field{
      fvar,
      412,
      "OutMainCntryUIndex",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 413 "CrossPercent"
    Field{
      fvar,
      413,
      "CrossPercent",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 414 "ProgRptReqs"
    Field{
      fvar,
      414,
      "ProgRptReqs",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "BuysideExplicitlyRequestsStatusUsingStatusrequest"                                      , .atom = 0}, // 0
        {.value = "2", .descr = "SellsidePeriodicallySendsStatusUsingListstatusPeriodOptionallySpecifiedInProgressperiod", .atom = 0}, // 1
        {.value = "3", .descr = "RealTimeExecutionReports"                                                               , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // BUYSIDE_EXPLICITLY_REQUESTS_STATUS_USING_STATUSREQUEST
          case '2': return f.value_atom(1); // SELLSIDE_PERIODICALLY_SENDS_STATUS_USING_LISTSTATUS_PERIOD_OPTIONALLY_SPECIFIED_IN_PROGRESSPERIOD
          case '3': return f.value_atom(2); // REAL_TIME_EXECUTION_REPORTS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 415 "ProgPeriodInterval"
    Field{
      fvar,
      415,
      "ProgPeriodInterval",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 416 "IncTaxInd"
    Field{
      fvar,
      416,
      "IncTaxInd",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Net"  , .atom = 0}, // 0
        {.value = "2", .descr = "Gross", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // NET
          case '2': return f.value_atom(1); // GROSS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 417 "NumBidders"
    Field{
      fvar,
      417,
      "NumBidders",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 418 "BidTradeType"
    Field{
      fvar,
      418,
      "BidTradeType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "R", .descr = "RiskTrade"      , .atom = 0}, // 0
        {.value = "G", .descr = "VwapGuarantee"  , .atom = 0}, // 1
        {.value = "A", .descr = "Agency"         , .atom = 0}, // 2
        {.value = "J", .descr = "GuaranteedClose", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'R': return f.value_atom(0); // RISK_TRADE
          case 'G': return f.value_atom(1); // VWAP_GUARANTEE
          case 'A': return f.value_atom(2); // AGENCY
          case 'J': return f.value_atom(3); // GUARANTEED_CLOSE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 419 "BasisPxType"
    Field{
      fvar,
      419,
      "BasisPxType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "2", .descr = "ClosingPriceAtMorningSession"           , .atom = 0}, // 0
        {.value = "3", .descr = "ClosingPrice"                           , .atom = 0}, // 1
        {.value = "4", .descr = "CurrentPrice"                           , .atom = 0}, // 2
        {.value = "5", .descr = "Sq"                                     , .atom = 0}, // 3
        {.value = "6", .descr = "VwapThroughADay"                        , .atom = 0}, // 4
        {.value = "7", .descr = "VwapThroughAMorningSession"             , .atom = 0}, // 5
        {.value = "8", .descr = "VwapThroughAnAfternoonSession"          , .atom = 0}, // 6
        {.value = "9", .descr = "VwapThroughADayExceptYori"              , .atom = 0}, // 7
        {.value = "A", .descr = "VwapThroughAMorningSessionExceptYori"   , .atom = 0}, // 8
        {.value = "B", .descr = "VwapThroughAnAfternoonSessionExceptYori", .atom = 0}, // 9
        {.value = "C", .descr = "Strike"                                 , .atom = 0}, // 10
        {.value = "D", .descr = "Open"                                   , .atom = 0}, // 11
        {.value = "Z", .descr = "Others"                                 , .atom = 0}, // 12
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '2': return f.value_atom(0); // CLOSING_PRICE_AT_MORNING_SESSION
          case '3': return f.value_atom(1); // CLOSING_PRICE
          case '4': return f.value_atom(2); // CURRENT_PRICE
          case '5': return f.value_atom(3); // SQ
          case '6': return f.value_atom(4); // VWAP_THROUGH_A_DAY
          case '7': return f.value_atom(5); // VWAP_THROUGH_A_MORNING_SESSION
          case '8': return f.value_atom(6); // VWAP_THROUGH_AN_AFTERNOON_SESSION
          case '9': return f.value_atom(7); // VWAP_THROUGH_A_DAY_EXCEPT_YORI
          case 'A': return f.value_atom(8); // VWAP_THROUGH_A_MORNING_SESSION_EXCEPT_YORI
          case 'B': return f.value_atom(9); // VWAP_THROUGH_AN_AFTERNOON_SESSION_EXCEPT_YORI
          case 'C': return f.value_atom(10); // STRIKE
          case 'D': return f.value_atom(11); // OPEN
          case 'Z': return f.value_atom(12); // OTHERS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 420 "NoBidComponents"
    Field{
      fvar,
      420,
      "NoBidComponents",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoBidComponents",
      420,
      nullptr
    },
    //--- Tag# 421 "Country"
    Field{
      fvar,
      421,
      "Country",
      FieldType::COUNTRY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 422 "TotNoStrikes"
    Field{
      fvar,
      422,
      "TotNoStrikes",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 423 "PriceType"
    Field{
      fvar,
      423,
      "PriceType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "Percentage"                      , .atom = 0}, // 0
        {.value = "2" , .descr = "PerUnit"                         , .atom = 0}, // 1
        {.value = "3" , .descr = "FixedAmount"                     , .atom = 0}, // 2
        {.value = "4" , .descr = "DiscountPercentagePointsBelowPar", .atom = 0}, // 3
        {.value = "5" , .descr = "PremiumPercentagePointsOverPar"  , .atom = 0}, // 4
        {.value = "6" , .descr = "Spread"                          , .atom = 0}, // 5
        {.value = "7" , .descr = "TedPrice"                        , .atom = 0}, // 6
        {.value = "8" , .descr = "TedYield"                        , .atom = 0}, // 7
        {.value = "9" , .descr = "Yield"                           , .atom = 0}, // 8
        {.value = "10", .descr = "FixedCabinetTradePrice"          , .atom = 0}, // 9
        {.value = "11", .descr = "VariableCabinetTradePrice"       , .atom = 0}, // 10
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // Percentage
          case           '2': return f.value_atom( 1); // PerUnit
          case           '3': return f.value_atom( 2); // FixedAmount
          case           '4': return f.value_atom( 3); // DiscountPercentagePointsBelowPar
          case           '5': return f.value_atom( 4); // PremiumPercentagePointsOverPar
          case           '6': return f.value_atom( 5); // Spread
          case           '7': return f.value_atom( 6); // TedPrice
          case           '8': return f.value_atom( 7); // TedYield
          case           '9': return f.value_atom( 8); // Yield
          case CINT<'1','0'>: return f.value_atom( 9); // FixedCabinetTradePrice
          case CINT<'1','1'>: return f.value_atom(10); // VariableCabinetTradePrice
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 424 "DayOrderQty"
    Field{
      fvar,
      424,
      "DayOrderQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 425 "DayCumQty"
    Field{
      fvar,
      425,
      "DayCumQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 426 "DayAvgPx"
    Field{
      fvar,
      426,
      "DayAvgPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 427 "GTBookingInst"
    Field{
      fvar,
      427,
      "GTBookingInst",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "BookOutAllTradesOnDayOfExecution"               , .atom = 0}, // 0
        {.value = "1", .descr = "AccumulateExecutionsUntilOrderIsFilledOrExpires", .atom = 0}, // 1
        {.value = "2", .descr = "AccumulateUntilVerballyNotifiedOtherwise"       , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // BOOK_OUT_ALL_TRADES_ON_DAY_OF_EXECUTION
          case '1': return f.value_atom(1); // ACCUMULATE_EXECUTIONS_UNTIL_ORDER_IS_FILLED_OR_EXPIRES
          case '2': return f.value_atom(2); // ACCUMULATE_UNTIL_VERBALLY_NOTIFIED_OTHERWISE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 428 "NoStrikes"
    Field{
      fvar,
      428,
      "NoStrikes",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoStrikes",
      428,
      nullptr
    },
    //--- Tag# 429 "ListStatusType"
    Field{
      fvar,
      429,
      "ListStatusType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Ack"        , .atom = 0}, // 0
        {.value = "2", .descr = "Response"   , .atom = 0}, // 1
        {.value = "3", .descr = "Timed"      , .atom = 0}, // 2
        {.value = "4", .descr = "Execstarted", .atom = 0}, // 3
        {.value = "5", .descr = "Alldone"    , .atom = 0}, // 4
        {.value = "6", .descr = "Alert"      , .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // ACK
          case '2': return f.value_atom(1); // RESPONSE
          case '3': return f.value_atom(2); // TIMED
          case '4': return f.value_atom(3); // EXECSTARTED
          case '5': return f.value_atom(4); // ALLDONE
          case '6': return f.value_atom(5); // ALERT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 430 "NetGrossInd"
    Field{
      fvar,
      430,
      "NetGrossInd",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Net"  , .atom = 0}, // 0
        {.value = "2", .descr = "Gross", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // NET
          case '2': return f.value_atom(1); // GROSS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 431 "ListOrderStatus"
    Field{
      fvar,
      431,
      "ListOrderStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Inbiddingprocess"    , .atom = 0}, // 0
        {.value = "2", .descr = "Receivedforexecution", .atom = 0}, // 1
        {.value = "3", .descr = "Executing"           , .atom = 0}, // 2
        {.value = "4", .descr = "Canceling"           , .atom = 0}, // 3
        {.value = "5", .descr = "Alert"               , .atom = 0}, // 4
        {.value = "6", .descr = "AllDone"             , .atom = 0}, // 5
        {.value = "7", .descr = "Reject"              , .atom = 0}, // 6
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // INBIDDINGPROCESS
          case '2': return f.value_atom(1); // RECEIVEDFOREXECUTION
          case '3': return f.value_atom(2); // EXECUTING
          case '4': return f.value_atom(3); // CANCELING
          case '5': return f.value_atom(4); // ALERT
          case '6': return f.value_atom(5); // ALL_DONE
          case '7': return f.value_atom(6); // REJECT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 432 "ExpireDate"
    Field{
      fvar,
      432,
      "ExpireDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 433 "ListExecInstType"
    Field{
      fvar,
      433,
      "ListExecInstType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Immediate"                                  , .atom = 0}, // 0
        {.value = "2", .descr = "WaitForExecuteInstruction"                  , .atom = 0}, // 1
        {.value = "3", .descr = "ExchangeSwitchCivOrderSellDriven"           , .atom = 0}, // 2
        {.value = "4", .descr = "ExchangeSwitchCivOrderBuyDrivenCashTopUp"   , .atom = 0}, // 3
        {.value = "5", .descr = "ExchangeSwitchCivOrderBuyDrivenCashWithdraw", .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // IMMEDIATE
          case '2': return f.value_atom(1); // WAIT_FOR_EXECUTE_INSTRUCTION
          case '3': return f.value_atom(2); // EXCHANGE_SWITCH_CIV_ORDER_SELL_DRIVEN
          case '4': return f.value_atom(3); // EXCHANGE_SWITCH_CIV_ORDER_BUY_DRIVEN_CASH_TOP_UP
          case '5': return f.value_atom(4); // EXCHANGE_SWITCH_CIV_ORDER_BUY_DRIVEN_CASH_WITHDRAW
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 434 "CxlRejResponseTo"
    Field{
      fvar,
      434,
      "CxlRejResponseTo",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "OrderCancelRequest"       , .atom = 0}, // 0
        {.value = "2", .descr = "OrderCancelReplaceRequest", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // ORDER_CANCEL_REQUEST
          case '2': return f.value_atom(1); // ORDER_CANCEL_REPLACE_REQUEST
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 435 "UnderlyingCouponRate"
    Field{
      fvar,
      435,
      "UnderlyingCouponRate",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 436 "UnderlyingContractMultiplier"
    Field{
      fvar,
      436,
      "UnderlyingContractMultiplier",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 437 "ContraTradeQty"
    Field{
      fvar,
      437,
      "ContraTradeQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 438 "ContraTradeTime"
    Field{
      fvar,
      438,
      "ContraTradeTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 439
    Field{},
    //--- Tag# 440
    Field{},
    //--- Tag# 441 "LiquidityNumSecurities"
    Field{
      fvar,
      441,
      "LiquidityNumSecurities",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 442 "MultiLegReportingType"
    Field{
      fvar,
      442,
      "MultiLegReportingType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "SingleSecurity"                  , .atom = 0}, // 0
        {.value = "2", .descr = "IndividualLegOfAMultiLegSecurity", .atom = 0}, // 1
        {.value = "3", .descr = "MultiLegSecurity"                , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // SINGLE_SECURITY
          case '2': return f.value_atom(1); // INDIVIDUAL_LEG_OF_A_MULTI_LEG_SECURITY
          case '3': return f.value_atom(2); // MULTI_LEG_SECURITY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 443 "StrikeTime"
    Field{
      fvar,
      443,
      "StrikeTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 444 "ListStatusText"
    Field{
      fvar,
      444,
      "ListStatusText",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 445 "EncodedListStatusTextLen"
    Field{
      fvar,
      445,
      "EncodedListStatusTextLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 446 "EncodedListStatusText"
    Field{
      fvar,
      446,
      "EncodedListStatusText",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedListStatusTextLen",
      445,
      nullptr
    },
    //--- Tag# 447 "PartyIDSource"
    Field{
      fvar,
      447,
      "PartyIDSource",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "B", .descr = "Bic"                                                                                 , .atom = 0}, // 0
        {.value = "C", .descr = "GenerallyAcceptedMarketParticipantIdentifier"                                        , .atom = 0}, // 1
        {.value = "D", .descr = "ProprietaryCustomCode"                                                               , .atom = 0}, // 2
        {.value = "E", .descr = "IsoCountryCode"                                                                      , .atom = 0}, // 3
        {.value = "F", .descr = "SettlementEntityLocation"                                                            , .atom = 0}, // 4
        {.value = "G", .descr = "Mic"                                                                                 , .atom = 0}, // 5
        {.value = "H", .descr = "CsdParticipantMemberCode"                                                            , .atom = 0}, // 6
        {.value = "1", .descr = "KoreanInvestorId"                                                                    , .atom = 0}, // 7
        {.value = "2", .descr = "TaiwaneseQualifiedForeignInvestorIdQfii"                                             , .atom = 0}, // 8
        {.value = "3", .descr = "TaiwaneseTradingAccount"                                                             , .atom = 0}, // 9
        {.value = "4", .descr = "MalaysianCentralDepository"                                                          , .atom = 0}, // 10
        {.value = "5", .descr = "ChineseBShare"                                                                       , .atom = 0}, // 11
        {.value = "6", .descr = "UkNationalInsuranceOrPensionNumber"                                                  , .atom = 0}, // 12
        {.value = "7", .descr = "UsSocialSecurityNumber"                                                              , .atom = 0}, // 13
        {.value = "8", .descr = "UsEmployerIdentificationNumber"                                                      , .atom = 0}, // 14
        {.value = "9", .descr = "AustralianBusinessNumber"                                                            , .atom = 0}, // 15
        {.value = "A", .descr = "AustralianTaxFileNumber"                                                             , .atom = 0}, // 16
        {.value = "I", .descr = "DirectedBrokerThreeCharacterAcronymAsDefinedInIsitcEtcBestPracticeGuidelinesDocument", .atom = 0}, // 17
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'B': return f.value_atom(0); // BIC
          case 'C': return f.value_atom(1); // GENERALLY_ACCEPTED_MARKET_PARTICIPANT_IDENTIFIER
          case 'D': return f.value_atom(2); // PROPRIETARY_CUSTOM_CODE
          case 'E': return f.value_atom(3); // ISO_COUNTRY_CODE
          case 'F': return f.value_atom(4); // SETTLEMENT_ENTITY_LOCATION
          case 'G': return f.value_atom(5); // MIC
          case 'H': return f.value_atom(6); // CSD_PARTICIPANT_MEMBER_CODE
          case '1': return f.value_atom(7); // KOREAN_INVESTOR_ID
          case '2': return f.value_atom(8); // TAIWANESE_QUALIFIED_FOREIGN_INVESTOR_ID_QFII
          case '3': return f.value_atom(9); // TAIWANESE_TRADING_ACCOUNT
          case '4': return f.value_atom(10); // MALAYSIAN_CENTRAL_DEPOSITORY
          case '5': return f.value_atom(11); // CHINESE_B_SHARE
          case '6': return f.value_atom(12); // UK_NATIONAL_INSURANCE_OR_PENSION_NUMBER
          case '7': return f.value_atom(13); // US_SOCIAL_SECURITY_NUMBER
          case '8': return f.value_atom(14); // US_EMPLOYER_IDENTIFICATION_NUMBER
          case '9': return f.value_atom(15); // AUSTRALIAN_BUSINESS_NUMBER
          case 'A': return f.value_atom(16); // AUSTRALIAN_TAX_FILE_NUMBER
          case 'I': return f.value_atom(17); // DIRECTED_BROKER_THREE_CHARACTER_ACRONYM_AS_DEFINED_IN_ISITC_ETC_BEST_PRACTICE_GUIDELINES_DOCUMENT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 448 "PartyID"
    Field{
      fvar,
      448,
      "PartyID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 449
    Field{},
    //--- Tag# 450
    Field{},
    //--- Tag# 451 "NetChgPrevDay"
    Field{
      fvar,
      451,
      "NetChgPrevDay",
      FieldType::PRICEOFFSET,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 452 "PartyRole"
    Field{
      fvar,
      452,
      "PartyRole",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "ExecutingFirm"                    , .atom = 0}, // 0
        {.value = "2" , .descr = "BrokerOfCredit"                   , .atom = 0}, // 1
        {.value = "3" , .descr = "ClientId"                         , .atom = 0}, // 2
        {.value = "4" , .descr = "ClearingFirm"                     , .atom = 0}, // 3
        {.value = "5" , .descr = "InvestorId"                       , .atom = 0}, // 4
        {.value = "6" , .descr = "IntroducingFirm"                  , .atom = 0}, // 5
        {.value = "7" , .descr = "EnteringFirm"                     , .atom = 0}, // 6
        {.value = "8" , .descr = "LocateLendingFirm"                , .atom = 0}, // 7
        {.value = "9" , .descr = "FundManagerClientId"              , .atom = 0}, // 8
        {.value = "10", .descr = "SettlementLocation"               , .atom = 0}, // 9
        {.value = "11", .descr = "OrderOriginationTrader"           , .atom = 0}, // 10
        {.value = "12", .descr = "ExecutingTrader"                  , .atom = 0}, // 11
        {.value = "13", .descr = "OrderOriginationFirm"             , .atom = 0}, // 12
        {.value = "14", .descr = "GiveupClearingFirm"               , .atom = 0}, // 13
        {.value = "15", .descr = "CorrespondantClearingFirm"        , .atom = 0}, // 14
        {.value = "16", .descr = "ExecutingSystem"                  , .atom = 0}, // 15
        {.value = "17", .descr = "ContraFirm"                       , .atom = 0}, // 16
        {.value = "18", .descr = "ContraClearingFirm"               , .atom = 0}, // 17
        {.value = "19", .descr = "SponsoringFirm"                   , .atom = 0}, // 18
        {.value = "20", .descr = "UnderlyingContraFirm"             , .atom = 0}, // 19
        {.value = "21", .descr = "ClearingOrganization"             , .atom = 0}, // 20
        {.value = "22", .descr = "Exchange"                         , .atom = 0}, // 21
        {.value = "24", .descr = "CustomerAccount"                  , .atom = 0}, // 22
        {.value = "25", .descr = "CorrespondentClearingOrganization", .atom = 0}, // 23
        {.value = "26", .descr = "CorrespondentBroker"              , .atom = 0}, // 24
        {.value = "27", .descr = "BuyerSeller"                      , .atom = 0}, // 25
        {.value = "28", .descr = "Custodian"                        , .atom = 0}, // 26
        {.value = "29", .descr = "Intermediary"                     , .atom = 0}, // 27
        {.value = "30", .descr = "Agent"                            , .atom = 0}, // 28
        {.value = "31", .descr = "SubCustodian"                     , .atom = 0}, // 29
        {.value = "32", .descr = "Beneficiary"                      , .atom = 0}, // 30
        {.value = "33", .descr = "InterestedParty"                  , .atom = 0}, // 31
        {.value = "34", .descr = "RegulatoryBody"                   , .atom = 0}, // 32
        {.value = "35", .descr = "LiquidityProvider"                , .atom = 0}, // 33
        {.value = "36", .descr = "EnteringTrader"                   , .atom = 0}, // 34
        {.value = "37", .descr = "ContraTrader"                     , .atom = 0}, // 35
        {.value = "38", .descr = "PositionAccount"                  , .atom = 0}, // 36
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // ExecutingFirm
          case           '2': return f.value_atom( 1); // BrokerOfCredit
          case           '3': return f.value_atom( 2); // ClientId
          case           '4': return f.value_atom( 3); // ClearingFirm
          case           '5': return f.value_atom( 4); // InvestorId
          case           '6': return f.value_atom( 5); // IntroducingFirm
          case           '7': return f.value_atom( 6); // EnteringFirm
          case           '8': return f.value_atom( 7); // LocateLendingFirm
          case           '9': return f.value_atom( 8); // FundManagerClientId
          case CINT<'1','0'>: return f.value_atom( 9); // SettlementLocation
          case CINT<'1','1'>: return f.value_atom(10); // OrderOriginationTrader
          case CINT<'1','2'>: return f.value_atom(11); // ExecutingTrader
          case CINT<'1','3'>: return f.value_atom(12); // OrderOriginationFirm
          case CINT<'1','4'>: return f.value_atom(13); // GiveupClearingFirm
          case CINT<'1','5'>: return f.value_atom(14); // CorrespondantClearingFirm
          case CINT<'1','6'>: return f.value_atom(15); // ExecutingSystem
          case CINT<'1','7'>: return f.value_atom(16); // ContraFirm
          case CINT<'1','8'>: return f.value_atom(17); // ContraClearingFirm
          case CINT<'1','9'>: return f.value_atom(18); // SponsoringFirm
          case CINT<'2','0'>: return f.value_atom(19); // UnderlyingContraFirm
          case CINT<'2','1'>: return f.value_atom(20); // ClearingOrganization
          case CINT<'2','2'>: return f.value_atom(21); // Exchange
          case CINT<'2','4'>: return f.value_atom(22); // CustomerAccount
          case CINT<'2','5'>: return f.value_atom(23); // CorrespondentClearingOrganization
          case CINT<'2','6'>: return f.value_atom(24); // CorrespondentBroker
          case CINT<'2','7'>: return f.value_atom(25); // BuyerSeller
          case CINT<'2','8'>: return f.value_atom(26); // Custodian
          case CINT<'2','9'>: return f.value_atom(27); // Intermediary
          case CINT<'3','0'>: return f.value_atom(28); // Agent
          case CINT<'3','1'>: return f.value_atom(29); // SubCustodian
          case CINT<'3','2'>: return f.value_atom(30); // Beneficiary
          case CINT<'3','3'>: return f.value_atom(31); // InterestedParty
          case CINT<'3','4'>: return f.value_atom(32); // RegulatoryBody
          case CINT<'3','5'>: return f.value_atom(33); // LiquidityProvider
          case CINT<'3','6'>: return f.value_atom(34); // EnteringTrader
          case CINT<'3','7'>: return f.value_atom(35); // ContraTrader
          case CINT<'3','8'>: return f.value_atom(36); // PositionAccount
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 453 "NoPartyIDs"
    Field{
      fvar,
      453,
      "NoPartyIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoPartyIDs",
      453,
      nullptr
    },
    //--- Tag# 454 "NoSecurityAltID"
    Field{
      fvar,
      454,
      "NoSecurityAltID",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoSecurityAltID",
      454,
      nullptr
    },
    //--- Tag# 455 "SecurityAltID"
    Field{
      fvar,
      455,
      "SecurityAltID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 456 "SecurityAltIDSource"
    Field{
      fvar,
      456,
      "SecurityAltIDSource",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 457 "NoUnderlyingSecurityAltID"
    Field{
      fvar,
      457,
      "NoUnderlyingSecurityAltID",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoUnderlyingSecurityAltID",
      457,
      nullptr
    },
    //--- Tag# 458 "UnderlyingSecurityAltID"
    Field{
      fvar,
      458,
      "UnderlyingSecurityAltID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 459 "UnderlyingSecurityAltIDSource"
    Field{
      fvar,
      459,
      "UnderlyingSecurityAltIDSource",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 460 "Product"
    Field{
      fvar,
      460,
      "Product",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "Agency"     , .atom = 0}, // 0
        {.value = "2" , .descr = "Commodity"  , .atom = 0}, // 1
        {.value = "3" , .descr = "Corporate"  , .atom = 0}, // 2
        {.value = "4" , .descr = "Currency"   , .atom = 0}, // 3
        {.value = "5" , .descr = "Equity"     , .atom = 0}, // 4
        {.value = "6" , .descr = "Government" , .atom = 0}, // 5
        {.value = "7" , .descr = "Index"      , .atom = 0}, // 6
        {.value = "8" , .descr = "Loan"       , .atom = 0}, // 7
        {.value = "9" , .descr = "Moneymarket", .atom = 0}, // 8
        {.value = "10", .descr = "Mortgage"   , .atom = 0}, // 9
        {.value = "11", .descr = "Municipal"  , .atom = 0}, // 10
        {.value = "12", .descr = "Other"      , .atom = 0}, // 11
        {.value = "13", .descr = "Financing"  , .atom = 0}, // 12
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // Agency
          case           '2': return f.value_atom( 1); // Commodity
          case           '3': return f.value_atom( 2); // Corporate
          case           '4': return f.value_atom( 3); // Currency
          case           '5': return f.value_atom( 4); // Equity
          case           '6': return f.value_atom( 5); // Government
          case           '7': return f.value_atom( 6); // Index
          case           '8': return f.value_atom( 7); // Loan
          case           '9': return f.value_atom( 8); // Moneymarket
          case CINT<'1','0'>: return f.value_atom( 9); // Mortgage
          case CINT<'1','1'>: return f.value_atom(10); // Municipal
          case CINT<'1','2'>: return f.value_atom(11); // Other
          case CINT<'1','3'>: return f.value_atom(12); // Financing
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 461 "CFICode"
    Field{
      fvar,
      461,
      "CFICode",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 462 "UnderlyingProduct"
    Field{
      fvar,
      462,
      "UnderlyingProduct",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 463 "UnderlyingCFICode"
    Field{
      fvar,
      463,
      "UnderlyingCFICode",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 464 "TestMessageIndicator"
    Field{
      fvar,
      464,
      "TestMessageIndicator",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 465
    Field{},
    //--- Tag# 466 "BookingRefID"
    Field{
      fvar,
      466,
      "BookingRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 467 "IndividualAllocID"
    Field{
      fvar,
      467,
      "IndividualAllocID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 468 "RoundingDirection"
    Field{
      fvar,
      468,
      "RoundingDirection",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "RoundToNearest", .atom = 0}, // 0
        {.value = "1", .descr = "RoundDown"     , .atom = 0}, // 1
        {.value = "2", .descr = "RoundUp"       , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // ROUND_TO_NEAREST
          case '1': return f.value_atom(1); // ROUND_DOWN
          case '2': return f.value_atom(2); // ROUND_UP
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 469 "RoundingModulus"
    Field{
      fvar,
      469,
      "RoundingModulus",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 470 "CountryOfIssue"
    Field{
      fvar,
      470,
      "CountryOfIssue",
      FieldType::COUNTRY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 471 "StateOrProvinceOfIssue"
    Field{
      fvar,
      471,
      "StateOrProvinceOfIssue",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 472 "LocaleOfIssue"
    Field{
      fvar,
      472,
      "LocaleOfIssue",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 473 "NoRegistDtls"
    Field{
      fvar,
      473,
      "NoRegistDtls",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoRegistDtls",
      473,
      nullptr
    },
    //--- Tag# 474 "MailingDtls"
    Field{
      fvar,
      474,
      "MailingDtls",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 475 "InvestorCountryOfResidence"
    Field{
      fvar,
      475,
      "InvestorCountryOfResidence",
      FieldType::COUNTRY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 476 "PaymentRef"
    Field{
      fvar,
      476,
      "PaymentRef",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 477 "DistribPaymentMethod"
    Field{
      fvar,
      477,
      "DistribPaymentMethod",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "Crest"                  , .atom = 0}, // 0
        {.value = "2" , .descr = "Nscc"                   , .atom = 0}, // 1
        {.value = "3" , .descr = "Euroclear"              , .atom = 0}, // 2
        {.value = "4" , .descr = "Clearstream"            , .atom = 0}, // 3
        {.value = "5" , .descr = "Cheque"                 , .atom = 0}, // 4
        {.value = "6" , .descr = "TelegraphicTransfer"    , .atom = 0}, // 5
        {.value = "7" , .descr = "Fedwire"                , .atom = 0}, // 6
        {.value = "8" , .descr = "DirectCredit"           , .atom = 0}, // 7
        {.value = "9" , .descr = "AchCredit"              , .atom = 0}, // 8
        {.value = "10", .descr = "Bpay"                   , .atom = 0}, // 9
        {.value = "11", .descr = "HighValueClearingSystem", .atom = 0}, // 10
        {.value = "12", .descr = "ReinvestInFund"         , .atom = 0}, // 11
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // Crest
          case           '2': return f.value_atom( 1); // Nscc
          case           '3': return f.value_atom( 2); // Euroclear
          case           '4': return f.value_atom( 3); // Clearstream
          case           '5': return f.value_atom( 4); // Cheque
          case           '6': return f.value_atom( 5); // TelegraphicTransfer
          case           '7': return f.value_atom( 6); // Fedwire
          case           '8': return f.value_atom( 7); // DirectCredit
          case           '9': return f.value_atom( 8); // AchCredit
          case CINT<'1','0'>: return f.value_atom( 9); // Bpay
          case CINT<'1','1'>: return f.value_atom(10); // HighValueClearingSystem
          case CINT<'1','2'>: return f.value_atom(11); // ReinvestInFund
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 478 "CashDistribCurr"
    Field{
      fvar,
      478,
      "CashDistribCurr",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 479 "CommCurrency"
    Field{
      fvar,
      479,
      "CommCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 480 "CancellationRights"
    Field{
      fvar,
      480,
      "CancellationRights",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes"              , .atom = 0}, // 0
        {.value = "N", .descr = "NoExecutionOnly"  , .atom = 0}, // 1
        {.value = "M", .descr = "NoWaiverAgreement", .atom = 0}, // 2
        {.value = "O", .descr = "NoInstitutional"  , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO_EXECUTION_ONLY
          case 'M': return f.value_atom(2); // NO_WAIVER_AGREEMENT
          case 'O': return f.value_atom(3); // NO_INSTITUTIONAL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 481 "MoneyLaunderingStatus"
    Field{
      fvar,
      481,
      "MoneyLaunderingStatus",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Passed"                                      , .atom = 0}, // 0
        {.value = "N", .descr = "NotChecked"                                  , .atom = 0}, // 1
        {.value = "1", .descr = "ExemptBelowTheLimit"                         , .atom = 0}, // 2
        {.value = "2", .descr = "ExemptClientMoneyTypeExemption"              , .atom = 0}, // 3
        {.value = "3", .descr = "ExemptAuthorisedCreditOrFinancialInstitution", .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // PASSED
          case 'N': return f.value_atom(1); // NOT_CHECKED
          case '1': return f.value_atom(2); // EXEMPT_BELOW_THE_LIMIT
          case '2': return f.value_atom(3); // EXEMPT_CLIENT_MONEY_TYPE_EXEMPTION
          case '3': return f.value_atom(4); // EXEMPT_AUTHORISED_CREDIT_OR_FINANCIAL_INSTITUTION
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 482 "MailingInst"
    Field{
      fvar,
      482,
      "MailingInst",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 483 "TransBkdTime"
    Field{
      fvar,
      483,
      "TransBkdTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 484 "ExecPriceType"
    Field{
      fvar,
      484,
      "ExecPriceType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "B", .descr = "BidPrice"                         , .atom = 0}, // 0
        {.value = "C", .descr = "CreationPrice"                    , .atom = 0}, // 1
        {.value = "D", .descr = "CreationPricePlusAdjustment"      , .atom = 0}, // 2
        {.value = "E", .descr = "CreationPricePlusAdjustmentAmount", .atom = 0}, // 3
        {.value = "O", .descr = "OfferPrice"                       , .atom = 0}, // 4
        {.value = "P", .descr = "OfferPriceMinusAdjustment"        , .atom = 0}, // 5
        {.value = "Q", .descr = "OfferPriceMinusAdjustmentAmount"  , .atom = 0}, // 6
        {.value = "S", .descr = "SinglePrice"                      , .atom = 0}, // 7
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'B': return f.value_atom(0); // BID_PRICE
          case 'C': return f.value_atom(1); // CREATION_PRICE
          case 'D': return f.value_atom(2); // CREATION_PRICE_PLUS_ADJUSTMENT
          case 'E': return f.value_atom(3); // CREATION_PRICE_PLUS_ADJUSTMENT_AMOUNT
          case 'O': return f.value_atom(4); // OFFER_PRICE
          case 'P': return f.value_atom(5); // OFFER_PRICE_MINUS_ADJUSTMENT
          case 'Q': return f.value_atom(6); // OFFER_PRICE_MINUS_ADJUSTMENT_AMOUNT
          case 'S': return f.value_atom(7); // SINGLE_PRICE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 485 "ExecPriceAdjustment"
    Field{
      fvar,
      485,
      "ExecPriceAdjustment",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 486 "DateOfBirth"
    Field{
      fvar,
      486,
      "DateOfBirth",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 487 "TradeReportTransType"
    Field{
      fvar,
      487,
      "TradeReportTransType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 488 "CardHolderName"
    Field{
      fvar,
      488,
      "CardHolderName",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 489 "CardNumber"
    Field{
      fvar,
      489,
      "CardNumber",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 490 "CardExpDate"
    Field{
      fvar,
      490,
      "CardExpDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 491 "CardIssNum"
    Field{
      fvar,
      491,
      "CardIssNum",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 492 "PaymentMethod"
    Field{
      fvar,
      492,
      "PaymentMethod",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "Crest"                  , .atom = 0}, // 0
        {.value = "2" , .descr = "Nscc"                   , .atom = 0}, // 1
        {.value = "3" , .descr = "Euroclear"              , .atom = 0}, // 2
        {.value = "4" , .descr = "Clearstream"            , .atom = 0}, // 3
        {.value = "5" , .descr = "Cheque"                 , .atom = 0}, // 4
        {.value = "6" , .descr = "TelegraphicTransfer"    , .atom = 0}, // 5
        {.value = "7" , .descr = "Fedwire"                , .atom = 0}, // 6
        {.value = "8" , .descr = "DebitCard"              , .atom = 0}, // 7
        {.value = "9" , .descr = "DirectDebit"            , .atom = 0}, // 8
        {.value = "10", .descr = "DirectCredit"           , .atom = 0}, // 9
        {.value = "11", .descr = "CreditCard"             , .atom = 0}, // 10
        {.value = "12", .descr = "AchDebit"               , .atom = 0}, // 11
        {.value = "13", .descr = "AchCredit"              , .atom = 0}, // 12
        {.value = "14", .descr = "Bpay"                   , .atom = 0}, // 13
        {.value = "15", .descr = "HighValueClearingSystem", .atom = 0}, // 14
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // Crest
          case           '2': return f.value_atom( 1); // Nscc
          case           '3': return f.value_atom( 2); // Euroclear
          case           '4': return f.value_atom( 3); // Clearstream
          case           '5': return f.value_atom( 4); // Cheque
          case           '6': return f.value_atom( 5); // TelegraphicTransfer
          case           '7': return f.value_atom( 6); // Fedwire
          case           '8': return f.value_atom( 7); // DebitCard
          case           '9': return f.value_atom( 8); // DirectDebit
          case CINT<'1','0'>: return f.value_atom( 9); // DirectCredit
          case CINT<'1','1'>: return f.value_atom(10); // CreditCard
          case CINT<'1','2'>: return f.value_atom(11); // AchDebit
          case CINT<'1','3'>: return f.value_atom(12); // AchCredit
          case CINT<'1','4'>: return f.value_atom(13); // Bpay
          case CINT<'1','5'>: return f.value_atom(14); // HighValueClearingSystem
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 493 "RegistAcctType"
    Field{
      fvar,
      493,
      "RegistAcctType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 494 "Designation"
    Field{
      fvar,
      494,
      "Designation",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 495 "TaxAdvantageType"
    Field{
      fvar,
      495,
      "TaxAdvantageType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "NoneNotApplicable"                  , .atom = 0}, // 0
        {.value = "1" , .descr = "MaxiIsa"                            , .atom = 0}, // 1
        {.value = "2" , .descr = "Tessa"                              , .atom = 0}, // 2
        {.value = "3" , .descr = "MiniCashIsa"                        , .atom = 0}, // 3
        {.value = "4" , .descr = "MiniStocksAndSharesIsa"             , .atom = 0}, // 4
        {.value = "5" , .descr = "MiniInsuranceIsa"                   , .atom = 0}, // 5
        {.value = "6" , .descr = "CurrentYearPayment"                 , .atom = 0}, // 6
        {.value = "7" , .descr = "PriorYearPayment"                   , .atom = 0}, // 7
        {.value = "8" , .descr = "AssetTransfer"                      , .atom = 0}, // 8
        {.value = "9" , .descr = "Employee"                           , .atom = 0}, // 9
        {.value = "10", .descr = "EmployeeCurrentYear"                , .atom = 0}, // 10
        {.value = "11", .descr = "Employer"                           , .atom = 0}, // 11
        {.value = "12", .descr = "EmployerCurrentYear"                , .atom = 0}, // 12
        {.value = "13", .descr = "NonFundPrototypeIra"                , .atom = 0}, // 13
        {.value = "14", .descr = "NonFundQualifiedPlan"               , .atom = 0}, // 14
        {.value = "15", .descr = "DefinedContributionPlan"            , .atom = 0}, // 15
        {.value = "16", .descr = "IndividualRetirementAccount"        , .atom = 0}, // 16
        {.value = "17", .descr = "IndividualRetirementAccountRollover", .atom = 0}, // 17
        {.value = "18", .descr = "Keogh"                              , .atom = 0}, // 18
        {.value = "19", .descr = "ProfitSharingPlan"                  , .atom = 0}, // 19
        {.value = "20", .descr = "401k"                               , .atom = 0}, // 20
        {.value = "21", .descr = "SelfDirectedIra"                    , .atom = 0}, // 21
        {.value = "22", .descr = "403"                                , .atom = 0}, // 22
        {.value = "23", .descr = "457"                                , .atom = 0}, // 23
        {.value = "24", .descr = "RothIra24"                          , .atom = 0}, // 24
        {.value = "25", .descr = "RothIra25"                          , .atom = 0}, // 25
        {.value = "26", .descr = "RothConversionIra26"                , .atom = 0}, // 26
        {.value = "27", .descr = "RothConversionIra27"                , .atom = 0}, // 27
        {.value = "28", .descr = "EducationIra28"                     , .atom = 0}, // 28
        {.value = "29", .descr = "EducationIra29"                     , .atom = 0}, // 29
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom( 0); // NoneNotApplicable
          case           '1': return f.value_atom( 1); // MaxiIsa
          case           '2': return f.value_atom( 2); // Tessa
          case           '3': return f.value_atom( 3); // MiniCashIsa
          case           '4': return f.value_atom( 4); // MiniStocksAndSharesIsa
          case           '5': return f.value_atom( 5); // MiniInsuranceIsa
          case           '6': return f.value_atom( 6); // CurrentYearPayment
          case           '7': return f.value_atom( 7); // PriorYearPayment
          case           '8': return f.value_atom( 8); // AssetTransfer
          case           '9': return f.value_atom( 9); // Employee
          case CINT<'1','0'>: return f.value_atom(10); // EmployeeCurrentYear
          case CINT<'1','1'>: return f.value_atom(11); // Employer
          case CINT<'1','2'>: return f.value_atom(12); // EmployerCurrentYear
          case CINT<'1','3'>: return f.value_atom(13); // NonFundPrototypeIra
          case CINT<'1','4'>: return f.value_atom(14); // NonFundQualifiedPlan
          case CINT<'1','5'>: return f.value_atom(15); // DefinedContributionPlan
          case CINT<'1','6'>: return f.value_atom(16); // IndividualRetirementAccount
          case CINT<'1','7'>: return f.value_atom(17); // IndividualRetirementAccountRollover
          case CINT<'1','8'>: return f.value_atom(18); // Keogh
          case CINT<'1','9'>: return f.value_atom(19); // ProfitSharingPlan
          case CINT<'2','0'>: return f.value_atom(20); // 401k
          case CINT<'2','1'>: return f.value_atom(21); // SelfDirectedIra
          case CINT<'2','2'>: return f.value_atom(22); // 403
          case CINT<'2','3'>: return f.value_atom(23); // 457
          case CINT<'2','4'>: return f.value_atom(24); // RothIra24
          case CINT<'2','5'>: return f.value_atom(25); // RothIra25
          case CINT<'2','6'>: return f.value_atom(26); // RothConversionIra26
          case CINT<'2','7'>: return f.value_atom(27); // RothConversionIra27
          case CINT<'2','8'>: return f.value_atom(28); // EducationIra28
          case CINT<'2','9'>: return f.value_atom(29); // EducationIra29
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 496 "RegistRejReasonText"
    Field{
      fvar,
      496,
      "RegistRejReasonText",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 497 "FundRenewWaiv"
    Field{
      fvar,
      497,
      "FundRenewWaiv",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 498 "CashDistribAgentName"
    Field{
      fvar,
      498,
      "CashDistribAgentName",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 499 "CashDistribAgentCode"
    Field{
      fvar,
      499,
      "CashDistribAgentCode",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 500 "CashDistribAgentAcctNumber"
    Field{
      fvar,
      500,
      "CashDistribAgentAcctNumber",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 501 "CashDistribPayRef"
    Field{
      fvar,
      501,
      "CashDistribPayRef",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 502 "CashDistribAgentAcctName"
    Field{
      fvar,
      502,
      "CashDistribAgentAcctName",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 503 "CardStartDate"
    Field{
      fvar,
      503,
      "CardStartDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 504 "PaymentDate"
    Field{
      fvar,
      504,
      "PaymentDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 505 "PaymentRemitterID"
    Field{
      fvar,
      505,
      "PaymentRemitterID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 506 "RegistStatus"
    Field{
      fvar,
      506,
      "RegistStatus",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "A", .descr = "Accepted"                                             , .atom = 0}, // 0
        {.value = "R", .descr = "Rejected"                                             , .atom = 0}, // 1
        {.value = "H", .descr = "Held"                                                 , .atom = 0}, // 2
        {.value = "N", .descr = "ReminderIeRegistrationInstructionsAreStillOutstanding", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'A': return f.value_atom(0); // ACCEPTED
          case 'R': return f.value_atom(1); // REJECTED
          case 'H': return f.value_atom(2); // HELD
          case 'N': return f.value_atom(3); // REMINDER_IE_REGISTRATION_INSTRUCTIONS_ARE_STILL_OUTSTANDING
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 507 "RegistRejReasonCode"
    Field{
      fvar,
      507,
      "RegistRejReasonCode",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "InvalidUnacceptableAccountType"               , .atom = 0}, // 0
        {.value = "2" , .descr = "InvalidUnacceptableTaxExemptType"             , .atom = 0}, // 1
        {.value = "3" , .descr = "InvalidUnacceptableOwnershipType"             , .atom = 0}, // 2
        {.value = "4" , .descr = "InvalidUnacceptableNoRegDetls"                , .atom = 0}, // 3
        {.value = "5" , .descr = "InvalidUnacceptableRegSeqNo"                  , .atom = 0}, // 4
        {.value = "6" , .descr = "InvalidUnacceptableRegDtls"                   , .atom = 0}, // 5
        {.value = "7" , .descr = "InvalidUnacceptableMailingDtls"               , .atom = 0}, // 6
        {.value = "8" , .descr = "InvalidUnacceptableMailingInst"               , .atom = 0}, // 7
        {.value = "9" , .descr = "InvalidUnacceptableInvestorId"                , .atom = 0}, // 8
        {.value = "10", .descr = "InvalidUnacceptableInvestorIdSource"          , .atom = 0}, // 9
        {.value = "11", .descr = "InvalidUnacceptableDateOfBirth"               , .atom = 0}, // 10
        {.value = "12", .descr = "InvalidUnacceptableInvestorCountryOfResidence", .atom = 0}, // 11
        {.value = "13", .descr = "InvalidUnacceptableNodistribinstns"           , .atom = 0}, // 12
        {.value = "14", .descr = "InvalidUnacceptableDistribPercentage"         , .atom = 0}, // 13
        {.value = "15", .descr = "InvalidUnacceptableDistribPaymentMethod"      , .atom = 0}, // 14
        {.value = "16", .descr = "InvalidUnacceptableCashDistribAgentAcctName"  , .atom = 0}, // 15
        {.value = "17", .descr = "InvalidUnacceptableCashDistribAgentCode"      , .atom = 0}, // 16
        {.value = "18", .descr = "InvalidUnacceptableCashDistribAgentAcctNum"   , .atom = 0}, // 17
        {.value = "99", .descr = "Other"                                        , .atom = 0}, // 18
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // InvalidUnacceptableAccountType
          case           '2': return f.value_atom( 1); // InvalidUnacceptableTaxExemptType
          case           '3': return f.value_atom( 2); // InvalidUnacceptableOwnershipType
          case           '4': return f.value_atom( 3); // InvalidUnacceptableNoRegDetls
          case           '5': return f.value_atom( 4); // InvalidUnacceptableRegSeqNo
          case           '6': return f.value_atom( 5); // InvalidUnacceptableRegDtls
          case           '7': return f.value_atom( 6); // InvalidUnacceptableMailingDtls
          case           '8': return f.value_atom( 7); // InvalidUnacceptableMailingInst
          case           '9': return f.value_atom( 8); // InvalidUnacceptableInvestorId
          case CINT<'1','0'>: return f.value_atom( 9); // InvalidUnacceptableInvestorIdSource
          case CINT<'1','1'>: return f.value_atom(10); // InvalidUnacceptableDateOfBirth
          case CINT<'1','2'>: return f.value_atom(11); // InvalidUnacceptableInvestorCountryOfResidence
          case CINT<'1','3'>: return f.value_atom(12); // InvalidUnacceptableNodistribinstns
          case CINT<'1','4'>: return f.value_atom(13); // InvalidUnacceptableDistribPercentage
          case CINT<'1','5'>: return f.value_atom(14); // InvalidUnacceptableDistribPaymentMethod
          case CINT<'1','6'>: return f.value_atom(15); // InvalidUnacceptableCashDistribAgentAcctName
          case CINT<'1','7'>: return f.value_atom(16); // InvalidUnacceptableCashDistribAgentCode
          case CINT<'1','8'>: return f.value_atom(17); // InvalidUnacceptableCashDistribAgentAcctNum
          case CINT<'9','9'>: return f.value_atom(18); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 508 "RegistRefID"
    Field{
      fvar,
      508,
      "RegistRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 509 "RegistDtls"
    Field{
      fvar,
      509,
      "RegistDtls",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 510 "NoDistribInsts"
    Field{
      fvar,
      510,
      "NoDistribInsts",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoDistribInsts",
      510,
      nullptr
    },
    //--- Tag# 511 "RegistEmail"
    Field{
      fvar,
      511,
      "RegistEmail",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 512 "DistribPercentage"
    Field{
      fvar,
      512,
      "DistribPercentage",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 513 "RegistID"
    Field{
      fvar,
      513,
      "RegistID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 514 "RegistTransType"
    Field{
      fvar,
      514,
      "RegistTransType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "New"    , .atom = 0}, // 0
        {.value = "1", .descr = "Replace", .atom = 0}, // 1
        {.value = "2", .descr = "Cancel" , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NEW
          case '1': return f.value_atom(1); // REPLACE
          case '2': return f.value_atom(2); // CANCEL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 515 "ExecValuationPoint"
    Field{
      fvar,
      515,
      "ExecValuationPoint",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 516 "OrderPercent"
    Field{
      fvar,
      516,
      "OrderPercent",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 517 "OwnershipType"
    Field{
      fvar,
      517,
      "OwnershipType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "J", .descr = "JointInvestors" , .atom = 0}, // 0
        {.value = "T", .descr = "TenantsInCommon", .atom = 0}, // 1
        {.value = "2", .descr = "JointTrustees"  , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'J': return f.value_atom(0); // JOINT_INVESTORS
          case 'T': return f.value_atom(1); // TENANTS_IN_COMMON
          case '2': return f.value_atom(2); // JOINT_TRUSTEES
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 518 "NoContAmts"
    Field{
      fvar,
      518,
      "NoContAmts",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoContAmts",
      518,
      nullptr
    },
    //--- Tag# 519 "ContAmtType"
    Field{
      fvar,
      519,
      "ContAmtType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "CommissionAmount"                  , .atom = 0}, // 0
        {.value = "2" , .descr = "Commission"                        , .atom = 0}, // 1
        {.value = "3" , .descr = "InitialChargeAmount"               , .atom = 0}, // 2
        {.value = "4" , .descr = "InitialCharge"                     , .atom = 0}, // 3
        {.value = "5" , .descr = "DiscountAmount"                    , .atom = 0}, // 4
        {.value = "6" , .descr = "Discount"                          , .atom = 0}, // 5
        {.value = "7" , .descr = "DilutionLevyAmount"                , .atom = 0}, // 6
        {.value = "8" , .descr = "DilutionLevy"                      , .atom = 0}, // 7
        {.value = "9" , .descr = "ExitChargeAmount"                  , .atom = 0}, // 8
        {.value = "10", .descr = "ExitCharge"                        , .atom = 0}, // 9
        {.value = "11", .descr = "FundBasedRenewalCommission"        , .atom = 0}, // 10
        {.value = "12", .descr = "ProjectedFundValue"                , .atom = 0}, // 11
        {.value = "13", .descr = "FundBasedRenewalCommissionAmount13", .atom = 0}, // 12
        {.value = "14", .descr = "FundBasedRenewalCommissionAmount14", .atom = 0}, // 13
        {.value = "15", .descr = "NetSettlementAmount"               , .atom = 0}, // 14
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // CommissionAmount
          case           '2': return f.value_atom( 1); // Commission
          case           '3': return f.value_atom( 2); // InitialChargeAmount
          case           '4': return f.value_atom( 3); // InitialCharge
          case           '5': return f.value_atom( 4); // DiscountAmount
          case           '6': return f.value_atom( 5); // Discount
          case           '7': return f.value_atom( 6); // DilutionLevyAmount
          case           '8': return f.value_atom( 7); // DilutionLevy
          case           '9': return f.value_atom( 8); // ExitChargeAmount
          case CINT<'1','0'>: return f.value_atom( 9); // ExitCharge
          case CINT<'1','1'>: return f.value_atom(10); // FundBasedRenewalCommission
          case CINT<'1','2'>: return f.value_atom(11); // ProjectedFundValue
          case CINT<'1','3'>: return f.value_atom(12); // FundBasedRenewalCommissionAmount13
          case CINT<'1','4'>: return f.value_atom(13); // FundBasedRenewalCommissionAmount14
          case CINT<'1','5'>: return f.value_atom(14); // NetSettlementAmount
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 520 "ContAmtValue"
    Field{
      fvar,
      520,
      "ContAmtValue",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 521 "ContAmtCurr"
    Field{
      fvar,
      521,
      "ContAmtCurr",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 522 "OwnerType"
    Field{
      fvar,
      522,
      "OwnerType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "IndividualInvestor"            , .atom = 0}, // 0
        {.value = "2" , .descr = "PublicCompany"                 , .atom = 0}, // 1
        {.value = "3" , .descr = "PrivateCompany"                , .atom = 0}, // 2
        {.value = "4" , .descr = "IndividualTrustee"             , .atom = 0}, // 3
        {.value = "5" , .descr = "CompanyTrustee"                , .atom = 0}, // 4
        {.value = "6" , .descr = "PensionPlan"                   , .atom = 0}, // 5
        {.value = "7" , .descr = "CustodianUnderGiftsToMinorsAct", .atom = 0}, // 6
        {.value = "8" , .descr = "Trusts"                        , .atom = 0}, // 7
        {.value = "9" , .descr = "Fiduciaries"                   , .atom = 0}, // 8
        {.value = "10", .descr = "NetworkingSubAccount"          , .atom = 0}, // 9
        {.value = "11", .descr = "NonProfitOrganization"         , .atom = 0}, // 10
        {.value = "12", .descr = "CorporateBody"                 , .atom = 0}, // 11
        {.value = "13", .descr = "Nominee"                       , .atom = 0}, // 12
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // IndividualInvestor
          case           '2': return f.value_atom( 1); // PublicCompany
          case           '3': return f.value_atom( 2); // PrivateCompany
          case           '4': return f.value_atom( 3); // IndividualTrustee
          case           '5': return f.value_atom( 4); // CompanyTrustee
          case           '6': return f.value_atom( 5); // PensionPlan
          case           '7': return f.value_atom( 6); // CustodianUnderGiftsToMinorsAct
          case           '8': return f.value_atom( 7); // Trusts
          case           '9': return f.value_atom( 8); // Fiduciaries
          case CINT<'1','0'>: return f.value_atom( 9); // NetworkingSubAccount
          case CINT<'1','1'>: return f.value_atom(10); // NonProfitOrganization
          case CINT<'1','2'>: return f.value_atom(11); // CorporateBody
          case CINT<'1','3'>: return f.value_atom(12); // Nominee
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 523 "PartySubID"
    Field{
      fvar,
      523,
      "PartySubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 524 "NestedPartyID"
    Field{
      fvar,
      524,
      "NestedPartyID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 525 "NestedPartyIDSource"
    Field{
      fvar,
      525,
      "NestedPartyIDSource",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 526 "SecondaryClOrdID"
    Field{
      fvar,
      526,
      "SecondaryClOrdID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 527 "SecondaryExecID"
    Field{
      fvar,
      527,
      "SecondaryExecID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 528 "OrderCapacity"
    Field{
      fvar,
      528,
      "OrderCapacity",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "A", .descr = "Agency"             , .atom = 0}, // 0
        {.value = "G", .descr = "Proprietary"        , .atom = 0}, // 1
        {.value = "I", .descr = "Individual"         , .atom = 0}, // 2
        {.value = "P", .descr = "Principal"          , .atom = 0}, // 3
        {.value = "R", .descr = "RisklessPrincipal"  , .atom = 0}, // 4
        {.value = "W", .descr = "AgentForOtherMember", .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'A': return f.value_atom(0); // AGENCY
          case 'G': return f.value_atom(1); // PROPRIETARY
          case 'I': return f.value_atom(2); // INDIVIDUAL
          case 'P': return f.value_atom(3); // PRINCIPAL
          case 'R': return f.value_atom(4); // RISKLESS_PRINCIPAL
          case 'W': return f.value_atom(5); // AGENT_FOR_OTHER_MEMBER
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 529 "OrderRestrictions"
    Field{
      fvar,
      529,
      "OrderRestrictions",
      FieldType::MULTIPLEVALUESTRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "ProgramTrade"                                                               , .atom = 0}, // 0
        {.value = "2", .descr = "IndexArbitrage"                                                             , .atom = 0}, // 1
        {.value = "3", .descr = "NonIndexArbitrage"                                                          , .atom = 0}, // 2
        {.value = "4", .descr = "CompetingMarketMaker"                                                       , .atom = 0}, // 3
        {.value = "5", .descr = "ActingAsMarketMakerOrSpecialistInTheSecurity"                               , .atom = 0}, // 4
        {.value = "6", .descr = "ActingAsMarketMakerOrSpecialistInTheUnderlyingSecurityOfADerivativeSecurity", .atom = 0}, // 5
        {.value = "7", .descr = "ForeignEntity"                                                              , .atom = 0}, // 6
        {.value = "8", .descr = "ExternalMarketParticipant"                                                  , .atom = 0}, // 7
        {.value = "9", .descr = "ExternalInterConnectedMarketLinkage"                                        , .atom = 0}, // 8
        {.value = "A", .descr = "RisklessArbitrage"                                                          , .atom = 0}, // 9
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // PROGRAM_TRADE
          case '2': return f.value_atom(1); // INDEX_ARBITRAGE
          case '3': return f.value_atom(2); // NON_INDEX_ARBITRAGE
          case '4': return f.value_atom(3); // COMPETING_MARKET_MAKER
          case '5': return f.value_atom(4); // ACTING_AS_MARKET_MAKER_OR_SPECIALIST_IN_THE_SECURITY
          case '6': return f.value_atom(5); // ACTING_AS_MARKET_MAKER_OR_SPECIALIST_IN_THE_UNDERLYING_SECURITY_OF_A_DERIVATIVE_SECURITY
          case '7': return f.value_atom(6); // FOREIGN_ENTITY
          case '8': return f.value_atom(7); // EXTERNAL_MARKET_PARTICIPANT
          case '9': return f.value_atom(8); // EXTERNAL_INTER_CONNECTED_MARKET_LINKAGE
          case 'A': return f.value_atom(9); // RISKLESS_ARBITRAGE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 530 "MassCancelRequestType"
    Field{
      fvar,
      530,
      "MassCancelRequestType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "CancelOrdersForASecurity"           , .atom = 0}, // 0
        {.value = "2", .descr = "CancelOrdersForAnUnderlyingSecurity", .atom = 0}, // 1
        {.value = "3", .descr = "CancelOrdersForAProduct"            , .atom = 0}, // 2
        {.value = "4", .descr = "CancelOrdersForACficode"            , .atom = 0}, // 3
        {.value = "5", .descr = "CancelOrdersForASecuritytype"       , .atom = 0}, // 4
        {.value = "6", .descr = "CancelOrdersForATradingSession"     , .atom = 0}, // 5
        {.value = "7", .descr = "CancelAllOrders"                    , .atom = 0}, // 6
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // CANCEL_ORDERS_FOR_A_SECURITY
          case '2': return f.value_atom(1); // CANCEL_ORDERS_FOR_AN_UNDERLYING_SECURITY
          case '3': return f.value_atom(2); // CANCEL_ORDERS_FOR_A_PRODUCT
          case '4': return f.value_atom(3); // CANCEL_ORDERS_FOR_A_CFICODE
          case '5': return f.value_atom(4); // CANCEL_ORDERS_FOR_A_SECURITYTYPE
          case '6': return f.value_atom(5); // CANCEL_ORDERS_FOR_A_TRADING_SESSION
          case '7': return f.value_atom(6); // CANCEL_ALL_ORDERS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 531 "MassCancelResponse"
    Field{
      fvar,
      531,
      "MassCancelResponse",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "CancelRequestRejected"              , .atom = 0}, // 0
        {.value = "1", .descr = "CancelOrdersForASecurity"           , .atom = 0}, // 1
        {.value = "2", .descr = "CancelOrdersForAnUnderlyingSecurity", .atom = 0}, // 2
        {.value = "3", .descr = "CancelOrdersForAProduct"            , .atom = 0}, // 3
        {.value = "4", .descr = "CancelOrdersForACficode"            , .atom = 0}, // 4
        {.value = "5", .descr = "CancelOrdersForASecuritytype"       , .atom = 0}, // 5
        {.value = "6", .descr = "CancelOrdersForATradingSession"     , .atom = 0}, // 6
        {.value = "7", .descr = "CancelAllOrders"                    , .atom = 0}, // 7
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // CANCEL_REQUEST_REJECTED
          case '1': return f.value_atom(1); // CANCEL_ORDERS_FOR_A_SECURITY
          case '2': return f.value_atom(2); // CANCEL_ORDERS_FOR_AN_UNDERLYING_SECURITY
          case '3': return f.value_atom(3); // CANCEL_ORDERS_FOR_A_PRODUCT
          case '4': return f.value_atom(4); // CANCEL_ORDERS_FOR_A_CFICODE
          case '5': return f.value_atom(5); // CANCEL_ORDERS_FOR_A_SECURITYTYPE
          case '6': return f.value_atom(6); // CANCEL_ORDERS_FOR_A_TRADING_SESSION
          case '7': return f.value_atom(7); // CANCEL_ALL_ORDERS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 532 "MassCancelRejectReason"
    Field{
      fvar,
      532,
      "MassCancelRejectReason",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "MassCancelNotSupported"        , .atom = 0}, // 0
        {.value = "1" , .descr = "InvalidOrUnknownSecurity"      , .atom = 0}, // 1
        {.value = "2" , .descr = "InvalidOrUnknownUnderlying"    , .atom = 0}, // 2
        {.value = "3" , .descr = "InvalidOrUnknownProduct"       , .atom = 0}, // 3
        {.value = "4" , .descr = "InvalidOrUnknownCficode"       , .atom = 0}, // 4
        {.value = "5" , .descr = "InvalidOrUnknownSecurityType"  , .atom = 0}, // 5
        {.value = "6" , .descr = "InvalidOrUnknownTradingSession", .atom = 0}, // 6
        {.value = "99", .descr = "Other"                         , .atom = 0}, // 7
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom(0); // MassCancelNotSupported
          case           '1': return f.value_atom(1); // InvalidOrUnknownSecurity
          case           '2': return f.value_atom(2); // InvalidOrUnknownUnderlying
          case           '3': return f.value_atom(3); // InvalidOrUnknownProduct
          case           '4': return f.value_atom(4); // InvalidOrUnknownCficode
          case           '5': return f.value_atom(5); // InvalidOrUnknownSecurityType
          case           '6': return f.value_atom(6); // InvalidOrUnknownTradingSession
          case CINT<'9','9'>: return f.value_atom(7); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 533 "TotalAffectedOrders"
    Field{
      fvar,
      533,
      "TotalAffectedOrders",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 534 "NoAffectedOrders"
    Field{
      fvar,
      534,
      "NoAffectedOrders",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoAffectedOrders",
      534,
      nullptr
    },
    //--- Tag# 535 "AffectedOrderID"
    Field{
      fvar,
      535,
      "AffectedOrderID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 536 "AffectedSecondaryOrderID"
    Field{
      fvar,
      536,
      "AffectedSecondaryOrderID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 537 "QuoteType"
    Field{
      fvar,
      537,
      "QuoteType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Indicative"         , .atom = 0}, // 0
        {.value = "1", .descr = "Tradeable"          , .atom = 0}, // 1
        {.value = "2", .descr = "RestrictedTradeable", .atom = 0}, // 2
        {.value = "3", .descr = "Counter"            , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // INDICATIVE
          case '1': return f.value_atom(1); // TRADEABLE
          case '2': return f.value_atom(2); // RESTRICTED_TRADEABLE
          case '3': return f.value_atom(3); // COUNTER
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 538 "NestedPartyRole"
    Field{
      fvar,
      538,
      "NestedPartyRole",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 539 "NoNestedPartyIDs"
    Field{
      fvar,
      539,
      "NoNestedPartyIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoNestedPartyIDs",
      539,
      nullptr
    },
    //--- Tag# 540 "TotalAccruedInterestAmt"
    Field{
      fvar,
      540,
      "TotalAccruedInterestAmt",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 541 "MaturityDate"
    Field{
      fvar,
      541,
      "MaturityDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 542 "UnderlyingMaturityDate"
    Field{
      fvar,
      542,
      "UnderlyingMaturityDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 543 "InstrRegistry"
    Field{
      fvar,
      543,
      "InstrRegistry",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 544 "CashMargin"
    Field{
      fvar,
      544,
      "CashMargin",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Cash"       , .atom = 0}, // 0
        {.value = "2", .descr = "MarginOpen" , .atom = 0}, // 1
        {.value = "3", .descr = "MarginClose", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // CASH
          case '2': return f.value_atom(1); // MARGIN_OPEN
          case '3': return f.value_atom(2); // MARGIN_CLOSE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 545 "NestedPartySubID"
    Field{
      fvar,
      545,
      "NestedPartySubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 546 "Scope"
    Field{
      fvar,
      546,
      "Scope",
      FieldType::MULTIPLEVALUESTRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Local"   , .atom = 0}, // 0
        {.value = "2", .descr = "National", .atom = 0}, // 1
        {.value = "3", .descr = "Global"  , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // LOCAL
          case '2': return f.value_atom(1); // NATIONAL
          case '3': return f.value_atom(2); // GLOBAL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 547 "MDImplicitDelete"
    Field{
      fvar,
      547,
      "MDImplicitDelete",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 548 "CrossID"
    Field{
      fvar,
      548,
      "CrossID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 549 "CrossType"
    Field{
      fvar,
      549,
      "CrossType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "CrossTradeWhichIsExecutedCompletelyOrNotBothSidesAreTreatedInTheSameMannerThisIsEquivalentToAnAllOrNone"                       , .atom = 0}, // 0
        {.value = "2", .descr = "CrossTradeWhichIsExecutedPartiallyAndTheRestIsCancelledOneSideIsFullyExecutedTheOtherSideIsPartiallyExecutedWithTheRemainder"  , .atom = 0}, // 1
        {.value = "3", .descr = "CrossTradeWhichIsPartiallyExecutedWithTheUnfilledPortionsRemainingActiveOneSideOfTheCrossIsFullyExecuted"                      , .atom = 0}, // 2
        {.value = "4", .descr = "CrossTradeIsExecutedWithExistingOrdersWithTheSamePriceInTheCaseOtherOrdersExistWithTheSamePriceTheQuantityOfTheCrossIsExecuted", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // CROSS_TRADE_WHICH_IS_EXECUTED_COMPLETELY_OR_NOT_BOTH_SIDES_ARE_TREATED_IN_THE_SAME_MANNER_THIS_IS_EQUIVALENT_TO_AN_ALL_OR_NONE
          case '2': return f.value_atom(1); // CROSS_TRADE_WHICH_IS_EXECUTED_PARTIALLY_AND_THE_REST_IS_CANCELLED_ONE_SIDE_IS_FULLY_EXECUTED_THE_OTHER_SIDE_IS_PARTIALLY_EXECUTED_WITH_THE_REMAINDER_BEING_CANCELLED_THIS_IS_EQUIVALENT_TO_AN_IMMEDIATE_OR_CANCEL_ON_THE_OTHER_SIDE_NOTE_THE_CROSSPRIORITZATION
          case '3': return f.value_atom(2); // CROSS_TRADE_WHICH_IS_PARTIALLY_EXECUTED_WITH_THE_UNFILLED_PORTIONS_REMAINING_ACTIVE_ONE_SIDE_OF_THE_CROSS_IS_FULLY_EXECUTED
          case '4': return f.value_atom(3); // CROSS_TRADE_IS_EXECUTED_WITH_EXISTING_ORDERS_WITH_THE_SAME_PRICE_IN_THE_CASE_OTHER_ORDERS_EXIST_WITH_THE_SAME_PRICE_THE_QUANTITY_OF_THE_CROSS_IS_EXECUTED_AGAINST_THE_EXISTING_ORDERS_AND_QUOTES_THE_REMAINDER_OF_THE_CROSS_IS_EXECUTED_AGAINST_THE_OTHER_SIDE_OF_THE_CROSS_THE_TWO_SIDES_POTENTIALLY_HAVE_DIFFERENT_QUANTITIES
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 550 "CrossPrioritization"
    Field{
      fvar,
      550,
      "CrossPrioritization",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "None"                 , .atom = 0}, // 0
        {.value = "1", .descr = "BuySideIsPrioritized" , .atom = 0}, // 1
        {.value = "2", .descr = "SellSideIsPrioritized", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NONE
          case '1': return f.value_atom(1); // BUY_SIDE_IS_PRIORITIZED
          case '2': return f.value_atom(2); // SELL_SIDE_IS_PRIORITIZED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 551 "OrigCrossID"
    Field{
      fvar,
      551,
      "OrigCrossID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 552 "NoSides"
    Field{
      fvar,
      552,
      "NoSides",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "OneSide"  , .atom = 0}, // 0
        {.value = "2", .descr = "BothSides", .atom = 0}, // 1
      }},
      "NoSides",
      552,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // ONE_SIDE
          case '2': return f.value_atom(1); // BOTH_SIDES
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 553 "Username"
    Field{
      fvar,
      553,
      "Username",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 554 "Password"
    Field{
      fvar,
      554,
      "Password",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 555 "NoLegs"
    Field{
      fvar,
      555,
      "NoLegs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoLegs",
      555,
      nullptr
    },
    //--- Tag# 556 "LegCurrency"
    Field{
      fvar,
      556,
      "LegCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 557 "TotNoSecurityTypes"
    Field{
      fvar,
      557,
      "TotNoSecurityTypes",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 558 "NoSecurityTypes"
    Field{
      fvar,
      558,
      "NoSecurityTypes",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoSecurityTypes",
      558,
      nullptr
    },
    //--- Tag# 559 "SecurityListRequestType"
    Field{
      fvar,
      559,
      "SecurityListRequestType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Symbol"                  , .atom = 0}, // 0
        {.value = "1", .descr = "SecuritytypeAndOrCficode", .atom = 0}, // 1
        {.value = "2", .descr = "Product"                 , .atom = 0}, // 2
        {.value = "3", .descr = "Tradingsessionid"        , .atom = 0}, // 3
        {.value = "4", .descr = "AllSecurities"           , .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // SYMBOL
          case '1': return f.value_atom(1); // SECURITYTYPE_AND_OR_CFICODE
          case '2': return f.value_atom(2); // PRODUCT
          case '3': return f.value_atom(3); // TRADINGSESSIONID
          case '4': return f.value_atom(4); // ALL_SECURITIES
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 560 "SecurityRequestResult"
    Field{
      fvar,
      560,
      "SecurityRequestResult",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "ValidRequest"                                , .atom = 0}, // 0
        {.value = "1", .descr = "InvalidOrUnsupportedRequest"                 , .atom = 0}, // 1
        {.value = "2", .descr = "NoInstrumentsFoundThatMatchSelectionCriteria", .atom = 0}, // 2
        {.value = "3", .descr = "NotAuthorizedToRetrieveInstrumentData"       , .atom = 0}, // 3
        {.value = "4", .descr = "InstrumentDataTemporarilyUnavailable"        , .atom = 0}, // 4
        {.value = "5", .descr = "RequestForInstrumentDataNotSupported"        , .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // VALID_REQUEST
          case '1': return f.value_atom(1); // INVALID_OR_UNSUPPORTED_REQUEST
          case '2': return f.value_atom(2); // NO_INSTRUMENTS_FOUND_THAT_MATCH_SELECTION_CRITERIA
          case '3': return f.value_atom(3); // NOT_AUTHORIZED_TO_RETRIEVE_INSTRUMENT_DATA
          case '4': return f.value_atom(4); // INSTRUMENT_DATA_TEMPORARILY_UNAVAILABLE
          case '5': return f.value_atom(5); // REQUEST_FOR_INSTRUMENT_DATA_NOT_SUPPORTED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 561 "RoundLot"
    Field{
      fvar,
      561,
      "RoundLot",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 562 "MinTradeVol"
    Field{
      fvar,
      562,
      "MinTradeVol",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 563 "MultiLegRptTypeReq"
    Field{
      fvar,
      563,
      "MultiLegRptTypeReq",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "ReportByMulitlegSecurityOnly"                                             , .atom = 0}, // 0
        {.value = "1", .descr = "ReportByMultilegSecurityAndByInstrumentLegsBelongingToTheMultilegSecurity", .atom = 0}, // 1
        {.value = "2", .descr = "ReportByInstrumentLegsBelongingToTheMultilegSecurityOnly"                 , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // REPORT_BY_MULITLEG_SECURITY_ONLY
          case '1': return f.value_atom(1); // REPORT_BY_MULTILEG_SECURITY_AND_BY_INSTRUMENT_LEGS_BELONGING_TO_THE_MULTILEG_SECURITY
          case '2': return f.value_atom(2); // REPORT_BY_INSTRUMENT_LEGS_BELONGING_TO_THE_MULTILEG_SECURITY_ONLY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 564 "LegPositionEffect"
    Field{
      fvar,
      564,
      "LegPositionEffect",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 565 "LegCoveredOrUncovered"
    Field{
      fvar,
      565,
      "LegCoveredOrUncovered",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 566 "LegPrice"
    Field{
      fvar,
      566,
      "LegPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 567 "TradSesStatusRejReason"
    Field{
      fvar,
      567,
      "TradSesStatusRejReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "UnknownOrInvalidTradingsessionid", .atom = 0}, // 0
        {.value = "99", .descr = "Other"                           , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom(0); // UnknownOrInvalidTradingsessionid
          case CINT<'9','9'>: return f.value_atom(1); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 568 "TradeRequestID"
    Field{
      fvar,
      568,
      "TradeRequestID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 569 "TradeRequestType"
    Field{
      fvar,
      569,
      "TradeRequestType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "AllTrades"                                     , .atom = 0}, // 0
        {.value = "1", .descr = "MatchedTradesMatchingCriteriaProvidedOnRequest", .atom = 0}, // 1
        {.value = "2", .descr = "UnmatchedTradesThatMatchCriteria"              , .atom = 0}, // 2
        {.value = "3", .descr = "UnreportedTradesThatMatchCriteria"             , .atom = 0}, // 3
        {.value = "4", .descr = "AdvisoriesThatMatchCriteria"                   , .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // ALL_TRADES
          case '1': return f.value_atom(1); // MATCHED_TRADES_MATCHING_CRITERIA_PROVIDED_ON_REQUEST
          case '2': return f.value_atom(2); // UNMATCHED_TRADES_THAT_MATCH_CRITERIA
          case '3': return f.value_atom(3); // UNREPORTED_TRADES_THAT_MATCH_CRITERIA
          case '4': return f.value_atom(4); // ADVISORIES_THAT_MATCH_CRITERIA
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 570 "PreviouslyReported"
    Field{
      fvar,
      570,
      "PreviouslyReported",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 571 "TradeReportID"
    Field{
      fvar,
      571,
      "TradeReportID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 572 "TradeReportRefID"
    Field{
      fvar,
      572,
      "TradeReportRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 573 "MatchStatus"
    Field{
      fvar,
      573,
      "MatchStatus",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "ComparedMatchedOrAffirmed"      , .atom = 0}, // 0
        {.value = "1", .descr = "UncomparedUnmatchedOrUnaffirmed", .atom = 0}, // 1
        {.value = "2", .descr = "AdvisoryOrAlert"                , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // COMPARED_MATCHED_OR_AFFIRMED
          case '1': return f.value_atom(1); // UNCOMPARED_UNMATCHED_OR_UNAFFIRMED
          case '2': return f.value_atom(2); // ADVISORY_OR_ALERT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 574 "MatchType"
    Field{
      fvar,
      574,
      "MatchType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "A1", .descr = "ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusFourBadgesAndExecutionTime", .atom = 0}, // 0
        {.value = "A2", .descr = "ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusFourBadges"                , .atom = 0}, // 1
        {.value = "A3", .descr = "ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusTwoBadgesAndExecutionTime" , .atom = 0}, // 2
        {.value = "A4", .descr = "ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusTwoBadges"                 , .atom = 0}, // 3
        {.value = "A5", .descr = "ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusExecutionTime"             , .atom = 0}, // 4
        {.value = "AQ", .descr = "ComparedRecordsResultingFromStampedAdvisoriesOrSpecialistAcceptsPairOffs"                                    , .atom = 0}, // 5
        {.value = "S1", .descr = "SummarizedMatchUsingA1ExactMatchCriteriaExceptQuantityIsSummarized"                                          , .atom = 0}, // 6
        {.value = "S2", .descr = "SummarizedMatchUsingA2ExactMatchCriteriaExceptQuantityIsSummarized"                                          , .atom = 0}, // 7
        {.value = "S3", .descr = "SummarizedMatchUsingA3ExactMatchCriteriaExceptQuantityIsSummarized"                                          , .atom = 0}, // 8
        {.value = "S4", .descr = "SummarizedMatchUsingA4ExactMatchCriteriaExceptQuantityIsSummarized"                                          , .atom = 0}, // 9
        {.value = "S5", .descr = "SummarizedMatchUsingA5ExactMatchCriteriaExceptQuantityIsSummarized"                                          , .atom = 0}, // 10
        {.value = "M1", .descr = "ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorMinusBadgesAndTimesActM1Match" , .atom = 0}, // 11
        {.value = "M2", .descr = "SummarizedMatchMinusBadgesAndTimesActM2Match"                                                                , .atom = 0}, // 12
        {.value = "MT", .descr = "OcsLockedInNonAct"                                                                                           , .atom = 0}, // 13
        {.value = "M3", .descr = "ActAcceptedTrade"                                                                                            , .atom = 0}, // 14
        {.value = "M4", .descr = "ActDefaultTrade"                                                                                             , .atom = 0}, // 15
        {.value = "M5", .descr = "ActDefaultAfterM2"                                                                                           , .atom = 0}, // 16
        {.value = "M6", .descr = "ActM6Match"                                                                                                  , .atom = 0}, // 17
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case CINT<'A','1'>: return f.value_atom( 0); // ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusFourBadgesAndExecutionTime
          case CINT<'A','2'>: return f.value_atom( 1); // ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusFourBadges
          case CINT<'A','3'>: return f.value_atom( 2); // ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusTwoBadgesAndExecutionTime
          case CINT<'A','4'>: return f.value_atom( 3); // ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusTwoBadges
          case CINT<'A','5'>: return f.value_atom( 4); // ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorPlusExecutionTime
          case CINT<'A','Q'>: return f.value_atom( 5); // ComparedRecordsResultingFromStampedAdvisoriesOrSpecialistAcceptsPairOffs
          case CINT<'S','1'>: return f.value_atom( 6); // SummarizedMatchUsingA1ExactMatchCriteriaExceptQuantityIsSummarized
          case CINT<'S','2'>: return f.value_atom( 7); // SummarizedMatchUsingA2ExactMatchCriteriaExceptQuantityIsSummarized
          case CINT<'S','3'>: return f.value_atom( 8); // SummarizedMatchUsingA3ExactMatchCriteriaExceptQuantityIsSummarized
          case CINT<'S','4'>: return f.value_atom( 9); // SummarizedMatchUsingA4ExactMatchCriteriaExceptQuantityIsSummarized
          case CINT<'S','5'>: return f.value_atom(10); // SummarizedMatchUsingA5ExactMatchCriteriaExceptQuantityIsSummarized
          case CINT<'M','1'>: return f.value_atom(11); // ExactMatchOnTradeDateStockSymbolQuantityPriceTradeTypeAndSpecialTradeIndicatorMinusBadgesAndTimesActM1Match
          case CINT<'M','2'>: return f.value_atom(12); // SummarizedMatchMinusBadgesAndTimesActM2Match
          case CINT<'M','T'>: return f.value_atom(13); // OcsLockedInNonAct
          case CINT<'M','3'>: return f.value_atom(14); // ActAcceptedTrade
          case CINT<'M','4'>: return f.value_atom(15); // ActDefaultTrade
          case CINT<'M','5'>: return f.value_atom(16); // ActDefaultAfterM2
          case CINT<'M','6'>: return f.value_atom(17); // ActM6Match
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 575 "OddLot"
    Field{
      fvar,
      575,
      "OddLot",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 576 "NoClearingInstructions"
    Field{
      fvar,
      576,
      "NoClearingInstructions",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoClearingInstructions",
      576,
      nullptr
    },
    //--- Tag# 577 "ClearingInstruction"
    Field{
      fvar,
      577,
      "ClearingInstruction",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "ProcessNormally"                , .atom = 0}, // 0
        {.value = "1" , .descr = "ExcludeFromAllNetting"          , .atom = 0}, // 1
        {.value = "2" , .descr = "BilateralNettingOnly"           , .atom = 0}, // 2
        {.value = "3" , .descr = "ExClearing"                     , .atom = 0}, // 3
        {.value = "4" , .descr = "SpecialTrade"                   , .atom = 0}, // 4
        {.value = "5" , .descr = "MultilateralNetting"            , .atom = 0}, // 5
        {.value = "6" , .descr = "ClearAgainstCentralCounterparty", .atom = 0}, // 6
        {.value = "7" , .descr = "ExcludeFromCentralCounterparty" , .atom = 0}, // 7
        {.value = "8" , .descr = "ManualMode"                     , .atom = 0}, // 8
        {.value = "9" , .descr = "AutomaticPostingMode"           , .atom = 0}, // 9
        {.value = "10", .descr = "AutomaticGiveUpMode"            , .atom = 0}, // 10
        {.value = "11", .descr = "QualifiedServiceRepresentative" , .atom = 0}, // 11
        {.value = "12", .descr = "CustomerTrade"                  , .atom = 0}, // 12
        {.value = "13", .descr = "SelfClearing"                   , .atom = 0}, // 13
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom( 0); // ProcessNormally
          case           '1': return f.value_atom( 1); // ExcludeFromAllNetting
          case           '2': return f.value_atom( 2); // BilateralNettingOnly
          case           '3': return f.value_atom( 3); // ExClearing
          case           '4': return f.value_atom( 4); // SpecialTrade
          case           '5': return f.value_atom( 5); // MultilateralNetting
          case           '6': return f.value_atom( 6); // ClearAgainstCentralCounterparty
          case           '7': return f.value_atom( 7); // ExcludeFromCentralCounterparty
          case           '8': return f.value_atom( 8); // ManualMode
          case           '9': return f.value_atom( 9); // AutomaticPostingMode
          case CINT<'1','0'>: return f.value_atom(10); // AutomaticGiveUpMode
          case CINT<'1','1'>: return f.value_atom(11); // QualifiedServiceRepresentative
          case CINT<'1','2'>: return f.value_atom(12); // CustomerTrade
          case CINT<'1','3'>: return f.value_atom(13); // SelfClearing
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 578 "TradeInputSource"
    Field{
      fvar,
      578,
      "TradeInputSource",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 579 "TradeInputDevice"
    Field{
      fvar,
      579,
      "TradeInputDevice",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 580 "NoDates"
    Field{
      fvar,
      580,
      "NoDates",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoDates",
      580,
      nullptr
    },
    //--- Tag# 581 "AccountType"
    Field{
      fvar,
      581,
      "AccountType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "AccountIsCarriedOnCustomerSideOfBooks"                     , .atom = 0}, // 0
        {.value = "2", .descr = "AccountIsCarriedOnNonCustomerSideOfBooks"                  , .atom = 0}, // 1
        {.value = "3", .descr = "HouseTrader"                                               , .atom = 0}, // 2
        {.value = "4", .descr = "FloorTrader"                                               , .atom = 0}, // 3
        {.value = "6", .descr = "AccountIsCarriedOnNonCustomerSideOfBooksAndIsCrossMargined", .atom = 0}, // 4
        {.value = "7", .descr = "AccountIsHouseTraderAndIsCrossMargined"                    , .atom = 0}, // 5
        {.value = "8", .descr = "JointBackofficeAccount"                                    , .atom = 0}, // 6
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // ACCOUNT_IS_CARRIED_ON_CUSTOMER_SIDE_OF_BOOKS
          case '2': return f.value_atom(1); // ACCOUNT_IS_CARRIED_ON_NON_CUSTOMER_SIDE_OF_BOOKS
          case '3': return f.value_atom(2); // HOUSE_TRADER
          case '4': return f.value_atom(3); // FLOOR_TRADER
          case '6': return f.value_atom(4); // ACCOUNT_IS_CARRIED_ON_NON_CUSTOMER_SIDE_OF_BOOKS_AND_IS_CROSS_MARGINED
          case '7': return f.value_atom(5); // ACCOUNT_IS_HOUSE_TRADER_AND_IS_CROSS_MARGINED
          case '8': return f.value_atom(6); // JOINT_BACKOFFICE_ACCOUNT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 582 "CustOrderCapacity"
    Field{
      fvar,
      582,
      "CustOrderCapacity",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "MemberTradingForTheirOwnAccount"            , .atom = 0}, // 0
        {.value = "2", .descr = "ClearingFirmTradingForItsProprietaryAccount", .atom = 0}, // 1
        {.value = "3", .descr = "MemberTradingForAnotherMember"              , .atom = 0}, // 2
        {.value = "4", .descr = "AllOther"                                   , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // MEMBER_TRADING_FOR_THEIR_OWN_ACCOUNT
          case '2': return f.value_atom(1); // CLEARING_FIRM_TRADING_FOR_ITS_PROPRIETARY_ACCOUNT
          case '3': return f.value_atom(2); // MEMBER_TRADING_FOR_ANOTHER_MEMBER
          case '4': return f.value_atom(3); // ALL_OTHER
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 583 "ClOrdLinkID"
    Field{
      fvar,
      583,
      "ClOrdLinkID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 584 "MassStatusReqID"
    Field{
      fvar,
      584,
      "MassStatusReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 585 "MassStatusReqType"
    Field{
      fvar,
      585,
      "MassStatusReqType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "StatusForOrdersForASecurity"           , .atom = 0}, // 0
        {.value = "2", .descr = "StatusForOrdersForAnUnderlyingSecurity", .atom = 0}, // 1
        {.value = "3", .descr = "StatusForOrdersForAProduct"            , .atom = 0}, // 2
        {.value = "4", .descr = "StatusForOrdersForACficode"            , .atom = 0}, // 3
        {.value = "5", .descr = "StatusForOrdersForASecuritytype"       , .atom = 0}, // 4
        {.value = "6", .descr = "StatusForOrdersForATradingSession"     , .atom = 0}, // 5
        {.value = "7", .descr = "StatusForAllOrders"                    , .atom = 0}, // 6
        {.value = "8", .descr = "StatusForOrdersForAPartyid"            , .atom = 0}, // 7
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // STATUS_FOR_ORDERS_FOR_A_SECURITY
          case '2': return f.value_atom(1); // STATUS_FOR_ORDERS_FOR_AN_UNDERLYING_SECURITY
          case '3': return f.value_atom(2); // STATUS_FOR_ORDERS_FOR_A_PRODUCT
          case '4': return f.value_atom(3); // STATUS_FOR_ORDERS_FOR_A_CFICODE
          case '5': return f.value_atom(4); // STATUS_FOR_ORDERS_FOR_A_SECURITYTYPE
          case '6': return f.value_atom(5); // STATUS_FOR_ORDERS_FOR_A_TRADING_SESSION
          case '7': return f.value_atom(6); // STATUS_FOR_ALL_ORDERS
          case '8': return f.value_atom(7); // STATUS_FOR_ORDERS_FOR_A_PARTYID
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 586 "OrigOrdModTime"
    Field{
      fvar,
      586,
      "OrigOrdModTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 587 "LegSettlType"
    Field{
      fvar,
      587,
      "LegSettlType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 588 "LegSettlDate"
    Field{
      fvar,
      588,
      "LegSettlDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 589 "DayBookingInst"
    Field{
      fvar,
      589,
      "DayBookingInst",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "CanTriggerBookingWithoutReferenceToTheOrderInitiator", .atom = 0}, // 0
        {.value = "1", .descr = "SpeakWithOrderInitiatorBeforeBooking"                , .atom = 0}, // 1
        {.value = "2", .descr = "Accumulate"                                          , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // CAN_TRIGGER_BOOKING_WITHOUT_REFERENCE_TO_THE_ORDER_INITIATOR
          case '1': return f.value_atom(1); // SPEAK_WITH_ORDER_INITIATOR_BEFORE_BOOKING
          case '2': return f.value_atom(2); // ACCUMULATE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 590 "BookingUnit"
    Field{
      fvar,
      590,
      "BookingUnit",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "EachPartialExecutionIsABookableUnit"                         , .atom = 0}, // 0
        {.value = "1", .descr = "AggregatePartialExecutionsOnThisOrderAndBookOneTradePerOrder", .atom = 0}, // 1
        {.value = "2", .descr = "AggregateExecutionsForThisSymbolSideAndSettlementDate"       , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // EACH_PARTIAL_EXECUTION_IS_A_BOOKABLE_UNIT
          case '1': return f.value_atom(1); // AGGREGATE_PARTIAL_EXECUTIONS_ON_THIS_ORDER_AND_BOOK_ONE_TRADE_PER_ORDER
          case '2': return f.value_atom(2); // AGGREGATE_EXECUTIONS_FOR_THIS_SYMBOL_SIDE_AND_SETTLEMENT_DATE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 591 "PreallocMethod"
    Field{
      fvar,
      591,
      "PreallocMethod",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "ProRata"                 , .atom = 0}, // 0
        {.value = "1", .descr = "DoNotProRataDiscussFirst", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // PRO_RATA
          case '1': return f.value_atom(1); // DO_NOT_PRO_RATA_DISCUSS_FIRST
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 592 "UnderlyingCountryOfIssue"
    Field{
      fvar,
      592,
      "UnderlyingCountryOfIssue",
      FieldType::COUNTRY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 593 "UnderlyingStateOrProvinceOfIssue"
    Field{
      fvar,
      593,
      "UnderlyingStateOrProvinceOfIssue",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 594 "UnderlyingLocaleOfIssue"
    Field{
      fvar,
      594,
      "UnderlyingLocaleOfIssue",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 595 "UnderlyingInstrRegistry"
    Field{
      fvar,
      595,
      "UnderlyingInstrRegistry",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 596 "LegCountryOfIssue"
    Field{
      fvar,
      596,
      "LegCountryOfIssue",
      FieldType::COUNTRY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 597 "LegStateOrProvinceOfIssue"
    Field{
      fvar,
      597,
      "LegStateOrProvinceOfIssue",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 598 "LegLocaleOfIssue"
    Field{
      fvar,
      598,
      "LegLocaleOfIssue",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 599 "LegInstrRegistry"
    Field{
      fvar,
      599,
      "LegInstrRegistry",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 600 "LegSymbol"
    Field{
      fvar,
      600,
      "LegSymbol",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 601 "LegSymbolSfx"
    Field{
      fvar,
      601,
      "LegSymbolSfx",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 602 "LegSecurityID"
    Field{
      fvar,
      602,
      "LegSecurityID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 603 "LegSecurityIDSource"
    Field{
      fvar,
      603,
      "LegSecurityIDSource",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 604 "NoLegSecurityAltID"
    Field{
      fvar,
      604,
      "NoLegSecurityAltID",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoLegSecurityAltID",
      604,
      nullptr
    },
    //--- Tag# 605 "LegSecurityAltID"
    Field{
      fvar,
      605,
      "LegSecurityAltID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 606 "LegSecurityAltIDSource"
    Field{
      fvar,
      606,
      "LegSecurityAltIDSource",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 607 "LegProduct"
    Field{
      fvar,
      607,
      "LegProduct",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 608 "LegCFICode"
    Field{
      fvar,
      608,
      "LegCFICode",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 609 "LegSecurityType"
    Field{
      fvar,
      609,
      "LegSecurityType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 610 "LegMaturityMonthYear"
    Field{
      fvar,
      610,
      "LegMaturityMonthYear",
      FieldType::MONTHYEAR,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 611 "LegMaturityDate"
    Field{
      fvar,
      611,
      "LegMaturityDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 612 "LegStrikePrice"
    Field{
      fvar,
      612,
      "LegStrikePrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 613 "LegOptAttribute"
    Field{
      fvar,
      613,
      "LegOptAttribute",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 614 "LegContractMultiplier"
    Field{
      fvar,
      614,
      "LegContractMultiplier",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 615 "LegCouponRate"
    Field{
      fvar,
      615,
      "LegCouponRate",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 616 "LegSecurityExchange"
    Field{
      fvar,
      616,
      "LegSecurityExchange",
      FieldType::EXCHANGE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 617 "LegIssuer"
    Field{
      fvar,
      617,
      "LegIssuer",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 618 "EncodedLegIssuerLen"
    Field{
      fvar,
      618,
      "EncodedLegIssuerLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 619 "EncodedLegIssuer"
    Field{
      fvar,
      619,
      "EncodedLegIssuer",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedLegIssuerLen",
      618,
      nullptr
    },
    //--- Tag# 620 "LegSecurityDesc"
    Field{
      fvar,
      620,
      "LegSecurityDesc",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 621 "EncodedLegSecurityDescLen"
    Field{
      fvar,
      621,
      "EncodedLegSecurityDescLen",
      FieldType::LENGTH,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 622 "EncodedLegSecurityDesc"
    Field{
      fvar,
      622,
      "EncodedLegSecurityDesc",
      FieldType::DATA,
      DataType::BINARY,
      std::vector<FieldChoice>(),
      "EncodedLegSecurityDescLen",
      621,
      nullptr
    },
    //--- Tag# 623 "LegRatioQty"
    Field{
      fvar,
      623,
      "LegRatioQty",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 624 "LegSide"
    Field{
      fvar,
      624,
      "LegSide",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 625 "TradingSessionSubID"
    Field{
      fvar,
      625,
      "TradingSessionSubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 626 "AllocType"
    Field{
      fvar,
      626,
      "AllocType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Calculated"           , .atom = 0}, // 0
        {.value = "2", .descr = "Preliminary"          , .atom = 0}, // 1
        {.value = "5", .descr = "ReadyToBook"          , .atom = 0}, // 2
        {.value = "7", .descr = "WarehouseInstruction" , .atom = 0}, // 3
        {.value = "8", .descr = "RequestToIntermediary", .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // CALCULATED
          case '2': return f.value_atom(1); // PRELIMINARY
          case '5': return f.value_atom(2); // READY_TO_BOOK
          case '7': return f.value_atom(3); // WAREHOUSE_INSTRUCTION
          case '8': return f.value_atom(4); // REQUEST_TO_INTERMEDIARY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 627 "NoHops"
    Field{
      fvar,
      627,
      "NoHops",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoHops",
      627,
      nullptr
    },
    //--- Tag# 628 "HopCompID"
    Field{
      fvar,
      628,
      "HopCompID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 629 "HopSendingTime"
    Field{
      fvar,
      629,
      "HopSendingTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 630 "HopRefID"
    Field{
      fvar,
      630,
      "HopRefID",
      FieldType::SEQNUM,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 631 "MidPx"
    Field{
      fvar,
      631,
      "MidPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 632 "BidYield"
    Field{
      fvar,
      632,
      "BidYield",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 633 "MidYield"
    Field{
      fvar,
      633,
      "MidYield",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 634 "OfferYield"
    Field{
      fvar,
      634,
      "OfferYield",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 635 "ClearingFeeIndicator"
    Field{
      fvar,
      635,
      "ClearingFeeIndicator",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "B", .descr = "CboeMember"                                                 , .atom = 0}, // 0
        {.value = "C", .descr = "NonMemberAndCustomer"                                       , .atom = 0}, // 1
        {.value = "E", .descr = "EquityMemberAndClearingMember"                              , .atom = 0}, // 2
        {.value = "F", .descr = "FullAndAssociateMemberTradingForOwnAccountAndAsFloorBrokers", .atom = 0}, // 3
        {.value = "H", .descr = "106hAnd106jFirms"                                           , .atom = 0}, // 4
        {.value = "I", .descr = "GimIdemAndComMembershipInterestHolders"                     , .atom = 0}, // 5
        {.value = "L", .descr = "LesseeAnd106fEmployees"                                     , .atom = 0}, // 6
        {.value = "M", .descr = "AllOtherOwnershipTypes"                                     , .atom = 0}, // 7
        {.value = "1", .descr = "1stYearDelegateTradingForHisOwnAccount"                     , .atom = 0}, // 8
        {.value = "2", .descr = "2ndYearDelegateTradingForHisOwnAccount"                     , .atom = 0}, // 9
        {.value = "3", .descr = "3rdYearDelegateTradingForHisOwnAccount"                     , .atom = 0}, // 10
        {.value = "4", .descr = "4thYearDelegateTradingForHisOwnAccount"                     , .atom = 0}, // 11
        {.value = "5", .descr = "5thYearDelegateTradingForHisOwnAccount"                     , .atom = 0}, // 12
        {.value = "9", .descr = "6thYearAndBeyondDelegateTradingForHisOwnAccount"            , .atom = 0}, // 13
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'B': return f.value_atom(0); // CBOE_MEMBER
          case 'C': return f.value_atom(1); // NON_MEMBER_AND_CUSTOMER
          case 'E': return f.value_atom(2); // EQUITY_MEMBER_AND_CLEARING_MEMBER
          case 'F': return f.value_atom(3); // FULL_AND_ASSOCIATE_MEMBER_TRADING_FOR_OWN_ACCOUNT_AND_AS_FLOOR_BROKERS
          case 'H': return f.value_atom(4); // 106H_AND_106J_FIRMS
          case 'I': return f.value_atom(5); // GIM_IDEM_AND_COM_MEMBERSHIP_INTEREST_HOLDERS
          case 'L': return f.value_atom(6); // LESSEE_AND_106F_EMPLOYEES
          case 'M': return f.value_atom(7); // ALL_OTHER_OWNERSHIP_TYPES
          case '1': return f.value_atom(8); // 1ST_YEAR_DELEGATE_TRADING_FOR_HIS_OWN_ACCOUNT
          case '2': return f.value_atom(9); // 2ND_YEAR_DELEGATE_TRADING_FOR_HIS_OWN_ACCOUNT
          case '3': return f.value_atom(10); // 3RD_YEAR_DELEGATE_TRADING_FOR_HIS_OWN_ACCOUNT
          case '4': return f.value_atom(11); // 4TH_YEAR_DELEGATE_TRADING_FOR_HIS_OWN_ACCOUNT
          case '5': return f.value_atom(12); // 5TH_YEAR_DELEGATE_TRADING_FOR_HIS_OWN_ACCOUNT
          case '9': return f.value_atom(13); // 6TH_YEAR_AND_BEYOND_DELEGATE_TRADING_FOR_HIS_OWN_ACCOUNT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 636 "WorkingIndicator"
    Field{
      fvar,
      636,
      "WorkingIndicator",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 637 "LegLastPx"
    Field{
      fvar,
      637,
      "LegLastPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 638 "PriorityIndicator"
    Field{
      fvar,
      638,
      "PriorityIndicator",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "PriorityUnchanged"                , .atom = 0}, // 0
        {.value = "1", .descr = "LostPriorityAsResultOfOrderChange", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // PRIORITY_UNCHANGED
          case '1': return f.value_atom(1); // LOST_PRIORITY_AS_RESULT_OF_ORDER_CHANGE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 639 "PriceImprovement"
    Field{
      fvar,
      639,
      "PriceImprovement",
      FieldType::PRICEOFFSET,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 640 "Price2"
    Field{
      fvar,
      640,
      "Price2",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 641 "LastForwardPoints2"
    Field{
      fvar,
      641,
      "LastForwardPoints2",
      FieldType::PRICEOFFSET,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 642 "BidForwardPoints2"
    Field{
      fvar,
      642,
      "BidForwardPoints2",
      FieldType::PRICEOFFSET,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 643 "OfferForwardPoints2"
    Field{
      fvar,
      643,
      "OfferForwardPoints2",
      FieldType::PRICEOFFSET,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 644 "RFQReqID"
    Field{
      fvar,
      644,
      "RFQReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 645 "MktBidPx"
    Field{
      fvar,
      645,
      "MktBidPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 646 "MktOfferPx"
    Field{
      fvar,
      646,
      "MktOfferPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 647 "MinBidSize"
    Field{
      fvar,
      647,
      "MinBidSize",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 648 "MinOfferSize"
    Field{
      fvar,
      648,
      "MinOfferSize",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 649 "QuoteStatusReqID"
    Field{
      fvar,
      649,
      "QuoteStatusReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 650 "LegalConfirm"
    Field{
      fvar,
      650,
      "LegalConfirm",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 651 "UnderlyingLastPx"
    Field{
      fvar,
      651,
      "UnderlyingLastPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 652 "UnderlyingLastQty"
    Field{
      fvar,
      652,
      "UnderlyingLastQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 653
    Field{},
    //--- Tag# 654 "LegRefID"
    Field{
      fvar,
      654,
      "LegRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 655 "ContraLegRefID"
    Field{
      fvar,
      655,
      "ContraLegRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 656 "SettlCurrBidFxRate"
    Field{
      fvar,
      656,
      "SettlCurrBidFxRate",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 657 "SettlCurrOfferFxRate"
    Field{
      fvar,
      657,
      "SettlCurrOfferFxRate",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 658 "QuoteRequestRejectReason"
    Field{
      fvar,
      658,
      "QuoteRequestRejectReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "UnknownSymbol"              , .atom = 0}, // 0
        {.value = "2" , .descr = "Exchange"                   , .atom = 0}, // 1
        {.value = "3" , .descr = "QuoteRequestExceedsLimit"   , .atom = 0}, // 2
        {.value = "4" , .descr = "TooLateToEnter"             , .atom = 0}, // 3
        {.value = "5" , .descr = "InvalidPrice"               , .atom = 0}, // 4
        {.value = "6" , .descr = "NotAuthorizedToRequestQuote", .atom = 0}, // 5
        {.value = "7" , .descr = "NoMatchForInquiry"          , .atom = 0}, // 6
        {.value = "8" , .descr = "NoMarketForInstrument"      , .atom = 0}, // 7
        {.value = "9" , .descr = "NoInventory"                , .atom = 0}, // 8
        {.value = "10", .descr = "Pass"                       , .atom = 0}, // 9
        {.value = "99", .descr = "Other"                      , .atom = 0}, // 10
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // UnknownSymbol
          case           '2': return f.value_atom( 1); // Exchange
          case           '3': return f.value_atom( 2); // QuoteRequestExceedsLimit
          case           '4': return f.value_atom( 3); // TooLateToEnter
          case           '5': return f.value_atom( 4); // InvalidPrice
          case           '6': return f.value_atom( 5); // NotAuthorizedToRequestQuote
          case           '7': return f.value_atom( 6); // NoMatchForInquiry
          case           '8': return f.value_atom( 7); // NoMarketForInstrument
          case           '9': return f.value_atom( 8); // NoInventory
          case CINT<'1','0'>: return f.value_atom( 9); // Pass
          case CINT<'9','9'>: return f.value_atom(10); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 659 "SideComplianceID"
    Field{
      fvar,
      659,
      "SideComplianceID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 660 "AcctIDSource"
    Field{
      fvar,
      660,
      "AcctIDSource",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "Bic"     , .atom = 0}, // 0
        {.value = "2" , .descr = "SidCode" , .atom = 0}, // 1
        {.value = "3" , .descr = "Tfm"     , .atom = 0}, // 2
        {.value = "4" , .descr = "Omgeo"   , .atom = 0}, // 3
        {.value = "5" , .descr = "DtccCode", .atom = 0}, // 4
        {.value = "99", .descr = "Other"   , .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom(0); // Bic
          case           '2': return f.value_atom(1); // SidCode
          case           '3': return f.value_atom(2); // Tfm
          case           '4': return f.value_atom(3); // Omgeo
          case           '5': return f.value_atom(4); // DtccCode
          case CINT<'9','9'>: return f.value_atom(5); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 661 "AllocAcctIDSource"
    Field{
      fvar,
      661,
      "AllocAcctIDSource",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 662 "BenchmarkPrice"
    Field{
      fvar,
      662,
      "BenchmarkPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 663 "BenchmarkPriceType"
    Field{
      fvar,
      663,
      "BenchmarkPriceType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 664 "ConfirmID"
    Field{
      fvar,
      664,
      "ConfirmID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 665 "ConfirmStatus"
    Field{
      fvar,
      665,
      "ConfirmStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Received"                     , .atom = 0}, // 0
        {.value = "2", .descr = "MismatchedAccount"            , .atom = 0}, // 1
        {.value = "3", .descr = "MissingSettlementInstructions", .atom = 0}, // 2
        {.value = "4", .descr = "Confirmed"                    , .atom = 0}, // 3
        {.value = "5", .descr = "RequestRejected"              , .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // RECEIVED
          case '2': return f.value_atom(1); // MISMATCHED_ACCOUNT
          case '3': return f.value_atom(2); // MISSING_SETTLEMENT_INSTRUCTIONS
          case '4': return f.value_atom(3); // CONFIRMED
          case '5': return f.value_atom(4); // REQUEST_REJECTED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 666 "ConfirmTransType"
    Field{
      fvar,
      666,
      "ConfirmTransType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "New"    , .atom = 0}, // 0
        {.value = "1", .descr = "Replace", .atom = 0}, // 1
        {.value = "2", .descr = "Cancel" , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NEW
          case '1': return f.value_atom(1); // REPLACE
          case '2': return f.value_atom(2); // CANCEL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 667 "ContractSettlMonth"
    Field{
      fvar,
      667,
      "ContractSettlMonth",
      FieldType::MONTHYEAR,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 668 "DeliveryForm"
    Field{
      fvar,
      668,
      "DeliveryForm",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Bookentry", .atom = 0}, // 0
        {.value = "2", .descr = "Bearer"   , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // BOOKENTRY
          case '2': return f.value_atom(1); // BEARER
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 669 "LastParPx"
    Field{
      fvar,
      669,
      "LastParPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 670 "NoLegAllocs"
    Field{
      fvar,
      670,
      "NoLegAllocs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoLegAllocs",
      670,
      nullptr
    },
    //--- Tag# 671 "LegAllocAccount"
    Field{
      fvar,
      671,
      "LegAllocAccount",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 672 "LegIndividualAllocID"
    Field{
      fvar,
      672,
      "LegIndividualAllocID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 673 "LegAllocQty"
    Field{
      fvar,
      673,
      "LegAllocQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 674 "LegAllocAcctIDSource"
    Field{
      fvar,
      674,
      "LegAllocAcctIDSource",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 675 "LegSettlCurrency"
    Field{
      fvar,
      675,
      "LegSettlCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 676 "LegBenchmarkCurveCurrency"
    Field{
      fvar,
      676,
      "LegBenchmarkCurveCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 677 "LegBenchmarkCurveName"
    Field{
      fvar,
      677,
      "LegBenchmarkCurveName",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 678 "LegBenchmarkCurvePoint"
    Field{
      fvar,
      678,
      "LegBenchmarkCurvePoint",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 679 "LegBenchmarkPrice"
    Field{
      fvar,
      679,
      "LegBenchmarkPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 680 "LegBenchmarkPriceType"
    Field{
      fvar,
      680,
      "LegBenchmarkPriceType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 681 "LegBidPx"
    Field{
      fvar,
      681,
      "LegBidPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 682 "LegIOIQty"
    Field{
      fvar,
      682,
      "LegIOIQty",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 683 "NoLegStipulations"
    Field{
      fvar,
      683,
      "NoLegStipulations",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoLegStipulations",
      683,
      nullptr
    },
    //--- Tag# 684 "LegOfferPx"
    Field{
      fvar,
      684,
      "LegOfferPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 685
    Field{},
    //--- Tag# 686 "LegPriceType"
    Field{
      fvar,
      686,
      "LegPriceType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 687 "LegQty"
    Field{
      fvar,
      687,
      "LegQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 688 "LegStipulationType"
    Field{
      fvar,
      688,
      "LegStipulationType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 689 "LegStipulationValue"
    Field{
      fvar,
      689,
      "LegStipulationValue",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 690 "LegSwapType"
    Field{
      fvar,
      690,
      "LegSwapType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "ParForPar"       , .atom = 0}, // 0
        {.value = "2", .descr = "ModifiedDuration", .atom = 0}, // 1
        {.value = "4", .descr = "Risk"            , .atom = 0}, // 2
        {.value = "5", .descr = "Proceeds"        , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // PAR_FOR_PAR
          case '2': return f.value_atom(1); // MODIFIED_DURATION
          case '4': return f.value_atom(2); // RISK
          case '5': return f.value_atom(3); // PROCEEDS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 691 "Pool"
    Field{
      fvar,
      691,
      "Pool",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 692 "QuotePriceType"
    Field{
      fvar,
      692,
      "QuotePriceType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "Percent"                         , .atom = 0}, // 0
        {.value = "2" , .descr = "PerShare"                        , .atom = 0}, // 1
        {.value = "3" , .descr = "FixedAmount"                     , .atom = 0}, // 2
        {.value = "4" , .descr = "DiscountPercentagePointsBelowPar", .atom = 0}, // 3
        {.value = "5" , .descr = "PremiumPercentagePointsOverPar"  , .atom = 0}, // 4
        {.value = "6" , .descr = "BasisPointsRelativeToBenchmark"  , .atom = 0}, // 5
        {.value = "7" , .descr = "TedPrice"                        , .atom = 0}, // 6
        {.value = "8" , .descr = "TedYield"                        , .atom = 0}, // 7
        {.value = "9" , .descr = "YieldSpread"                     , .atom = 0}, // 8
        {.value = "10", .descr = "Yield"                           , .atom = 0}, // 9
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // Percent
          case           '2': return f.value_atom( 1); // PerShare
          case           '3': return f.value_atom( 2); // FixedAmount
          case           '4': return f.value_atom( 3); // DiscountPercentagePointsBelowPar
          case           '5': return f.value_atom( 4); // PremiumPercentagePointsOverPar
          case           '6': return f.value_atom( 5); // BasisPointsRelativeToBenchmark
          case           '7': return f.value_atom( 6); // TedPrice
          case           '8': return f.value_atom( 7); // TedYield
          case           '9': return f.value_atom( 8); // YieldSpread
          case CINT<'1','0'>: return f.value_atom( 9); // Yield
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 693 "QuoteRespID"
    Field{
      fvar,
      693,
      "QuoteRespID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 694 "QuoteRespType"
    Field{
      fvar,
      694,
      "QuoteRespType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "HitLift" , .atom = 0}, // 0
        {.value = "2", .descr = "Counter" , .atom = 0}, // 1
        {.value = "3", .descr = "Expired" , .atom = 0}, // 2
        {.value = "4", .descr = "Cover"   , .atom = 0}, // 3
        {.value = "5", .descr = "DoneAway", .atom = 0}, // 4
        {.value = "6", .descr = "Pass"    , .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // HIT_LIFT
          case '2': return f.value_atom(1); // COUNTER
          case '3': return f.value_atom(2); // EXPIRED
          case '4': return f.value_atom(3); // COVER
          case '5': return f.value_atom(4); // DONE_AWAY
          case '6': return f.value_atom(5); // PASS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 695 "QuoteQualifier"
    Field{
      fvar,
      695,
      "QuoteQualifier",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 696 "YieldRedemptionDate"
    Field{
      fvar,
      696,
      "YieldRedemptionDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 697 "YieldRedemptionPrice"
    Field{
      fvar,
      697,
      "YieldRedemptionPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 698 "YieldRedemptionPriceType"
    Field{
      fvar,
      698,
      "YieldRedemptionPriceType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 699 "BenchmarkSecurityID"
    Field{
      fvar,
      699,
      "BenchmarkSecurityID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 700 "ReversalIndicator"
    Field{
      fvar,
      700,
      "ReversalIndicator",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 701 "YieldCalcDate"
    Field{
      fvar,
      701,
      "YieldCalcDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 702 "NoPositions"
    Field{
      fvar,
      702,
      "NoPositions",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoPositions",
      702,
      nullptr
    },
    //--- Tag# 703 "PosType"
    Field{
      fvar,
      703,
      "PosType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "TQ" , .descr = "TransactionQuantity"      , .atom = 0}, // 0
        {.value = "IAS", .descr = "IntraSpreadQty"           , .atom = 0}, // 1
        {.value = "IES", .descr = "InterSpreadQty"           , .atom = 0}, // 2
        {.value = "FIN", .descr = "EndOfDayQty"              , .atom = 0}, // 3
        {.value = "SOD", .descr = "StartOfDayQty"            , .atom = 0}, // 4
        {.value = "EX" , .descr = "OptionExerciseQty"        , .atom = 0}, // 5
        {.value = "AS" , .descr = "OptionAssignment"         , .atom = 0}, // 6
        {.value = "TX" , .descr = "TransactionFromExercise"  , .atom = 0}, // 7
        {.value = "TA" , .descr = "TransactionFromAssignment", .atom = 0}, // 8
        {.value = "PIT", .descr = "PitTradeQty"              , .atom = 0}, // 9
        {.value = "TRF", .descr = "TransferTradeQty"         , .atom = 0}, // 10
        {.value = "ETR", .descr = "ElectronicTradeQty"       , .atom = 0}, // 11
        {.value = "ALC", .descr = "AllocationTradeQty"       , .atom = 0}, // 12
        {.value = "PA" , .descr = "AdjustmentQty"            , .atom = 0}, // 13
        {.value = "ASF", .descr = "AsOfTradeQty"             , .atom = 0}, // 14
        {.value = "DLV", .descr = "DeliveryQty"              , .atom = 0}, // 15
        {.value = "TOT", .descr = "TotalTransactionQty"      , .atom = 0}, // 16
        {.value = "XM" , .descr = "CrossMarginQty"           , .atom = 0}, // 17
        {.value = "SPL", .descr = "IntegralSplit"            , .atom = 0}, // 18
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case     CINT<'T','Q'>: return f.value_atom( 0); // TransactionQuantity
          case CINT<'I','A','S'>: return f.value_atom( 1); // IntraSpreadQty
          case CINT<'I','E','S'>: return f.value_atom( 2); // InterSpreadQty
          case CINT<'F','I','N'>: return f.value_atom( 3); // EndOfDayQty
          case CINT<'S','O','D'>: return f.value_atom( 4); // StartOfDayQty
          case     CINT<'E','X'>: return f.value_atom( 5); // OptionExerciseQty
          case     CINT<'A','S'>: return f.value_atom( 6); // OptionAssignment
          case     CINT<'T','X'>: return f.value_atom( 7); // TransactionFromExercise
          case     CINT<'T','A'>: return f.value_atom( 8); // TransactionFromAssignment
          case CINT<'P','I','T'>: return f.value_atom( 9); // PitTradeQty
          case CINT<'T','R','F'>: return f.value_atom(10); // TransferTradeQty
          case CINT<'E','T','R'>: return f.value_atom(11); // ElectronicTradeQty
          case CINT<'A','L','C'>: return f.value_atom(12); // AllocationTradeQty
          case     CINT<'P','A'>: return f.value_atom(13); // AdjustmentQty
          case CINT<'A','S','F'>: return f.value_atom(14); // AsOfTradeQty
          case CINT<'D','L','V'>: return f.value_atom(15); // DeliveryQty
          case CINT<'T','O','T'>: return f.value_atom(16); // TotalTransactionQty
          case     CINT<'X','M'>: return f.value_atom(17); // CrossMarginQty
          case CINT<'S','P','L'>: return f.value_atom(18); // IntegralSplit
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 704 "LongQty"
    Field{
      fvar,
      704,
      "LongQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 705 "ShortQty"
    Field{
      fvar,
      705,
      "ShortQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 706 "PosQtyStatus"
    Field{
      fvar,
      706,
      "PosQtyStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Submitted", .atom = 0}, // 0
        {.value = "1", .descr = "Accepted" , .atom = 0}, // 1
        {.value = "2", .descr = "Rejected" , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // SUBMITTED
          case '1': return f.value_atom(1); // ACCEPTED
          case '2': return f.value_atom(2); // REJECTED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 707 "PosAmtType"
    Field{
      fvar,
      707,
      "PosAmtType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "FMTM", .descr = "FinalMarkToMarketAmount"      , .atom = 0}, // 0
        {.value = "IMTM", .descr = "IncrementalMarkToMarketAmount", .atom = 0}, // 1
        {.value = "TVAR", .descr = "TradeVariationAmount"         , .atom = 0}, // 2
        {.value = "SMTM", .descr = "StartOfDayMarkToMarketAmount" , .atom = 0}, // 3
        {.value = "PREM", .descr = "PremiumAmount"                , .atom = 0}, // 4
        {.value = "CRES", .descr = "CashResidualAmount"           , .atom = 0}, // 5
        {.value = "CASH", .descr = "CashAmount"                   , .atom = 0}, // 6
        {.value = "VADJ", .descr = "ValueAdjustedAmount"          , .atom = 0}, // 7
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case CINT<'F','M','T','M'>: return f.value_atom(0); // FinalMarkToMarketAmount
          case CINT<'I','M','T','M'>: return f.value_atom(1); // IncrementalMarkToMarketAmount
          case CINT<'T','V','A','R'>: return f.value_atom(2); // TradeVariationAmount
          case CINT<'S','M','T','M'>: return f.value_atom(3); // StartOfDayMarkToMarketAmount
          case CINT<'P','R','E','M'>: return f.value_atom(4); // PremiumAmount
          case CINT<'C','R','E','S'>: return f.value_atom(5); // CashResidualAmount
          case CINT<'C','A','S','H'>: return f.value_atom(6); // CashAmount
          case CINT<'V','A','D','J'>: return f.value_atom(7); // ValueAdjustedAmount
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 708 "PosAmt"
    Field{
      fvar,
      708,
      "PosAmt",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 709 "PosTransType"
    Field{
      fvar,
      709,
      "PosTransType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Exercise"                                 , .atom = 0}, // 0
        {.value = "2", .descr = "DoNotExercise"                            , .atom = 0}, // 1
        {.value = "3", .descr = "PositionAdjustment"                       , .atom = 0}, // 2
        {.value = "4", .descr = "PositionChangeSubmissionMarginDisposition", .atom = 0}, // 3
        {.value = "5", .descr = "Pledge"                                   , .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // EXERCISE
          case '2': return f.value_atom(1); // DO_NOT_EXERCISE
          case '3': return f.value_atom(2); // POSITION_ADJUSTMENT
          case '4': return f.value_atom(3); // POSITION_CHANGE_SUBMISSION_MARGIN_DISPOSITION
          case '5': return f.value_atom(4); // PLEDGE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 710 "PosReqID"
    Field{
      fvar,
      710,
      "PosReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 711 "NoUnderlyings"
    Field{
      fvar,
      711,
      "NoUnderlyings",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoUnderlyings",
      711,
      nullptr
    },
    //--- Tag# 712 "PosMaintAction"
    Field{
      fvar,
      712,
      "PosMaintAction",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "NewUsedToIncrementTheOverallTransactionQuantity"                                             , .atom = 0}, // 0
        {.value = "2", .descr = "ReplaceUsedToOverrideTheOverallTransactionQuantityOrSpecificAddMessagesBasedOnTheReferenceId", .atom = 0}, // 1
        {.value = "3", .descr = "CancelUsedToRemoveTheOverallTransactionOrSpecificAddMessagesBasedOnReferenceId"              , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // NEW_USED_TO_INCREMENT_THE_OVERALL_TRANSACTION_QUANTITY
          case '2': return f.value_atom(1); // REPLACE_USED_TO_OVERRIDE_THE_OVERALL_TRANSACTION_QUANTITY_OR_SPECIFIC_ADD_MESSAGES_BASED_ON_THE_REFERENCE_ID
          case '3': return f.value_atom(2); // CANCEL_USED_TO_REMOVE_THE_OVERALL_TRANSACTION_OR_SPECIFIC_ADD_MESSAGES_BASED_ON_REFERENCE_ID
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 713 "OrigPosReqRefID"
    Field{
      fvar,
      713,
      "OrigPosReqRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 714 "PosMaintRptRefID"
    Field{
      fvar,
      714,
      "PosMaintRptRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 715 "ClearingBusinessDate"
    Field{
      fvar,
      715,
      "ClearingBusinessDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 716 "SettlSessID"
    Field{
      fvar,
      716,
      "SettlSessID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>{{
        {.value = "ITD", .descr = "Intraday"              , .atom = 0}, // 0
        {.value = "RTH", .descr = "RegularTradingHours"   , .atom = 0}, // 1
        {.value = "ETH", .descr = "ElectronicTradingHours", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case CINT<'I','T','D'>: return f.value_atom(0); // Intraday
          case CINT<'R','T','H'>: return f.value_atom(1); // RegularTradingHours
          case CINT<'E','T','H'>: return f.value_atom(2); // ElectronicTradingHours
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 717 "SettlSessSubID"
    Field{
      fvar,
      717,
      "SettlSessSubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 718 "AdjustmentType"
    Field{
      fvar,
      718,
      "AdjustmentType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "ProcessRequestAsMarginDisposition", .atom = 0}, // 0
        {.value = "1", .descr = "DeltaPlus"                        , .atom = 0}, // 1
        {.value = "2", .descr = "DeltaMinus"                       , .atom = 0}, // 2
        {.value = "3", .descr = "Final"                            , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // PROCESS_REQUEST_AS_MARGIN_DISPOSITION
          case '1': return f.value_atom(1); // DELTA_PLUS
          case '2': return f.value_atom(2); // DELTA_MINUS
          case '3': return f.value_atom(3); // FINAL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 719 "ContraryInstructionIndicator"
    Field{
      fvar,
      719,
      "ContraryInstructionIndicator",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 720 "PriorSpreadIndicator"
    Field{
      fvar,
      720,
      "PriorSpreadIndicator",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 721 "PosMaintRptID"
    Field{
      fvar,
      721,
      "PosMaintRptID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 722 "PosMaintStatus"
    Field{
      fvar,
      722,
      "PosMaintStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Accepted"             , .atom = 0}, // 0
        {.value = "1", .descr = "AcceptedWithWarnings" , .atom = 0}, // 1
        {.value = "2", .descr = "Rejected"             , .atom = 0}, // 2
        {.value = "3", .descr = "Completed"            , .atom = 0}, // 3
        {.value = "4", .descr = "CompletedWithWarnings", .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // ACCEPTED
          case '1': return f.value_atom(1); // ACCEPTED_WITH_WARNINGS
          case '2': return f.value_atom(2); // REJECTED
          case '3': return f.value_atom(3); // COMPLETED
          case '4': return f.value_atom(4); // COMPLETED_WITH_WARNINGS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 723 "PosMaintResult"
    Field{
      fvar,
      723,
      "PosMaintResult",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "SuccessfulCompletion", .atom = 0}, // 0
        {.value = "1" , .descr = "Rejected"            , .atom = 0}, // 1
        {.value = "99", .descr = "Other"               , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom(0); // SuccessfulCompletion
          case           '1': return f.value_atom(1); // Rejected
          case CINT<'9','9'>: return f.value_atom(2); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 724 "PosReqType"
    Field{
      fvar,
      724,
      "PosReqType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Positions"  , .atom = 0}, // 0
        {.value = "1", .descr = "Trades"     , .atom = 0}, // 1
        {.value = "2", .descr = "Exercises"  , .atom = 0}, // 2
        {.value = "3", .descr = "Assignments", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // POSITIONS
          case '1': return f.value_atom(1); // TRADES
          case '2': return f.value_atom(2); // EXERCISES
          case '3': return f.value_atom(3); // ASSIGNMENTS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 725 "ResponseTransportType"
    Field{
      fvar,
      725,
      "ResponseTransportType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "InbandTransportTheRequestWasSentOver"          , .atom = 0}, // 0
        {.value = "1", .descr = "OutOfBandPreArrangedOutOfBandDeliveryMechanism", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // INBAND_TRANSPORT_THE_REQUEST_WAS_SENT_OVER
          case '1': return f.value_atom(1); // OUT_OF_BAND_PRE_ARRANGED_OUT_OF_BAND_DELIVERY_MECHANISM
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 726 "ResponseDestination"
    Field{
      fvar,
      726,
      "ResponseDestination",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 727 "TotalNumPosReports"
    Field{
      fvar,
      727,
      "TotalNumPosReports",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 728 "PosReqResult"
    Field{
      fvar,
      728,
      "PosReqResult",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "ValidRequest"                     , .atom = 0}, // 0
        {.value = "1" , .descr = "InvalidOrUnsupportedRequest"      , .atom = 0}, // 1
        {.value = "2" , .descr = "NoPositionsFoundThatMatchCriteria", .atom = 0}, // 2
        {.value = "3" , .descr = "NotAuthorizedToRequestPositions"  , .atom = 0}, // 3
        {.value = "4" , .descr = "RequestForPositionNotSupported"   , .atom = 0}, // 4
        {.value = "99", .descr = "Other"                            , .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom(0); // ValidRequest
          case           '1': return f.value_atom(1); // InvalidOrUnsupportedRequest
          case           '2': return f.value_atom(2); // NoPositionsFoundThatMatchCriteria
          case           '3': return f.value_atom(3); // NotAuthorizedToRequestPositions
          case           '4': return f.value_atom(4); // RequestForPositionNotSupported
          case CINT<'9','9'>: return f.value_atom(5); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 729 "PosReqStatus"
    Field{
      fvar,
      729,
      "PosReqStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Completed"            , .atom = 0}, // 0
        {.value = "1", .descr = "CompletedWithWarnings", .atom = 0}, // 1
        {.value = "2", .descr = "Rejected"             , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // COMPLETED
          case '1': return f.value_atom(1); // COMPLETED_WITH_WARNINGS
          case '2': return f.value_atom(2); // REJECTED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 730 "SettlPrice"
    Field{
      fvar,
      730,
      "SettlPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 731 "SettlPriceType"
    Field{
      fvar,
      731,
      "SettlPriceType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Final"      , .atom = 0}, // 0
        {.value = "2", .descr = "Theoretical", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // FINAL
          case '2': return f.value_atom(1); // THEORETICAL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 732 "UnderlyingSettlPrice"
    Field{
      fvar,
      732,
      "UnderlyingSettlPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 733 "UnderlyingSettlPriceType"
    Field{
      fvar,
      733,
      "UnderlyingSettlPriceType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 734 "PriorSettlPrice"
    Field{
      fvar,
      734,
      "PriorSettlPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 735 "NoQuoteQualifiers"
    Field{
      fvar,
      735,
      "NoQuoteQualifiers",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoQuoteQualifiers",
      735,
      nullptr
    },
    //--- Tag# 736 "AllocSettlCurrency"
    Field{
      fvar,
      736,
      "AllocSettlCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 737 "AllocSettlCurrAmt"
    Field{
      fvar,
      737,
      "AllocSettlCurrAmt",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 738 "InterestAtMaturity"
    Field{
      fvar,
      738,
      "InterestAtMaturity",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 739 "LegDatedDate"
    Field{
      fvar,
      739,
      "LegDatedDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 740 "LegPool"
    Field{
      fvar,
      740,
      "LegPool",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 741 "AllocInterestAtMaturity"
    Field{
      fvar,
      741,
      "AllocInterestAtMaturity",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 742 "AllocAccruedInterestAmt"
    Field{
      fvar,
      742,
      "AllocAccruedInterestAmt",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 743 "DeliveryDate"
    Field{
      fvar,
      743,
      "DeliveryDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 744 "AssignmentMethod"
    Field{
      fvar,
      744,
      "AssignmentMethod",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "R", .descr = "Random" , .atom = 0}, // 0
        {.value = "P", .descr = "Prorata", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'R': return f.value_atom(0); // RANDOM
          case 'P': return f.value_atom(1); // PRORATA
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 745 "AssignmentUnit"
    Field{
      fvar,
      745,
      "AssignmentUnit",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 746 "OpenInterest"
    Field{
      fvar,
      746,
      "OpenInterest",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 747 "ExerciseMethod"
    Field{
      fvar,
      747,
      "ExerciseMethod",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "A", .descr = "Automatic", .atom = 0}, // 0
        {.value = "M", .descr = "Manual"   , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'A': return f.value_atom(0); // AUTOMATIC
          case 'M': return f.value_atom(1); // MANUAL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 748 "TotNumTradeReports"
    Field{
      fvar,
      748,
      "TotNumTradeReports",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 749 "TradeRequestResult"
    Field{
      fvar,
      749,
      "TradeRequestResult",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "Successful"                              , .atom = 0}, // 0
        {.value = "1" , .descr = "InvalidOrUnknownInstrument"              , .atom = 0}, // 1
        {.value = "2" , .descr = "InvalidTypeOfTradeRequested"             , .atom = 0}, // 2
        {.value = "3" , .descr = "InvalidParties"                          , .atom = 0}, // 3
        {.value = "4" , .descr = "InvalidTransportTypeRequested"           , .atom = 0}, // 4
        {.value = "5" , .descr = "InvalidDestinationRequested"             , .atom = 0}, // 5
        {.value = "8" , .descr = "TraderequesttypeNotSupported"            , .atom = 0}, // 6
        {.value = "9" , .descr = "UnauthorizedForTradeCaptureReportRequest", .atom = 0}, // 7
        {.value = "99", .descr = "Other"                                   , .atom = 0}, // 8
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom(0); // Successful
          case           '1': return f.value_atom(1); // InvalidOrUnknownInstrument
          case           '2': return f.value_atom(2); // InvalidTypeOfTradeRequested
          case           '3': return f.value_atom(3); // InvalidParties
          case           '4': return f.value_atom(4); // InvalidTransportTypeRequested
          case           '5': return f.value_atom(5); // InvalidDestinationRequested
          case           '8': return f.value_atom(6); // TraderequesttypeNotSupported
          case           '9': return f.value_atom(7); // UnauthorizedForTradeCaptureReportRequest
          case CINT<'9','9'>: return f.value_atom(8); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 750 "TradeRequestStatus"
    Field{
      fvar,
      750,
      "TradeRequestStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Accepted" , .atom = 0}, // 0
        {.value = "1", .descr = "Completed", .atom = 0}, // 1
        {.value = "2", .descr = "Rejected" , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // ACCEPTED
          case '1': return f.value_atom(1); // COMPLETED
          case '2': return f.value_atom(2); // REJECTED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 751 "TradeReportRejectReason"
    Field{
      fvar,
      751,
      "TradeReportRejectReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "Successful"                , .atom = 0}, // 0
        {.value = "1" , .descr = "InvalidPartyInformation"   , .atom = 0}, // 1
        {.value = "2" , .descr = "UnknownInstrument"         , .atom = 0}, // 2
        {.value = "3" , .descr = "UnauthorizedToReportTrades", .atom = 0}, // 3
        {.value = "4" , .descr = "InvalidTradeType"          , .atom = 0}, // 4
        {.value = "99", .descr = "Other"                     , .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom(0); // Successful
          case           '1': return f.value_atom(1); // InvalidPartyInformation
          case           '2': return f.value_atom(2); // UnknownInstrument
          case           '3': return f.value_atom(3); // UnauthorizedToReportTrades
          case           '4': return f.value_atom(4); // InvalidTradeType
          case CINT<'9','9'>: return f.value_atom(5); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 752 "SideMultiLegReportingType"
    Field{
      fvar,
      752,
      "SideMultiLegReportingType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "SingleSecurity"                  , .atom = 0}, // 0
        {.value = "2", .descr = "IndividualLegOfAMultiLegSecurity", .atom = 0}, // 1
        {.value = "3", .descr = "MultiLegSecurity"                , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // SINGLE_SECURITY
          case '2': return f.value_atom(1); // INDIVIDUAL_LEG_OF_A_MULTI_LEG_SECURITY
          case '3': return f.value_atom(2); // MULTI_LEG_SECURITY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 753 "NoPosAmt"
    Field{
      fvar,
      753,
      "NoPosAmt",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoPosAmt",
      753,
      nullptr
    },
    //--- Tag# 754 "AutoAcceptIndicator"
    Field{
      fvar,
      754,
      "AutoAcceptIndicator",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 755 "AllocReportID"
    Field{
      fvar,
      755,
      "AllocReportID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 756 "NoNested2PartyIDs"
    Field{
      fvar,
      756,
      "NoNested2PartyIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoNested2PartyIDs",
      756,
      nullptr
    },
    //--- Tag# 757 "Nested2PartyID"
    Field{
      fvar,
      757,
      "Nested2PartyID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 758 "Nested2PartyIDSource"
    Field{
      fvar,
      758,
      "Nested2PartyIDSource",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 759 "Nested2PartyRole"
    Field{
      fvar,
      759,
      "Nested2PartyRole",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 760 "Nested2PartySubID"
    Field{
      fvar,
      760,
      "Nested2PartySubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 761 "BenchmarkSecurityIDSource"
    Field{
      fvar,
      761,
      "BenchmarkSecurityIDSource",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 762 "SecuritySubType"
    Field{
      fvar,
      762,
      "SecuritySubType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 763 "UnderlyingSecuritySubType"
    Field{
      fvar,
      763,
      "UnderlyingSecuritySubType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 764 "LegSecuritySubType"
    Field{
      fvar,
      764,
      "LegSecuritySubType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 765 "AllowableOneSidednessPct"
    Field{
      fvar,
      765,
      "AllowableOneSidednessPct",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 766 "AllowableOneSidednessValue"
    Field{
      fvar,
      766,
      "AllowableOneSidednessValue",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 767 "AllowableOneSidednessCurr"
    Field{
      fvar,
      767,
      "AllowableOneSidednessCurr",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 768 "NoTrdRegTimestamps"
    Field{
      fvar,
      768,
      "NoTrdRegTimestamps",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoTrdRegTimestamps",
      768,
      nullptr
    },
    //--- Tag# 769 "TrdRegTimestamp"
    Field{
      fvar,
      769,
      "TrdRegTimestamp",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 770 "TrdRegTimestampType"
    Field{
      fvar,
      770,
      "TrdRegTimestampType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "ExecutionTime"  , .atom = 0}, // 0
        {.value = "2", .descr = "TimeIn"         , .atom = 0}, // 1
        {.value = "3", .descr = "TimeOut"        , .atom = 0}, // 2
        {.value = "4", .descr = "BrokerReceipt"  , .atom = 0}, // 3
        {.value = "5", .descr = "BrokerExecution", .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // EXECUTION_TIME
          case '2': return f.value_atom(1); // TIME_IN
          case '3': return f.value_atom(2); // TIME_OUT
          case '4': return f.value_atom(3); // BROKER_RECEIPT
          case '5': return f.value_atom(4); // BROKER_EXECUTION
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 771 "TrdRegTimestampOrigin"
    Field{
      fvar,
      771,
      "TrdRegTimestampOrigin",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 772 "ConfirmRefID"
    Field{
      fvar,
      772,
      "ConfirmRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 773 "ConfirmType"
    Field{
      fvar,
      773,
      "ConfirmType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Status"                     , .atom = 0}, // 0
        {.value = "2", .descr = "Confirmation"               , .atom = 0}, // 1
        {.value = "3", .descr = "ConfirmationRequestRejected", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // STATUS
          case '2': return f.value_atom(1); // CONFIRMATION
          case '3': return f.value_atom(2); // CONFIRMATION_REQUEST_REJECTED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 774 "ConfirmRejReason"
    Field{
      fvar,
      774,
      "ConfirmRejReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "MismatchedAccount"            , .atom = 0}, // 0
        {.value = "2" , .descr = "MissingSettlementInstructions", .atom = 0}, // 1
        {.value = "99", .descr = "Other"                        , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom(0); // MismatchedAccount
          case           '2': return f.value_atom(1); // MissingSettlementInstructions
          case CINT<'9','9'>: return f.value_atom(2); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 775 "BookingType"
    Field{
      fvar,
      775,
      "BookingType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "RegularBooking" , .atom = 0}, // 0
        {.value = "1", .descr = "Cfd"            , .atom = 0}, // 1
        {.value = "2", .descr = "TotalReturnSwap", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // REGULAR_BOOKING
          case '1': return f.value_atom(1); // CFD
          case '2': return f.value_atom(2); // TOTAL_RETURN_SWAP
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 776 "IndividualAllocRejCode"
    Field{
      fvar,
      776,
      "IndividualAllocRejCode",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 777 "SettlInstMsgID"
    Field{
      fvar,
      777,
      "SettlInstMsgID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 778 "NoSettlInst"
    Field{
      fvar,
      778,
      "NoSettlInst",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoSettlInst",
      778,
      nullptr
    },
    //--- Tag# 779 "LastUpdateTime"
    Field{
      fvar,
      779,
      "LastUpdateTime",
      FieldType::UTCTIMESTAMP,
      DataType::DATETIME,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 780 "AllocSettlInstType"
    Field{
      fvar,
      780,
      "AllocSettlInstType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "UseDefaultInstructions"      , .atom = 0}, // 0
        {.value = "1", .descr = "DeriveFromParametersProvided", .atom = 0}, // 1
        {.value = "2", .descr = "FullDetailsProvided"         , .atom = 0}, // 2
        {.value = "3", .descr = "SsiDbIdsProvided"            , .atom = 0}, // 3
        {.value = "4", .descr = "PhoneForInstructions"        , .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // USE_DEFAULT_INSTRUCTIONS
          case '1': return f.value_atom(1); // DERIVE_FROM_PARAMETERS_PROVIDED
          case '2': return f.value_atom(2); // FULL_DETAILS_PROVIDED
          case '3': return f.value_atom(3); // SSI_DB_IDS_PROVIDED
          case '4': return f.value_atom(4); // PHONE_FOR_INSTRUCTIONS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 781 "NoSettlPartyIDs"
    Field{
      fvar,
      781,
      "NoSettlPartyIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoSettlPartyIDs",
      781,
      nullptr
    },
    //--- Tag# 782 "SettlPartyID"
    Field{
      fvar,
      782,
      "SettlPartyID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 783 "SettlPartyIDSource"
    Field{
      fvar,
      783,
      "SettlPartyIDSource",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 784 "SettlPartyRole"
    Field{
      fvar,
      784,
      "SettlPartyRole",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 785 "SettlPartySubID"
    Field{
      fvar,
      785,
      "SettlPartySubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 786 "SettlPartySubIDType"
    Field{
      fvar,
      786,
      "SettlPartySubIDType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 787 "DlvyInstType"
    Field{
      fvar,
      787,
      "DlvyInstType",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>{{
        {.value = "S", .descr = "Securities", .atom = 0}, // 0
        {.value = "C", .descr = "Cash"      , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'S': return f.value_atom(0); // SECURITIES
          case 'C': return f.value_atom(1); // CASH
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 788 "TerminationType"
    Field{
      fvar,
      788,
      "TerminationType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Overnight", .atom = 0}, // 0
        {.value = "2", .descr = "Term"     , .atom = 0}, // 1
        {.value = "3", .descr = "Flexible" , .atom = 0}, // 2
        {.value = "4", .descr = "Open"     , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // OVERNIGHT
          case '2': return f.value_atom(1); // TERM
          case '3': return f.value_atom(2); // FLEXIBLE
          case '4': return f.value_atom(3); // OPEN
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 789 "NextExpectedMsgSeqNum"
    Field{
      fvar,
      789,
      "NextExpectedMsgSeqNum",
      FieldType::SEQNUM,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 790 "OrdStatusReqID"
    Field{
      fvar,
      790,
      "OrdStatusReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 791 "SettlInstReqID"
    Field{
      fvar,
      791,
      "SettlInstReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 792 "SettlInstReqRejCode"
    Field{
      fvar,
      792,
      "SettlInstReqRejCode",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "UnableToProcessRequest"               , .atom = 0}, // 0
        {.value = "1" , .descr = "UnknownAccount"                       , .atom = 0}, // 1
        {.value = "2" , .descr = "NoMatchingSettlementInstructionsFound", .atom = 0}, // 2
        {.value = "99", .descr = "Other"                                , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom(0); // UnableToProcessRequest
          case           '1': return f.value_atom(1); // UnknownAccount
          case           '2': return f.value_atom(2); // NoMatchingSettlementInstructionsFound
          case CINT<'9','9'>: return f.value_atom(3); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 793 "SecondaryAllocID"
    Field{
      fvar,
      793,
      "SecondaryAllocID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 794 "AllocReportType"
    Field{
      fvar,
      794,
      "AllocReportType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "3", .descr = "SellsideCalculatedUsingPreliminary"  , .atom = 0}, // 0
        {.value = "4", .descr = "SellsideCalculatedWithoutPreliminary", .atom = 0}, // 1
        {.value = "5", .descr = "WarehouseRecap"                      , .atom = 0}, // 2
        {.value = "8", .descr = "RequestToIntermediary"               , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '3': return f.value_atom(0); // SELLSIDE_CALCULATED_USING_PRELIMINARY
          case '4': return f.value_atom(1); // SELLSIDE_CALCULATED_WITHOUT_PRELIMINARY
          case '5': return f.value_atom(2); // WAREHOUSE_RECAP
          case '8': return f.value_atom(3); // REQUEST_TO_INTERMEDIARY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 795 "AllocReportRefID"
    Field{
      fvar,
      795,
      "AllocReportRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 796 "AllocCancReplaceReason"
    Field{
      fvar,
      796,
      "AllocCancReplaceReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "OriginalDetailsIncompleteIncorrect", .atom = 0}, // 0
        {.value = "2" , .descr = "ChangeInUnderlyingOrderDetails"    , .atom = 0}, // 1
        {.value = "99", .descr = "Other"                             , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom(0); // OriginalDetailsIncompleteIncorrect
          case           '2': return f.value_atom(1); // ChangeInUnderlyingOrderDetails
          case CINT<'9','9'>: return f.value_atom(2); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 797 "CopyMsgIndicator"
    Field{
      fvar,
      797,
      "CopyMsgIndicator",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 798 "AllocAccountType"
    Field{
      fvar,
      798,
      "AllocAccountType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "AccountIsCarriedOnCustomerSideOfBooks"                     , .atom = 0}, // 0
        {.value = "2", .descr = "AccountIsCarriedOnNonCustomerSideOfBooks"                  , .atom = 0}, // 1
        {.value = "3", .descr = "HouseTrader"                                               , .atom = 0}, // 2
        {.value = "4", .descr = "FloorTrader"                                               , .atom = 0}, // 3
        {.value = "6", .descr = "AccountIsCarriedOnNonCustomerSideOfBooksAndIsCrossMargined", .atom = 0}, // 4
        {.value = "7", .descr = "AccountIsHouseTraderAndIsCrossMargined"                    , .atom = 0}, // 5
        {.value = "8", .descr = "JointBackofficeAccount"                                    , .atom = 0}, // 6
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // ACCOUNT_IS_CARRIED_ON_CUSTOMER_SIDE_OF_BOOKS
          case '2': return f.value_atom(1); // ACCOUNT_IS_CARRIED_ON_NON_CUSTOMER_SIDE_OF_BOOKS
          case '3': return f.value_atom(2); // HOUSE_TRADER
          case '4': return f.value_atom(3); // FLOOR_TRADER
          case '6': return f.value_atom(4); // ACCOUNT_IS_CARRIED_ON_NON_CUSTOMER_SIDE_OF_BOOKS_AND_IS_CROSS_MARGINED
          case '7': return f.value_atom(5); // ACCOUNT_IS_HOUSE_TRADER_AND_IS_CROSS_MARGINED
          case '8': return f.value_atom(6); // JOINT_BACKOFFICE_ACCOUNT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 799 "OrderAvgPx"
    Field{
      fvar,
      799,
      "OrderAvgPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 800 "OrderBookingQty"
    Field{
      fvar,
      800,
      "OrderBookingQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 801 "NoSettlPartySubIDs"
    Field{
      fvar,
      801,
      "NoSettlPartySubIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoSettlPartySubIDs",
      801,
      nullptr
    },
    //--- Tag# 802 "NoPartySubIDs"
    Field{
      fvar,
      802,
      "NoPartySubIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoPartySubIDs",
      802,
      nullptr
    },
    //--- Tag# 803 "PartySubIDType"
    Field{
      fvar,
      803,
      "PartySubIDType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "Firm"                    , .atom = 0}, // 0
        {.value = "2" , .descr = "Person"                  , .atom = 0}, // 1
        {.value = "3" , .descr = "System"                  , .atom = 0}, // 2
        {.value = "4" , .descr = "Application"             , .atom = 0}, // 3
        {.value = "5" , .descr = "FullLegalNameOfFirm"     , .atom = 0}, // 4
        {.value = "6" , .descr = "PostalAddress"           , .atom = 0}, // 5
        {.value = "7" , .descr = "PhoneNumber"             , .atom = 0}, // 6
        {.value = "8" , .descr = "EmailAddress"            , .atom = 0}, // 7
        {.value = "9" , .descr = "ContactName"             , .atom = 0}, // 8
        {.value = "10", .descr = "SecuritiesAccountNumber" , .atom = 0}, // 9
        {.value = "11", .descr = "RegistrationNumber"      , .atom = 0}, // 10
        {.value = "12", .descr = "RegisteredAddress12"     , .atom = 0}, // 11
        {.value = "13", .descr = "RegulatoryStatus"        , .atom = 0}, // 12
        {.value = "14", .descr = "RegistrationName"        , .atom = 0}, // 13
        {.value = "15", .descr = "CashAccountNumber"       , .atom = 0}, // 14
        {.value = "16", .descr = "Bic"                     , .atom = 0}, // 15
        {.value = "17", .descr = "CsdParticipantMemberCode", .atom = 0}, // 16
        {.value = "18", .descr = "RegisteredAddress18"     , .atom = 0}, // 17
        {.value = "19", .descr = "FundAccountName"         , .atom = 0}, // 18
        {.value = "20", .descr = "TelexNumber"             , .atom = 0}, // 19
        {.value = "21", .descr = "FaxNumber"               , .atom = 0}, // 20
        {.value = "22", .descr = "SecuritiesAccountName"   , .atom = 0}, // 21
        {.value = "23", .descr = "CashAccountName"         , .atom = 0}, // 22
        {.value = "24", .descr = "Department"              , .atom = 0}, // 23
        {.value = "25", .descr = "Location"                , .atom = 0}, // 24
        {.value = "26", .descr = "PositionAccountType"     , .atom = 0}, // 25
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // Firm
          case           '2': return f.value_atom( 1); // Person
          case           '3': return f.value_atom( 2); // System
          case           '4': return f.value_atom( 3); // Application
          case           '5': return f.value_atom( 4); // FullLegalNameOfFirm
          case           '6': return f.value_atom( 5); // PostalAddress
          case           '7': return f.value_atom( 6); // PhoneNumber
          case           '8': return f.value_atom( 7); // EmailAddress
          case           '9': return f.value_atom( 8); // ContactName
          case CINT<'1','0'>: return f.value_atom( 9); // SecuritiesAccountNumber
          case CINT<'1','1'>: return f.value_atom(10); // RegistrationNumber
          case CINT<'1','2'>: return f.value_atom(11); // RegisteredAddress12
          case CINT<'1','3'>: return f.value_atom(12); // RegulatoryStatus
          case CINT<'1','4'>: return f.value_atom(13); // RegistrationName
          case CINT<'1','5'>: return f.value_atom(14); // CashAccountNumber
          case CINT<'1','6'>: return f.value_atom(15); // Bic
          case CINT<'1','7'>: return f.value_atom(16); // CsdParticipantMemberCode
          case CINT<'1','8'>: return f.value_atom(17); // RegisteredAddress18
          case CINT<'1','9'>: return f.value_atom(18); // FundAccountName
          case CINT<'2','0'>: return f.value_atom(19); // TelexNumber
          case CINT<'2','1'>: return f.value_atom(20); // FaxNumber
          case CINT<'2','2'>: return f.value_atom(21); // SecuritiesAccountName
          case CINT<'2','3'>: return f.value_atom(22); // CashAccountName
          case CINT<'2','4'>: return f.value_atom(23); // Department
          case CINT<'2','5'>: return f.value_atom(24); // Location
          case CINT<'2','6'>: return f.value_atom(25); // PositionAccountType
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 804 "NoNestedPartySubIDs"
    Field{
      fvar,
      804,
      "NoNestedPartySubIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoNestedPartySubIDs",
      804,
      nullptr
    },
    //--- Tag# 805 "NestedPartySubIDType"
    Field{
      fvar,
      805,
      "NestedPartySubIDType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 806 "NoNested2PartySubIDs"
    Field{
      fvar,
      806,
      "NoNested2PartySubIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoNested2PartySubIDs",
      806,
      nullptr
    },
    //--- Tag# 807 "Nested2PartySubIDType"
    Field{
      fvar,
      807,
      "Nested2PartySubIDType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 808 "AllocIntermedReqType"
    Field{
      fvar,
      808,
      "AllocIntermedReqType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "PendingAccept"     , .atom = 0}, // 0
        {.value = "2", .descr = "PendingRelease"    , .atom = 0}, // 1
        {.value = "3", .descr = "PendingReversal"   , .atom = 0}, // 2
        {.value = "4", .descr = "Accept"            , .atom = 0}, // 3
        {.value = "5", .descr = "BlockLevelReject"  , .atom = 0}, // 4
        {.value = "6", .descr = "AccountLevelReject", .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // PENDING_ACCEPT
          case '2': return f.value_atom(1); // PENDING_RELEASE
          case '3': return f.value_atom(2); // PENDING_REVERSAL
          case '4': return f.value_atom(3); // ACCEPT
          case '5': return f.value_atom(4); // BLOCK_LEVEL_REJECT
          case '6': return f.value_atom(5); // ACCOUNT_LEVEL_REJECT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 809
    Field{},
    //--- Tag# 810 "UnderlyingPx"
    Field{
      fvar,
      810,
      "UnderlyingPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 811 "PriceDelta"
    Field{
      fvar,
      811,
      "PriceDelta",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 812 "ApplQueueMax"
    Field{
      fvar,
      812,
      "ApplQueueMax",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 813 "ApplQueueDepth"
    Field{
      fvar,
      813,
      "ApplQueueDepth",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 814 "ApplQueueResolution"
    Field{
      fvar,
      814,
      "ApplQueueResolution",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "NoActionTaken", .atom = 0}, // 0
        {.value = "1", .descr = "QueueFlushed" , .atom = 0}, // 1
        {.value = "2", .descr = "OverlayLast"  , .atom = 0}, // 2
        {.value = "3", .descr = "EndSession"   , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NO_ACTION_TAKEN
          case '1': return f.value_atom(1); // QUEUE_FLUSHED
          case '2': return f.value_atom(2); // OVERLAY_LAST
          case '3': return f.value_atom(3); // END_SESSION
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 815 "ApplQueueAction"
    Field{
      fvar,
      815,
      "ApplQueueAction",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "NoActionTaken", .atom = 0}, // 0
        {.value = "1", .descr = "QueueFlushed" , .atom = 0}, // 1
        {.value = "2", .descr = "OverlayLast"  , .atom = 0}, // 2
        {.value = "3", .descr = "EndSession"   , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NO_ACTION_TAKEN
          case '1': return f.value_atom(1); // QUEUE_FLUSHED
          case '2': return f.value_atom(2); // OVERLAY_LAST
          case '3': return f.value_atom(3); // END_SESSION
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 816 "NoAltMDSource"
    Field{
      fvar,
      816,
      "NoAltMDSource",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoAltMDSource",
      816,
      nullptr
    },
    //--- Tag# 817 "AltMDSourceID"
    Field{
      fvar,
      817,
      "AltMDSourceID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 818 "SecondaryTradeReportID"
    Field{
      fvar,
      818,
      "SecondaryTradeReportID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 819 "AvgPxIndicator"
    Field{
      fvar,
      819,
      "AvgPxIndicator",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "NoAveragePricing"                                          , .atom = 0}, // 0
        {.value = "1", .descr = "TradeIsPartOfAnAveragePriceGroupIdentifiedByTheTradelinkid", .atom = 0}, // 1
        {.value = "2", .descr = "LastTradeInTheAveragePriceGroupIdentifiedByTheTradelinkid" , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NO_AVERAGE_PRICING
          case '1': return f.value_atom(1); // TRADE_IS_PART_OF_AN_AVERAGE_PRICE_GROUP_IDENTIFIED_BY_THE_TRADELINKID
          case '2': return f.value_atom(2); // LAST_TRADE_IN_THE_AVERAGE_PRICE_GROUP_IDENTIFIED_BY_THE_TRADELINKID
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 820 "TradeLinkID"
    Field{
      fvar,
      820,
      "TradeLinkID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 821 "OrderInputDevice"
    Field{
      fvar,
      821,
      "OrderInputDevice",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 822 "UnderlyingTradingSessionID"
    Field{
      fvar,
      822,
      "UnderlyingTradingSessionID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 823 "UnderlyingTradingSessionSubID"
    Field{
      fvar,
      823,
      "UnderlyingTradingSessionSubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 824 "TradeLegRefID"
    Field{
      fvar,
      824,
      "TradeLegRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 825 "ExchangeRule"
    Field{
      fvar,
      825,
      "ExchangeRule",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 826 "TradeAllocIndicator"
    Field{
      fvar,
      826,
      "TradeAllocIndicator",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "AllocationNotRequired"            , .atom = 0}, // 0
        {.value = "1", .descr = "AllocationRequired"               , .atom = 0}, // 1
        {.value = "2", .descr = "UseAllocationProvidedWithTheTrade", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // ALLOCATION_NOT_REQUIRED
          case '1': return f.value_atom(1); // ALLOCATION_REQUIRED
          case '2': return f.value_atom(2); // USE_ALLOCATION_PROVIDED_WITH_THE_TRADE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 827 "ExpirationCycle"
    Field{
      fvar,
      827,
      "ExpirationCycle",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "ExpireOnTradingSessionClose", .atom = 0}, // 0
        {.value = "1", .descr = "ExpireOnTradingSessionOpen" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // EXPIRE_ON_TRADING_SESSION_CLOSE
          case '1': return f.value_atom(1); // EXPIRE_ON_TRADING_SESSION_OPEN
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 828 "TrdType"
    Field{
      fvar,
      828,
      "TrdType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "RegularTrade"             , .atom = 0}, // 0
        {.value = "1" , .descr = "BlockTrade"               , .atom = 0}, // 1
        {.value = "2" , .descr = "Efp"                      , .atom = 0}, // 2
        {.value = "3" , .descr = "Transfer"                 , .atom = 0}, // 3
        {.value = "4" , .descr = "LateTrade"                , .atom = 0}, // 4
        {.value = "5" , .descr = "TTrade"                   , .atom = 0}, // 5
        {.value = "6" , .descr = "WeightedAveragePriceTrade", .atom = 0}, // 6
        {.value = "7" , .descr = "BunchedTrade"             , .atom = 0}, // 7
        {.value = "8" , .descr = "LateBunchedTrade"         , .atom = 0}, // 8
        {.value = "9" , .descr = "PriorReferencePriceTrade" , .atom = 0}, // 9
        {.value = "10", .descr = "AfterHoursTrade"          , .atom = 0}, // 10
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom( 0); // RegularTrade
          case           '1': return f.value_atom( 1); // BlockTrade
          case           '2': return f.value_atom( 2); // Efp
          case           '3': return f.value_atom( 3); // Transfer
          case           '4': return f.value_atom( 4); // LateTrade
          case           '5': return f.value_atom( 5); // TTrade
          case           '6': return f.value_atom( 6); // WeightedAveragePriceTrade
          case           '7': return f.value_atom( 7); // BunchedTrade
          case           '8': return f.value_atom( 8); // LateBunchedTrade
          case           '9': return f.value_atom( 9); // PriorReferencePriceTrade
          case CINT<'1','0'>: return f.value_atom(10); // AfterHoursTrade
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 829 "TrdSubType"
    Field{
      fvar,
      829,
      "TrdSubType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 830 "TransferReason"
    Field{
      fvar,
      830,
      "TransferReason",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 831
    Field{},
    //--- Tag# 832 "TotNumAssignmentReports"
    Field{
      fvar,
      832,
      "TotNumAssignmentReports",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 833 "AsgnRptID"
    Field{
      fvar,
      833,
      "AsgnRptID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 834 "ThresholdAmount"
    Field{
      fvar,
      834,
      "ThresholdAmount",
      FieldType::PRICEOFFSET,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 835 "PegMoveType"
    Field{
      fvar,
      835,
      "PegMoveType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Floating", .atom = 0}, // 0
        {.value = "1", .descr = "Fixed"   , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // FLOATING
          case '1': return f.value_atom(1); // FIXED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 836 "PegOffsetType"
    Field{
      fvar,
      836,
      "PegOffsetType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Price"      , .atom = 0}, // 0
        {.value = "1", .descr = "BasisPoints", .atom = 0}, // 1
        {.value = "2", .descr = "Ticks"      , .atom = 0}, // 2
        {.value = "3", .descr = "PriceTier"  , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // PRICE
          case '1': return f.value_atom(1); // BASIS_POINTS
          case '2': return f.value_atom(2); // TICKS
          case '3': return f.value_atom(3); // PRICE_TIER
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 837 "PegLimitType"
    Field{
      fvar,
      837,
      "PegLimitType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "OrBetter"                                                           , .atom = 0}, // 0
        {.value = "1", .descr = "StrictLimitIsAStrictLimit"                                          , .atom = 0}, // 1
        {.value = "2", .descr = "OrWorseForABuyThePegLimitIsAMinimumAndForASellThePegLimitIsAMaximum", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // OR_BETTER
          case '1': return f.value_atom(1); // STRICT_LIMIT_IS_A_STRICT_LIMIT
          case '2': return f.value_atom(2); // OR_WORSE_FOR_A_BUY_THE_PEG_LIMIT_IS_A_MINIMUM_AND_FOR_A_SELL_THE_PEG_LIMIT_IS_A_MAXIMUM
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 838 "PegRoundDirection"
    Field{
      fvar,
      838,
      "PegRoundDirection",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "MoreAggressiveOnABuyOrderRoundThePriceUpRoundUpToTheNearestTickOnASellRoundDownToTheNearestTick", .atom = 0}, // 0
        {.value = "2", .descr = "MorePassiveOnABuyOrderRoundDownToNearestTickOnASellOrderRoundUpToNearestTick"                   , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // MORE_AGGRESSIVE_ON_A_BUY_ORDER_ROUND_THE_PRICE_UP_ROUND_UP_TO_THE_NEAREST_TICK_ON_A_SELL_ROUND_DOWN_TO_THE_NEAREST_TICK
          case '2': return f.value_atom(1); // MORE_PASSIVE_ON_A_BUY_ORDER_ROUND_DOWN_TO_NEAREST_TICK_ON_A_SELL_ORDER_ROUND_UP_TO_NEAREST_TICK
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 839 "PeggedPrice"
    Field{
      fvar,
      839,
      "PeggedPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 840 "PegScope"
    Field{
      fvar,
      840,
      "PegScope",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Local"                 , .atom = 0}, // 0
        {.value = "2", .descr = "National"              , .atom = 0}, // 1
        {.value = "3", .descr = "Global"                , .atom = 0}, // 2
        {.value = "4", .descr = "NationalExcludingLocal", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // LOCAL
          case '2': return f.value_atom(1); // NATIONAL
          case '3': return f.value_atom(2); // GLOBAL
          case '4': return f.value_atom(3); // NATIONAL_EXCLUDING_LOCAL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 841 "DiscretionMoveType"
    Field{
      fvar,
      841,
      "DiscretionMoveType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Floating", .atom = 0}, // 0
        {.value = "1", .descr = "Fixed"   , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // FLOATING
          case '1': return f.value_atom(1); // FIXED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 842 "DiscretionOffsetType"
    Field{
      fvar,
      842,
      "DiscretionOffsetType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Price"      , .atom = 0}, // 0
        {.value = "1", .descr = "BasisPoints", .atom = 0}, // 1
        {.value = "2", .descr = "Ticks"      , .atom = 0}, // 2
        {.value = "3", .descr = "PriceTier"  , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // PRICE
          case '1': return f.value_atom(1); // BASIS_POINTS
          case '2': return f.value_atom(2); // TICKS
          case '3': return f.value_atom(3); // PRICE_TIER
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 843 "DiscretionLimitType"
    Field{
      fvar,
      843,
      "DiscretionLimitType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "OrBetter"                                                                         , .atom = 0}, // 0
        {.value = "1", .descr = "StrictLimitIsAStrictLimit"                                                        , .atom = 0}, // 1
        {.value = "2", .descr = "OrWorseForABuyTheDiscretionPriceIsAMinimumAndForASellTheDiscretionPriceIsAMaximum", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // OR_BETTER
          case '1': return f.value_atom(1); // STRICT_LIMIT_IS_A_STRICT_LIMIT
          case '2': return f.value_atom(2); // OR_WORSE_FOR_A_BUY_THE_DISCRETION_PRICE_IS_A_MINIMUM_AND_FOR_A_SELL_THE_DISCRETION_PRICE_IS_A_MAXIMUM
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 844 "DiscretionRoundDirection"
    Field{
      fvar,
      844,
      "DiscretionRoundDirection",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "MoreAggressiveOnABuyOrderRoundThePriceUpRoundUpToTheNearestTickOnASellRoundDownToTheNearestTick", .atom = 0}, // 0
        {.value = "2", .descr = "MorePassiveOnABuyOrderRoundDownToNearestTickOnASellOrderRoundUpToNearestTick"                   , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // MORE_AGGRESSIVE_ON_A_BUY_ORDER_ROUND_THE_PRICE_UP_ROUND_UP_TO_THE_NEAREST_TICK_ON_A_SELL_ROUND_DOWN_TO_THE_NEAREST_TICK
          case '2': return f.value_atom(1); // MORE_PASSIVE_ON_A_BUY_ORDER_ROUND_DOWN_TO_NEAREST_TICK_ON_A_SELL_ORDER_ROUND_UP_TO_NEAREST_TICK
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 845 "DiscretionPrice"
    Field{
      fvar,
      845,
      "DiscretionPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 846 "DiscretionScope"
    Field{
      fvar,
      846,
      "DiscretionScope",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Local"                 , .atom = 0}, // 0
        {.value = "2", .descr = "National"              , .atom = 0}, // 1
        {.value = "3", .descr = "Global"                , .atom = 0}, // 2
        {.value = "4", .descr = "NationalExcludingLocal", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // LOCAL
          case '2': return f.value_atom(1); // NATIONAL
          case '3': return f.value_atom(2); // GLOBAL
          case '4': return f.value_atom(3); // NATIONAL_EXCLUDING_LOCAL
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 847 "TargetStrategy"
    Field{
      fvar,
      847,
      "TargetStrategy",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Vwap"                , .atom = 0}, // 0
        {.value = "2", .descr = "Participate"         , .atom = 0}, // 1
        {.value = "3", .descr = "MininizeMarketImpact", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // VWAP
          case '2': return f.value_atom(1); // PARTICIPATE
          case '3': return f.value_atom(2); // MININIZE_MARKET_IMPACT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 848 "TargetStrategyParameters"
    Field{
      fvar,
      848,
      "TargetStrategyParameters",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 849 "ParticipationRate"
    Field{
      fvar,
      849,
      "ParticipationRate",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 850 "TargetStrategyPerformance"
    Field{
      fvar,
      850,
      "TargetStrategyPerformance",
      FieldType::FLOAT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 851 "LastLiquidityInd"
    Field{
      fvar,
      851,
      "LastLiquidityInd",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "AddedLiquidity"    , .atom = 0}, // 0
        {.value = "2", .descr = "RemovedLiquidity"  , .atom = 0}, // 1
        {.value = "3", .descr = "LiquidityRoutedOut", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // ADDED_LIQUIDITY
          case '2': return f.value_atom(1); // REMOVED_LIQUIDITY
          case '3': return f.value_atom(2); // LIQUIDITY_ROUTED_OUT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 852 "PublishTrdIndicator"
    Field{
      fvar,
      852,
      "PublishTrdIndicator",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 853 "ShortSaleReason"
    Field{
      fvar,
      853,
      "ShortSaleReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "DealerSoldShort"                  , .atom = 0}, // 0
        {.value = "1", .descr = "DealerSoldShortExempt"            , .atom = 0}, // 1
        {.value = "2", .descr = "SellingCustomerSoldShort"         , .atom = 0}, // 2
        {.value = "3", .descr = "SellingCustomerSoldShortExempt"   , .atom = 0}, // 3
        {.value = "4", .descr = "QualifedServiceRepresentative"    , .atom = 0}, // 4
        {.value = "5", .descr = "QsrOrAguContraSideSoldShortExempt", .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // DEALER_SOLD_SHORT
          case '1': return f.value_atom(1); // DEALER_SOLD_SHORT_EXEMPT
          case '2': return f.value_atom(2); // SELLING_CUSTOMER_SOLD_SHORT
          case '3': return f.value_atom(3); // SELLING_CUSTOMER_SOLD_SHORT_EXEMPT
          case '4': return f.value_atom(4); // QUALIFED_SERVICE_REPRESENTATIVE
          case '5': return f.value_atom(5); // QSR_OR_AGU_CONTRA_SIDE_SOLD_SHORT_EXEMPT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 854 "QtyType"
    Field{
      fvar,
      854,
      "QtyType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Units"    , .atom = 0}, // 0
        {.value = "1", .descr = "Contracts", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // UNITS
          case '1': return f.value_atom(1); // CONTRACTS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 855 "SecondaryTrdType"
    Field{
      fvar,
      855,
      "SecondaryTrdType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 856 "TradeReportType"
    Field{
      fvar,
      856,
      "TradeReportType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Submit"            , .atom = 0}, // 0
        {.value = "1", .descr = "Alleged"           , .atom = 0}, // 1
        {.value = "2", .descr = "Accept"            , .atom = 0}, // 2
        {.value = "3", .descr = "Decline"           , .atom = 0}, // 3
        {.value = "4", .descr = "Addendum"          , .atom = 0}, // 4
        {.value = "5", .descr = "NoWas"             , .atom = 0}, // 5
        {.value = "6", .descr = "TradeReportCancel" , .atom = 0}, // 6
        {.value = "7", .descr = "LockedInTradeBreak", .atom = 0}, // 7
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // SUBMIT
          case '1': return f.value_atom(1); // ALLEGED
          case '2': return f.value_atom(2); // ACCEPT
          case '3': return f.value_atom(3); // DECLINE
          case '4': return f.value_atom(4); // ADDENDUM
          case '5': return f.value_atom(5); // NO_WAS
          case '6': return f.value_atom(6); // TRADE_REPORT_CANCEL
          case '7': return f.value_atom(7); // LOCKED_IN_TRADE_BREAK
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 857 "AllocNoOrdersType"
    Field{
      fvar,
      857,
      "AllocNoOrdersType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "NotSpecified"        , .atom = 0}, // 0
        {.value = "1", .descr = "ExplicitListProvided", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NOT_SPECIFIED
          case '1': return f.value_atom(1); // EXPLICIT_LIST_PROVIDED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 858 "SharedCommission"
    Field{
      fvar,
      858,
      "SharedCommission",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 859 "ConfirmReqID"
    Field{
      fvar,
      859,
      "ConfirmReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 860 "AvgParPx"
    Field{
      fvar,
      860,
      "AvgParPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 861 "ReportedPx"
    Field{
      fvar,
      861,
      "ReportedPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 862 "NoCapacities"
    Field{
      fvar,
      862,
      "NoCapacities",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoCapacities",
      862,
      nullptr
    },
    //--- Tag# 863 "OrderCapacityQty"
    Field{
      fvar,
      863,
      "OrderCapacityQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 864 "NoEvents"
    Field{
      fvar,
      864,
      "NoEvents",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoEvents",
      864,
      nullptr
    },
    //--- Tag# 865 "EventType"
    Field{
      fvar,
      865,
      "EventType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "Put"            , .atom = 0}, // 0
        {.value = "2" , .descr = "Call"           , .atom = 0}, // 1
        {.value = "3" , .descr = "Tender"         , .atom = 0}, // 2
        {.value = "4" , .descr = "SinkingFundCall", .atom = 0}, // 3
        {.value = "99", .descr = "Other"          , .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom(0); // Put
          case           '2': return f.value_atom(1); // Call
          case           '3': return f.value_atom(2); // Tender
          case           '4': return f.value_atom(3); // SinkingFundCall
          case CINT<'9','9'>: return f.value_atom(4); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 866 "EventDate"
    Field{
      fvar,
      866,
      "EventDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 867 "EventPx"
    Field{
      fvar,
      867,
      "EventPx",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 868 "EventText"
    Field{
      fvar,
      868,
      "EventText",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 869 "PctAtRisk"
    Field{
      fvar,
      869,
      "PctAtRisk",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 870 "NoInstrAttrib"
    Field{
      fvar,
      870,
      "NoInstrAttrib",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoInstrAttrib",
      870,
      nullptr
    },
    //--- Tag# 871 "InstrAttribType"
    Field{
      fvar,
      871,
      "InstrAttribType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "Flat"                                                                     , .atom = 0}, // 0
        {.value = "2" , .descr = "ZeroCoupon"                                                               , .atom = 0}, // 1
        {.value = "3" , .descr = "InterestBearing"                                                          , .atom = 0}, // 2
        {.value = "4" , .descr = "NoPeriodicPayments"                                                       , .atom = 0}, // 3
        {.value = "5" , .descr = "VariableRate"                                                             , .atom = 0}, // 4
        {.value = "6" , .descr = "LessFeeForPut"                                                            , .atom = 0}, // 5
        {.value = "7" , .descr = "SteppedCoupon"                                                            , .atom = 0}, // 6
        {.value = "8" , .descr = "CouponPeriod"                                                             , .atom = 0}, // 7
        {.value = "9" , .descr = "WhenAndIfIssued"                                                          , .atom = 0}, // 8
        {.value = "10", .descr = "OriginalIssueDiscount"                                                    , .atom = 0}, // 9
        {.value = "11", .descr = "CallablePuttable"                                                         , .atom = 0}, // 10
        {.value = "12", .descr = "EscrowedToMaturity"                                                       , .atom = 0}, // 11
        {.value = "13", .descr = "EscrowedToRedemptionDateCallableSupplyRedemptionDateInTheInstrattribvalue", .atom = 0}, // 12
        {.value = "14", .descr = "Prerefunded"                                                              , .atom = 0}, // 13
        {.value = "15", .descr = "InDefault"                                                                , .atom = 0}, // 14
        {.value = "16", .descr = "Unrated"                                                                  , .atom = 0}, // 15
        {.value = "17", .descr = "Taxable"                                                                  , .atom = 0}, // 16
        {.value = "18", .descr = "Indexed"                                                                  , .atom = 0}, // 17
        {.value = "19", .descr = "SubjectToAlternativeMinimumTax"                                           , .atom = 0}, // 18
        {.value = "20", .descr = "OriginalIssueDiscountPriceSupplyPriceInTheInstrattribvalue"               , .atom = 0}, // 19
        {.value = "21", .descr = "CallableBelowMaturityValue"                                               , .atom = 0}, // 20
        {.value = "22", .descr = "CallableWithoutNoticeByMailToHolderUnlessRegistered"                      , .atom = 0}, // 21
        {.value = "99", .descr = "TextSupplyTheTextOfTheAttributeOrDisclaimerInTheInstrattribvalue"         , .atom = 0}, // 22
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom( 0); // Flat
          case           '2': return f.value_atom( 1); // ZeroCoupon
          case           '3': return f.value_atom( 2); // InterestBearing
          case           '4': return f.value_atom( 3); // NoPeriodicPayments
          case           '5': return f.value_atom( 4); // VariableRate
          case           '6': return f.value_atom( 5); // LessFeeForPut
          case           '7': return f.value_atom( 6); // SteppedCoupon
          case           '8': return f.value_atom( 7); // CouponPeriod
          case           '9': return f.value_atom( 8); // WhenAndIfIssued
          case CINT<'1','0'>: return f.value_atom( 9); // OriginalIssueDiscount
          case CINT<'1','1'>: return f.value_atom(10); // CallablePuttable
          case CINT<'1','2'>: return f.value_atom(11); // EscrowedToMaturity
          case CINT<'1','3'>: return f.value_atom(12); // EscrowedToRedemptionDateCallableSupplyRedemptionDateInTheInstrattribvalue
          case CINT<'1','4'>: return f.value_atom(13); // Prerefunded
          case CINT<'1','5'>: return f.value_atom(14); // InDefault
          case CINT<'1','6'>: return f.value_atom(15); // Unrated
          case CINT<'1','7'>: return f.value_atom(16); // Taxable
          case CINT<'1','8'>: return f.value_atom(17); // Indexed
          case CINT<'1','9'>: return f.value_atom(18); // SubjectToAlternativeMinimumTax
          case CINT<'2','0'>: return f.value_atom(19); // OriginalIssueDiscountPriceSupplyPriceInTheInstrattribvalue
          case CINT<'2','1'>: return f.value_atom(20); // CallableBelowMaturityValue
          case CINT<'2','2'>: return f.value_atom(21); // CallableWithoutNoticeByMailToHolderUnlessRegistered
          case CINT<'9','9'>: return f.value_atom(22); // TextSupplyTheTextOfTheAttributeOrDisclaimerInTheInstrattribvalue
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 872 "InstrAttribValue"
    Field{
      fvar,
      872,
      "InstrAttribValue",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 873 "DatedDate"
    Field{
      fvar,
      873,
      "DatedDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 874 "InterestAccrualDate"
    Field{
      fvar,
      874,
      "InterestAccrualDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 875 "CPProgram"
    Field{
      fvar,
      875,
      "CPProgram",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1" , .descr = "3"    , .atom = 0}, // 0
        {.value = "2" , .descr = "4"    , .atom = 0}, // 1
        {.value = "99", .descr = "Other", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '1': return f.value_atom(0); // 3
          case           '2': return f.value_atom(1); // 4
          case CINT<'9','9'>: return f.value_atom(2); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 876 "CPRegType"
    Field{
      fvar,
      876,
      "CPRegType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 877 "UnderlyingCPProgram"
    Field{
      fvar,
      877,
      "UnderlyingCPProgram",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 878 "UnderlyingCPRegType"
    Field{
      fvar,
      878,
      "UnderlyingCPRegType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 879 "UnderlyingQty"
    Field{
      fvar,
      879,
      "UnderlyingQty",
      FieldType::QTY,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 880 "TrdMatchID"
    Field{
      fvar,
      880,
      "TrdMatchID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 881 "SecondaryTradeReportRefID"
    Field{
      fvar,
      881,
      "SecondaryTradeReportRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 882 "UnderlyingDirtyPrice"
    Field{
      fvar,
      882,
      "UnderlyingDirtyPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 883 "UnderlyingEndPrice"
    Field{
      fvar,
      883,
      "UnderlyingEndPrice",
      FieldType::PRICE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 884 "UnderlyingStartValue"
    Field{
      fvar,
      884,
      "UnderlyingStartValue",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 885 "UnderlyingCurrentValue"
    Field{
      fvar,
      885,
      "UnderlyingCurrentValue",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 886 "UnderlyingEndValue"
    Field{
      fvar,
      886,
      "UnderlyingEndValue",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 887 "NoUnderlyingStips"
    Field{
      fvar,
      887,
      "NoUnderlyingStips",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoUnderlyingStips",
      887,
      nullptr
    },
    //--- Tag# 888 "UnderlyingStipType"
    Field{
      fvar,
      888,
      "UnderlyingStipType",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 889 "UnderlyingStipValue"
    Field{
      fvar,
      889,
      "UnderlyingStipValue",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 890 "MaturityNetMoney"
    Field{
      fvar,
      890,
      "MaturityNetMoney",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 891 "MiscFeeBasis"
    Field{
      fvar,
      891,
      "MiscFeeBasis",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Absolute"  , .atom = 0}, // 0
        {.value = "1", .descr = "PerUnit"   , .atom = 0}, // 1
        {.value = "2", .descr = "Percentage", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // ABSOLUTE
          case '1': return f.value_atom(1); // PER_UNIT
          case '2': return f.value_atom(2); // PERCENTAGE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 892 "TotNoAllocs"
    Field{
      fvar,
      892,
      "TotNoAllocs",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 893 "LastFragment"
    Field{
      fvar,
      893,
      "LastFragment",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>{{
        {.value = "Y", .descr = "Yes", .atom = 0}, // 0
        {.value = "N", .descr = "No" , .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case 'Y': return f.value_atom(0); // YES
          case 'N': return f.value_atom(1); // NO
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 894 "CollReqID"
    Field{
      fvar,
      894,
      "CollReqID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 895 "CollAsgnReason"
    Field{
      fvar,
      895,
      "CollAsgnReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Initial"                , .atom = 0}, // 0
        {.value = "1", .descr = "Scheduled"              , .atom = 0}, // 1
        {.value = "2", .descr = "TimeWarning"            , .atom = 0}, // 2
        {.value = "3", .descr = "MarginDeficiency"       , .atom = 0}, // 3
        {.value = "4", .descr = "MarginExcess"           , .atom = 0}, // 4
        {.value = "5", .descr = "ForwardCollateralDemand", .atom = 0}, // 5
        {.value = "6", .descr = "EventOfDefault"         , .atom = 0}, // 6
        {.value = "7", .descr = "AdverseTaxEvent"        , .atom = 0}, // 7
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // INITIAL
          case '1': return f.value_atom(1); // SCHEDULED
          case '2': return f.value_atom(2); // TIME_WARNING
          case '3': return f.value_atom(3); // MARGIN_DEFICIENCY
          case '4': return f.value_atom(4); // MARGIN_EXCESS
          case '5': return f.value_atom(5); // FORWARD_COLLATERAL_DEMAND
          case '6': return f.value_atom(6); // EVENT_OF_DEFAULT
          case '7': return f.value_atom(7); // ADVERSE_TAX_EVENT
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 896 "CollInquiryQualifier"
    Field{
      fvar,
      896,
      "CollInquiryQualifier",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Tradedate"           , .atom = 0}, // 0
        {.value = "1", .descr = "GcInstrument"        , .atom = 0}, // 1
        {.value = "2", .descr = "Collateralinstrument", .atom = 0}, // 2
        {.value = "3", .descr = "SubstitutionEligible", .atom = 0}, // 3
        {.value = "4", .descr = "NotAssigned"         , .atom = 0}, // 4
        {.value = "5", .descr = "PartiallyAssigned"   , .atom = 0}, // 5
        {.value = "6", .descr = "FullyAssigned"       , .atom = 0}, // 6
        {.value = "7", .descr = "OutstandingTrades"   , .atom = 0}, // 7
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // TRADEDATE
          case '1': return f.value_atom(1); // GC_INSTRUMENT
          case '2': return f.value_atom(2); // COLLATERALINSTRUMENT
          case '3': return f.value_atom(3); // SUBSTITUTION_ELIGIBLE
          case '4': return f.value_atom(4); // NOT_ASSIGNED
          case '5': return f.value_atom(5); // PARTIALLY_ASSIGNED
          case '6': return f.value_atom(6); // FULLY_ASSIGNED
          case '7': return f.value_atom(7); // OUTSTANDING_TRADES
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 897 "NoTrades"
    Field{
      fvar,
      897,
      "NoTrades",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoTrades",
      897,
      nullptr
    },
    //--- Tag# 898 "MarginRatio"
    Field{
      fvar,
      898,
      "MarginRatio",
      FieldType::PERCENTAGE,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 899 "MarginExcess"
    Field{
      fvar,
      899,
      "MarginExcess",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 900 "TotalNetValue"
    Field{
      fvar,
      900,
      "TotalNetValue",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 901 "CashOutstanding"
    Field{
      fvar,
      901,
      "CashOutstanding",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 902 "CollAsgnID"
    Field{
      fvar,
      902,
      "CollAsgnID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 903 "CollAsgnTransType"
    Field{
      fvar,
      903,
      "CollAsgnTransType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "New"    , .atom = 0}, // 0
        {.value = "1", .descr = "Replace", .atom = 0}, // 1
        {.value = "2", .descr = "Cancel" , .atom = 0}, // 2
        {.value = "3", .descr = "Release", .atom = 0}, // 3
        {.value = "4", .descr = "Reverse", .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // NEW
          case '1': return f.value_atom(1); // REPLACE
          case '2': return f.value_atom(2); // CANCEL
          case '3': return f.value_atom(3); // RELEASE
          case '4': return f.value_atom(4); // REVERSE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 904 "CollRespID"
    Field{
      fvar,
      904,
      "CollRespID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 905 "CollAsgnRespType"
    Field{
      fvar,
      905,
      "CollAsgnRespType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Received", .atom = 0}, // 0
        {.value = "1", .descr = "Accepted", .atom = 0}, // 1
        {.value = "2", .descr = "Declined", .atom = 0}, // 2
        {.value = "3", .descr = "Rejected", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // RECEIVED
          case '1': return f.value_atom(1); // ACCEPTED
          case '2': return f.value_atom(2); // DECLINED
          case '3': return f.value_atom(3); // REJECTED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 906 "CollAsgnRejectReason"
    Field{
      fvar,
      906,
      "CollAsgnRejectReason",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "UnknownDeal"               , .atom = 0}, // 0
        {.value = "1" , .descr = "UnknownOrInvalidInstrument", .atom = 0}, // 1
        {.value = "2" , .descr = "UnauthorizedTransaction"   , .atom = 0}, // 2
        {.value = "3" , .descr = "InsufficientCollateral"    , .atom = 0}, // 3
        {.value = "4" , .descr = "InvalidTypeOfCollateral"   , .atom = 0}, // 4
        {.value = "5" , .descr = "ExcessiveSubstitution"     , .atom = 0}, // 5
        {.value = "99", .descr = "Other"                     , .atom = 0}, // 6
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom(0); // UnknownDeal
          case           '1': return f.value_atom(1); // UnknownOrInvalidInstrument
          case           '2': return f.value_atom(2); // UnauthorizedTransaction
          case           '3': return f.value_atom(3); // InsufficientCollateral
          case           '4': return f.value_atom(4); // InvalidTypeOfCollateral
          case           '5': return f.value_atom(5); // ExcessiveSubstitution
          case CINT<'9','9'>: return f.value_atom(6); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 907 "CollAsgnRefID"
    Field{
      fvar,
      907,
      "CollAsgnRefID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 908 "CollRptID"
    Field{
      fvar,
      908,
      "CollRptID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 909 "CollInquiryID"
    Field{
      fvar,
      909,
      "CollInquiryID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 910 "CollStatus"
    Field{
      fvar,
      910,
      "CollStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Unassigned"        , .atom = 0}, // 0
        {.value = "1", .descr = "PartiallyAssigned" , .atom = 0}, // 1
        {.value = "2", .descr = "AssignmentProposed", .atom = 0}, // 2
        {.value = "3", .descr = "Assigned"          , .atom = 0}, // 3
        {.value = "4", .descr = "Challenged"        , .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // UNASSIGNED
          case '1': return f.value_atom(1); // PARTIALLY_ASSIGNED
          case '2': return f.value_atom(2); // ASSIGNMENT_PROPOSED
          case '3': return f.value_atom(3); // ASSIGNED
          case '4': return f.value_atom(4); // CHALLENGED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 911 "TotNumReports"
    Field{
      fvar,
      911,
      "TotNumReports",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 912 "LastRptRequested"
    Field{
      fvar,
      912,
      "LastRptRequested",
      FieldType::BOOLEAN,
      DataType::BOOL,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 913 "AgreementDesc"
    Field{
      fvar,
      913,
      "AgreementDesc",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 914 "AgreementID"
    Field{
      fvar,
      914,
      "AgreementID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 915 "AgreementDate"
    Field{
      fvar,
      915,
      "AgreementDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 916 "StartDate"
    Field{
      fvar,
      916,
      "StartDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 917 "EndDate"
    Field{
      fvar,
      917,
      "EndDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 918 "AgreementCurrency"
    Field{
      fvar,
      918,
      "AgreementCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 919 "DeliveryType"
    Field{
      fvar,
      919,
      "DeliveryType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "VersusPaymentDeliver", .atom = 0}, // 0
        {.value = "1", .descr = "FreeDeliver"         , .atom = 0}, // 1
        {.value = "2", .descr = "TriParty"            , .atom = 0}, // 2
        {.value = "3", .descr = "HoldInCustody"       , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // VERSUS_PAYMENT_DELIVER
          case '1': return f.value_atom(1); // FREE_DELIVER
          case '2': return f.value_atom(2); // TRI_PARTY
          case '3': return f.value_atom(3); // HOLD_IN_CUSTODY
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 920 "EndAccruedInterestAmt"
    Field{
      fvar,
      920,
      "EndAccruedInterestAmt",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 921 "StartCash"
    Field{
      fvar,
      921,
      "StartCash",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 922 "EndCash"
    Field{
      fvar,
      922,
      "EndCash",
      FieldType::AMT,
      DataType::DOUBLE,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 923 "UserRequestID"
    Field{
      fvar,
      923,
      "UserRequestID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 924 "UserRequestType"
    Field{
      fvar,
      924,
      "UserRequestType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Logonuser"                  , .atom = 0}, // 0
        {.value = "2", .descr = "Logoffuser"                 , .atom = 0}, // 1
        {.value = "3", .descr = "Changepasswordforuser"      , .atom = 0}, // 2
        {.value = "4", .descr = "RequestIndividualUserStatus", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // LOGONUSER
          case '2': return f.value_atom(1); // LOGOFFUSER
          case '3': return f.value_atom(2); // CHANGEPASSWORDFORUSER
          case '4': return f.value_atom(3); // REQUEST_INDIVIDUAL_USER_STATUS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 925 "NewPassword"
    Field{
      fvar,
      925,
      "NewPassword",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 926 "UserStatus"
    Field{
      fvar,
      926,
      "UserStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "LoggedIn"         , .atom = 0}, // 0
        {.value = "2", .descr = "NotLoggedIn"      , .atom = 0}, // 1
        {.value = "3", .descr = "UserNotRecognised", .atom = 0}, // 2
        {.value = "4", .descr = "PasswordIncorrect", .atom = 0}, // 3
        {.value = "5", .descr = "PasswordChanged"  , .atom = 0}, // 4
        {.value = "6", .descr = "Other"            , .atom = 0}, // 5
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // LOGGED_IN
          case '2': return f.value_atom(1); // NOT_LOGGED_IN
          case '3': return f.value_atom(2); // USER_NOT_RECOGNISED
          case '4': return f.value_atom(3); // PASSWORD_INCORRECT
          case '5': return f.value_atom(4); // PASSWORD_CHANGED
          case '6': return f.value_atom(5); // OTHER
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 927 "UserStatusText"
    Field{
      fvar,
      927,
      "UserStatusText",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 928 "StatusValue"
    Field{
      fvar,
      928,
      "StatusValue",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Connected"                   , .atom = 0}, // 0
        {.value = "2", .descr = "NotConnectedDownExpectedUp"  , .atom = 0}, // 1
        {.value = "3", .descr = "NotConnectedDownExpectedDown", .atom = 0}, // 2
        {.value = "4", .descr = "InProcess"                   , .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // CONNECTED
          case '2': return f.value_atom(1); // NOT_CONNECTED_DOWN_EXPECTED_UP
          case '3': return f.value_atom(2); // NOT_CONNECTED_DOWN_EXPECTED_DOWN
          case '4': return f.value_atom(3); // IN_PROCESS
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 929 "StatusText"
    Field{
      fvar,
      929,
      "StatusText",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 930 "RefCompID"
    Field{
      fvar,
      930,
      "RefCompID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 931 "RefSubID"
    Field{
      fvar,
      931,
      "RefSubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 932 "NetworkResponseID"
    Field{
      fvar,
      932,
      "NetworkResponseID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 933 "NetworkRequestID"
    Field{
      fvar,
      933,
      "NetworkRequestID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 934 "LastNetworkResponseID"
    Field{
      fvar,
      934,
      "LastNetworkResponseID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 935 "NetworkRequestType"
    Field{
      fvar,
      935,
      "NetworkRequestType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Snapshot"                                 , .atom = 0}, // 0
        {.value = "2", .descr = "Subscribe"                                , .atom = 0}, // 1
        {.value = "4", .descr = "StopSubscribing"                          , .atom = 0}, // 2
        {.value = "8", .descr = "LevelOfDetailThenNocompidsBecomesRequired", .atom = 0}, // 3
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // SNAPSHOT
          case '2': return f.value_atom(1); // SUBSCRIBE
          case '4': return f.value_atom(2); // STOP_SUBSCRIBING
          case '8': return f.value_atom(3); // LEVEL_OF_DETAIL_THEN_NOCOMPIDS_BECOMES_REQUIRED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 936 "NoCompIDs"
    Field{
      fvar,
      936,
      "NoCompIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoCompIDs",
      936,
      nullptr
    },
    //--- Tag# 937 "NetworkStatusResponseType"
    Field{
      fvar,
      937,
      "NetworkStatusResponseType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Full"             , .atom = 0}, // 0
        {.value = "2", .descr = "IncrementalUpdate", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // FULL
          case '2': return f.value_atom(1); // INCREMENTAL_UPDATE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 938 "NoCollInquiryQualifier"
    Field{
      fvar,
      938,
      "NoCollInquiryQualifier",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoCollInquiryQualifier",
      938,
      nullptr
    },
    //--- Tag# 939 "TrdRptStatus"
    Field{
      fvar,
      939,
      "TrdRptStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Accepted", .atom = 0}, // 0
        {.value = "1", .descr = "Rejected", .atom = 0}, // 1
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // ACCEPTED
          case '1': return f.value_atom(1); // REJECTED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 940 "AffirmStatus"
    Field{
      fvar,
      940,
      "AffirmStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "1", .descr = "Received"                    , .atom = 0}, // 0
        {.value = "2", .descr = "ConfirmRejectedIeNotAffirmed", .atom = 0}, // 1
        {.value = "3", .descr = "Affirmed"                    , .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '1': return f.value_atom(0); // RECEIVED
          case '2': return f.value_atom(1); // CONFIRM_REJECTED_IE_NOT_AFFIRMED
          case '3': return f.value_atom(2); // AFFIRMED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 941 "UnderlyingStrikeCurrency"
    Field{
      fvar,
      941,
      "UnderlyingStrikeCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 942 "LegStrikeCurrency"
    Field{
      fvar,
      942,
      "LegStrikeCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 943 "TimeBracket"
    Field{
      fvar,
      943,
      "TimeBracket",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 944 "CollAction"
    Field{
      fvar,
      944,
      "CollAction",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Retain", .atom = 0}, // 0
        {.value = "1", .descr = "Add"   , .atom = 0}, // 1
        {.value = "2", .descr = "Remove", .atom = 0}, // 2
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // RETAIN
          case '1': return f.value_atom(1); // ADD
          case '2': return f.value_atom(2); // REMOVE
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 945 "CollInquiryStatus"
    Field{
      fvar,
      945,
      "CollInquiryStatus",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0", .descr = "Accepted"             , .atom = 0}, // 0
        {.value = "1", .descr = "AcceptedWithWarnings" , .atom = 0}, // 1
        {.value = "2", .descr = "Completed"            , .atom = 0}, // 2
        {.value = "3", .descr = "CompletedWithWarnings", .atom = 0}, // 3
        {.value = "4", .descr = "Rejected"             , .atom = 0}, // 4
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        switch (code[0]) {
          case '0': return f.value_atom(0); // ACCEPTED
          case '1': return f.value_atom(1); // ACCEPTED_WITH_WARNINGS
          case '2': return f.value_atom(2); // COMPLETED
          case '3': return f.value_atom(3); // COMPLETED_WITH_WARNINGS
          case '4': return f.value_atom(4); // REJECTED
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 946 "CollInquiryResult"
    Field{
      fvar,
      946,
      "CollInquiryResult",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>{{
        {.value = "0" , .descr = "Successful"                           , .atom = 0}, // 0
        {.value = "1" , .descr = "InvalidOrUnknownInstrument"           , .atom = 0}, // 1
        {.value = "2" , .descr = "InvalidOrUnknownCollateralType"       , .atom = 0}, // 2
        {.value = "3" , .descr = "InvalidParties"                       , .atom = 0}, // 3
        {.value = "4" , .descr = "InvalidTransportTypeRequested"        , .atom = 0}, // 4
        {.value = "5" , .descr = "InvalidDestinationRequested"          , .atom = 0}, // 5
        {.value = "6" , .descr = "NoCollateralFoundForTheTradeSpecified", .atom = 0}, // 6
        {.value = "7" , .descr = "NoCollateralFoundForTheOrderSpecified", .atom = 0}, // 7
        {.value = "8" , .descr = "CollateralInquiryTypeNotSupported"    , .atom = 0}, // 8
        {.value = "9" , .descr = "UnauthorizedForCollateralInquiry"     , .atom = 0}, // 9
        {.value = "99", .descr = "Other"                                , .atom = 0}, // 10
      }},
      nullptr,
      0,
      [](const Field& f, ErlNifEnv* env, const char* code, int len) {
        auto hash = len == 1 ? uint64_t(code[0]) : hash_val(code, len);
        switch (hash) {
          case           '0': return f.value_atom( 0); // Successful
          case           '1': return f.value_atom( 1); // InvalidOrUnknownInstrument
          case           '2': return f.value_atom( 2); // InvalidOrUnknownCollateralType
          case           '3': return f.value_atom( 3); // InvalidParties
          case           '4': return f.value_atom( 4); // InvalidTransportTypeRequested
          case           '5': return f.value_atom( 5); // InvalidDestinationRequested
          case           '6': return f.value_atom( 6); // NoCollateralFoundForTheTradeSpecified
          case           '7': return f.value_atom( 7); // NoCollateralFoundForTheOrderSpecified
          case           '8': return f.value_atom( 8); // CollateralInquiryTypeNotSupported
          case           '9': return f.value_atom( 9); // UnauthorizedForCollateralInquiry
          case CINT<'9','9'>: return f.value_atom(10); // Other
          default: return am_undefined;
        }
      },
    },
    //--- Tag# 947 "StrikeCurrency"
    Field{
      fvar,
      947,
      "StrikeCurrency",
      FieldType::CURRENCY,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 948 "NoNested3PartyIDs"
    Field{
      fvar,
      948,
      "NoNested3PartyIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoNested3PartyIDs",
      948,
      nullptr
    },
    //--- Tag# 949 "Nested3PartyID"
    Field{
      fvar,
      949,
      "Nested3PartyID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 950 "Nested3PartyIDSource"
    Field{
      fvar,
      950,
      "Nested3PartyIDSource",
      FieldType::CHAR,
      DataType::CHAR,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 951 "Nested3PartyRole"
    Field{
      fvar,
      951,
      "Nested3PartyRole",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 952 "NoNested3PartySubIDs"
    Field{
      fvar,
      952,
      "NoNested3PartySubIDs",
      FieldType::NUMINGROUP,
      DataType::GROUP,
      std::vector<FieldChoice>(),
      "NoNested3PartySubIDs",
      952,
      nullptr
    },
    //--- Tag# 953 "Nested3PartySubID"
    Field{
      fvar,
      953,
      "Nested3PartySubID",
      FieldType::STRING,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 954 "Nested3PartySubIDType"
    Field{
      fvar,
      954,
      "Nested3PartySubIDType",
      FieldType::INT,
      DataType::INT,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 955 "LegContractSettlMonth"
    Field{
      fvar,
      955,
      "LegContractSettlMonth",
      FieldType::MONTHYEAR,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
    //--- Tag# 956 "LegInterestAccrualDate"
    Field{
      fvar,
      956,
      "LegInterestAccrualDate",
      FieldType::LOCALMKTDATE,
      DataType::STRING,
      std::vector<FieldChoice>(),
      nullptr,
      0,
      nullptr
    },
  }};
}

} // namespace

//------------------------------------------------------------------------------
// Main FIX variant creation function exported from the shared object
//------------------------------------------------------------------------------
extern "C" {
  std::vector<Field> create_fix_fields(FixVariant* fvar)
  {
    return make_all_fields(fvar);
  }

  const char* get_fix_variant_name() { return "default"; }
}

