# FIX Parsing Library

This is a non-backward compatible fork of the
[original work](https://github.com/maxlapshin/fix) by Maxim Lapshin,
which is nearly a complete rewrite that implements performance improvements to the
NIF FIX parser.  The intent of this fork and rewrite was to improve and convert this
project to a standalone FIX marshaling library suitable for use in other projects
rather than having a functional FIX client/server implementation.

## FIX protocol

This code is implementation of http://fixprotocol.org/
It is a trading protocol that allows to make orders, receive quotes and do many
other things, described in protocol.

This repo includes specification of different FIX versions taken from
http://quickfix.org/ project.

## Usage

`code-gen.es` is a parser generator that creates C++ and Erlang code based on
the XML description of protocol. Generated files include:
```
c_src/fix_variant.cpp
c_src/util.hxx
include/fix_adm_msgs.hrl
include/fix_app_msgs.hrl
src/fix_variant.erl
src/fix_decoder.erl
src/fix_groups.erl
```
The implementation allows to create custom FIX variants. Each variant's decoder
is implemented in a separate set of files. The generate a custom variant execute
the generator in some other directory given the variant name (e.g. `cme`):
```
$ mkdir cme
$ cd cme
$ ../code-gen.es -f /path/to/FIX.xml -var cme -c ../fix.config
Writing file: ./c_src/fix_variant.cpp
Writing file: ./src/fix_variant_cme.erl
Writing file: ./src/fix_variant_cme.erl
Writing file: ./src/fix_decoder_cme.erl
Writing file: ./include/fix_adm_msgs_cme.hrl
Writing file: ./include/fix_app_msgs_cme.hrl
Writing file: ./src/fix_groups_cme.erl
```

This generated code can be integrated in another application specific to the
given FIX variant.  The generated and compiled shared object `fix_variant_cme.so`
contains field decoding definitions and is loaded by the `fix` application using
either the following call:

```erlang
1> fix_nif:load_fix_variant(Variant|AppName).
```
The `default` FIX variant is the one included in the `fix` project based on the
`spec/FIX44.xml` template.  The `fix_so_paths` can contain multiple paths that
are `:` delimited.

The NIF library of the `fix` project will load all `fix_variant*.so` shared
objects for each FIX variant requested.

### Encoding/Decoding

FIX messages are delimited by SOH field separator <<1>>.  The `fix_util:dump/1`
function can be used to change that SOH to <<"|">>.

Encoding FIX messages:
```
1> fix_util:dump(fix_util:encode(#'Heartbeat'{}, 1, <<"Sender">>, <<"Target">>)).
<<"8=FIX.4.4|9=58|35=0|49=Sender|56=Target|34=1|52=20220306-06:37:47.353711|10=063|">>
```

The decoder automatically recognizes the SOH separator and <<"|">> and it can
parse both formats.

Decoding FIX messages:
```
2> {ok, BinRest, {MatchedFldCount, Header, Msg, UnparsedFields}} = fix_util:decode(<<"8=FIX.4.4|9=58|35=0|49=Sender|56=Target|34=1|52=20220306-06:39:29.387565|10=079|">>, [binary]).
4> Header.
#{'BeginString' => undefined,'BodyLength' => undefined,
  'LastMsgSeqNumProcessed' => undefined,'MsgSeqNum' => 1,
  'MsgType' => 'Heartbeat','PossDupFlag' => false,
  'PossResend' => false,'SenderCompID' => <<"Sender">>,
  'SendingTime' => 1646548769387565,
  'TargetCompID' => <<"Target">>}
5> Msg.
#{}
```

## FIX printer

A `fixdump` script is provided for printing content of files produced by `fix_logger`.

```erlang
# Display help screen
$ _build/default/bin/fixdump -h

# Print decoded file
$ _build/default/bin/fixdump -f log/20210102-fix1.log

────────────────────────────────────────────────────────────────────
00000779: 20210614-23:07:19.534 FIX1 <- Kraken: Logon
──No─┬───Tag─┬─Field─────────────────────┬─Decoded Value────────────
  01 │    35 │ MsgType                   │ Logon
  02 │    34 │ MsgSeqNum                 │ 1
  03 │    49 │ SenderCompID              │ Coinbase
  04 │    52 │ SendingTime               │ 20220714-23:07:18.135
  05 │    56 │ TargetCompID              │ 8YVU3rvC0LMI8OoReNk=
  06 │    96 │ RawData                   │ 4eZGzeWjpnIVJ5nReNk=
  07 │    98 │ EncryptMethod             │ NONE
  08 │   108 │ HeartBtInt                │ 10
  09 │   554 │ Password                  │ <secret>
  10 │  8013 │ CancelOrdersOnDisconnect  │ SESSION
  11 │  9406 │ DropCopyFlag              │ false 
```
