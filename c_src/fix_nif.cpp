//------------------------------------------------------------------------------
// Author: Serge Aleynikov <saleyn at gmail dot com>
//
// The work is derived from Maxim Lapshin's open source work:
// https://github.com/maxlapshin/fix under the same open source MIT
// licensing terms as the original.
//------------------------------------------------------------------------------
#include <erl_nif.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#include <ctime>
#include <cstdio>
#include <cassert>
#include <regex>
#include <iostream>
#include <sstream>
#include <iterator>

//------------------------------------------------------------------------------
// Static values and types
//------------------------------------------------------------------------------

#include "util.hpp"

static ERL_NIF_TERM am_utc;
static ERL_NIF_TERM am_local;
static ERL_NIF_TERM am_us;
static ERL_NIF_TERM am_ms;
static ERL_NIF_TERM am_sec;
static ERL_NIF_TERM am_ts_type;
static ERL_NIF_TERM am_binary;
static ERL_NIF_TERM am_offset;
static ERL_NIF_TERM am_delim;
static ERL_NIF_TERM am_badarg;
static ERL_NIF_TERM am_badenv;
static ERL_NIF_TERM am_badvariant;
static ERL_NIF_TERM am_full;
static ERL_NIF_TERM am_so_files;
static ERL_NIF_TERM am_debug;

//------------------------------------------------------------------------------
// NIFs
//------------------------------------------------------------------------------

inline Persistent* get_pers(ErlNifEnv* env)
{
  return static_cast<Persistent*>(enif_priv_data(env));
}

/// Args: (binary(), [binary])
static ERL_NIF_TERM
split_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  auto pers = get_pers(env);
  if (!pers) [[unlikely]]
    return enif_raise_exception(env, am_badenv);

  ErlNifBinary input;
  bool ret_binary = false;
  bool full       = false; // Include 'BeginString', 'BodyLength', 'CheckSum'?

  FixVariant* var;

  if (argc < 2 || !enif_inspect_binary(env, argv[1], &input)) [[unlikely]]
    return enif_make_badarg(env);
  if (!pers->get(argv[0], var)) [[unlikely]]
    return enif_raise_exception(env, am_badvariant);
  if (argc == 3) {
    if (!enif_is_empty_list(env, argv[2])) {
      if (!enif_is_list(env, argv[2])) [[unlikely]]
        return enif_make_badarg(env);

      ERL_NIF_TERM  head, list = argv[2];
      while (enif_get_list_cell(env, list, &head, &list)) {
        if (enif_is_identical(head, am_binary))
          ret_binary = true;
        else if (enif_is_identical(head, am_full))
          full = true;
        else [[unlikely]]
          return enif_make_badarg(env);
      }
    }
  }

  assert(var);

  const auto* begin = input.data;
  const auto* end   = input.data + input.size;

  return do_split(var, env, begin, end, ret_binary, full);
}

// "is_name" is 0, if val is integer or binary encoded integer
//              1, if val is atom or binary string
//             -1  either integer or atom
// Convert:
//   is_name <= 0: (10)             -> 10
//   is_name <= 0: (<<"10">>)       -> 10
//   is_name != 0: ('CheckSum')     -> 10
//   is_name != 0: (<<"CheckSum">>) -> 10
static bool
arg_code(FixVariant* var, ErlNifEnv* env, ERL_NIF_TERM val, int& code, int is_name = 0)
{
  return var->field_code(env, val, code, is_name);
}

static ERL_NIF_TERM
field_meta_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  auto pers = get_pers(env);
  if (!pers) [[unlikely]]
    return enif_raise_exception(env, am_badenv);

  FixVariant* var;
  int         code;

  // Args must be:
  // (Variant::atom(), Name::atom()|binary()|integer())
  if ((argc != 2) || !pers->get(argv[0], var)
                  || !arg_code(var, env, argv[1], code, -1)) [[unlikely]]
    return enif_make_badarg(env);

  assert(var);
  assert(code > 0 && code < var->field_count());

  auto field = var->field(code);

  assert(field);

  return field->meta(env);
}

// Convert (int()|binary()) -> atom():
//   (<<"10">>) -> 'CheckSum'
//   (10)       -> 'CheckSum'
static ERL_NIF_TERM
tag_to_field_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  auto pers = get_pers(env);
  if (!pers) [[unlikely]]
    return enif_raise_exception(env, am_badenv);

  FixVariant* var;
  int         code;

  // Args must be: (Name::integer()|binary())
  if (argc != 2 || !pers->get(argv[0], var)
                || !arg_code(var, env, argv[1], code, 0)) [[unlikely]]
    return enif_make_badarg(env);

  // NOTE: This is guaranteed by the arg_code() call!
  assert(var);
  assert(code > 0 && code < var->field_count());

  auto field = var->field(code);

  assert(field);

  return field->get_atom();
}

// Convert (atom()|binary()) -> integer()|binary()
//   (<<"CheckSum">>, true)  -> <<"10">>
//   (<<"CheckSum">>, false) -> 10
//   ('CheckSum',     true)  -> <<"10">>
//   ('CheckSum',     false) -> 10
static ERL_NIF_TERM
field_to_tag_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  auto pers = get_pers(env);
  if (!pers) [[unlikely]]
    return enif_raise_exception(env, am_badenv);

  FixVariant* var;
  int         code;
  bool        is_bin = false;

  // Args must be:
  // (Variant::atom(), Name::atom()|binary()) or (atom(), atom(), 'binary')
  if ((argc == 3  && !(is_bin = enif_is_identical(am_binary, argv[2]))) ||
      (argc != 2) || !pers->get(argv[0], var)
                  || !arg_code(var, env, argv[1], code, true)) [[unlikely]]
    return enif_make_badarg(env);

  return is_bin ? var->copy_bin_tag(env, code) : enif_make_int(env, code);
}

// Convert (Variant()::atom(), atom()|integer()|binary(), binary()) ->
//           integer()|atom()|float()|binary()
//   E.g. ('default', <<"35">>, <<"0">>) -> 'Heartbeat'
static ERL_NIF_TERM
decode_field_value_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  auto pers = get_pers(env);
  if (!pers) [[unlikely]]
    return enif_raise_exception(env, am_badenv);

  FixVariant*  var;
  int          code;
  ErlNifBinary bin;

  if (argc != 3 || !pers->get(argv[0], var)
                || !arg_code(var, env, argv[1], code, 0)
                || !enif_inspect_binary(env, argv[2], &bin)) [[unlikely]]
    return enif_make_badarg(env);

  // The following is guaranteed by arg_code() call:
  assert(code > 0 && code < var->field_count());

  auto field = var->field(code);

  assert(field);

  // Does the field have any values?
  if (field->has_values()) [[likely]]
    return field->decode(env, (const char*)bin.data, bin.size);

  return argv[1]; // Return unmodified binary if no field values are defined.
}

static ERL_NIF_TERM
encode_field_value_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  auto pers = get_pers(env);
  if (!pers) [[unlikely]]
    return enif_raise_exception(env, am_badenv);

  FixVariant* var;

  if (argc != 3 || !pers->get(argv[0], var)) [[unlikely]]
    return enif_make_badarg(env);

  auto tag = argv[1];
  auto val = argv[2];
  long n;
  char sbuf[128];

  auto [field, numtag] = var->field_with_tag(env, tag);
  if (!field)
    return enif_raise_exception(env, enif_make_tuple2(env, am_badarg, tag));

  if (enif_is_atom(env, val)) {
    if (!field->has_values()) [[unlikely]]
      return enif_raise_exception(env, enif_make_tuple2(env, am_badarg, tag));
    return field->encode(env, val); // This creates a copy of the cached binary
  } else if (enif_is_binary(env, val)) {
    return val;
  } else if (enif_get_long(env, val, &n)) {
    ERL_NIF_TERM bin;
    char buf[32];
    int  len = (field->dtype() == DataType::DATETIME)
             ? encode_timestamp(buf, n, pers->ts_type())
             : snprintf(buf, sizeof(buf), "%ld", n);
    if (len == 0) [[unlikely]]
      return enif_raise_exception(env, enif_make_tuple2(env, am_badarg, tag));
    auto p   = (char*)enif_make_new_binary(env, len, &bin);
    if (!p) [[unlikely]]
      return enif_raise_exception(env, am_enomem);
    memcpy(p, buf, len);
    return bin;
  } else if (enif_get_string(env, val, sbuf, sizeof(sbuf), ERL_NIF_LATIN1)) {
    return val;
  }

  return enif_raise_exception(env, enif_make_tuple2(env, am_badarg, tag));
}

static ERL_NIF_TERM
encode_field_tagvalue_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  auto pers = get_pers(env);
  if (!pers) [[unlikely]]
    return enif_raise_exception(env, am_badenv);

  FixVariant* var;

  if (argc != 3 || !pers->get(argv[0], var)) [[unlikely]]
    return enif_make_badarg(env);

  auto tag = argv[1];
  auto val = argv[2];

  auto [field, numtag] = var->field_with_tag(env, tag);
  if (!field)
    return enif_make_badarg(env);

  int offset = 0;
  ErlNifBinary output{};
  auto guard = binary_guard(output);

  auto res = field->encode_with_tag(env, offset, output, numtag, val);

  if  (res != am_ok) [[unlikely]]
    return res == am_error
         ? enif_raise_exception(env, enif_make_tuple2(env, res, tag))
         : enif_raise_exception(env, res);

  if (offset < int(output.size) && !enif_realloc_binary(&output, offset))
    return enif_raise_exception(env, am_enomem);

  guard.release();

  return enif_make_binary(env, &output);
}

static ERL_NIF_TERM
encode_fields_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  auto pers = get_pers(env);
  if (!pers) [[unlikely]]
    return enif_raise_exception(env, am_badenv);

  FixVariant* var;

  if (argc != 2 || !pers->get(argv[0], var) ||
      !(enif_is_list(env, argv[1]) || enif_is_empty_list(env, argv[1]))) [[unlikely]]
    return enif_make_badarg(env);

  int   offset = 0;
  ErlNifBinary output{};

  if (!enif_alloc_binary(256, &output))
    return enif_raise_exception(env, am_enomem);

  // This releases binary memory when it goes out of scope.
  // When there is an error inside the `while` loop below, the allocated
  // binary will get automatically released. However after the loop, we
  // explicitely release the guard, since we need the binary to be returned
  // to the caller.
  auto output_guard = binary_guard(output);

  ERL_NIF_TERM  head, list = argv[1];
  const ERL_NIF_TERM* tagval;
  int arity;
  while (enif_get_list_cell(env, list, &head, &list)) {
    if (!enif_get_tuple(env, head, &arity, &tagval) || arity != 2) [[unlikely]]
      return enif_make_badarg(env);

    auto tag = tagval[0];
    auto val = tagval[1];

    auto [field, numtag] = var->field_with_tag(env, tag);
    if (!field)
      return enif_raise_exception(env, enif_make_tuple2(env, am_badarg, tag));

    auto res = field->encode_with_tag(env, offset, output, numtag, val);
    if  (res != am_ok) [[unlikely]]
      return res == am_error
           ? enif_raise_exception(env, enif_make_tuple2(env, res, tag))
           : enif_raise_exception(env, res);
  }

  if (offset < int(output.size) && !enif_realloc_binary(&output, offset))
    return enif_raise_exception(env, am_enomem);

  output_guard.release(); // Don't free up the binary when leaving this scope!

  return enif_make_binary(env, &output);
}

static ERL_NIF_TERM
list_field_values_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  auto pers = get_pers(env);
  if (!pers) [[unlikely]]
    return enif_raise_exception(env, am_badenv);

  FixVariant* var;
  int         code;

  if (argc != 2
           || !pers->get(argv[0], var)
           || !arg_code(var, env, argv[1], code, -1)) [[unlikely]]
    return enif_make_badarg(env);

  auto f = var->field(code);

  assert(f);  // This is guaranteed by the check above

  std::vector<ERL_NIF_TERM> res;
  res.reserve(f->values().size());

  for (auto& fc : f->values()) {
    ERL_NIF_TERM t;
    auto         n = strlen(fc.value);
    auto         p = enif_make_new_binary(env, n, &t);
    if (!p) [[unlikely]]
      return enif_raise_exception(env, am_enomem);
    memcpy(p, fc.value, n);
    res.push_back(enif_make_tuple2(env, t, fc.get_atom(env)));
  }

  return enif_make_list_from_array(env, &res[0], res.size());
}

static ERL_NIF_TERM
list_fix_variants_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  auto pers = get_pers(env);
  if (!pers) [[unlikely]]
    return enif_raise_exception(env, am_badenv);

  if (argc != 0) [[unlikely]]
    return enif_make_badarg(env);

  std::vector<ERL_NIF_TERM> res;
  res.reserve(pers->count());

  for (auto& v : pers->variants())
    res.push_back(enif_make_atom(env, v->variant().c_str()));

  return enif_make_list_from_array(env, &res[0], res.size());
}

static ERL_NIF_TERM
bin_to_integer_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  size_t offset = 0;
  int    delim  = '\0';

  ErlNifBinary bin;
  if (argc < 1 || argc > 2 || !enif_inspect_binary(env, argv[0], &bin)) [[unlikely]]
    return enif_make_badarg(env);
  else if (argc == 2 && !enif_is_empty_list(env, argv[1])) {
    if (!enif_is_list(env, argv[1])) [[unlikely]]
      return enif_make_badarg(env);

    ERL_NIF_TERM  head, list = argv[1];
    const ERL_NIF_TERM* opts;
    int arity;
    while (enif_get_list_cell(env, list, &head, &list)) {
      if (!enif_get_tuple(env, head, &arity, &opts) || arity != 2) [[unlikely]]
        return enif_make_badarg(env);

      if (enif_is_identical(opts[0], am_offset)) {
        if (!enif_get_uint64(env, opts[1], &offset) || offset >= bin.size) [[unlikely]]
          return enif_make_badarg(env);
      } else if (enif_is_identical(opts[0], am_delim)) {
        if (!enif_get_int(env, opts[1], &delim)) [[unlikely]]
          return enif_make_badarg(env);
      } else
        return enif_make_badarg(env);
    }
  }

  long res;
  auto b = (const char*)bin.data;
  auto e = b + bin.size;
  b += offset;
  assert(b <= e);
  auto p = str_to_int(b, e, res, char(delim));

  if (p == nullptr) [[unlikely]]
    return enif_make_badarg(env);

  return enif_make_tuple2(env, enif_make_int(env, p-b), enif_make_long(env, res));
}

// Args:
// 1. Timestamp  :: binary()      - "YYYYMMSS hh:mm:ss[.ttt[ttt]]"
// 2. 'utc'|'local' :: atom()     - 'utc' if UTC time, 'local' if local time
static ERL_NIF_TERM
decode_timestamp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  if (argc == 0 || !enif_inspect_binary(env, argv[0], &bin) || argc > 2)
    return enif_make_badarg(env);
  auto utc = true;
  if (argc > 1) {
    if (!enif_is_atom(env, argv[1]))
      return enif_make_badarg(env);
    else if (enif_is_identical(argv[1], am_local))
      utc = false;
    else if (!enif_is_identical(argv[1], am_utc))
      return enif_make_badarg(env);
  }

  auto res = decode_timestamp(env, (const char*)bin.data, bin.size, utc);

  if (res < 0) [[unlikely]]
    return enif_make_badarg(env);

  return enif_make_long(env, res);
}

// Args:
// 1. Timestamp :: integer()  - Usec or Msec from epoch
// 2. utc|local :: atom()     - 'utc' if UTC time, 'local' if local time
// 3. sec|us|ms :: atom()     - seconds (default), microseconds, milliseconds
static ERL_NIF_TERM
encode_timestamp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  auto utc  = true;
  uint64_t t;

  auto pers = get_pers(env);
  if (!pers) [[unlikely]]
    return enif_raise_exception(env, am_badenv);

  TsType ts_type = pers->ts_type();

  if (argc == 0 || !enif_get_uint64(env, argv[0], &t) || argc > 3)
    return enif_make_badarg(env);

  if (argc > 1) {
    if (!enif_is_atom(env, argv[1]))
      return enif_make_badarg(env);
    else if (enif_is_identical(argv[1], am_local))
      utc = false;
    else if (!enif_is_identical(argv[1], am_utc))
      return enif_make_badarg(env);

    if (argc == 3) {
      if (!enif_is_atom(env, argv[2]))
        return enif_make_badarg(env);
      else if (enif_is_identical(argv[2], am_sec))
        ts_type = TsType::Seconds;
      else if (enif_is_identical(argv[2], am_ms))
        ts_type = TsType::Millisec;
      else if (enif_is_identical(argv[2], am_us))
        ts_type = TsType::Microsec;
      else
        return enif_make_badarg(env);
    }
  }

  return encode_timestamp(env, t, ts_type, utc);
}

// Calculate the FIX checksum of a binary message body
static std::pair<bool,uint8_t> calc_checksum(const ErlNifBinary& bin)
{
  const auto* p = bin.data;
  const auto* e = p + bin.size;
  const auto* end = e-7;

  // Must end with: "10=...\x1"
  if (bin.size < 8 || memcmp(end, "10=", 3) != 0)
    return std::make_pair(false, 0);

  uint32_t cs  = 0;
  char     soh = SOH;

  // Guess the SOH separator symbol, which can be '\1' or '|'
  for (; p != end; cs += uint32_t(*p), ++p)
    if (*p == 1 || *p == '|') {
      soh = *p;
      break;
    }

  // Now that we guessed the SOH, check that the message ends with the SOH char
  if (*(e-1) != soh)
    return std::make_pair(false, 0);

  if (soh == SOH) [[likely]]
    for (; p != end; ++p)
      cs += uint32_t(*p);
  else
    for (; p != end; ++p)
      cs += uint32_t(*p==soh ? SOH : *p);

  // Take the result modulo 256, ie the lowest-order byte:
  return std::make_pair(true, cs & 0xff);
}

// Calculate the FIX checksum of a binary message body
// Args:  binary()
// Res:   int()
static ERL_NIF_TERM
checksum_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  if (argc != 1 || !enif_inspect_binary(env, argv[0], &bin) || bin.size < 8) [[unlikely]]
    return enif_make_badarg(env);

  auto [res, cs] = calc_checksum(bin);

  return res ? enif_make_int(env, cs) : enif_make_badarg(env);
}

// Calculate the FIX checksum of a binary message body
// Args:  binary()
// Res:   int()
static ERL_NIF_TERM
update_checksum_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  if (argc != 1 || !enif_inspect_binary(env, argv[0], &bin) || bin.size < 8)
    return enif_make_badarg(env);

  auto [res, cs] = calc_checksum(bin);

  if (!res)
    return enif_make_badarg(env);

  auto n1 = cs / 100;
  auto n  = cs - (n1*100);
  auto n2 = n  / 10;
  auto n3 = n - n2 * 10;

  auto q = (char*)bin.data + bin.size - 4;
  *q++   = '0' + n1;
  *q++   = '0' + n2;
  *q++   = '0' + n3;

  return enif_make_binary(env, &bin);
}

static std::string home() {
  const  char* env = getenv("HOME");
  return env ? env : "";
}

// Update the input string.
static void replace_env_vars(std::string& text) {
  static const std::regex env("\\$\\{([^\\$}]*)\\}");

  if (text.size() > 0 && text[0] == '~')
      text.replace(text.begin(), text.begin()+1, home());

  std::smatch match;
  while (std::regex_search(text, match, env)) {
    std::string var;
    if (match.length(1)) {
      const char* s = getenv(match[1].str().c_str());
      if (s) var = s;
    }
    text.replace(match.position(0), match.length(0), var);
  }
}

// Common functionality of strftime(3) callable from other functions
//  (Format :: string() | binary(), NowSecsSinceEpoch :: integer(), utc | local) ->
//      {NumberOfBytesWrittenToRes, IsBinary::boolean()}
static std::pair<int, bool>
strftime_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv, char* res, size_t sz)
{
  char         buf[512];
  const char*  pbuf;
  ErlNifBinary bin;

  if (argc < 2 || argc > 3) [[unlikely]]
    return std::make_pair(-1, false);

  bool is_bin = enif_is_binary(env, argv[0]);

  if (is_bin) {
    if (!enif_inspect_binary(env, argv[0], &bin)) [[unlikely]]
      return std::make_pair(-1, false);

    if (sz == 0) [[unlikely]]
      return std::make_pair(0, true);
    if (sz == 1 || bin.size <= 1) [[unlikely]] {
      res[0] = '\0';
      return std::make_pair(0, true);
    }
    // Make sure that the buf space is '\0' terminated
    if (bin.data[bin.size-1] == '\0')
      pbuf = (const char*)bin.data;
    else {
      size_t n = bin.size < sizeof(buf) ? bin.size : sizeof(buf)-1;
      strncpy(buf, (char*)bin.data, n);
      buf[n] = '\0';
      pbuf   = buf;
    }
  } else if (enif_get_string(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1) > 0) [[likely]]
    pbuf = buf;
  else [[unlikely]]
    return std::make_pair(-1, false);

  time_t t;
  if (!enif_get_long(env, argv[1], &t)) [[unlikely]] // Seconds since epoch
    return std::make_pair(-1, false);

  bool utc = true;
  if (argc == 3) {
    if (!enif_is_atom(env, argv[2])) [[unlikely]]
      return std::make_pair(-1, false);
    if (enif_is_identical(argv[2], am_utc))
      utc = true;
    else if (enif_is_identical(argv[2], am_local))
      utc = false;
    else [[unlikely]]
      return std::make_pair(-1, false);
  }

  struct tm tm;

  auto ok = (utc ? gmtime_r(&t, &tm) : localtime_r(&t, &tm)) != nullptr;
  if (!ok) [[unlikely]]
    return std::make_pair(-1, false);

  return std::make_pair((int)strftime(res, sz, pbuf, &tm), is_bin);
}

static ERL_NIF_TERM str_to_term(ErlNifEnv* env, bool is_bin, const char* str, int sz)
{
  if (sz <= 0)
    return enif_make_badarg(env);

  // Output is a binary
  if (is_bin) {
    ERL_NIF_TERM ret_bin;
    auto p = (char*)enif_make_new_binary(env, sz, &ret_bin);
    if (!p) [[unlikely]]
      return enif_raise_exception(env, am_enomem);
    memcpy(p, str, sz);
    return ret_bin;
  }

  // Output is a string
  return enif_make_string_len(env, str, sz, ERL_NIF_LATIN1);
}

// Same functionality as strftime(3)
//  (Format :: string() | binary(),  NowSecs :: integer(), utc | local) ->
//      string() | binary().
static ERL_NIF_TERM strftime_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char res[1024];

  int  n;
  bool is_bin;
  std::tie(n, is_bin) = strftime_impl(env, argc, argv, res, sizeof(res));

  return str_to_term(env, is_bin, res, n);
}

// Same functionality as strftime(3) plus environment variable substitution
//  (Format :: string() | binary(), NowSecs :: integer(), utc | local) ->
//      string() | binary().
static ERL_NIF_TERM
pathftime_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char res[1024];

  int  n;
  bool is_bin;
  std::tie(n, is_bin) = strftime_impl(env, argc, argv, res, sizeof(res));

  if (n <= 0)
    return enif_make_badarg(env);

  std::string s(res, n);

  replace_env_vars(s);

  return str_to_term(env, is_bin, s.c_str(), s.size());
}

//------------------------------------------------------------------------------
// Utility functions
//------------------------------------------------------------------------------

static int init_env(void** priv_data, std::vector<std::string> const& so_files,
                    int debug, TsType ts_type)
{
  // Init persistent environment that owns some binaries shared across NIF calls
  try   {*priv_data = (void*)(new Persistent(so_files, debug, ts_type));}
  catch (std::exception const& e)
  {
    std::ostringstream str;
    std::copy(so_files.begin(), so_files.end(),
      std::ostream_iterator<std::string>(str, "\r\n"));
    fprintf(stderr, "Cannot initialize FIX NIF environment: %s\r\nLibs:\r\n%s\r\n",
            e.what(), str.str().c_str());
    return 1;
  }
  return 0;
}

inline void free_env(void** p)
{
  // NOTE: "delete" checks for null before deleting
  delete static_cast<Persistent*>(*p);
  *p = nullptr;
}

//------------------------------------------------------------------------------
// NIF Interface API
//------------------------------------------------------------------------------

// load_info - input arguments:
//  [{debug, Level::integer()} | {so_files, [File::string()]}]
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  int                       debug = 0;
  const char*               errs;
  ERL_NIF_TERM arg_files=0, head, list=load_info;

  std::vector<std::string>  so_files;
  TsType                    ts_type = TsType::Seconds;

  if (!enif_is_list(env, load_info)) {
    errs = "Initialization argument is not a list!";
    goto ERR;
  }

  try {
    am_utc        = safe_make_atom(env, "utc");
    am_local      = safe_make_atom(env, "local");
    am_us         = safe_make_atom(env, "us");
    am_ms         = safe_make_atom(env, "ms");
    am_sec        = safe_make_atom(env, "sec");
    am_ts_type    = safe_make_atom(env, "ts_type");
    am_binary     = safe_make_atom(env, "binary");
    am_offset     = safe_make_atom(env, "offset");
    am_delim      = safe_make_atom(env, "delim");
    am_badarg     = safe_make_atom(env, "badarg");
    am_badenv     = safe_make_atom(env, "badenv");
    am_badvariant = safe_make_atom(env, "badvariant");
    am_full       = safe_make_atom(env, "full");
    am_so_files   = safe_make_atom(env, "so_files");
    am_debug      = safe_make_atom(env, "debug");
  }
  catch (std::exception const& e) {
    errs = e.what();
    goto ERR;
  }

  while (enif_get_list_cell(env, list, &head, &list)) {
    const ERL_NIF_TERM* opts;
    int                 arity;
    if (!enif_get_tuple(env, head, &arity, &opts) || arity != 2) [[unlikely]] {
      errs = "Invalid argument: list must contain {key, value} pairs!";
      goto ERR;
    }

    if (enif_is_identical(opts[0], am_debug) && enif_get_int(env, opts[1], &debug))
      continue;
    else if (enif_is_identical(opts[0], am_so_files) && enif_is_list(env, opts[1]))
      arg_files = opts[1];
    else if (enif_is_identical(opts[0], am_ts_type)  && enif_is_atom(env, opts[1])) {
      if      (enif_is_identical(opts[1], am_sec))  ts_type = TsType::Seconds;
      else if (enif_is_identical(opts[1], am_us))   ts_type = TsType::Microsec;
      else if (enif_is_identical(opts[1], am_ms))   ts_type = TsType::Millisec;
      else {
        errs = "Invalid ts_type";
        goto ERR;
      }
    } else {
      errs = "Invalid argument";
      goto ERR;
    }
  }

  if (arg_files) {
    list = arg_files;
    while (enif_get_list_cell(env, list, &head, &list)) {
      char buf[2048];
      auto res = enif_get_string(env, head, buf, sizeof(buf), ERL_NIF_LATIN1);
      if  (!res) [[unlikely]] {
        errs = "FIX shared object file path is not provided!";
        goto ERR;
      }

      so_files.push_back(std::string(buf, std::abs(res)));
    }
  }

  if (so_files.empty()) {
    errs = "Missing required so_files argument!";
    goto ERR;
  } else if (debug > 0) {
    for (auto& f : so_files)
      DBGPRINT(debug, 1, "FIX: file %s", f.c_str());
  }

  return init_env(priv_data, so_files, debug, ts_type);

ERR:
  fprintf(stderr, "%s\r\n", errs);
  return -1;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  // Initialize new environment
  FixVariant* p;
  int res = load(env, (void**)&p, load_info);

  if (res != 0) [[unlikely]]
    return res;

  // Delete old environment
  free_env(old_priv_data);

  // Memorize new environment
  *priv_data = (void*)p;

  return 0;
}

static void unload(ErlNifEnv* caller_env, void* priv_data)
{
  free_env(&priv_data);
}

static ErlNifFunc fix_nif_funcs[] =
{
  {"split",                 2, split_nif},
  {"split",                 3, split_nif},
  {"field_meta",            2, field_meta_nif},
  {"tag_to_field",          2, tag_to_field_nif},
  {"field_to_tag",          2, field_to_tag_nif},
  {"decode_field_value",    3, decode_field_value_nif},
  {"encode_field_value",    3, encode_field_value_nif},
  {"encode_field_tagvalue", 3, encode_field_tagvalue_nif},
  {"encode_fields",         2, encode_fields_nif},
  {"list_field_values",     2, list_field_values_nif},
  {"list_fix_variants",     0, list_fix_variants_nif},
  {"bin_to_integer",        1, bin_to_integer_nif},
  {"bin_to_integer",        2, bin_to_integer_nif},
  {"decode_timestamp",      1, decode_timestamp_nif},
  {"decode_timestamp",      2, decode_timestamp_nif},
  {"encode_timestamp",      1, encode_timestamp_nif},
  {"encode_timestamp",      2, encode_timestamp_nif},
  {"encode_timestamp",      3, encode_timestamp_nif},
  {"checksum",              1, checksum_nif},
  {"update_checksum",       1, update_checksum_nif},
  {"strftime",              2, strftime_nif},
  {"strftime",              3, strftime_nif},
  {"pathftime",             2, pathftime_nif},
  {"pathftime",             3, pathftime_nif},
};

ERL_NIF_INIT(fix_nif, fix_nif_funcs, load, nullptr, upgrade, unload)
