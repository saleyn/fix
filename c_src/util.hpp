//------------------------------------------------------------------------------
// Author: Serge Aleynikov <saleyn at gmail dot com>
//------------------------------------------------------------------------------

#pragma once

#include <erl_nif.h>
#include <dlfcn.h>
#include <string.h>
#include <unordered_map>
#include <unordered_set>
#include <functional>
#include <memory>
#include <cstdint>
#include <cstring>
#include <cassert>
#include <cmath>
#include <string>
#include <vector>
#include <tuple>
#include <mutex>
#include <filesystem>
#ifndef NDEBUG
#include <fstream>
#endif

#define IS_LIKELY(Expr)   __builtin_expect(!!(Expr), 1)
#define IS_UNLIKELY(Expr) __builtin_expect(!!(Expr), 0)

#define STRINGIFY(x) #x
#define TOSTRING(x)                    STRINGIFY(x)
#define FILE_SRC_LOCATION __FILE__ ":" TOSTRING(__LINE__)

#define PRINT(Fmt, ...)                                   \
  fprintf(stderr, Fmt " [%s]\r\n", __VA_ARGS__,           \
          basename(FILE_SRC_LOCATION))

#ifndef NDEBUG
#define DBGPRINT(Debug, Level, Fmt, ...)                  \
  do {                                                    \
    if (Debug >= Level) [[unlikely]]                      \
      PRINT(Fmt, __VA_ARGS__);                            \
  } while(0)
#else
#define DBGPRINT(Debug, Level, Fmt, ...)
#endif

#ifndef NDEBUG
#define ASSERT(Cond, Begin, End)                          \
  if (!(Cond)) [[unlikely]] {                             \
    std::ofstream out("/tmp/dump.bin");                   \
    out << std::string((const char*)Begin, End-Begin);    \
    out.close();                                          \
    fprintf(stderr, "Asserting failed [%s]\r\n",          \
            basename(FILE_SRC_LOCATION));                 \
    assert(Cond);                                         \
  } else {}
#else
#define ASSERT(Cond, Begin, End)
#endif

#ifndef NDEBUG
#define DUMP(Cond, Begin, End)                            \
  if ((Cond) > 0) {                                       \
    std::ofstream out("/tmp/dump.bin");                   \
    out << std::string((const char*)Begin, End-Begin);    \
    out.close();                                          \
  } else {}
#else
#define DUMP(Cond, Begin, End)
#endif

//------------------------------------------------------------------------------
// Static variables
//------------------------------------------------------------------------------
static ERL_NIF_TERM am_badarg;
static ERL_NIF_TERM am_calendar;
static ERL_NIF_TERM am_day;
static ERL_NIF_TERM am_decimal;
static ERL_NIF_TERM am_decode_error;
static ERL_NIF_TERM am_encode_error;
static ERL_NIF_TERM am_enomem;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_exception;
static ERL_NIF_TERM am_false;
static ERL_NIF_TERM am_fix;
static ERL_NIF_TERM am_group;
static ERL_NIF_TERM am_hour;
static ERL_NIF_TERM am_magnitude;
static ERL_NIF_TERM am_message;
static ERL_NIF_TERM am_microsecond;
static ERL_NIF_TERM am_minute;
static ERL_NIF_TERM am_month;
static ERL_NIF_TERM am_more;
static ERL_NIF_TERM am_naive_calendar;
static ERL_NIF_TERM am_naive_struct;
static ERL_NIF_TERM am_nil;
static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_pos;
static ERL_NIF_TERM am_precision;
static ERL_NIF_TERM am_reason;
static ERL_NIF_TERM am_second;
static ERL_NIF_TERM am_src;
static ERL_NIF_TERM am_struct;
static ERL_NIF_TERM am_tag;
static ERL_NIF_TERM am_true;
static ERL_NIF_TERM am_year;

static const char SOH = 1;

//------------------------------------------------------------------------------
// FIX data types
//------------------------------------------------------------------------------
struct DataType {
  enum Type {
    UNDEFINED,
    CHAR,
    INT,
    DOUBLE,
    BOOL,
    STRING,
    DATETIME,
    GROUP,
    BINARY,
    __LAST,  // Helper, must be last enum
  };
  DataType(int v) : value(v) {} // This is to enable assignment: DataType t = DataType::INT;
  operator int() const { return value; }
  int value;

  static const char* to_string(unsigned val) {
    static const char* s_vals[] = {
      "undefined",
      "char",
      "int",
      "double",
      "bool",
      "string",
      "datetime",
      "group",
      "binary",
    };

    return val < __LAST ? s_vals[val] : s_vals[0];
  }
};

// FIX field types found in FIX*.xml specs
enum FieldType {
  UNDEFINED,
  AMT,
  BOOLEAN,
  CHAR,
  COUNTRY,
  CURRENCY,
  DATA,
  DAYOFMONTH,
  EXCHANGE,
  FLOAT,
  INT,
  LENGTH,
  LOCALMKTDATE,
  MONTHYEAR,
  MULTIPLEVALUESTRING,
  NUMINGROUP,
  PERCENTAGE,
  PRICE,
  PRICEOFFSET,
  QTY,
  SEQNUM,
  STRING,
  UTCDATE,
  UTCDATEONLY,
  UTCTIMEONLY,
  UTCTIMESTAMP,
};

static const char* type_to_string(FieldType val) {
  static const char* s_vals[] = {
    "undefined",
    "amt",
    "boolean",
    "char",
    "country",
    "currency",
    "data",
    "dayofmonth",
    "exchange",
    "float",
    "int",
    "length",
    "localmktdate",
    "monthyear",
    "multiplevaluestring",
    "numingroup",
    "percentage",
    "price",
    "priceoffset",
    "qty",
    "seqnum",
    "string",
    "utcdate",
    "utcdateonly",
    "utctimeonly",
    "utctimestamp",
  };

  return val <= UTCTIMESTAMP ? s_vals[val] : s_vals[0];
}

enum class DoubleFmt {
  Double,
  Decimal,
  Binary,
  String,
  BigIntDecimal
};

enum class TimeFmt {
  EpochUSec,
  EpochMSec,
  EpochSec,
  Binary,
  String,
  Tuple,    // { {{Y,M,D},{HH,MM,SS}}, Microseconds }
  Naive,    // Elixir's NaiveDataTime map
  None
};

enum class TsType {
  Seconds  = 0,
  Millisec = 3,
  Microsec = 6,
};

struct FixVariant;
struct Persistent;

struct FieldChoice {
  const char*           value;
  const char*           descr;
  mutable ERL_NIF_TERM  atom;

  ERL_NIF_TERM get_atom(ErlNifEnv* env) const;
};

struct Field;

// Function returning atom representation of a field's choice code
using DecFieldValFun = std::function<ERL_NIF_TERM (const Field&, ErlNifEnv*, const char*, int len)>;
using AtomToTagMap   = std::unordered_map<ERL_NIF_TERM,     int>;
using NameToTagMap   = std::unordered_map<std::string_view, int>;

namespace {
  template <int N>
  inline const char* basename(const char (&file)[N]) {
    auto p    = file + N-1;
    auto end  = file;
    while (p != end && *p != '/') --p;
    return p == end ? file : p+1;
  }
}

// Out of memory error
struct enomem_error : public std::bad_alloc {
  template <typename... Args>
  enomem_error(const char* fmt, Args&&... args)
  {
    snprintf(buf, sizeof(buf), fmt, std::forward<Args>(args)...);
  }

  virtual ~enomem_error() noexcept {}

  const char* what() const noexcept { return buf; }

private:
  char buf[128];
};

/// Convert a `term` that can be an atom/integer/binary to a string field name
std::string term_to_field_name(ErlNifEnv*, ERL_NIF_TERM term);

//------------------------------------------------------------------------------
// Field
//------------------------------------------------------------------------------
struct Field {
  Field();
  Field(Field&&);
  Field(Field const&);

  enum class WriteOp {
    Success,
    Error,
    NoSpace
  };

  void operator=(Field&& a_rhs);
  void operator=(Field const& a_rhs);

  template <typename DecF>
  Field
  (
    FixVariant*                 var,
    int                         id,
    const char*                 name,
    enum FieldType              type,
    DataType                    dtype,
    std::vector<FieldChoice>&&  choices,
    const char*                 len_field,      // Name of the length field for 'NUMINGROUP' and 'DATA' field types
    const int                   len_field_id,   // ID of the length field
    DecF                        dec_fun
  );

  ~Field();

  int                     debug()           const;

  int                     id()              const { return m_id;           }
  std::string const&      id_str()          const { return m_id_str;       }
  std::string_view const& name_str()        const { return m_name;         }
  const char*             name()            const { return m_name.data();  }
  FieldType               type()            const { return m_type;         }
  DataType                dtype()           const { return m_dtype;        }
  ERL_NIF_TERM            atom()            const { return m_atom;         }
  const int               ch_len()          const { return m_ch_len;       }
  const char*             len_field()       const { return m_len_field;    }
  const int               len_field_id()    const { return m_len_field_id; }

  bool                    assigned()        const { return m_id != 0;      }

  ERL_NIF_TERM            get_atom()        const;
  ERL_NIF_TERM            meta(ErlNifEnv*)  const; // Get metadata

  bool                    has_values()      const { return m_choices.size(); }
  size_t                  values_count()    const { return m_choices.size(); }

  FieldChoice const*      value(unsigned idx) const;
  FieldChoice const*      value(std::string_view const& val) const;

  ERL_NIF_TERM            value_atom(unsigned idx) const;
  ERL_NIF_TERM            value_atom(std::string_view const& val) const;

  // Lookup atom value in m_choices, and save its binary representation to `res`
  bool atom_to_bin(ErlNifEnv*, ERL_NIF_TERM atom, ErlNifBinary& res) const;

  bool                    is_elixir()   const;

  FixVariant const*       variant()     const { assert(m_var); return m_var; }
  Persistent const*       pers()        const;

  std::vector<FieldChoice> const& values() const { return m_choices; }

  // Return atom value from a binary for an enumerated field
  template <typename Def>
  ERL_NIF_TERM
  decode_value(ErlNifEnv*, const char* val, int len, const Def& def) const;

  // Decode field's value
  ERL_NIF_TERM decode(ErlNifEnv*, const char* val, int len,
                      DoubleFmt dbl_fmt  = DoubleFmt::Decimal,
                      TimeFmt   time_fmt = TimeFmt::EpochUSec) const;

  // Encode a value `val` of this field as binary stored in res as
  // `<<Tag/binary, 1, Value/binary, 1>>`. Return am_ok if all good or Erlang
  // exception on error.
  ERL_NIF_TERM encode(ErlNifEnv*, int& offset, ErlNifBinary& res, ERL_NIF_TERM val);

private:
  using TermMap = std::unordered_map<ERL_NIF_TERM, ERL_NIF_TERM>;
  using NameMap = std::unordered_map<std::string_view, const FieldChoice*>;

  FixVariant*                m_var;
  int                        m_id;
  std::string                m_id_str;
  std::string_view           m_name;
  FieldType                  m_type;
  DataType                   m_dtype;
  mutable ERL_NIF_TERM       m_atom;
  int                        m_ch_len;
  int                        m_max_val_len;    // Max length of codes in 'choices'
  std::vector<FieldChoice>   m_choices;
  const char*                m_len_field;      // Length field name for field types
                                               // 'NUMINGROUP' and 'DATA'
  int                        m_len_field_id;   // ID of the length field
  DecFieldValFun             m_decf;
  TermMap                    m_atom_map;       // Maps atom name Name -> value Code
  NameMap                    m_name_map;
};

//------------------------------------------------------------------------------
// Registry of FIX variant factories
// Environment persistent across NIF calls
//------------------------------------------------------------------------------
struct Persistent {
  using VariantStore = std::unordered_map<ERL_NIF_TERM, std::unique_ptr<FixVariant>>;
  using IndexMap     = std::unordered_map<ERL_NIF_TERM, FixVariant*>;

  // Takes a vector of shared object files to load containing FIX variants
  Persistent
  (
    std::vector<std::string> const& so_files,
    int                             debug   = 0,
    TsType                          ts_type = TsType::Seconds
  );

  ~Persistent();

  ErlNifEnv*          env()                   { return m_env;     }
  ErlNifEnv const*    env()             const { return m_env;     }

  int                 debug()           const { return m_debug;   }
  TsType              ts_type()         const { return m_ts_type; }

  // Get the variant by either variant or application name
  FixVariant*         get(ERL_NIF_TERM);
  bool                get(ERL_NIF_TERM, FixVariant*& var);

  size_t              count()           const { return m_var_storage.size(); }
  VariantStore const& variants()        const { return m_var_storage;        }

  bool                load_fix_variant(const std::string& so_file,  bool replace);
  bool                unload_fix_variant(ERL_NIF_TERM variant_name, bool lock);

  template <typename... Args>
  void Debug(int level, const char* fmt, Args... args) {
    if (level <= m_debug)
      fprintf(stderr, fmt, std::forward<Args>(args)...);
  }
private:
  std::mutex          m_mutex;
  VariantStore        m_var_storage;
  IndexMap            m_variants;
  ErlNifEnv*          m_env;
  int                 m_debug;
  TsType              m_ts_type;

  ERL_NIF_TERM        make_atom(const char* am);
};

//------------------------------------------------------------------------------
// Manages FIX variant fields metadata and shared binaries: <<"1">>,<<"2">>, ...
//------------------------------------------------------------------------------
struct FixVariant {
  using FieldAndTagConst = std::pair<Field const*, ERL_NIF_TERM>;
  using FieldAndTag      = std::pair<Field*,       ERL_NIF_TERM>;

  FixVariant(Persistent* pers, const std::string& path, bool replace=true);

  ~FixVariant();

  int                 debug()         const { return m_pers->debug(); }
  Persistent  const*  pers()          const { return m_pers;          }

  std::string const&  name()          const { return m_name;          }
  ERL_NIF_TERM        name_atom()     const { return m_name_am;       }
  std::string const&  app_name()      const { return m_app_name;      }
  ERL_NIF_TERM        app_name_atom() const { return m_app_name_am;   }
  bool                is_elixir()     const { return m_elixir;        }
  std::string const&  so_path()       const { return m_so_path;       }
  ErlNifEnv*          env()                 { return m_pers->env();   }
  ErlNifEnv const*    env()           const { return m_pers->env();   }
  ERL_NIF_TERM        null()                { return ERL_NIF_TERM(0); }

  Field const*        field(unsigned tag) const;
  Field*              field(unsigned tag);

  Field const*        field(ErlNifEnv*, ERL_NIF_TERM tag) const;
  Field*              field(ErlNifEnv*, ERL_NIF_TERM tag);

  int                 field_count() const { return m_fields.size(); }

  int find_tag_by_name(std::string_view&& name) const
  {
    auto   it  = m_name_to_tag_map.find(name);
    return it == m_name_to_tag_map.end() ? 0 : it->second;
  }

  int find_tag_by_atom(ERL_NIF_TERM name) const
  {
    auto   it  = m_atom_to_tag_map.find(name);
    return it == m_atom_to_tag_map.end() ? 0 : it->second;
  }

  // Get the numeric value of the FIX field from an integer/atom/binary representation.
  // "is_name" is 0, if val is integer or binary encoded integer
  //              1, if val is atom or binary string
  //             -1  either integer or atom
  // Convert:
  //   is_name <= 0: (10)             -> 10
  //   is_name <= 0: (<<"10">>)       -> 10
  //   is_name != 0: ('CheckSum')     -> 10
  //   is_name != 0: (<<"CheckSum">>) -> 10
  bool field_code(ErlNifEnv* env, ERL_NIF_TERM fld, int& code, int is_name) const;

private:
  Persistent*           m_pers;
  std::string           m_name;
  ERL_NIF_TERM          m_name_am;
  std::string           m_app_name;
  ERL_NIF_TERM          m_app_name_am;
  bool                  m_elixir;
  std::string           m_so_path;
  void*                 m_so_handle;
  std::vector<Field>    m_fields;
  NameToTagMap          m_name_to_tag_map;
  AtomToTagMap          m_atom_to_tag_map;
};

struct NonUniqueVariant : public std::exception
{
  NonUniqueVariant(std::string const& var_name) : m_name(var_name) {}

  const char* name() const { return m_name.c_str(); }
private:
  std::string m_name;
};

template<int N> struct power10    { static constexpr uint64_t value = 10 * power10<N-1>::value; };
template<>      struct power10<0> { static constexpr uint64_t value = 1; };

inline uint64_t quick_pow10(size_t n) {
  assert(n < 20);
  static uint64_t s_table[] = {
    power10< 0>::value, power10< 1>::value, power10< 2>::value, power10< 3>::value,
    power10< 4>::value, power10< 5>::value, power10< 6>::value, power10< 7>::value,
    power10< 8>::value, power10< 9>::value, power10<10>::value, power10<11>::value,
    power10<12>::value, power10<13>::value, power10<14>::value, power10<15>::value,
    power10<16>::value, power10<17>::value, power10<18>::value, power10<19>::value,
  };

  return s_table[n];
}

//------------------------------------------------------------------------------
// Main FIX variant creation function
//------------------------------------------------------------------------------

// Returns {VariantName, ApplicationName} tuple
extern "C" {
  typedef std::pair<const char*, const char*> (*fix_variant_name_fun)();
  typedef const char* (*fix_target_compiler_fun)();
  typedef std::vector<Field> (*fix_create_fields_fun)(FixVariant* pers);
}

//------------------------------------------------------------------------------
// Utility functions
//------------------------------------------------------------------------------
template <typename T, typename Ch = unsigned char>
const Ch* str_to_int(const Ch* s, const Ch* end, T& res, char delim = '\0', char delim2 = '\0')
{
  assert(s <= end);

  T sign   = 1;
  T n      = 0;
  auto beg = s;
  if (s < end) [[likely]] {
    if (*s == '-') {
      sign   = -1;
      ++s;
    } else if (*s == '+') [[unlikely]]
      ++s;
  }
  for (; s != end && *s >= '0' && *s <= '9'; ++s)
    n = n*10 + (*s - '0');

  if (beg == s) [[unlikely]]
    return nullptr;

  if (s == end) {
    if (delim || delim2) [[unlikely]]
      return nullptr;
  } else if (!((delim  && *s == (Ch)delim) ||
               (delim2 && *s == (Ch)delim2))) [[unlikely]]
    return nullptr;

  res = sign*n;
  return s;
}

template <typename T, typename U, typename Ch, bool Zero = true, unsigned Base = 10u>
Ch* int_to_str(T value, Ch* result, U& size)
{
  static_assert(std::is_integral_v<T>, "Integral required");
  static_assert(Base <= 16);

  //static constexpr int s_size = sizeof(T)*2 + sizeof(T)/2 +
  //                              (std::is_signed_v<T> ? 2 : 1); // '\0' and '-'
  //assert(size >= s_size);

  char* p = (char*)result;
  T     tmp;

  do {
    tmp    = value;
    value /= Base;
    *p++   = "fedcba9876543210123456789abcdef" [15 + (tmp - value * Base)];
  } while (value);

  // Apply negative sign
  if constexpr(std::is_signed_v<T>)
    if (value < 0) *p++ = '-';

  auto end = (Ch*)p;

  if constexpr(Zero)
    *p = '\0';

  // Reverse
  for(auto q=(char*)result; q < --p; q++)
    std::swap(*p, *q);

  //p      = result;
  //result = end;
  //return p;
  size = U(end - result);
  return result;
}

template <int N, typename Ch, typename U>
Ch* decimal_to_str(long magnitude, int prec, Ch (&buf)[N], U& size)
{
  long pow = 1;
  for (auto i=0; i < prec; ++i) pow *= 10;
  auto mm  = long(magnitude / pow);
  auto pp  = magnitude  -  mm*pow;
  auto tmp = (char*)buf;

  size = U(prec==0
            ? snprintf(tmp, N, "%ld", magnitude)
            : snprintf(tmp, N, "%ld.%0*ld", mm, prec, pp));
  return buf;
}

struct MagnitudeBigInt {
  // Creates a small BigInteger (< 256 digits) Erlang term that corresponds to
  // the magnitude of a number encoded in a string.  E.g. "123.45678901234567890123"
  // will be returned as a BigInt 12345678901234567890123.
  static ERL_NIF_TERM
  encode(ErlNifEnv* env, int sign, const unsigned char* begin, const unsigned char* end)
  {
    std::vector<unsigned char> result;
    result.reserve(4 + approx_digits(end - begin));
    result.push_back(131);         // Version byte
    result.push_back(110);         // Small BIG integer
    result.push_back(0);           // Length placeholder
    result.push_back(sign < 0);    // Sign byte
    convert_to_base256(result, result.begin()+4, (const char*)begin, (const char*)end);
    auto size = result.size() - 4; // Get byte length

    if (size > 255) [[unlikely]]
      return 0;

    result[2] = size;              // Update byte length

    ERL_NIF_TERM out;
    auto data = &*result.begin();
    if (!enif_binary_to_term(env, data, result.size(), &out, 0)) [[unlikely]]
      return 0;
    return out;
  }

private:
  static void convert_to_base256(
    std::vector<unsigned char>& result,
    std::vector<unsigned char>::iterator it,
    const char* begin, const char* end)
  {
    result.push_back(*begin++ - '0');

    for(; begin != end; ++begin)
    {
      if (*begin == '.') [[unlikely]] continue; // skip the '.'
      mul10(result, it);           // multiply result by 10
      add(result, it, *begin-'0'); // add current digit
    }
  }

  static size_t approx_digits(size_t decimal_dig_count) {
    const auto factor = std::log(10) / std::log(256);
    return std::ceil(factor * decimal_dig_count);
  }

  static void add(
    std::vector<unsigned char>& num,
    std::vector<unsigned char>::iterator start,
    int digit
  ) {
    for (auto end=num.end(); digit && start != end; ++start) {
      int x  = *start + digit;
      *start = x & 255;
      digit  = x >> 8;
    }
    if (digit)
      num.push_back(digit);
  }

  static void mul10(
    std::vector<unsigned char>& num,
    std::vector<unsigned char>::iterator start
  ) {
    auto carry = 0;
    for (auto end=num.end(); start != end; ++start) {
      carry   = 10 * *start + carry;
      *start  = static_cast<uint8_t>(carry);
      carry >>= 8;
    }
    if (carry)
      num.push_back(carry);
  }
};

inline std::unique_ptr<ERL_NIF_TERM, void(*)(ERL_NIF_TERM*)>
unique_term_ptr(void* ptr)
{
  return {(ERL_NIF_TERM*)ptr, [](ERL_NIF_TERM* p){ if (p) free((void*)p); }};
}

// A unique smart pointer that releases ErlNifBinary when it goes out of scope
inline std::unique_ptr<ErlNifBinary, void(*)(ErlNifBinary*)>
guard(ErlNifBinary& bin)
{
  return {&bin, [](ErlNifBinary* p){ if (p) enif_release_binary(p); }};
}

// A unique smart pointer that releases FixVariant when it goes out of scope
inline std::unique_ptr<FixVariant, void(*)(FixVariant*)>
guard(FixVariant* fvar)
{
  return {fvar, [](FixVariant* p){ if (p) delete p; }};
}

std::tuple<ERL_NIF_TERM, const char*, int>
do_decode_value
(
  const Field* field, ErlNifEnv* env,
  const unsigned char*& p,   const unsigned char* end,
  DataType dtype, DoubleFmt  double_fmt, TimeFmt time_fmt,
  bool ret_binary, char soh, int&     next_data_length,
  const unsigned char* msg_begin = nullptr
);

namespace {
  template<int N, char... Chars>
  struct to_int_helper;

  template<int N, char C>
  struct to_int_helper<N, C> {
    static const size_t value = ((size_t)C) << (8*N);
  };

  template <int N, char C, char... Tail>
  struct to_int_helper<N, C, Tail...> {
    static const size_t value =
      ((size_t)C) << (8*N) | to_int_helper<N-1, Tail...>::value;
  };
}

// Compile-time list of chars to integer converter
template <char... Chars>
struct to_int {
  static constexpr size_t value() {
    static_assert(sizeof...(Chars) <= 8, "Invalid size!");
    return to_int_helper<(sizeof...(Chars))-1, Chars...>::value;
  }
};

/// Convert list of chars to integer (up to 8 bytes).
/// I.e. to_int_v<'A','B','C','Z'> is a compile-time equivalent of
/// size_t("\0\0\0\0ABCZ"). This type of value can be quickly used in switch
/// statements instead of doing string comparisons.
template <char... Chars>
inline constexpr uint64_t CINT = to_int<Chars...>::value();

/// Convert string to its integer representation for short strings (<= 8 bytes)
inline constexpr uint64_t hash_val(const char* s, int len) {
  assert(len <= 8);
  uint64_t res = 0, i = 0;
  for(auto p = s+len-1; p >= s; --p, i += 8)
    res |= (uint64_t)*p << i;
  return res;
}

inline bool decode_timestamp(tm& out, int& us, const char* p, size_t size, bool utc)
{
  int  y=0, mon=0, d=0, h=0, m=0, s=0;
  bool res;

  us = 0;

  auto parse = [p](int offset, int len, int &i)
  {
    auto b = p + offset;
    return str_to_int(b, b + len, i) != nullptr;
  };

  switch (size)
  {
    case 8:  // Format: YYYYMMDD
      res  = parse(0, 4, y) && parse(4, 2, mon) && parse(6, 2, d);
      h = m = s = 0;
      break;
    case 17: // Format: YYYYMMDD-HH:MI:SS
      res  = parse(0, 4, y) && parse(4,  2, mon) && parse(6,  2, d) &&
             parse(9, 2, h) && parse(12, 2, m)   && parse(15, 2, s);
      break;
    case 21: // Read milliseconds: YYYYMMDD-HH:MI:SS.ttt
      res  = parse(0, 4, y) && parse(4,  2, mon) && parse(6,  2, d) &&
             parse(9, 2, h) && parse(12, 2, m)   && parse(15, 2, s) &&
             parse(18,3, us);
      us  *= 1000;
      break;
    case 24:  // Read microseconds: YYYYMMDD-HH:MI:SS.tttttt
      res  = parse(0, 4, y) && parse(4,  2, mon) && parse(6,  2, d) &&
             parse(9, 2, h) && parse(12, 2, m)   && parse(15, 2, s) &&
             parse(18,6, us);
      break;
    default:
      return false;
  }

  out = {
    .tm_sec  = s,
    .tm_min  = m,
    .tm_hour = h,
    .tm_mday = d,
    .tm_mon  = mon  - 1,   // Months are between 0-11
    .tm_year = y - 1900,   // Years since 1900
  };

  return res;
}

inline int64_t decode_timestamp(ErlNifEnv* env, const char* p, size_t size, bool utc=true)
{
  struct tm t; int us;
  if (!decode_timestamp(t, us, p, size, utc)) [[unlikely]]
    return -1;

  uint64_t epoch = utc ? timegm(&t) : mktime(&t);
  return   epoch * 1'000'000 + us;
}

template <int N, typename Ch = char>
int encode_timestamp(Ch (&buf)[N], uint64_t usecs, TsType tt = TsType::Seconds,
                     bool utc=true)
{
  static_assert(N > 24);

  auto secs = usecs / 1'000'000;
  int  us   = int(usecs - secs * 1'000'000);

  struct tm tm;
  auto res = utc ? gmtime_r   ((const time_t*)&secs, &tm)
                 : localtime_r((const time_t*)&secs, &tm);
  if (res == nullptr)
    return 0;

  if (tt == TsType::Millisec) us /= 1000;

  const auto fmt = tt == TsType::Seconds  ? "%04d%02d%02d-%02d:%02d:%02d"
                 : tt == TsType::Millisec ? "%04d%02d%02d-%02d:%02d:%02d.%03d"
                                          : "%04d%02d%02d-%02d:%02d:%02d.%06d";

  return snprintf((char*)buf, N, fmt,
                  tm.tm_year+1900, tm.tm_mon+1, tm.tm_mday,
                  tm.tm_hour, tm.tm_min, tm.tm_sec, us);
}

inline ERL_NIF_TERM
encode_timestamp(ErlNifEnv* env, uint64_t usecs, TsType tt=TsType::Seconds,
                 bool utc=true)
{
  char buf[64];

  auto n = encode_timestamp(buf, usecs, tt, utc);

  if (n == 0) [[unlikely]]
    return enif_make_badarg(env);

  ERL_NIF_TERM ret;

  auto data = enif_make_new_binary(env, n, &ret);
  if (!data)
    return enif_raise_exception(env, am_enomem);

  memcpy(data, buf, n);

  return ret;
}

inline std::string term_to_field_name(ErlNifEnv* env, ERL_NIF_TERM term)
{
  char buf[128];
  if (enif_get_atom(env, term, buf, sizeof(buf), ERL_NIF_LATIN1) == 0)
    return std::string(buf);

  long lname;
  if (enif_get_long(env, term, &lname) == 0)
    return std::to_string(lname);

  ErlNifBinary bname;
  if (enif_inspect_binary(env, term, &bname) == 0)
    return std::string((const char*)bname.data, bname.size);

  return std::string();
}

struct ErlMapIterator {
  ErlMapIterator() : m_iter{}, m_initialized(false) {}
  ErlMapIterator(ErlNifEnv *env, ERL_NIF_TERM map) { init(env, map); }

  ~ErlMapIterator() {
    if (m_initialized)
      enif_map_iterator_destroy(m_env, &m_iter);
  }

  bool init(ErlNifEnv *env, ERL_NIF_TERM map) {
    m_env         = env;
    m_initialized =
      enif_map_iterator_create(env, map, &m_iter, ERL_NIF_MAP_ITERATOR_FIRST);
    return m_initialized;
  }

  bool next(ERL_NIF_TERM& key, ERL_NIF_TERM& value) {
    assert(m_initialized);
    if (!enif_map_iterator_get_pair(m_env, &m_iter, &key, &value))
      return false;

    enif_map_iterator_next(m_env, &m_iter);
    return true;
  }
private:
  ErlNifEnv*        m_env;
  ErlNifMapIterator m_iter;
  bool              m_initialized;
};

//------------------------------------------------------------------------------
// Tag/Value parser
//------------------------------------------------------------------------------

// Parser state
struct ParserState : public DataType {
  enum {
    CODE = DataType::__LAST,
  };
  ParserState(int v)      : DataType(v) {}
  ParserState(DataType v) : DataType(v) {}
};

ERL_NIF_TERM
do_split(FixVariant* fvar, ErlNifEnv* env,
         const unsigned char*& begin, const unsigned char* end,
         bool ret_binary, bool full, DoubleFmt double_fmt, TimeFmt time_fmt);

//------------------------------------------------------------------------------
// Implementation
//------------------------------------------------------------------------------

// Use erl_make_copy() to copy shared binaries created by this function.
// http://erlang.org/pipermail/erlang-questions/2019-February/097247.html
// NOTE: check for am_enomem upon return!
inline ERL_NIF_TERM create_binary(ErlNifEnv* env, const std::string_view& s)
{
  assert(env);

  ErlNifBinary bin;
  if (!enif_alloc_binary(s.size(), &bin)) [[unlikely]]
    return am_enomem;
  memcpy(bin.data, s.data(), s.size());
  return enif_make_binary(env, &bin);
}

inline ERL_NIF_TERM create_binary(ErlNifEnv* env, int num)
{
  assert(env);

  char buf[24];
  int  n = snprintf(buf, sizeof(buf), "%d", num);
  return create_binary(env, std::string_view(buf, n));
}

// NOTE: check for am_enomem upon return!
template <typename Ch>
inline ERL_NIF_TERM create_binary(ErlNifEnv* env, const Ch* s, int len)
{
  return create_binary(env, std::string_view((const char*)s, len));
}

template <class... Args>
inline std::string to_string(const char* fmt, Args&&... args) {
  char buf[2048];
  snprintf(buf, sizeof(buf), fmt, std::forward<Args>(args)...);
  return buf;
}

#define MAKE_DECODE_ERROR(Var, Env, Err, Pos, ...) \
  make_decode_error(FILE_SRC_LOCATION, Var, Env, Err, Pos, __VA_ARGS__)
#define MAKE_ENCODE_ERROR(Fld, Env, ...) \
  make_encode_error(FILE_SRC_LOCATION, Fld, Env, __VA_ARGS__)

template <int N>
ERL_NIF_TERM
make_decode_error(
  const char (&src)[N], const FixVariant* var, ErlNifEnv* env, const char* error,
  long pos, int tag, int err_line = 0)
{
  const char* err_loc = basename(src);

  char err_loc_buf[128];

  // Overwrite the source location of err_line != 0
  if (err_line) {
    auto p = strchr(err_loc, ':');
    ASSERT(p, err_loc, src+N-1);
    p++;
    auto len = p - err_loc;
    strncpy(err_loc_buf, err_loc, len);
    int_to_str(err_line, err_loc_buf+len, len);
    err_loc = err_loc_buf;
  }

  if (var->is_elixir()) {
    /*
    %{'__exception__' => true, '__struct__' => 'Elixir.FIX.DecodeError',
      message => <<"FIX decode error">>, tag => Tag, pos => Pos, reason => Error,
      src => <<"file:line">>}
    */
    char buf[128];
    auto len =
      snprintf(buf, sizeof(buf),
               "Cannot decode FIX field (tag=%d, pos=%ld): %s", tag, pos, error);

    ERL_NIF_TERM keys[] = {
      am_exception,
      am_struct,
      am_message,
      am_tag,
      am_pos,
      am_reason,
      am_src
    };

    auto reason = create_binary(env, buf, len);

    ERL_NIF_TERM values[] = {
      am_true,
      am_decode_error,
      reason,
      enif_make_long(env, tag),
      enif_make_long(env, pos),
      enif_make_atom(env, error),
      create_binary (env, err_loc)
    };

    if (reason != am_enomem) [[likely]] {
      ERL_NIF_TERM map;

      if (enif_make_map_from_arrays(env, keys, values,
                                    std::extent<decltype(keys)>::value, &map))
        return enif_raise_exception(env, map);
    }
    // Follow through with the general return
  }

  return enif_raise_exception(env,
          enif_make_tuple5(env, am_fix,
          enif_make_int(env,  tag),
          enif_make_atom(env, error),
          enif_make_long(env, pos),
          create_binary(env,  err_loc)));
}

template <int N, typename... Args>
ERL_NIF_TERM make_encode_error
(
  const char (&src)[N], Field const* fld, ErlNifEnv* env,
  const char err[], Args&&... args
)
{
  char buf[256];
  auto len = snprintf(buf, sizeof(buf),
                      "Cannot encode FIX field '%s': ", fld->name());
  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wformat-security"
  len += snprintf(buf+len, sizeof(buf)-len, err, std::forward<Args>(args)...);
  #pragma GCC diagnostic pop

  auto reason = create_binary(env, buf, len);

  if (reason == am_enomem) [[unlikely]]
    return enif_raise_exception(env, reason);

  if (fld->is_elixir()) {
    /*
    %{'__exception__' => true, '__struct__' => 'Elixir.FIX.EncodeError',
      message => <<"FIX parser error">>, tag => tag, pos => pos, reason => error,
      src => <<"file:line">>}
    */
    ERL_NIF_TERM keys[] = {
      am_exception,
      am_struct,
      am_tag,
      am_message,
      am_src
    };

    ERL_NIF_TERM values[] = {
      am_true,
      am_encode_error,
      fld->get_atom(),
      reason,
      create_binary(env, basename(src))
    };

    ERL_NIF_TERM map;

    if (enif_make_map_from_arrays(env, keys, values,
                                  std::extent<decltype(keys)>::value, &map))
      return enif_raise_exception(env, map);
    // Follow through with the general return
  }

  return enif_raise_exception(env,
          enif_make_tuple3(env, am_fix, fld->get_atom(), reason));
}

inline ERL_NIF_TERM safe_make_atom(ErlNifEnv* env, const char* am)
{
  auto res = enif_make_atom(env, am);
  if (!res)
    throw std::runtime_error("Couldn't create atom");
  return res;
}

//------------------------------------------------------------------------------
inline ERL_NIF_TERM
FieldChoice::get_atom(ErlNifEnv* env) const {
  if (atom == 0) [[unlikely]]
    atom = enif_make_atom(env, descr);

  return atom;
}

//------------------------------------------------------------------------------
// Field implementation
//------------------------------------------------------------------------------

inline Field::Field()
  : m_var         (nullptr)
  , m_id          (0)
  , m_type        (FieldType::UNDEFINED)
  , m_dtype       (DataType::UNDEFINED)
  , m_atom        (0)
  , m_ch_len      (0)
  , m_max_val_len (0)
  , m_choices     {}
  , m_len_field   (nullptr)
  , m_len_field_id(0)
{}

//------------------------------------------------------------------------------
template <typename DecF>
Field::Field
(
  FixVariant*                 var,
  int                         id,
  const char*                 name,
  enum FieldType              type,
  DataType                    dtype,
  std::vector<FieldChoice>&&  choices,
  const char*                 len_field,      // Name of the length field for 'NUMINGROUP' and 'DATA' field types
  const int                   len_field_id,   // ID of the length field
  DecF                        dec_fun
)
  : m_var         (var)
  , m_id          (id)
  , m_id_str      (std::to_string(id))
  , m_name        (name)
  , m_type        (type)
  , m_dtype       (dtype)
  , m_atom        (0)
  , m_ch_len      (choices.size())
  , m_max_val_len (0)
  , m_choices     (std::move(choices))
  , m_len_field   (len_field)
  , m_len_field_id(len_field_id)
  , m_decf        (dec_fun)
{
  assert(var);
  assert(var->env());
  for(auto& fc : m_choices) {
    auto val  = create_binary(var->env(), fc.value);
    if  (val == am_enomem) [[unlikely]]
      throw enomem_error("Failed to allocate %d bytes of memory for field#%d",
                         strlen(fc.value), id);

    m_max_val_len = std::max(strlen(fc.value), size_t(m_max_val_len));
    m_atom_map.emplace(enif_make_atom(var->env(), fc.descr), val);
    m_name_map.emplace(std::string_view(fc.value, strlen(fc.value)), &fc);
  }
}

inline Field::Field(Field const& a_rhs)
  : m_var         (a_rhs.m_var)
  , m_id          (a_rhs.m_id)
  , m_id_str      (a_rhs.m_id_str)
  , m_name        (a_rhs.m_name)
  , m_type        (a_rhs.m_type)
  , m_dtype       (a_rhs.m_dtype)
  , m_atom        (a_rhs.m_atom)
  , m_ch_len      (a_rhs.m_ch_len)
  , m_max_val_len (a_rhs.m_max_val_len)
  , m_len_field   (a_rhs.m_len_field)
  , m_len_field_id(a_rhs.m_len_field_id)
  , m_decf        (a_rhs.m_decf)
{
  m_choices  = a_rhs.m_choices;
  m_atom_map = a_rhs.m_atom_map;
  m_name_map = a_rhs.m_name_map;
}

inline Field::Field(Field&& a_rhs)
  : m_var         (a_rhs.m_var)
  , m_id          (a_rhs.m_id)
  , m_id_str      (std::move(a_rhs.m_id_str))
  , m_name        (a_rhs.m_name)
  , m_type        (a_rhs.m_type)
  , m_dtype       (a_rhs.m_dtype)
  , m_atom        (a_rhs.m_atom)
  , m_ch_len      (a_rhs.m_ch_len)
  , m_max_val_len (a_rhs.m_max_val_len)
  , m_len_field   (a_rhs.m_len_field)
  , m_len_field_id(a_rhs.m_len_field_id)
  , m_decf        (a_rhs.m_decf)
{
  m_choices.swap(a_rhs.m_choices);
  m_atom_map.swap(a_rhs.m_atom_map);
  m_name_map.swap(a_rhs.m_name_map);
}

Field::~Field()
{
  m_atom_map.clear();
  m_name_map.clear();
  m_choices.clear();
}

inline void Field::operator=(Field&& a_rhs)
{
  m_var          = a_rhs.m_var;
  m_id           = a_rhs.m_id;
  m_id_str       = std::move(a_rhs.m_id_str);
  m_name         = a_rhs.m_name;
  m_type         = a_rhs.m_type;
  m_dtype        = a_rhs.m_dtype;
  m_atom         = a_rhs.m_atom;
  m_ch_len       = a_rhs.m_ch_len;
  m_max_val_len  = a_rhs.m_max_val_len;
  m_len_field    = a_rhs.m_len_field;
  m_len_field_id = a_rhs.m_len_field_id;

  m_choices.swap(a_rhs.m_choices);
  m_atom_map.swap(a_rhs.m_atom_map);
  m_name_map.swap(a_rhs.m_name_map);
  m_decf.swap(a_rhs.m_decf);
}

inline void Field::operator=(Field const& a_rhs)
{
  m_var          = a_rhs.m_var;
  m_id           = a_rhs.m_id;
  m_id_str       = a_rhs.m_id_str;
  m_name         = a_rhs.m_name;
  m_type         = a_rhs.m_type;
  m_dtype        = a_rhs.m_dtype;
  m_atom         = a_rhs.m_atom;
  m_ch_len       = a_rhs.m_ch_len;
  m_max_val_len  = a_rhs.m_max_val_len;
  m_len_field    = a_rhs.m_len_field;
  m_len_field_id = a_rhs.m_len_field_id;
  m_decf         = a_rhs.m_decf;

  m_choices      = a_rhs.m_choices;
  m_atom_map     = a_rhs.m_atom_map;
  m_name_map     = a_rhs.m_name_map;
}

inline int
Field::debug()      const { assert(m_var); return m_var->debug(); }

inline Persistent   const*
Field::pers()       const { assert(m_var); return m_var->pers(); }

inline bool
Field::is_elixir()  const { return variant()->is_elixir(); }

inline ERL_NIF_TERM
Field::get_atom()   const
{
  if (m_atom == 0) [[unlikely]]
    m_atom = enif_make_atom(m_var->env(), m_name.data());

  return m_atom;
}

inline ERL_NIF_TERM
Field::meta(ErlNifEnv* env) const
{
  std::vector<ERL_NIF_TERM> choices;
  choices.reserve(m_choices.size());

  for (auto c : m_choices) {
    auto val = create_binary(env, c.value);
    if (val == am_enomem) [[unlikely]]
      return enif_raise_exception(m_var->env(), am_enomem);

    choices.push_back(
      enif_make_tuple2(env, val, c.get_atom(env)));
  }

  std::vector<ERL_NIF_TERM> res{{
    enif_make_tuple2(env, enif_make_atom(env, "id"),   enif_make_long(env, m_id)),
    enif_make_tuple2(env, enif_make_atom(env, "name"), get_atom()),
    enif_make_tuple2(env, enif_make_atom(env, "data_type"),
                          enif_make_atom(env, m_dtype.to_string(m_dtype.value))),
    enif_make_tuple2(env, enif_make_atom(env, "type"),
                          enif_make_atom(env, type_to_string(m_type))),
  }};
  if (m_choices.size())
    res.push_back(
      enif_make_tuple2(env,
        enif_make_atom(env, "values"),
        m_choices.empty()
          ? enif_make_list(env,0)
          : enif_make_list_from_array(env, &choices.front(), m_choices.size())));

  return enif_make_list_from_array(env, &res.front(), res.size());
}

inline FieldChoice const*
Field::value(unsigned idx) const
{
  return idx < m_choices.size() ? &m_choices[idx] : nullptr;
}

inline FieldChoice const*
Field::value(std::string_view const& val) const
{
  auto   it =  m_name_map.find(val);
  return it == m_name_map.end() ? nullptr : it->second;
}

inline ERL_NIF_TERM
Field::value_atom(unsigned idx) const
{
  return idx < m_choices.size() ? m_choices[idx].get_atom(m_var->env())
                                : enif_make_badarg(m_var->env());
}

inline bool
Field::atom_to_bin(ErlNifEnv* env, ERL_NIF_TERM atom, ErlNifBinary& res) const
{
  auto it = m_atom_map.find(atom);
  if (it == m_atom_map.end()) [[unlikely]]
    return false;
  auto& v = it->second;
  return !!enif_inspect_binary(env, v, &res);
}

template <typename Def>
inline ERL_NIF_TERM Field::decode_value(
  ErlNifEnv* env, const char* val, int len, const Def& def) const
{
  if (!m_decf || len < 1 || len > m_max_val_len) [[unlikely]]
    return def();

  auto res = m_decf(*this, env, val, len);

  return IS_LIKELY(res != 0) ? res : def();
}

inline ERL_NIF_TERM
Field::decode(ErlNifEnv* env, const char* val, int len,
              DoubleFmt dfmt, TimeFmt tfmt) const
{
  int next_data_length = -1;
  const char       soh = '\0';
  auto               p = (const unsigned char*)val;

  auto [value, err_str, err_line] =
    do_decode_value(this, env, p, p+len, m_dtype,
                    dfmt, tfmt, true, soh, next_data_length);

  if (value == am_nil) [[unlikely]]
    return MAKE_DECODE_ERROR(variant(), env, err_str, 0, m_id, err_line);

  return value;
}

// Return: ok | exception Reason::binary()
// Store result in `res` as a binary: <<Tag/binary, $=, Value/binary, 1>>
inline ERL_NIF_TERM
Field::encode(ErlNifEnv* env, int& offset, ErlNifBinary& res, ERL_NIF_TERM val)
{
  unsigned char tmp[256];
  ErlNifBinary  bval;
  unsigned      grp_len = 0;

  if (has_values() && enif_is_atom(env, val)) {
    if (!atom_to_bin(env, val, bval)) [[unlikely]]
      return MAKE_ENCODE_ERROR(this, env, "invalid atom value");
  }
  else if (!enif_inspect_binary(env, val, &bval)) {
    bval.data = tmp;

    switch (m_dtype) {
      case DataType::CHAR: {
        int n;
        if (!enif_get_int(env, val, &n) && n >= 0 && n < 256) [[unlikely]]
          return MAKE_ENCODE_ERROR(this, env, "invalid integer");
        tmp[0] = (char)n;
        bval.size = 1;
        break;
      }
      case DataType::INT: {
        long n;
        if (!enif_get_long(env, val, &n)) [[unlikely]]
          return MAKE_ENCODE_ERROR(this, env, "invalid integer");

        int_to_str(n, tmp, bval.size);
        assert(bval.size < sizeof(tmp)-1);
        break;
      }
      case DataType::DOUBLE: {
        double d;
        int    prec, arity;
        long   magn;
        const  ERL_NIF_TERM* tup;
        ERL_NIF_TERM magnitude, precision;

        if (enif_get_double(env, val, &d))
          bval.size = snprintf((char*)tmp, sizeof(tmp), "%.8f", d);
        // {decimal, Magnitude, Precision}
        else if (enif_get_tuple   (env, val, &arity, &tup) && arity == 3 &&
                 enif_is_identical(am_decimal, tup[0]) &&
                 enif_get_long    (env, tup[1], &magn) &&
                 enif_get_int     (env, tup[2], &prec))
          decimal_to_str(magn, prec, tmp, bval.size);
        // #{magnitude => Magnitude, precision => Precision}
        else if (enif_is_map(env, val) &&
                 enif_get_map_value(env, val, am_magnitude, &magnitude) &&
                 enif_get_map_value(env, val, am_precision, &precision) &&
                 enif_get_long(env, magnitude, &magn)                   &&
                 enif_get_int (env, precision, &prec))
          decimal_to_str(magn, prec, tmp, bval.size);
        else
          return MAKE_ENCODE_ERROR(this, env, "invalid float");
        break;
      }
      case DataType::BOOL:
        if      (enif_is_identical(am_true,  val)) tmp[0] = 'Y';
        else if (enif_is_identical(am_false, val)) tmp[0] = 'N';
        else
          return MAKE_ENCODE_ERROR(this, env, "invalid boolean");
        bval.size = 1;
        break;

      case DataType::STRING: {
        auto n = enif_get_string(env, val, (char*)tmp, sizeof(tmp), ERL_NIF_LATIN1);
        if (!n) [[unlikely]]
          return MAKE_ENCODE_ERROR(this, env, "invalid string");
        bval.size = std::abs(n)-1;  // n includes '\0'
        break;
      }
      case DataType::DATETIME: {
        long n;
        if (!enif_get_long(env, val, &n) ||
            !(bval.size = encode_timestamp(tmp, n, pers()->ts_type()))) [[unlikely]]
          return MAKE_ENCODE_ERROR(this, env, "invalid timestamp");
        break;
      }
      case DataType::GROUP:
        if (enif_is_empty_list(env, val))
          grp_len = 0;
        else if (!enif_get_list_length(env, val, &grp_len)) [[unlikely]]
          return MAKE_ENCODE_ERROR(this, env, "list of group maps expected");

        int_to_str(grp_len, tmp, bval.size);
        break;

      case DataType::BINARY:
        // If value were a binary, it would be converted before the switch statement
        return MAKE_ENCODE_ERROR(this, env, "invalid binary");

      default:
        return MAKE_ENCODE_ERROR(this, env, "invalid data type");
    }
  } else if (m_dtype == DataType::GROUP)
    return MAKE_ENCODE_ERROR(this, env, "invalid group value");

  assert(bval.data);

  auto   tag_sz = m_id_str.size();
  auto   sz     = tag_sz + bval.size + 2 /* 2xSOH */;
  size_t needed = sz;
  unsigned char*  p;

  if (!offset) {
    assert(res.data == nullptr);
    if (!enif_alloc_binary(needed, &res))
      return MAKE_ENCODE_ERROR(this, env, "cannot allocate %d bytes", needed);
    p = res.data;
  } else {
    needed += offset;
    assert(res.data);
    if (needed > res.size && !enif_realloc_binary(&res, needed+256))
      return MAKE_ENCODE_ERROR(this, env, "cannot allocate %d bytes", needed+256);
    p = res.data + offset;
  }

  memcpy(p, m_id_str.data(), tag_sz);
  p   += tag_sz;
  *p++ = '=';
  memcpy(p, bval.data, bval.size);
  p   += bval.size;
  *p++ = SOH;

  offset += sz;

  // Serialize groups
  if (grp_len > 0) {
    assert(!enif_is_empty_list(env, val));

    ERL_NIF_TERM head,  list  = val;
    ERL_NIF_TERM        delim = 0;

    int i=0;

    while (enif_get_list_cell(env, list, &head, &list)) {
      // Expecting [#{Name => Value}]
      if (!enif_is_map(env, head)) [[unlikely]]
        return MAKE_ENCODE_ERROR(this, env, "group must be a map");

      ErlMapIterator it(env, head);

      ERL_NIF_TERM fldname, fldval;

      while (it.next(fldname, fldval)) {
        auto field = m_var->field(env, fldname);

        if (!field) [[unlikely]] {
          auto name = term_to_field_name(env, fldname);
          return name.empty()
            ? enif_raise_exception(env, am_badarg)
            : MAKE_ENCODE_ERROR(this, env, "undefined field %s", name.c_str());
        }

        if (i++ == 0) {
          if (delim == 0)
            delim = fldname;
          else if (!enif_is_identical(delim, fldname)) {
            char expect[128], got[128];
            if (!enif_get_atom(env, delim,   expect, sizeof(expect), ERL_NIF_LATIN1) ||
                !enif_get_atom(env, fldname, got,    sizeof(got),    ERL_NIF_LATIN1))
              return am_error;

            return MAKE_ENCODE_ERROR(this, env,
              "invalid first tag for group %s [%d/%d]: expected=%s, got=%s",
              name(), i, grp_len, expect, got);
          }
        }
        auto rres = field->encode(env, offset, res, fldval);
        if (rres != am_ok)
          return rres;
      }
    }
  }

  return am_ok;
}

//------------------------------------------------------------------------------
// Persistent
//------------------------------------------------------------------------------
inline Persistent::Persistent(std::vector<std::string> const& so_files,
                              int debug, TsType ts_type)
  : m_env(enif_alloc_env())
  , m_debug(debug)
  , m_ts_type(ts_type)
{
  for (auto& file : so_files)
  {
    if (!std::filesystem::exists(file))
      throw std::runtime_error
        (to_string("FIX Variant shared object file '%s' not found!", file.c_str()));

    // Load the FIX variant implementation
    DBGPRINT(debug, 1, "FIX: loading %s", file.c_str());

    load_fix_variant(file, true);
  }

  am_badarg         = make_atom("badarg");
  am_calendar       = make_atom("calendar");
  am_day            = make_atom("day");
  am_decimal        = make_atom("decimal");
  am_decode_error   = make_atom("Elixir.FIX.DecodeError");
  am_encode_error   = make_atom("Elixir.FIX.EncodeError");
  am_enomem         = make_atom("enomem");
  am_error          = make_atom("error");
  am_exception      = make_atom("__exception__");
  am_false          = make_atom("false");
  am_fix            = make_atom("fix");
  am_group          = make_atom("group");
  am_hour           = make_atom("hour");
  am_magnitude      = make_atom("magnitude");
  am_message        = make_atom("message");
  am_microsecond    = make_atom("microsecond");
  am_minute         = make_atom("minute");
  am_month          = make_atom("month");
  am_more           = make_atom("more");
  am_naive_calendar = make_atom("Elixir.Calendar.ISO");
  am_naive_struct   = make_atom("Elixir.NaiveDateTime");
  am_nil            = make_atom("nil");
  am_ok             = make_atom("ok");
  am_pos            = make_atom("pos");
  am_precision      = make_atom("precision");
  am_reason         = make_atom("reason");
  am_second         = make_atom("second");
  am_src            = make_atom("src");
  am_struct         = make_atom("__struct__");
  am_tag            = make_atom("tag");
  am_true           = make_atom("true");
  am_year           = make_atom("year");
}

inline Persistent::~Persistent()
{
  m_variants.clear();
  m_var_storage.clear();
  enif_free_env(m_env);
  m_env = nullptr;
}

inline bool Persistent::load_fix_variant(const std::string& so_file, bool replace)
{
  std::unique_lock<std::mutex> lock(m_mutex);

  try {
    auto p        = new FixVariant(this, so_file, replace);
    auto guardp   = guard(p);

    // Insert the FixVariant instance into storage
    m_var_storage.emplace(
      std::make_pair(p->name_atom(),std::unique_ptr<FixVariant>(p)));

    // Add indices
    m_variants.emplace(std::make_pair(p->name_atom(),     p)); // Idx by variant
    m_variants.emplace(std::make_pair(p->app_name_atom(), p)); // Idx by app_name

    guardp.release(); // This doesn't delete the pointer

    return true;
  }
  catch (NonUniqueVariant const& e) {
    DBGPRINT(m_debug, 1, "FIX: ignoring non-unique variant '%s'", e.name());
    return false;
  }
}

inline bool Persistent::unload_fix_variant(ERL_NIF_TERM variant_name, bool lock)
{
  std::unique_lock<std::mutex> guard(m_mutex, std::defer_lock);
  if (lock)
    guard.lock();

  auto it    = m_var_storage.find(variant_name);
  auto found = it != m_var_storage.end();

  if (found) {
    auto   p = it->second.get();
    assert(p);
    m_variants.erase(p->name_atom());
    m_variants.erase(p->app_name_atom());
    m_var_storage.erase(it);
  }

  return found;
}

inline ERL_NIF_TERM Persistent::make_atom(const char* am)
{
  return safe_make_atom(m_env, am);
}

FixVariant* Persistent::get(ERL_NIF_TERM variant)
{
  assert(enif_is_atom(m_env, variant));

  auto   it = m_variants.find(variant);
  return IS_UNLIKELY(it == m_variants.end()) ? nullptr : it->second;
}

bool Persistent::get(ERL_NIF_TERM var_name, FixVariant*& var_ptr)
{
  if (!enif_is_atom(m_env, var_name)) [[unlikely]]
    return false;

  var_ptr = get(var_name);

  return !!var_ptr;
}

//------------------------------------------------------------------------------
inline FixVariant
::FixVariant(Persistent* pers, std::string const& so_file, bool replace)
  : m_pers(pers)
  , m_so_path(so_file)
  , m_so_handle(dlopen(m_so_path.c_str(), RTLD_LAZY|RTLD_GLOBAL))
{
  char buf[2048];

  if (!m_so_handle) {
    std::string err = dlerror();
    snprintf(buf, sizeof(buf), "Cannot load fix library '%s': %s",
      so_file.c_str(), err.c_str());
    throw std::runtime_error(buf);
  }

  auto find_fun = [this, &buf, &so_file](const char* fun_name) {
    void*   sym = dlsym(m_so_handle, fun_name);

    if (!sym || dlerror()) {
      auto err  = std::string(dlerror());
      auto beg  = so_file.find_last_of('/');
      auto name = beg == std::string::npos ? so_file : so_file.substr(beg+1);
      snprintf(buf, sizeof(buf), "Cannot find entry point '%s' in '%s': %s",
        fun_name, name.c_str(), err.c_str());
      throw std::runtime_error(buf);
    }

    return sym;
  };

  auto get_variant_and_app_names = [&find_fun](const char* fun) {
    auto   f = reinterpret_cast<fix_variant_name_fun>(find_fun(fun));
    assert(f);
    return f();
  };

  auto is_elixir = [&find_fun](const char* fun) {
    auto   f = reinterpret_cast<fix_target_compiler_fun>(find_fun(fun));
    assert(f);
    return std::string(f()) == "elixir";
  };

  auto get_fields = [this, &find_fun](const char* fun) {
    auto    factory = reinterpret_cast<fix_create_fields_fun>(find_fun(fun));
    assert (factory);
    return (factory)(this);
  };

  auto names    = get_variant_and_app_names("fix_variant_name");

  if (names.first  == 0 || names.first[0] == '\0')
    throw std::runtime_error("Empty FIX variant in file: " + so_file);
  if (names.second == 0 || names.second[0] == '\0')
    throw std::runtime_error("Empty FIX variant app name in file: " + so_file);

  m_name        = names.first;
  m_app_name    = names.second;

  m_name_am     = enif_make_atom(pers->env(), m_name.c_str());
  m_app_name_am = enif_make_atom(pers->env(), m_app_name.c_str());

  if (m_pers->get(m_name_am) != nullptr) {
    if (replace)
      m_pers->unload_fix_variant(m_name_am, false);
    else {
      DBGPRINT(m_pers->debug(), 1,
               "FIX: variant '%s' (app=%s) already loaded",
               m_name.c_str(), m_app_name.c_str());
      dlclose(m_so_handle);
      m_so_handle = nullptr;
      throw NonUniqueVariant(m_name);
    }
  }

  m_elixir = is_elixir("fix_target_compiler");
  m_fields = get_fields("fix_create_fields");

  if (m_fields.size() == 0)
    throw std::runtime_error
      ("FIX variant '"+m_name+"' doesn't have any field definitions: "+so_file);

  DBGPRINT(m_pers->debug(), 1,
      "FIX: initializing variant '%s' (app=%s) with %lu fields from: %s (type=%s)",
      m_name.c_str(), m_app_name.c_str(), m_fields.size(), so_file.c_str(),
      replace ? "replace" : "preserve");

  for (int i=0; i < field_count(); ++i)
  {
    // Populate the map that converts field names to tags
    if (m_fields[i].id() == 0)
      continue;

    m_name_to_tag_map.emplace
      (std::make_pair(m_fields[i].name_str(), i));
    m_atom_to_tag_map.emplace
      (std::make_pair(enif_make_atom(env(), m_fields[i].name()), i));
    if (m_elixir && strncmp(m_fields[i].name(), "Elixir.", 7) == 0) {
      m_name_to_tag_map.emplace
        (std::make_pair(m_fields[i].name()+7, i));
      m_atom_to_tag_map.emplace
        (std::make_pair(enif_make_atom(env(), m_fields[i].name()+7), i));
    }
  }
}

inline FixVariant::~FixVariant()
{
  if (m_pers)
    DBGPRINT(m_pers->debug(), 1, "FIX: removing variant %s", m_name.c_str());

  m_fields.clear();
  m_name_to_tag_map.clear();
  m_atom_to_tag_map.clear();

  if (m_so_handle)
    dlclose(m_so_handle);
}

bool
FixVariant::field_code(ErlNifEnv* env, ERL_NIF_TERM fld, int& code, int is_name) const
{
  ErlNifBinary bin;

  if (enif_inspect_binary(env, fld, &bin)) {
    if (is_name > 0)
      code = find_tag_by_name(std::string_view((const char*)bin.data, bin.size));
    else if (!str_to_int((char*)bin.data, (char*)bin.data+bin.size, code)) [[unlikely]]
      code = is_name == 0 ? 0
           : find_tag_by_name(std::string_view((const char*)bin.data, bin.size));
  }
  else if (is_name != 0)
    code = enif_is_atom(env, fld) ? find_tag_by_atom(fld)
         : enif_get_int(env, fld, &code) ? code : 0;
  else if (is_name <= 0 && !enif_get_int(env, fld, &code)) [[unlikely]]
    code = 0;

  return code > 0 && code < field_count();
}

inline Field const*
FixVariant::field(ErlNifEnv* env, ERL_NIF_TERM tag) const
{
  int code;

  if (!field_code(env, tag, code, 1)) [[unlikely]]
    return nullptr;

  auto res = field(code);

  // The following is guaranteed by arg_code() call:
  assert(code > 0 && code < field_count());
  assert(res); // This is guaranteed by the check above

  return res;
}

inline Field*
FixVariant::field(ErlNifEnv* env, ERL_NIF_TERM tag)
{
  int code;

  if (!field_code(env, tag, code, 1)) [[unlikely]]
    return nullptr;

  auto res = field(code);

  // The following is guaranteed by arg_code() call:
  assert(code > 0 && code < field_count());
  assert(res); // This is guaranteed by the check above

  return res;
}

inline Field const*
FixVariant::field(unsigned idx) const
{
  return idx < m_fields.size() ? &m_fields[idx] : nullptr;
}

inline Field*
FixVariant::field(unsigned idx)
{
  return idx < m_fields.size() ? &m_fields[idx] : nullptr;
}

//------------------------------------------------------------------------------

inline bool check_end(const unsigned char* p, const unsigned char* end, char soh)
{
  return (p < end && *p == soh) || (p == end && !soh);
}

std::tuple<ERL_NIF_TERM, const char*, int>
do_decode_value
(
  const Field*      field, ErlNifEnv*           env,
  const unsigned char*& p, const unsigned char* end,
  DataType          dtype, DoubleFmt     double_fmt, TimeFmt time_fmt,
  bool         ret_binary, char                 soh,
  int&   next_data_length, const unsigned char* msg_begin
)
{
  assert(field);

  auto ptr = p;

  ERL_NIF_TERM value = am_nil;

  switch (dtype) {
    case DataType::INT: {
      long ival=0;
      p = str_to_int(ptr, end, ival, soh);
      if (p == nullptr) [[unlikely]]
        return std::make_tuple(am_nil, "expecting integer", __LINE__);

      // Field number 9 is special, as it indicates the length of the FIX
      // message payload, so treat it as integer
      auto is_length   = field->type() == FieldType::LENGTH && field->id() != 9;
      next_data_length = IS_UNLIKELY(is_length) ? ival : -1;

      auto def = [=]() { return enif_make_long(env, ival); };

      value = field->has_values()
            ? field->decode_value(env, (const char*)ptr, p-ptr, def)
            : enif_make_long(env, ival);

      break;
    }

    case DataType::DOUBLE: {
      uint64_t n=0, f=0;
      long     sign=1, i=0;

      if (p < end) [[likely]] {
        if (*p == '-') [[unlikely]] {
          sign = -1;
          ++p;
        } else if (*p == '+') [[unlikely]]
          ++p;
      }

      for(; p < end && *p >= '0' && *p <= '9'; ++p, ++i)
        n = n*10 + (*p - '0');

      long flen = 0;

      if (p != end) [[likely]] {
        if (*p == '.') [[likely]] {
          auto q = ++p; // ++p skips '.'

          for(; p<end && *p >= '0' && *p <= '9'; ++p)
            f = f*10 + (*p - '0');

          flen = p - q; // Length of fractional part
          i   += flen;  // Total # of digits
        }
      }

      const auto s_max_len = 19;
      const auto s_max     = (1ull << 63) - 1;
      uint64_t   mant      = 0;

      // If the long value results in the integer overflow, leave it as binary
      if (double_fmt == DoubleFmt::Decimal) {
        if (i > s_max_len) [[unlikely]]
          double_fmt = DoubleFmt::BigIntDecimal;
        else {
          mant = quick_pow10(flen)*n + f;
          if (mant > s_max) [[unlikely]]
            double_fmt = DoubleFmt::BigIntDecimal;
        }
      }

      switch (double_fmt) {
        case DoubleFmt::Decimal:
          value = enif_make_tuple3(env, am_decimal,
                    enif_make_long(env, sign * int64_t(mant)), enif_make_long(env, flen));
          break;
        case DoubleFmt::BigIntDecimal: {
          auto m = MagnitudeBigInt::encode(env, sign, ptr, p);
          if (m != 0) [[likely]] {
            value = enif_make_tuple3(env, am_decimal, m, enif_make_long(env, flen));
            break;
          }
          [[fallthrough]]; // if couldn't convert - fall through to the binary representation
        }
        case DoubleFmt::Binary: {
          auto sz   = p - ptr;
          auto data = enif_make_new_binary(env, sz, &value);

          if (!data) [[unlikely]]
            return std::make_tuple(am_nil, "out of memory", __LINE__);

          memcpy(data, ptr, sz);
          break;
        }
        case DoubleFmt::String:
          value = enif_make_string_len(env, (const char*)ptr, p-ptr, ERL_NIF_LATIN1);
          break;
        case DoubleFmt::Double:
        default: {
          double frac = double(f) / quick_pow10(flen);
          double res  = (double(mant) + frac) * sign;
          value       = enif_make_double(env, res);
          break;
        }
      }

      break;
    }

    case DataType::GROUP: {
      int ival=0;
      p = str_to_int(ptr, end, ival, soh); // Get the number of repeating groups
      if (p == nullptr)
        return std::make_tuple(am_nil, "invalid group count", __LINE__);

      value = enif_make_long(env, ival);
      break;
    }

    case DataType::BINARY: {
      // This field should have preceeded by the LENGTH integer field
      // that would have set the "next_data_length" variable
      if (next_data_length < 0)
        while (p < end && *p != soh) ++p;
      else {
        p += next_data_length;
        next_data_length = -1;
      }

      if (p > end) [[unlikely]]
        return std::make_tuple(am_nil, "invalid binary length", __LINE__);

      const auto len = p - ptr;

      if (ret_binary) {
        auto data = enif_make_new_binary(env, len, &value);
        if (!data)
          return std::make_tuple(am_nil, "out of memory", __LINE__);
        memcpy(data, ptr, len);
      } else {
        value = enif_make_tuple2(env,
                                  enif_make_uint(env, ptr-msg_begin),
                                  enif_make_uint(env, len));
      }
      break;
    }

    case DataType::STRING: {
      // If the previous field was LENGTH which set "next_data_length", use it
      if(next_data_length < 0) [[likely]]
        while (p < end && *p != soh) ++p;
      else {
        p += next_data_length;
        next_data_length = -1;
      }

      if (p > end) [[unlikely]]
        return std::make_tuple(am_nil, "invalid string length", __LINE__);

      auto def = [=]() {
        return ret_binary ? create_binary(env, ptr, p-ptr)
                          : enif_make_tuple2(env,
                              enif_make_uint(env, ptr-msg_begin),
                              enif_make_uint(env, p-ptr));
      };

      value = field->has_values()
            ? field->decode_value(env, (const char*)ptr, p-ptr, def)
            : def();

      break;
    }

    case DataType::DATETIME: {
      while (p < end && *p != soh) ++p;

      if (!check_end(p, end, soh)) [[unlikely]]
        return std::make_tuple(am_nil, "missing field delimiter", __LINE__);

      auto    len = p - ptr;
      auto    q   = reinterpret_cast<const char*>(ptr);
      int64_t ts  = 0;

      switch (time_fmt) {
        case TimeFmt::Binary:
          value = create_binary(env, ptr, len);
          break;
        case TimeFmt::String:
          value = enif_make_string_len(env, q, len, ERL_NIF_LATIN1);
          break;
        case TimeFmt::EpochUSec:
          ts    = decode_timestamp(env, q, len);
          value = enif_make_int64(env, ts);
          break;
        case TimeFmt::EpochMSec:
          ts    = decode_timestamp(env, q, len);
          value = enif_make_int64(env, ts / 1000);
          break;
        case TimeFmt::EpochSec:
          ts    = decode_timestamp(env, q, len);
          value = enif_make_int64(env, ts / 1'000'000);
          break;
        case TimeFmt::Naive: {
          tm t; int us;
          if (!decode_timestamp(t, us, q, len, true)) {
            ts = -1;
            break;
          }
          ERL_NIF_TERM keys[] = {
            am_struct,
            am_calendar,
            am_year,
            am_month,
            am_day,
            am_hour,
            am_minute,
            am_second,
            am_microsecond,
          };
          ERL_NIF_TERM values[] = {
            am_naive_struct,
            am_naive_calendar,
            enif_make_int(env, t.tm_year+1900),
            enif_make_int(env, t.tm_mon+1),
            enif_make_int(env, t.tm_mday),
            enif_make_int(env, t.tm_hour),
            enif_make_int(env, t.tm_min),
            enif_make_int(env, t.tm_sec),
            enif_make_tuple2(env, enif_make_int(env, us),
                                  enif_make_int(env, 6)),
          };
          if (!enif_make_map_from_arrays(
              env, keys, values, std::extent<decltype(keys)>::value, &value))
            return std::make_tuple(am_nil, "cannot create naive datetime map", __LINE__);
          break;
        }
        case TimeFmt::Tuple: {
          tm t; int us;
          if (!decode_timestamp(t, us, q, len, true))
            ts = -1;
          else
            value = enif_make_tuple2(env,
                      enif_make_tuple2(env,
                        enif_make_tuple3(env,
                          enif_make_int(env, t.tm_year+1900),
                          enif_make_int(env, t.tm_mon+1),
                          enif_make_int(env, t.tm_mday)
                        ),
                        enif_make_tuple3(env,
                          enif_make_int(env, t.tm_hour),
                          enif_make_int(env, t.tm_min),
                          enif_make_int(env, t.tm_sec)
                        )
                      ),
                      enif_make_int(env, us));
          break;
        }
        case TimeFmt::None:
        default:
          value = enif_make_tuple2(env, ptr-msg_begin, p-ptr);
          break;
      }

      if (ts < 0) [[unlikely]]
        return std::make_tuple(am_nil, "invalid datetime value", __LINE__);

      break;
    }

    case DataType::BOOL: {
      value = (*p == 'Y' || *p == 'y') ? am_true : am_false;
      ++p; // Advance 1 char

      break;
    }

    case DataType::CHAR: {
      ++p; // Advance 1 char

      auto def = [=]() { return enif_make_int(env, int(*ptr)); };

      value = field->has_values()
            ? field->decode_value(env, (const char*)ptr, p-ptr, def)
            : def();
      break;
    }
    default:
      return std::make_tuple(am_nil, "unsupported data type", __LINE__);
  }

  if (!check_end(p, end, soh)) [[unlikely]]
    return std::make_tuple(am_nil,
            p < end ? "invalid value" : "missing field delimiter", __LINE__);

  return std::make_tuple(value, "", __LINE__);
}

//------------------------------------------------------------------------------
inline ERL_NIF_TERM
do_split(FixVariant* fvar, ErlNifEnv* env,
         const unsigned char*&  begin,  const unsigned char* end,
         bool  ret_binary, bool full, DoubleFmt double_fmt, TimeFmt time_fmt)
{
  const auto len = int(end - begin);

  if (len < 25)
    return enif_make_tuple2(env, am_more, enif_make_int(env, 25 - len));

  if (memcmp("8=FIX", begin, 5) != 0) [[unlikely]]
    return MAKE_DECODE_ERROR(fvar, env, "missing_tag", 0, 8);

  char soh = 0;

  // Guess SOH (which is either '\1' or '|') by looking at the first delimiter
  for (auto* q = begin+9, *e = q+10; q != e; ++q)
    if (*q == SOH || *q == '|') {
      soh = *q;
      break;
    }

  // Did we find the SOH?
  if (soh == 0) [[unlikely]]
    return MAKE_DECODE_ERROR(fvar, env, "missing_soh", 0, 8);

  // Find FIX length (tag9) in prefix: "8=FIX.M.N|9=LLLL|"...
  const char s_prefix[] = {soh, '9', '='};
  auto f9 = (const unsigned char*)memmem(begin+5, len, s_prefix, sizeof(s_prefix));

  if (!f9) [[unlikely]]
    return MAKE_DECODE_ERROR(fvar, env, "missing_tag", 0, 9);

  f9 += 3; // Adjust for <<1,"9=">>

  int msg_len = 0;

  auto body = str_to_int(f9, end, msg_len, soh);
  if (body == nullptr)
    return MAKE_DECODE_ERROR(fvar, env, "invalid_tag", 0, 9);

  auto end_len = msg_len + 1+7; //  SOH + "10=XXX" + SOH
  auto msg_end = body + end_len;

  if (msg_end > end)
    return enif_make_tuple2(env, am_more, enif_make_long(env, msg_end - end));

  auto pcsum = msg_end-7;

  static const char s_checksum[] = {'1', '0', '='};
  if (memcmp(pcsum, s_checksum, sizeof(s_checksum)) != 0) [[unlikely]]
    return MAKE_DECODE_ERROR(fvar, env, "invalid_msg_length", msg_len, 9);

  msg_len = msg_end - begin;  // Complete length of current FIX message

  if (*(msg_end-1) != soh) [[unlikely]]
    return MAKE_DECODE_ERROR(fvar, env, "invalid_msg_terminator", msg_len, 0);

  DBGPRINT(fvar->debug(), 1, "FIX(%s): got message of size %d",
    fvar->name().c_str(), msg_len);

  DUMP(fvar->debug(), begin, msg_end);  // For debugging in case of a crash

  auto  state = ParserState(ParserState::CODE);
  int   code  = 0;
  auto* field = fvar->field(0);

  int  next_data_length = -1;
  int  reply_capacity   = 24;
  int  reply_size       = 0;
  auto reply       = unique_term_ptr(calloc(reply_capacity, sizeof(ERL_NIF_TERM)));
  auto ptr         = begin;
  auto val_begin   = ptr;
  auto val_end     = ptr;
  ERL_NIF_TERM tag = 0;

  if (!reply) [[unlikely]]
    return enif_raise_exception(env, am_enomem);

  // Add {Tag::atom(),  Val::any(), TagCode::integer(),
  //         {ValOffset::integer(), ValLen ::integer()}}
  auto append = [=, &reply, &reply_size, &val_begin, &val_end]
    (ERL_NIF_TERM tag, int code, ERL_NIF_TERM val)
  {
    // Don't include 'BeginString', 'BodyLength', 'CheckSum' when not requested
    if (!full && code >= 8 && code <= 10)
      return true;

    reply.get()[reply_size++] =
      enif_make_tuple4(env, tag, val, enif_make_int(env, code),
        enif_make_tuple2(env, enif_make_int(env, val_begin-begin),
                              enif_make_int(env, val_end-val_begin)));
    return true;
  };

  for (const auto* p = ptr; ptr < msg_end; ptr = p) {
    if (state == ParserState::CODE) {
      code = 0;
      p    = str_to_int(ptr, end, code, '=');
      if (!p)
        return MAKE_DECODE_ERROR(fvar, env, "field_tag", ptr - begin, code);

      field = (code > 0 && code < fvar->field_count())
            ? fvar->field(code) : fvar->field(0);

      ASSERT(field, begin, end);

      if (reply_size >= reply_capacity - 1) {
        reply_capacity *= 2;
        int size        = reply_capacity*sizeof(ERL_NIF_TERM);

        DBGPRINT(fvar->debug(), 2,
          "FIX(%s): resizing reply to capacity: %d (size=%d)",
          fvar->name().c_str(), reply_capacity, size);

        reply.reset((ERL_NIF_TERM*)realloc(reply.release(), size));

        if (!reply)
          return MAKE_DECODE_ERROR(fvar, env, "code_out_of_memory", ptr - begin, code);
      }

      tag = IS_LIKELY(code < fvar->field_count() && field->assigned())
          ? field->get_atom() : enif_make_long(env, code);

      ASSERT(*p == '=', begin, end);
      val_begin = val_end = ++p; // Skip '='

      state = field->dtype();

      if (state == ParserState::UNDEFINED) [[unlikely]] {
        state = ParserState::BINARY;
        next_data_length = -1;
      }

      ASSERT(state != ParserState::UNDEFINED && state != ParserState::CODE,
              begin, end);

      continue;
    }

    auto [value, err_str, err_line] =
      do_decode_value(field, env, p, end, DataType(state),
                      double_fmt, time_fmt, ret_binary, soh, next_data_length,
                      begin);

    if (value == am_nil) [[unlikely]]
      return MAKE_DECODE_ERROR(fvar, env, err_str, ptr - begin, code, err_line);

    state   = ParserState::CODE;
    val_end = p++;  // Skip SOH

    DBGPRINT(fvar->debug(), 2, "  [%02d] Field%5d: \"%s\"", reply_size, code,
      std::string((const char*)val_begin, val_end-val_begin).c_str());

    // Append the value to the result array
    if (!append(tag, code, value)) [[unlikely]]
      return MAKE_DECODE_ERROR(fvar, env, "out_of_memory", ptr-begin, code);
  }

  begin = ptr;

  auto res = enif_make_list_from_array(env, (ERL_NIF_TERM*)reply.get(), reply_size);
  return res
       ? enif_make_tuple3(env, am_ok, enif_make_int(env, msg_len), res)
       : MAKE_DECODE_ERROR(fvar, env, "error_in_make_list_from_array", ptr-begin, code);
}
