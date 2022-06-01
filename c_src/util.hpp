//------------------------------------------------------------------------------
// Author: Serge Aleynikov <saleyn at gmail dot com>
//------------------------------------------------------------------------------

#pragma once

#include <erl_nif.h>
#include <dlfcn.h>
#include <string.h>
#include <unordered_map>
#include <functional>
#include <memory>
#include <cstdint>
#include <cstring>
#include <cassert>
#include <string>
#include <filesystem>

#define IS_LIKELY(Expr)   __builtin_expect(!!(Expr), 1)
#define IS_UNLIKELY(Expr) __builtin_expect(!!(Expr), 0)

//------------------------------------------------------------------------------
// Static variables
//------------------------------------------------------------------------------
static ERL_NIF_TERM am_undefined;
static ERL_NIF_TERM am_true;
static ERL_NIF_TERM am_false;
static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_more;
static ERL_NIF_TERM am_enomem;

static const char SOH = 1;

//------------------------------------------------------------------------------
// FIX data types
//------------------------------------------------------------------------------
struct DataType {
  enum {
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

struct FixVariant;

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

//------------------------------------------------------------------------------
// Field
//------------------------------------------------------------------------------
struct Field {
  Field();

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

  int                     id()           const { return m_id;           }
  std::string_view const& name_str()     const { return m_name;         }
  const char*             name()         const { return m_name.data();  }
  FieldType               type()         const { return m_type;         }
  DataType                dtype()        const { return m_dtype;        }
  ERL_NIF_TERM            atom()         const { return m_atom;         }
  const int               ch_len()       const { return m_ch_len;       }
  const char*             len_field()    const { return m_len_field;    }
  const int               len_field_id() const { return m_len_field_id; }

  bool                    assigned()     const { return m_id != 0;      }

  ERL_NIF_TERM            get_atom()     const;

  bool                    has_values()   const { return m_choices.size(); }
  size_t                  values_count() const { return m_choices.size(); }

  FieldChoice const*      value(unsigned idx) const;
  FieldChoice const*      value(std::string_view const& val) const;

  ERL_NIF_TERM            value_atom(unsigned idx) const;
  ERL_NIF_TERM            value_atom(std::string_view const& val) const;

  std::vector<FieldChoice> const& values() const { return m_choices; }

  ERL_NIF_TERM decode(ErlNifEnv* env, const char* code, int len);

  // Find a value of the field as a term in the map identified by the atom value
  // and return a copy suitable for returning from a NIF.
  ERL_NIF_TERM encode(ErlNifEnv* env, ERL_NIF_TERM val);
private:
  using TermMap = std::unordered_map<ERL_NIF_TERM, ERL_NIF_TERM>;
  using NameMap = std::unordered_map<std::string_view, const FieldChoice*>;

  FixVariant*                m_var;
  int                        m_id;
  std::string_view           m_name;
  FieldType                  m_type;
  DataType                   m_dtype;
  mutable ERL_NIF_TERM       m_atom;
  const int                  m_ch_len;
  int                        m_max_val_len;    // Max length of codes in 'choices'
  std::vector<FieldChoice>   m_choices;
  const char*                m_len_field;      // Name of the length field for 'NUMINGROUP' and 'DATA' field types
  const int                  m_len_field_id;   // ID of the length field
  DecFieldValFun             m_decf;
  TermMap                    m_atom_map;       // Maps atom name Name -> value Code
  NameMap                    m_name_map;
};

//------------------------------------------------------------------------------
// Registry of FIX variant factories
// Environment persistent across NIF calls
//------------------------------------------------------------------------------
struct Persistent {
  using Variants = std::vector<std::unique_ptr<FixVariant>>;
  using Map      = std::unordered_map<ERL_NIF_TERM, FixVariant*>;

  // Takes a vector of shared object files to load containing FIX variants
  Persistent(std::vector<std::string> const& so_files, int debug = 0);

  ~Persistent();

  ErlNifEnv*          env()                   { return m_env;   }
  ErlNifEnv const*    env()             const { return m_env;   }

  int                 debug()           const { return m_debug; }

  FixVariant*         get(ERL_NIF_TERM);
  bool                get(ERL_NIF_TERM, FixVariant*& var);

  size_t              count()           const { return m_variants.size(); }
  Variants const&     variants()        const { return m_variants;        }

  template <typename... Args>
  void Debug(int level, const char* fmt, Args... args) {
    if (level <= m_debug)
      fprintf(stderr, fmt, std::forward<Args>(args)...);
  }
private:
  Variants            m_variants;
  Map                 m_variants_map;
  ErlNifEnv*          m_env;
  int                 m_debug;

  ERL_NIF_TERM        make_atom(const char* am);
};

//------------------------------------------------------------------------------
// Manages FIX variant fields metadata and shared binaries: <<"1">>,<<"2">>, ...
//------------------------------------------------------------------------------
struct FixVariant {
  using BinsArray      = std::vector<ERL_NIF_TERM>;
  using FieldValsArray = std::vector<std::vector<FieldChoice>>;

  FixVariant(Persistent* pers, const std::string& path);

  ~FixVariant();

  std::string const&  variant() const { return m_variant;       }
  std::string const&  so_path() const { return m_so_path;       }
  ErlNifEnv*          env()           { return m_pers->env();   }
  ErlNifEnv const*    env()     const { return m_pers->env();   }
  ERL_NIF_TERM        null()          { return ERL_NIF_TERM(0); }

  Field const*        field(unsigned idx) const;
  Field*              field(unsigned idx);

  int                 field_count() const { return m_fields.size(); }

  // Return a shared binary representation of an integer that can be returned
  // from a NIF
  ERL_NIF_TERM copy_bin_tag(ErlNifEnv* env, unsigned idx)
  {
    if (idx==0 || idx >= m_fields.size()) [[unlikely]]
      return null();
    return enif_make_copy(env, m_bins[idx]);
  }

  int find_tag_by_name(std::string_view&& name)
  {
    auto   it  = m_name_to_tag_map.find(name);
    return it == m_name_to_tag_map.end() ? 0 : it->second;
  }

  int find_tag_by_atom(ERL_NIF_TERM name)
  {
    auto   it  = m_atom_to_tag_map.find(name);
    return it == m_atom_to_tag_map.end() ? 0 : it->second;
  }

private:
  Persistent*           m_pers;
  std::string           m_variant;
  std::string           m_so_path;
  void*                 m_so_handle;
  std::vector<Field>    m_fields;
  BinsArray             m_bins;       // <<"1">>, <<"2">>, ...
  NameToTagMap          m_name_to_tag_map;
  AtomToTagMap          m_atom_to_tag_map;
};

//------------------------------------------------------------------------------
// Main FIX variant creation function
//------------------------------------------------------------------------------

extern "C"
{
  typedef const char* (*fix_variant_name)();
  typedef std::vector<Field> (*fields_factory)(FixVariant* pers);
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
  if (s < end && *s == '-') {
    sign   = -1;
    s++;
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

std::unique_ptr<ERL_NIF_TERM, void(*)(ERL_NIF_TERM*)>
inline unique_term_ptr(void* ptr)
{
  return {(ERL_NIF_TERM*)ptr, [](ERL_NIF_TERM* p){ if (p) free((void*)p); }};
}

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

inline int64_t decode_timestamp(ErlNifEnv* env, const char* p, size_t size, bool utc=true)
{
  int y, mon, d, h, m, s, us;

  auto parse = [p](int offset, int len, int &i)
  {
    auto b = p + offset;
    return str_to_int(b, b + len, i) != nullptr;
  };

  auto res = true;

  switch (size)
  {
    case 8:  // Format: YYYYMMDD
      res  = parse(0, 4, y) && parse(4, 2, mon) && parse(6, 2, d);
      h = m = s = us = 0;
      break;
    case 17: // Format: YYYYMMDD-HH:MI:SS
      res  = parse(0, 4, y) && parse(4,  2, mon) && parse(6,  2, d) &&
             parse(9, 2, h) && parse(12, 2, m)   && parse(15, 2, s);
      us   = 0;
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
      res = false;
  }

  if (!res) [[unlikely]]
    return -1;

  struct tm t = {
    .tm_sec  = s,
    .tm_min  = m,
    .tm_hour = h,
    .tm_mday = d,
    .tm_mon  = mon  - 1,   // Months are between 0-11
    .tm_year = y - 1900,   // Years since 1900
  };
  uint64_t epoch = utc ? timegm(&t) : mktime(&t);

  return epoch * 1'000'000 + us;
}

inline ERL_NIF_TERM
encode_timestamp(ErlNifEnv* env, uint64_t usecs, bool msec=false, bool utc=true)
{
  auto secs = usecs / 1'000'000;
  int  us   = int(usecs - secs * 1'000'000);

  struct tm tm;
  auto res = utc ? gmtime_r   ((const time_t*)&secs, &tm)
                 : localtime_r((const time_t*)&secs, &tm);
  if (res == nullptr)
    return enif_make_badarg(env);

  if (msec) us /= 1000;

  char buf[64];
  snprintf(buf, sizeof(buf), msec ? "%04d%02d%02d-%02d:%02d:%02d.%03d"
                                  : "%04d%02d%02d-%02d:%02d:%02d.%06d",
          tm.tm_year+1900, tm.tm_mon+1, tm.tm_mday,
          tm.tm_hour, tm.tm_min, tm.tm_sec, us);

  ERL_NIF_TERM ret;

  auto size = msec ? 21 : 24;
  auto data = enif_make_new_binary(env, size, &ret);
  if (!data)
    return enif_raise_exception(env, am_enomem);

  memcpy(data, buf, size);

  return ret;
}

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
         bool ret_binary, bool full);

//------------------------------------------------------------------------------
// Implementation
//------------------------------------------------------------------------------

// Use erl_make_copy() to copy shared binaries created by this function.
// http://erlang.org/pipermail/erlang-questions/2019-February/097247.html
inline ERL_NIF_TERM create_binary(ErlNifEnv* env, const std::string_view& s)
{
  ErlNifBinary bin;
  if (!enif_alloc_binary(s.size(), &bin)) [[unlikely]]
    return enif_make_badarg(env);
  memcpy(bin.data, s.data(), s.size());
  return enif_make_binary(env, &bin);
}

template <class... Args>
inline std::string to_string(const char* fmt, Args&&... args) {
  char buf[2048];
  snprintf(buf, sizeof(buf), fmt, std::forward<Args>(args)...);
  return buf;
}

inline ERL_NIF_TERM
make_error(ErlNifEnv* env, const char* error, long pos, int tag)
{
  return enif_make_tuple2(env, am_error,
          enif_make_tuple3(env, enif_make_atom(env, error),
                                enif_make_long(env, pos),
                                enif_make_int(env, tag)));
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
  for(auto& fc : m_choices) {
    m_max_val_len = std::max(strlen(fc.value), size_t(m_max_val_len));
    m_atom_map.emplace(enif_make_atom(var->env(), fc.descr),
                       create_binary(var->env(), fc.value));
    m_name_map.emplace(std::string_view(fc.value, strlen(fc.value)), &fc);
  }
}

inline ERL_NIF_TERM
Field::get_atom() const
{
  if (m_atom == 0) [[unlikely]]
    m_atom = enif_make_atom(m_var->env(), m_name.data());

  return m_atom;
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

inline ERL_NIF_TERM
Field::decode(ErlNifEnv* env, const char* code, int len)
{
  if (!m_decf || len < 1 || len > m_max_val_len) [[unlikely]]
    return am_undefined;

  assert(m_decf);

  return m_decf(*this, env, code, len);
}

// Find a value of the field as a term in the map identified by the atom value
// and return a copy suitable for returning from a NIF.
inline ERL_NIF_TERM
Field::encode(ErlNifEnv* env, ERL_NIF_TERM val)
{
  auto it = m_atom_map.find(val);
  if (!enif_is_atom(env, val) || it == m_atom_map.end()) [[unlikely]]
    return enif_make_badarg(env);
  return enif_make_copy(env, it->second);
}

//------------------------------------------------------------------------------
inline Persistent::Persistent(std::vector<std::string> const& so_files, int debug)
  : m_env(enif_alloc_env())
  , m_debug(debug)
{
  for (auto& file : so_files)
  {
    if (!std::filesystem::exists(file))
      throw std::runtime_error
        (to_string("FIX Variant shared object file '%s' not found!", file.c_str()));

    // Load the FIX variant implementation
    auto p  = new FixVariant(this, file);
    m_variants.emplace_back(std::unique_ptr<FixVariant>(p));
    auto am = enif_make_atom(m_env, p->variant().c_str());
    m_variants_map.emplace(std::make_pair(am, m_variants.back().get()));
  }

  am_undefined = make_atom("undefined");
  am_true      = make_atom("true");
  am_false     = make_atom("false");
  am_ok        = make_atom("ok");
  am_error     = make_atom("error");
  am_more      = make_atom("more");
  am_enomem    = make_atom("enomem");
}

inline Persistent::~Persistent()
{
  m_variants_map.clear();
  m_variants.clear();
  enif_free_env(m_env);
  m_env = nullptr;
}

inline ERL_NIF_TERM Persistent::make_atom(const char* am)
{
  return safe_make_atom(m_env, am);
}

FixVariant* Persistent::get(ERL_NIF_TERM variant)
{
  assert(enif_is_atom(m_env, variant));

  auto   it = m_variants_map.find(variant);
  return IS_UNLIKELY(it == m_variants_map.end()) ? nullptr : it->second;
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
::FixVariant(Persistent* pers, std::string const& so_file)
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

  auto get_variant_name = [&find_fun](const char* fun) {
    auto f = reinterpret_cast<fix_variant_name>(find_fun(fun));
    return std::string(f());
  };

  auto get_fields = [this, &find_fun](const char* fun) {
    auto    factory = reinterpret_cast<fields_factory>(find_fun(fun));
    return (factory)(this);
  };

  m_variant = get_variant_name("get_fix_variant_name");
  m_fields  = std::move(get_fields("create_fix_fields"));

  if (m_variant.size() == 0)
    throw std::runtime_error("Empty FIX variant in file: " + so_file);
  if (m_fields.size() == 0)
    throw std::runtime_error
      ("FIX variant '" + m_variant + "' empty fields: " + so_file);

  m_bins.resize(0);
  m_bins.reserve(m_fields.size());

  for (int i=1; i < field_count(); ++i)
  {
    // Initialize m_bins[i] with integer_to_binary(i)
    char s[16];
    int  len = snprintf(s, sizeof(s), "%d", i);
    m_bins.emplace_back(create_binary(env(), std::string_view(s, len)));

    // Populate the map that converts field names to tags
    if (m_fields[i].id() != 0) {
      m_name_to_tag_map.emplace
        (std::make_pair(m_fields[i].name_str(), i));
      m_atom_to_tag_map.emplace
        (std::make_pair(enif_make_atom(env(), m_fields[i].name()), i));
    }
  }
}

inline FixVariant::~FixVariant()
{
  if (m_so_handle)
    dlclose(m_so_handle);
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
// Field implementation
//------------------------------------------------------------------------------

inline Field::Field()
  : m_id          (0)
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
inline ERL_NIF_TERM
do_split(FixVariant* fvar, ErlNifEnv* env,
         const unsigned char*&  begin,  const unsigned char* end,
         bool  ret_binary, bool full)
{
  auto len = int(end - begin);

  if (len < 25)
    return enif_make_tuple2(env, am_more, enif_make_int(env, 25 - len));

  if (memcmp("8=FIX", begin, 5) != 0) [[unlikely]]
    return make_error(env, "missing_tag", 0, 8);

  char soh = 0;

  // Guess SOH (which is either '\1' or '|') by looking at the first delimiter
  for (auto* q = begin+9, *e = q+10; q != e; ++q)
    if (*q == SOH || *q == '|') {
      soh = *q;
      break;
    }

  // Did we find the SOH?
  if (soh == 0) [[unlikely]]
    return make_error(env, "missing_soh", 0, 8);

  // Find FIX length (tag9) in prefix: "8=FIX.M.N|9=LLLL|"...
  const char s_prefix[] = {soh, '9', '='};
  auto f9 = (const unsigned char*)memmem(begin+5, len, s_prefix, sizeof(s_prefix));

  if (!f9) [[unlikely]]
    return make_error(env, "missing_tag", 0, 9);

  f9 += 3; // Adjust for <<1,"9=">>

  int msg_len = 0;

  auto body = str_to_int(f9, end, msg_len, soh);
  if (body == nullptr)
    return make_error(env, "invalid_tag", 0, 9);

  auto end_len = msg_len + 1+7; //  SOH + "10=XXX" + SOH
  auto msg_end = body + end_len;

  if (msg_end > end)
    return enif_make_tuple2(env, am_more, enif_make_long(env, msg_end - end));

  auto pcsum = msg_end-7;

  static const char s_checksum[] = {'1', '0', '='};
  if (memcmp(pcsum, s_checksum, sizeof(s_checksum)) != 0)
    return make_error(env, "invalid_msg_length", msg_len, 9);

  msg_len = msg_end - begin;  // Complete length of current FIX message

  if (*(msg_end-1) != soh)
    return make_error(env, "invalid_msg_terminator", msg_len, 0);

  auto  state = ParserState(ParserState::CODE);
  int   code  = 0;
  auto* field = fvar->field(0);

  int  next_data_length = -1;
  int  reply_capacity   = 16;
  int  reply_size       = 0;
  auto reply       = unique_term_ptr(calloc(reply_capacity, sizeof(ERL_NIF_TERM)));
  auto ptr         = begin;
  auto tag_begin   = ptr;
  auto tag_end     = ptr;
  ERL_NIF_TERM tag = 0;

  // Add {Tag::atom(),  Val::any(), TagCode::integer(),
  //         {ValOffset::integer(), ValLen ::integer()}}
  auto append = [=, &reply, &reply_size, &tag_begin, &tag_end]
    (ERL_NIF_TERM tag, int code, ERL_NIF_TERM val)
  {
    // Don't include 'BeginString', 'BodyLength', 'CheckSum' when not requested
    if (!full && code >= 8 && code <= 10)
      return;

    reply.get()[reply_size++] =
      enif_make_tuple4(env, tag, val, enif_make_int(env, code),
        enif_make_tuple2(env, enif_make_int(env, tag_begin-begin),
                              enif_make_int(env, tag_end-tag_begin)));
  };

  for (const auto* p = ptr; ptr < msg_end; ptr = p) {
    ERL_NIF_TERM value = 0;

    switch (state) {
      case ParserState::CODE:
        code = 0;
        p    = str_to_int(ptr, end, code, '=');
        if (!p)
          return make_error(env, "code", ptr - begin, code);

        field = (code > 0 && code < fvar->field_count())
              ? fvar->field(code) : fvar->field(0);

        assert(field);

        if (reply_size >= reply_capacity - 1) {
          reply_capacity *= 2;
          int size        = reply_capacity*sizeof(ERL_NIF_TERM);
          reply.reset((ERL_NIF_TERM*)realloc(reply.release(), size));
          if (!reply)
            return make_error(env, "code_out_of_memory", ptr - begin, code);
        }

        tag = IS_LIKELY(code < fvar->field_count() && field->assigned())
            ? field->get_atom() : am_undefined;

        assert(*p == '=');
        tag_begin = tag_end = ++p; // Skip '='

        state = field->dtype();

        if (state == ParserState::UNDEFINED) [[unlikely]] {
          state = ParserState::STRING;
          next_data_length = -1;
        }

        assert(state != ParserState::UNDEFINED && state != ParserState::CODE);

        continue;

      case ParserState::INT: {
        long ival=0;
        p = str_to_int(ptr, end, ival, soh);
        if (p == nullptr)
          return make_error(env, "int", ptr - begin, code);

        assert(field);

        // Field number 9 is special, as it indicates the length of the FIX
        // message payload, so treat it as integer
        auto is_length   = field->type() == FieldType::LENGTH && code != 9;
        next_data_length = IS_UNLIKELY(is_length) ? ival : -1;

        value = enif_make_long(env, ival);
        break;
      }

      case ParserState::DOUBLE: {
        long ival=0;
        p = str_to_int(ptr, end, ival, soh, '.');
        if (p == nullptr)
          return make_error(env, "double", ptr - begin, code);

        double dval = ival;

        if (*(p+1) == '.') {
          auto coeff = 1.0;
          for(++p /* Skip '.' */; p < end && *p >= '0' && *p <= '9'; ++p) {
            coeff *= 0.1;
            dval  += (*p - '0')*coeff;
          }
          if (p == end || *p != soh) // Skip SOH
            return make_error(env, "double_soh", ptr - begin, code);
          dval += ival < 0 ? -dval : +dval;
        }

        value = enif_make_double(env, dval);
        break;
      }

      case ParserState::GROUP: {
        int ival=0;
        p = str_to_int(ptr, end, ival, soh); // Get the number of repeating groups
        if (p == nullptr)
          return make_error(env, "group", ptr - begin, code);

        value = enif_make_long(env, ival);
        break;
      }

      case ParserState::BINARY: {
        // This field should have preceeded by the LENGTH integer field
        // that would have set the "next_data_length" variable
        if (next_data_length < 0) [[unlikely]]
          return make_error(env, "binary_missing_len", ptr-begin, code);

        p += next_data_length;
        next_data_length = -1;

        if (p >= end || *p != soh)
          return make_error(env, "binary", ptr - begin, code);

        const auto   len = p - ptr;

        if (ret_binary) {
          auto data = enif_make_new_binary(env, len, &value);
          if (!data)
            return make_error(env, "binary_alloc", ptr - begin, code);
          memcpy(data, ptr, len);
        } else {
          value = enif_make_tuple2(env,
                                   enif_make_uint(env, ptr-begin),
                                   enif_make_uint(env, len));
        }
        break;
      }

      case ParserState::STRING: {
        // If the previous field was LENGTH which set "next_data_length", use it
        if(next_data_length < 0) [[likely]]
          while (p < end && *p != soh) ++p;
        else {
          p += next_data_length;
          next_data_length = -1;
        }

        if (p >= end || *p != soh)
          return make_error(env, "string", ptr - begin, code);

        assert(field);

        value = field->has_values() ? field->decode(env, (const char*)ptr, p-ptr)
                                    : am_undefined;
        if (value == am_undefined) {
          auto len = p - ptr;
          if (ret_binary) {
            auto data = enif_make_new_binary(env, len, &value);
            if (!data)
              return make_error(env, "string_alloc_binary", ptr-begin, code);
            memcpy(data, ptr, len);
          } else
            value = enif_make_tuple2(env,
                      enif_make_uint(env, ptr-begin),
                      enif_make_uint(env, len));
        }

        break;
      }

      case ParserState::DATETIME: {
        while (p < end && *p != soh) ++p;

        if (*p != soh)
          return make_error(env, "datetime", ptr - begin, code);

        auto len = p - ptr;
        auto ts  = decode_timestamp(env, reinterpret_cast<const char*>(ptr), len);

        if (ts < 0) [[unlikely]]
          return make_error(env, "datetime", ptr - begin, code);

        value = enif_make_int64(env, ts);
        break;
      }

      case ParserState::BOOL: {
        value = (*ptr == 'Y' || *ptr == 'y') ? am_true : am_false;
        if (++p == end || *p != soh)
          return make_error(env, "boolean_soh", ptr - begin, code);

        break;
      }

      case ParserState::CHAR: {
        if (++p == end || *p != soh)
          return make_error(env, "char_soh", ptr - begin, code);

        value = field->has_values() ? field->decode(env, (const char*)ptr, p-ptr)
                                    : enif_make_int(env, (int)*ptr);
        if (value == am_undefined)
          enif_make_int(env, (int)*ptr);

        break;
      }
      default:
        return make_error(env, "unsupported_data_type", ptr - begin, code);
    }

    assert(*p == soh);

    state   = ParserState::CODE;
    tag_end = p++;  // Skip SOH

    append(tag, code, value); // Append the value to the result array
  }

  begin = ptr;
  return enif_make_tuple3(env, am_ok, enif_make_int(env, msg_len),
    enif_make_list_from_array(env, (ERL_NIF_TERM*)reply.get(), reply_size));
}
