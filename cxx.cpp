#include <iostream>
#include <vector>
#include <deque>
#include <string>
#include <sstream>
#include <utility>
#include <set>
#include <map>
#include <algorithm>
#include <cmath>
#include <array>
#include <initializer_list>
#include <tuple>
#include <iomanip>
#include <type_traits>
#include <bitset>
#include <cstdlib>
#include <cstring>

using namespace std;

using ll = long long;
using ld = long double;
using pii = pair<int, int>;
using pll = pair<ll, ll>;
using vi = vector<int>;
using vll = vector<ll>;
using mii = map<int, int>;
using si = set<int>;
using sc = set<char>;
using str = string;
#define f(i, s, e) for (long long int i = s; i < e; i++)
#define cf(i, s, e) for (long long int i = s; i <= e; i++)
#define rf(i, e, s) for (long long int i = e - 1; i >= s; i--)
#define pb push_back
#define eb emplace_back
#define MOD 1000000007
#define tpl template
#define tn typename
#define vec vector

template <typename T = int>
T input()
{
  T x;
  cin >> x;
  return x;
}

template<typename T, typename = void>
struct tostr_helper {
  static str tostr(const T& t) { return to_string(t); }
};

template<typename T>
struct tostr_helper<T, enable_if_t<is_floating_point_v<T>>> {
  static str tostr(T t) {
    stringstream ss;
    ss << setprecision(20) << t;
    return ss.str();
  }
};

#define customtostr(T, body) template<> struct tostr_helper<T> { static str tostr(const T& it) { body } }

template<typename T>
str tostr(const T& t) { return tostr_helper<T>::tostr(t); }

customtostr(char*, return it;);
customtostr(string, return it;);
template<size_t N>
struct tostr_helper<char[N]> {
  static str tostr(const char* c) {
    return c;
  }
};

template<typename A, typename B>
struct tostr_helper<pair<A,B>> {
  static str tostr(const pair<A,B>& t) {
    return str("(") + tostr_helper<remove_cv_t<A>>::tostr(t.first) + ", " + tostr_helper<remove_cv_t<B>>::tostr(t.second) + ")";
  } 
};

template <typename T, typename C>
string conttostr(const C &v)
{
  stringstream ss;
  ss << "[";
  bool first = true;
  for(const auto& t : v) {
    if(!first) {
        ss << ", ";
    }
    ss << tostr(t);
    first = false;
  }
  ss << "]";
  return ss.str();
}

template<typename T>
struct tostr_helper<vec<T>> {
  static str tostr(const vec<T>& v) { return conttostr<T, vec<T>>(v); }
};

template<typename T, size_t N>
struct tostr_helper<array<T, N>> {
  static str tostr(const array<T, N>& v) { return conttostr<T, array<T, N>>(v); }
};

template<typename T>
struct tostr_helper<initializer_list<T>> {
  static str tostr(const initializer_list<T>& v) { return conttostr<T, initializer_list<T>>(v); }
};

template<typename T>
struct tostr_helper<set<T>> {
  static str tostr(const set<T>& v) { return conttostr(v); }
};

template<typename K, typename V>
struct tostr_helper<map<K, V>> {
  static str tostr(const map<K, V>& v) { return conttostr<pair<K, V>>(v); }
};

#define all(x) begin(x), end(x)
#define alla(x) a, a+sizeof(a)/sizeof(a[0])
#define pr(...) do { p(__VA_ARGS__); return; } while(0)
#define lbd(...) [&](auto&& it) { return __VA_ARGS__; }

void p()
{
  cout << "\n";
}

template <typename T, typename... Args>
void p(const T &first, const Args &...rem)
{
  cout << tostr(first) << " ";
  p(rem...);
}

void perr()
{
  cerr << "\n";
}

template <typename T, typename... Args>
void perr(const T &first, const Args &...rem)
{
  cerr << tostr(first) << " ";
  perr(rem...);
}

#ifdef ONLINE_JUDGE
#define DBG(...)
#else
template<typename... Args>
void dbg(const char* label, Args&&... args) {
  cerr << label << " = ";
  perr(args...);
}
#define DBG(...) dbg(#__VA_ARGS__, __VA_ARGS__)
#endif

template<typename T = int>
vector<T> load(int n) {
  vector<T> r(n);
  f(i,0,n) r[i] = input<T>();
  return r;
}

#define maxv(v) *max_element(all(v))
#define minv(v) *min_element(all(v))

template<typename Int = int, typename Predicate>
Int bs_ftrue(Int begin, Int end, Predicate a) {
  // false false false ... false true true ... true
  while(begin != end) {
    Int mid = (begin + end) / 2;
    if(a(mid)) end = mid; else begin = mid+1;
  }
  return begin;
}

template<typename Int = int, typename Predicate>
Int bs_right(Int begin, Int end, Predicate a) {
  // true true true ... true false false ... false
  while(begin != end) {
    Int mid = (begin + end) / 2;
    if(a(mid)) begin = mid; else end = mid-1;
  }
  return begin;
}

ll chooses(ll n, ll k) {
  if(n < k || k < 0) {
    return 0;
  }
  if(k == 0) {
    return 1;
  }
  return n * chooses(n-1,k-1) / k;
}

template<typename Vec>
Vec pfs(Vec& v) {
  Vec s;
  s.resize(v.size() + 1, 0);
  f(i,0,v.size()) s[i+1]=s[i]+v[i];
  return s;
}

template<typename Int = ll>
Int powmod(Int a, Int n, Int mod) {
  Int r = 1;
  while(n > 0) {
    if(n&1) r=(r*a)%mod;
    a = (a*a)%mod;
    n>>=1;
  }
  return r;
}

// bit ops
#define setbit(a, i) (a) |= (1 << (i))
#define clearbit(a, i) (a) &= ~(1 << (i))
#define getbit(a, i) (a & (1 << i))
#define ctz __builtin_ctz
#define clz __builtin_clz
#define nbit __builtin_popcount

tpl<tn I> I lsb(I x) { return x&~(x-1); }
tpl<tn I> I hsb(I x) { return x&-x; }
string tostrbin(ll x) { return bitset<64>(x).to_string(); }

template<typename T = int>
struct ftree {
  vec<T> v;
  
  ftree(int n): v(n+1,0) {}
  
  template<typename U = T>
  U sum(int r) {
    int ret = 0;
    for(++r; r>0; r-=r&-r) ret += v[r];
    return ret;
  }

  template<typename U = T>
  U sum(int l, int r) {
    return sum(r) - sum(l-1);
  }

  void add(int idx, T delta) {
    for(++idx; idx < v.size(); idx += idx&-idx) {
      v[idx] += delta;
    }
  }
};

template<typename T = int>
struct stree {
  vec<T> v;
  stree(int mx_level): v(1 << mx_level) {}
};

template<typename T = int, bool ubs = true>
struct dsu {
  map<T, pair<T, size_t>> td;

  void ms(T v) {
    td[v] = {v, 1};
  }

  T find(T v) {
    return v == td[v].first? v : (td[v].first = find(td[v].first));
  }

  void uni(T a, T b) {
    a = find(a);
    b = find(b);
    if(a != b) {
      if(ubs && td[b].second > td[a].second)
        swap(a, b);
      td[b].first = a;
      td[b].second += td[a].second;
    }
  }
};

ll modsgn(ll a, ll m) {
  return a >= 0? a%m : (m+a%m);
}

// find b s.t. m | a*b - 1
ll modinv(ll a, ll m) {
  ll b = m;
  ll up = 1, vp = 0, u = 0, v = 1, *r = &u;
  if(a < b) { swap(a, b); r = &v; }
  while(b!=0) {
    auto d = a / b;
    auto e = a % b;
    ll un = modsgn(up - d * u, m);
    ll vn = modsgn(vp - d * v, m);
    up = exchange(u, un);
    vp = exchange(v, vn);
    a = exchange(b, e);
    if(e == 1) return *r;
  }
  return -1;
}

void test(ll a, ll m) {
  ll x = modinv(a, m);
  if(a*x%m==1) pr("YES");
  else DBG(a,x,m);
}

void solve() {
}

int main(int argc, char **argv)
{
  ios_base::sync_with_stdio(0);
  cin.tie(0); cout.tie(0);
  int tc = 1;
  // cin >> tc;
  for (int t = 1; t <= tc; t++) {
      // cout << "Case #" << t << ": ";
      solve();
  }
}