#include "timeplyr.h"
// #include <chrono>
// #include "cctz/time_zone.h"
// #include <unordered_map>
// #include <optional>
// #include <vector>
//
// const std::unordered_map<std::string, int> TZMAP {
//   {"GMT", 0},
//   {"CEST", 2}, {"CET", 1}, {"EDT", -4}, {"EEST", 3}, {"EET", 2}, {"EST", -5},
//    {"PDT", -7}, {"PST", -8}, {"WEST", 1}, {"WET", 0}
// };
//
// #define SET_NEGATIVE(x) if (do_negative) { is_negative = x < 0; do_negative = false; }
//
//
// namespace chrono = std::chrono;
// using sys_seconds = chrono::duration<int_fast64_t>;
// using time_point = chrono::time_point<std::chrono::system_clock, sys_seconds>;
//
//
// enum class RollMonth { FULL, PREDAY, BOUNDARY, POSTDAY, NA, NAym };
//
// // inline RollMonth parse_month_roll(const std::string& roll) {
// //   if (roll == "preday") return RollMonth::PREDAY;
// //   if (roll == "boundary") return RollMonth::BOUNDARY;
// //   if (roll == "postday") return RollMonth::POSTDAY;
// //   if (roll == "full") return RollMonth::FULL;
// //   if (roll == "NA") return RollMonth::NA;
// //   if (roll == "NAym") return RollMonth::NAym;
// //   // backward compatibility
// //   if (roll == "first") return RollMonth::POSTDAY;
// //   if (roll == "last") return RollMonth::PREDAY;
// //   if (roll == "skip") return RollMonth::FULL;
// //   Rf_error("Invalid roll_month type (%s)", roll.c_str());
// // }
//
// enum class RollDST { PRE, BOUNDARY, POST, NA, XFIRST, XLAST};
//
// inline RollDST parse_dst_roll(const std::string& roll, bool allow_x = false) {
//   if (roll != "NA"){
//     Rf_error("Invalid roll_dst type (%s)", roll.c_str());
//   }
//   return RollDST::NA;
// }
//
// // inline RollDST parse_dst_roll(const std::string& roll, bool allow_x = false) {
// //   if (roll == "boundary") return RollDST::BOUNDARY;
// //   if (roll == "post") return RollDST::POST;
// //   if (roll == "pre") return RollDST::PRE;
// //   if (roll == "NA") return RollDST::NA;
// //   if (roll == "xfirst") {
// //     if (allow_x) return RollDST::XFIRST;
// //     else Rf_error("'xfirst' dst_roll is not meaningful here");
// //   }
// //   if (roll == "xlast") {
// //     if (allow_x) return RollDST::XLAST;
// //     else Rf_error("'xlast' dst_roll is not meaningful here");
// //   }
// //   // backward compatibility
// //   if (roll == "first") return RollDST::POST;
// //   if (roll == "last") return RollDST::PRE;
// //   Rf_error("Invalid roll_dst type (%s)", roll.c_str());
// // }
//
// struct DST {
//   RollDST skipped;
//   RollDST repeated;
//   DST(RollDST skipped, RollDST repeated): skipped(skipped), repeated(repeated) {}
//   DST(const cpp11::strings roll_dst, bool allow_x = false) {
//     if (roll_dst.empty() || roll_dst.size() > 2)
//       Rf_error("roll_dst must be a character vector of length 1 or 2");
//     std::string dst_repeated(roll_dst[0]);
//     skipped = parse_dst_roll(dst_repeated, allow_x);
//     if (roll_dst.size() > 1) {
//       std::string dst_skipped(roll_dst[1]);
//       repeated = parse_dst_roll(dst_skipped, allow_x);
//     } else {
//       repeated = skipped;
//     }
//   }
// };
//
// const char* tz_from_R_tzone(SEXP tz) {
//   if (Rf_isNull(tz)) {
//     return "";
//   } else {
//     if (!Rf_isString(tz))
//       Rf_error("'tz' is not a character vector");
//     const char* tz0 = CHAR(STRING_ELT(tz, 0));
//     if (strlen(tz0) == 0) {
//       if (LENGTH(tz) > 1) {
//         return CHAR(STRING_ELT(tz, 1));
//       }
//     }
//     return tz0;
//   }
// }
//
// const char* tz_from_tzone_attr(SEXP x){
//   return tz_from_R_tzone(Rf_getAttrib(x, Rf_install("tzone")));
// }
//
// const char* system_tz() {
//   auto sys_timezone = cpp11::package("base")["Sys.timezone"];
//   SEXP sys_tz = STRING_ELT(sys_timezone(), 0);
//   if (sys_tz == NA_STRING || strlen(CHAR(sys_tz)) == 0) {
//     Rf_warning("System timezone name is unknown. Please set environment variable TZ. Using UTC.");
//     return "UTC";
//   } else {
//     return CHAR(sys_tz);
//   }
// }
//
// const char* local_tz() {
//   // initialize once per session
//   static const char* SYS_TZ = strdup(system_tz());
//   const char* tz_env = std::getenv("TZ");
//   if (tz_env == NULL) {
//     return SYS_TZ;
//   } else if (strlen(tz_env) == 0) {
//     // If set but empty, R behaves in a system specific way and there is no way
//     // to infer local time zone.
//     Rf_warning("Environment variable TZ is set to \"\". Using system TZ.");
//     return SYS_TZ;
//   } else {
//     return tz_env;
//   }
// }
//
// bool load_tz(std::string tzstr, cctz::time_zone& tz) {
//   // return `true` if loaded, else false
//   if (tzstr.empty()) {
//     // CCTZ doesn't work on windows https://github.com/google/cctz/issues/53
//     /* std::cout << "Local TZ: " << local_tz() << std::endl; */
//     return cctz::load_time_zone(local_tz(), &tz);
//   } else {
//     if (!cctz::load_time_zone(tzstr, &tz)) {
//       auto el = TZMAP.find(tzstr);
//       if (el != TZMAP.end()) {
//         tz = cctz::fixed_time_zone(chrono::hours(el->second));
//       } else {
//         return false;
//       }
//     }
//     return true;
//   }
// }
//
// void load_tz_or_fail(std::string tzstr, cctz::time_zone& tz, std::string error_msg) {
//   if (!load_tz(tzstr, tz)) {
//     Rf_error(error_msg.c_str(), tzstr.c_str());
//   }
// }
//
// // common
//
// int_fast64_t floor_to_int64(double x) {
//   // maybe fixme: no warning yet on integer overflow
//   if (x != x)
//     return LLONG_MIN;
//   x = std::floor(x);
//   if (x > static_cast<double>(std::numeric_limits<int_fast64_t>::max()) || x <= static_cast<double>(std::numeric_limits<int_fast64_t>::min())) {
//     return LLONG_MIN;
//   }
//   return static_cast<int_fast64_t>(x);
// }
//
// double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl,
//                              const DST& dst,
//                              const bool is_negative) noexcept {
//   time_point tp;
//   switch (cl.kind) {
//   case cctz::time_zone::civil_lookup::SKIPPED:
//     return NA_REAL;
//   case cctz::time_zone::civil_lookup::REPEATED:
//     return NA_REAL;
//   case cctz::time_zone::civil_lookup::UNIQUE:
//     tp = cl.pre;
//   }
//   return tp.time_since_epoch().count();
// }
//
// // double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl,
// //                              const DST& dst,
// //                              const bool is_negative) noexcept {
// //   time_point tp;
// //   switch (cl.kind) {
// //   case cctz::time_zone::civil_lookup::SKIPPED:
// //     // meaning of pre/post in CCTZ is not the same as here. It's inverted.
// //     switch (dst.skipped) {
// //     case RollDST::PRE: tp = cl.post; break;
// //     case RollDST::BOUNDARY: tp = cl.trans; break;
// //     case RollDST::POST: tp = cl.pre; break;
// //     case RollDST::XFIRST: tp = is_negative ? cl.pre : cl.post; break;
// //     case RollDST::XLAST: tp = is_negative ? cl.post : cl.pre; break;
// //     case RollDST::NA: return NA_REAL;
// //     }
// //     break;
// //   case cctz::time_zone::civil_lookup::REPEATED:
// //     switch (dst.repeated) {
// //     case RollDST::PRE: tp = cl.pre; break;
// //     case RollDST::BOUNDARY: tp = cl.trans; break;
// //     case RollDST::POST: tp = cl.post; break;
// //     case RollDST::XFIRST: tp = is_negative ? cl.post : cl.pre; break;
// //     case RollDST::XLAST: tp = is_negative ? cl.pre : cl.post; break;
// //     case RollDST::NA: return NA_REAL;
// //     }
// //     break;
// //   case cctz::time_zone::civil_lookup::UNIQUE:
// //     tp = cl.pre;
// //   }
// //   return tp.time_since_epoch().count();
// // }
//
// // Helper for conversion functions. Get seconds from civil_lookup, but relies on
// // original time pre/post time if cl_new falls in repeated interval.
// double civil_lookup_to_posix(const cctz::time_zone::civil_lookup& cl_new, // new lookup
//                              const cctz::time_zone& tz_old,              // original time zone
//                              const time_point& tp_old,                   // original time point
//                              const cctz::civil_second& cs_old,           // original time in secs
//                              const DST& dst,
//                              double remainder) noexcept {
//   if (cl_new.kind == cctz::time_zone::civil_lookup::REPEATED) {
//     if (dst.repeated == RollDST::BOUNDARY)
//       remainder = 0.0;
//     // match pre or post time of original time
//     time_point tp_new;
//     const cctz::time_zone::civil_lookup cl_old = tz_old.lookup(cs_old);
//     if (cl_old.kind == cctz::time_zone::civil_lookup::REPEATED) {
//       if (tp_old >= cl_old.trans) {
//         tp_new = cl_new.post;
//       } else {
//         tp_new = cl_new.pre;
//       }
//       return tp_new.time_since_epoch().count() + remainder;
//     }
//   } else if (cl_new.kind == cctz::time_zone::civil_lookup::SKIPPED) {
//     if (dst.repeated == RollDST::BOUNDARY)
//       remainder = 0.0;
//   }
//
//   return civil_lookup_to_posix(cl_new, dst, false) + remainder;
//
// }
//
//
// [[cpp11::register]]
// double cpp_time_add(const double timestamp,
//                     const std::string timezone,
//                     double num,
//                     const std::string unit){
//
//   if (num != num) return NA_REAL;
//
//   static const std::vector<std::string> units = {
//     "years", "months", "weeks", "days", "hours", "minutes", "seconds"
//   };
//
//   int uniti = 0;
//   int unitsn = units.size();
//   // Search for the string and return its position
//   for (int i = 0; i < unitsn; ++i) {
//     if (units[i] == unit) {
//       uniti = i + 1;
//       break;
//     }
//   }
//   if (uniti == 0){
//     Rf_error("unit must be one of: years, months, weeks, days, hours, minutes or seconds");
//   }
//
//   const cpp11::strings roll_dst = Rf_mkString("NA");
//   DST rdst(roll_dst, true);
//
//   double dti = timestamp;
//   const std::string tz_name = timezone;
//
//   cctz::time_zone tz;
//   load_tz_or_fail(tz_name, tz, "Invalid timezone");
//
//   int y = 0, m = 0, w = 0, d = 0, H = 0, M = 0;
//   double s = 0;
//   int_fast64_t S = 0;
//
//   cctz::civil_year cy;
//   cctz::civil_month cm;
//   cctz::civil_day cd;
//   cctz::civil_hour cH;
//   cctz::civil_minute cM;
//   cctz::civil_second cS;
//
//   double out;
//
//   bool is_negative = false;
//   bool do_negative = true;
//
//   int_fast64_t secs = floor_to_int64(dti);
//
//   if ( (dti != dti) || secs == LLONG_MIN) {
//     if (dti == R_PosInf)
//       out = R_PosInf;
//     else if (dti == R_NegInf)
//       out = R_NegInf;
//     else
//       out = NA_REAL;
//   }
//
//   bool add_my_hms = true;
//   double rem = dti - secs;
//   sys_seconds ss(secs);
//   time_point tp(ss);
//   cctz::civil_second cs = cctz::convert(tp, tz);
//
//   int_fast64_t
//   ty = cs.year(), tm = cs.month(), td = cs.day(),
//     tH = cs.hour(), tM = cs.minute(), tS = cs.second();
//
//   cy = cctz::civil_year(ty);
//
//   // Year
//   if (uniti == 1) {
//     y = num;
//     SET_NEGATIVE(y)
//       cy += y;
//   }
//   cm = cctz::civil_month(cy) + (tm - 1);
//
//   // Month
//   if (uniti == 2) {
//     m = num;
//     SET_NEGATIVE(m)
//       cm += m;
//   }
//   cd = cctz::civil_day(cm) + (td - 1);
//   if (cd.day() != td) {
//     // roll_month = 'preday'
//     cd = cctz::civil_day(cctz::civil_month(cd)) - 1;
//   }
//   // Week
//   if (uniti == 3){
//     w = num;
//     SET_NEGATIVE(w)
//       cd += w * 7;
//   }
//   // Day
//   if (uniti == 4) {
//     d = num;
//     SET_NEGATIVE(d)
//       cd += d;
//   }
//   cH = cctz::civil_hour(cd);
//   if (add_my_hms) cH += tH;
//   // Hour
//   if (uniti == 5) {
//     H = num;
//     SET_NEGATIVE(H)
//       cH += H;
//   }
//   cM = cctz::civil_minute(cH);
//   if (add_my_hms)
//     cM += tM;
//   // Minute
//   if (uniti == 6) {
//     M = num;
//     SET_NEGATIVE(M)
//       cM += M;
//   }
//   cS = cctz::civil_second(cM);
//   if (add_my_hms) cS += tS;
//   // Second
//   if (uniti == 7) {
//     S = num;
//     S = floor_to_int64(S);
//     SET_NEGATIVE(S)
//       rem += s - S;
//     cS += S;
//   }
//
//   s = civil_lookup_to_posix(tz.lookup(cS), rdst, is_negative);
//   out = s + rem;
//   return out;
// }
//
// [[cpp11::register]]
// cpp11::doubles adjust_duration_estimate(
//     cpp11::writable::doubles est, cpp11::doubles start, cpp11::doubles end,
//     cpp11::doubles width, const std::string unit
// ){
//
//   R_xlen_t n = est.size();
//   const std::string timezone = CHAR(STRING_ELT(Rf_getAttrib(start, Rf_install("tzone")), 0));
//   const cpp11::strings roll_month = Rf_mkString("preday");
//   const cpp11::strings roll_dst = Rf_mkString("NA");
//
//   for (R_xlen_t i = 0; i < n; ++i){
//     est[i] = std::ceil(est[i]);
//   }
//
//   // Temporary variables
//   cpp11::writable::doubles temp_adj(n);
//   cpp11::writable::list period_add(1);
//   period_add.names() = unit;
//
//   double res;
//
//   // width * est
//   for (R_xlen_t i = 0; i < n; ++i){
//     res = width[i] * est[i];
//     temp_adj[i] = res != res ? NA_REAL : res;
//   }
//   period_add[0] = temp_adj;
//   cpp11::writable::doubles up_date =
//     cpp11::as_cpp<cpp11::writable::doubles>(
//       cpp11::package("timechange")["C_time_add"](
//           start, period_add, roll_month, roll_dst
//       )
//     );
//
//   bool adj = true;
//
//   while (adj){
//     adj = false;
//     for (R_xlen_t i = 0; i < n; ++i){
//       if (up_date[i] < end[i]){
//         adj = true;
//         est[i] += 1;
//         res = width[i] * est[i];
//         up_date[i] = cpp_time_add(start[i], timezone, res, unit);
//       }
//     }
//   }
//
//   cpp11::writable::doubles low_date = up_date;
//
//   adj = true;
//
//   while (adj){
//     adj = false;
//     for (R_xlen_t i = 0; i < n; ++i){
//       if (low_date[i] > end[i]){
//         adj = true;
//         est[i] -= 1;
//         up_date[i] = low_date[i];
//         res = width[i] * est[i];
//         low_date[i] = cpp_time_add(start[i], timezone, res, unit);
//       }
//     }
//   }
//
//   double frac;
//   cpp11::writable::doubles out(n);
//
//   for (R_xlen_t i = 0; i < n; ++i){
//     frac = (low_date[i] == up_date[i]) ?
//     0 : (end[i] - low_date[i]) / (up_date[i] - low_date[i]);
//     out[i] = est[i] + frac;
//   }
//   return out;
// }
//
// // double cpp_time_add(const double timestamp,
// //                     const std::string timezone,
// //                     double num,
// //                     const std::string unit){
// //
// //   static const std::vector<std::string> units = {
// //     "years", "months", "weeks", "days", "hours", "minutes", "seconds"
// //   };
// //
// //   int uniti = 0;
// //   int unitsn = units.size();
// //   // Search for the string and return its position
// //   for (int i = 0; i < unitsn; ++i) {
// //     if (units[i] == unit) {
// //       uniti = i + 1;
// //       break;
// //     }
// //   }
// //   if (uniti == 0){
// //     Rf_error("unit must be one of: years, months, weeks, days, hours, minutes or seconds");
// //   }
// //
// //
// //   const cpp11::strings roll_dst = Rf_mkString("NA");
// //   RollMonth rmonth = RollMonth::PREDAY;
// //   DST rdst(roll_dst, true);
// //
// //   double dti = timestamp;
// //   const std::string tz_name = timezone;
// //
// //   cctz::time_zone tz;
// //   load_tz_or_fail(tz_name, tz, "Invalid timezone");
// //
// //   int y = 0, m = 0, w = 0, d = 0, H = 0, M = 0;
// //   double s = 0;
// //   int_fast64_t S = 0;
// //
// //   cctz::civil_year cy;
// //   cctz::civil_month cm;
// //   cctz::civil_day cd;
// //   cctz::civil_hour cH;
// //   cctz::civil_minute cM;
// //   cctz::civil_second cS;
// //
// //   double out;
// //
// //   bool is_negative = false;
// //   bool do_negative = true;
// //
// //   int_fast64_t secs = floor_to_int64(dti);
// //
// //   if ( (dti != dti) || secs == LLONG_MIN) {
// //     if (dti == R_PosInf)
// //       out = R_PosInf;
// //     else if (dti == R_NegInf)
// //       out = R_NegInf;
// //     else
// //       out = NA_REAL;
// //   }
// //
// //   bool add_my_hms = true;
// //   double rem = dti - secs;
// //   sys_seconds ss(secs);
// //   time_point tp(ss);
// //   cctz::civil_second cs = cctz::convert(tp, tz);
// //
// //   int_fast64_t
// //   ty = cs.year(), tm = cs.month(), td = cs.day(),
// //     tH = cs.hour(), tM = cs.minute(), tS = cs.second();
// //
// //   cy = cctz::civil_year(ty);
// //
// //   // Year
// //   if (uniti == 1) {
// //     if (num != num){
// //       y = NA_INTEGER;
// //       out = NA_REAL;
// //     } else {
// //       y = num;
// //     }
// //     SET_NEGATIVE(y)
// //       cy += y;
// //   }
// //   cm = cctz::civil_month(cy) + (tm -1);
// //
// //   // Month
// //   if (uniti == 2) {
// //     if (num != num){
// //       m = NA_INTEGER;
// //       out = NA_REAL;
// //     } else {
// //       m = num;
// //     }
// //     SET_NEGATIVE(m)
// //       cm += m;
// //   }
// //   cd = cctz::civil_day(cm) + (td - 1);
// //   if (cd.day() != td) {
// //     cd = cctz::civil_day(cm) + (td - 1);
// //     // month rolling kicks in
// //     switch(rmonth) {
// //     case RollMonth::FULL: break;
// //     case RollMonth::PREDAY:
// //       cd = cctz::civil_day(cctz::civil_month(cd)) - 1;
// //       break;
// //     case RollMonth::BOUNDARY:
// //       cd = cctz::civil_day(cctz::civil_month(cd));
// //       add_my_hms = false;
// //       break;
// //     case RollMonth::POSTDAY:
// //       cd = cctz::civil_day(cctz::civil_month(cd));
// //       break;
// //     case RollMonth::NA:
// //       out = NA_REAL;
// //     case RollMonth::NAym: break;
// //     }
// //   }
// //   // Week
// //   if (uniti == 3){
// //     if (num != num){
// //       w = NA_INTEGER;
// //       out = NA_REAL;
// //     } else {
// //       w = num;
// //     }
// //     SET_NEGATIVE(w)
// //       cd += w * 7;
// //   }
// //   // Day
// //   if (uniti == 4) {
// //     if (num != num){
// //       d = NA_INTEGER;
// //       out = NA_REAL;
// //     } else {
// //       d = num;
// //     }
// //     SET_NEGATIVE(d)
// //       cd += d;
// //   }
// //   cH = cctz::civil_hour(cd);
// //   if (add_my_hms) cH += tH;
// //   // Hour
// //   if (uniti == 5) {
// //     if (num != num){
// //       H = NA_INTEGER;
// //       out = NA_REAL;
// //     } else {
// //       H = num;
// //     }
// //     SET_NEGATIVE(H)
// //       cH += H;
// //   }
// //   cM = cctz::civil_minute(cH);
// //   if (add_my_hms)
// //     cM += tM;
// //   // Minute
// //   if (uniti == 6) {
// //     if (num != num){
// //       M = NA_INTEGER;
// //       out = NA_REAL;
// //     } else {
// //       M = num;
// //     }
// //     SET_NEGATIVE(M)
// //       cM += M;
// //   }
// //   cS = cctz::civil_second(cM);
// //   if (add_my_hms) cS += tS;
// //   // Second
// //   if (uniti == 7) {
// //     if (num != num){
// //       S = NA_REAL;
// //       out = NA_REAL;
// //     } else {
// //       S = num;
// //     }
// //     S = floor_to_int64(S);
// //     SET_NEGATIVE(S)
// //       rem += s - S;
// //     cS += S;
// //   }
// //
// //   s = civil_lookup_to_posix(tz.lookup(cS), rdst, is_negative);
// //   out = s + rem;
// //   return out;
// // }
//
// // double cpp_time_add2(const double timestamp,
// //                     const std::string timezone,
// //                     int years = 0,
// //                     int months = 0,
// //                     int weeks = 0,
// //                     int days = 0,
// //                     int hours = 0,
// //                     int minutes = 0,
// //                     double seconds = 0,
// //                     std::string roll_month = "NA",
// //                     cpp11::strings roll_dst = R_NilValue){
// //
// //   RollMonth rmonth = parse_month_roll(roll_month);
// //   DST rdst(roll_dst, true);
// //
// //   bool
// //   do_year = years != 0,
// //     do_month = months != 0,
// //     do_week = weeks != 0,
// //     do_day = days != 0,
// //     do_hour = hours != 0,
// //     do_minute = minutes != 0,
// //     do_second = seconds != 0;
// //
// //   double dti = timestamp;
// //   const std::string tz_name = timezone;
// //
// //   cctz::time_zone tz;
// //   load_tz_or_fail(tz_name, tz, "Invalid timezone");
// //
// //   int y = 0, m = 0, w = 0, d = 0, H = 0, M = 0;
// //   double s = 0.0;
// //   int_fast64_t S = 0;
// //
// //   cctz::civil_year cy;
// //   cctz::civil_month cm;
// //   cctz::civil_day cd;
// //   cctz::civil_hour cH;
// //   cctz::civil_minute cM;
// //   cctz::civil_second cS;
// //
// //   double out;
// //
// //   bool is_negative = false;
// //   bool do_negative = true;
// //
// //   int_fast64_t secs = floor_to_int64(dti);
// //
// //     if ( (dti != dti) || secs == LLONG_MIN) {
// //       if (dti == R_PosInf)
// //         out = R_PosInf;
// //       else if (dti == R_NegInf)
// //         out = R_NegInf;
// //       else
// //         out = NA_REAL;
// //     }
// //
// //     bool add_my_hms = true;
// //     double rem = dti - secs;
// //     sys_seconds ss(secs);
// //     time_point tp(ss);
// //     cctz::civil_second cs = cctz::convert(tp, tz);
// //
// //     int_fast64_t
// //     ty = cs.year(), tm = cs.month(), td = cs.day(),
// //       tH = cs.hour(), tM = cs.minute(), tS = cs.second();
// //
// //     cy = cctz::civil_year(ty);
// //
// //     if (do_year) {
// //       y = years;
// //       if (y == NA_INTEGER) { out = NA_REAL;}
// //       SET_NEGATIVE(y)
// //         cy += y;
// //     }
// //     cm = cctz::civil_month(cy) + (tm -1);
// //     if (do_month) {
// //       m = months;
// //       if (m == NA_INTEGER) { out = NA_REAL; }
// //       SET_NEGATIVE(m)
// //         cm += m;
// //     }
// //     cd = cctz::civil_day(cm) + (td - 1);
// //     if (cd.day() != td) {
// //       // month rolling kicks in
// //       switch(rmonth) {
// //       case RollMonth::FULL: break;
// //       case RollMonth::PREDAY:
// //         cd = cctz::civil_day(cctz::civil_month(cd)) - 1;
// //         break;
// //       case RollMonth::BOUNDARY:
// //         cd = cctz::civil_day(cctz::civil_month(cd));
// //         add_my_hms = false;
// //         break;
// //       case RollMonth::POSTDAY:
// //         cd = cctz::civil_day(cctz::civil_month(cd));
// //         break;
// //       case RollMonth::NA:
// //         out = NA_REAL;
// //       case RollMonth::NAym: break;
// //       }
// //     }
// //     if (do_week) {
// //       w = weeks;
// //       if (w == NA_INTEGER) { out = NA_REAL; }
// //       SET_NEGATIVE(w)
// //         cd += w * 7;
// //     }
// //     if (do_day) {
// //       d = days;
// //       if (d == NA_INTEGER) { out = NA_REAL; }
// //       SET_NEGATIVE(d)
// //         cd += d;
// //     }
// //     cH = cctz::civil_hour(cd);
// //     if (add_my_hms) cH += tH;
// //     if (do_hour) {
// //       H = hours;
// //       if (H == NA_INTEGER) { out = NA_REAL; }
// //       SET_NEGATIVE(H)
// //         cH += H;
// //     }
// //     cM = cctz::civil_minute(cH);
// //     if (add_my_hms)
// //       cM += tM;
// //     if (do_minute) {
// //       M = minutes;
// //       if (M == NA_INTEGER) { out = NA_REAL; }
// //       SET_NEGATIVE(M)
// //         cM += M;
// //     }
// //     cS = cctz::civil_second(cM);
// //     if (add_my_hms) cS += tS;
// //     if (do_second) {
// //       s = seconds;
// //       if (s != s) { out = NA_REAL; }
// //       S = floor_to_int64(s);
// //       if (S == LLONG_MIN) { out = NA_REAL; }
// //       SET_NEGATIVE(S)
// //         rem += s - S;
// //       cS += S;
// //     }
// //
// //     s = civil_lookup_to_posix(tz.lookup(cS), rdst, is_negative);
// //     out = s + rem;
// //     return out;
// // }
//
// // cpp11::doubles adjust_duration_estimate(
// //     cpp11::writable::doubles est, cpp11::doubles start, cpp11::doubles end,
// //     cpp11::doubles width, const std::string unit
// // ){
// //
// //   R_xlen_t n = est.size();
// //   R_xlen_t end_size = end.size();
// //   cpp11::strings roll_dst = Rf_protect(Rf_ScalarString(Rf_mkChar("NA")));
// //   std::string roll_month = "preday";
// //
// //   for (R_xlen_t i = 0; i < n; ++i){
// //     est[i] = std::ceil(est[i]);
// //   }
// //
// //   // Temporary variables
// //   cpp11::writable::doubles temp_adj(n);
// //   cpp11::writable::list period_add(1);
// //   period_add.names() = unit;
// //
// //   double res;
// //
// //   // width * est
// //   for (R_xlen_t i = 0; i < n; ++i){
// //     res = width[i] * est[i];
// //     temp_adj[i] = res != res ? NA_REAL : res;
// //   }
// //   period_add[0] = temp_adj;
// //   cpp11::writable::doubles up_date = C_time_add(start, period_add, roll_month, roll_dst);
// //
// //   R_xlen_t n_adj, k;
// //
// //   cpp11::function cheapr_which = cpp11::package("cheapr")["which_"];
// //   cpp11::writable::logicals compare(n);
// //
// //   for (R_xlen_t i = 0; i < n; ++i){
// //     compare[i] = up_date[i] < end[i];
// //   }
// //   cpp11::integers which = cpp11::as_integers(cheapr_which(compare));
// //
// //   n_adj = which.size();
// //   while (n_adj > 0){
// //     cpp11::writable::doubles temp2(n_adj);
// //     cpp11::writable::doubles temp_start(n_adj);
// //     for (R_xlen_t j = 0; j < n_adj; ++j){
// //       k = which[j];
// //       est[k] += 1;
// //       temp2[j] = width[k] * est[k];
// //       temp_start[j] = start[k];
// //     }
// //     period_add[0] = temp2;
// //     temp_start = C_time_add(temp_start, period_add, roll_month, roll_dst);
// //     for (R_xlen_t j = 0; j < n_adj; ++j){
// //       k = which[j];
// //       up_date[k] = temp_start[j];
// //     }
// //     for (R_xlen_t i = 0; i < n; ++i){
// //       compare[i] = up_date[i] < end[i];
// //     }
// //     which = cpp11::as_integers(cheapr_which(compare));
// //     n_adj = which.size();
// //   }
// //
// //   cpp11::writable::doubles low_date = up_date;
// //
// //   for (R_xlen_t i = 0; i < n; ++i){
// //     compare[i] = low_date[i] > end[i];
// //   }
// //   which = cpp11::as_integers(cheapr_which(compare));
// //
// //   n_adj = which.size();
// //   while (n_adj > 0){
// //     cpp11::writable::doubles temp2(n_adj);
// //     cpp11::writable::doubles temp_start(n_adj);
// //     for (R_xlen_t j = 0; j < n_adj; ++j){
// //       k = which[j];
// //       est[k] -= 1;
// //       up_date[k] = low_date[k];
// //       temp2[j] = width[k] * est[k];
// //       temp_start[j] = start[k];
// //     }
// //     period_add[0] = temp2;
// //     temp_start = C_time_add(temp_start, period_add, roll_month, roll_dst);
// //     for (R_xlen_t j = 0; j < n_adj; ++j){
// //       k = which[j];
// //       low_date[k] = temp_start[j];
// //     }
// //     for (R_xlen_t i = 0; i < n; ++i){
// //       compare[i] = low_date[i] > end[i];
// //     }
// //     which = cpp11::as_integers(cheapr_which(compare));
// //     n_adj = which.size();
// //   }
// //
// //   double frac;
// //   cpp11::writable::doubles out(n);
// //
// //   for (R_xlen_t i = 0; i < n; ++i){
// //     frac = (low_date[i] == up_date[i]) ?
// //     0 : (end[i % end_size] - low_date[i]) / (up_date[i] - low_date[i]);
// //     out[i] = est[i] + frac;
// //   }
// //   Rf_unprotect(1);
// //   return out;
// // }


// int64_t add_months_optimized(int64_t date, int months_add){
//   using namespace std::chrono;
//   const sys_days sd{days{date}};          // Single conversion to time_point
//   const year_month_day ymd{sd};           // Calendar date from days
//
//   // Extract components directly
//   const auto ym = ymd.year()/ymd.month();
//   const auto day = ymd.day();
//   const bool was_last = day == (ym/last).day();  // Direct last-day check
//
//   // Add months at year_month level (cheaper than year_month_day)
//   auto new_ym = ym + months{months_add};
//
//   // Preserve-day-then-adjust pattern
//   year_month_day result = new_ym/day;
//   if (!result.ok() || was_last) {
//     result = new_ym/last;
//   }
//
//   return sys_days{result} - sd.time_since_epoch();
// }

// C++20 solution

// using namespace std::chrono;
// int add_months(int date, int months_add){
//   year_month_day ymd = year_month_day(sys_days(days(date)));
//   // year_month_day ymd = year_month_day{sys_days{days{date}}};
//   months m = months(months_add);
//   bool was_last = ymd == ymd.year()/ymd.month()/last;
//   ymd = ymd + m;
//   if (!ymd.ok() || was_last)
//     ymd = ymd.year()/ymd.month()/last;
//   sys_days days_since_epoch = sys_days(ymd);
//   return days_since_epoch.time_since_epoch().count();
// }

#include "date/date.h"

int add_months(int date, int months_add){
  using namespace date;
  year_month_day ymd = year_month_day(sys_days(days(date)));
  months m = months(months_add);
  bool was_last = ymd == ymd.year()/ymd.month()/last;
  ymd = ymd + m;
  if (!ymd.ok() || was_last)
    ymd = ymd.year()/ymd.month()/last;
  sys_days days_since_epoch = sys_days(ymd);
  return days_since_epoch.time_since_epoch().count();
}

// Add months to date vector with month-rollback

[[cpp11::register]]
SEXP cpp_add_months(SEXP date, SEXP num_months){
  R_xlen_t dn = Rf_xlength(date);
  R_xlen_t mn = Rf_xlength(num_months);
  R_xlen_t n = std::max(dn, mn);
  if (dn <= 0 || dn <= 0) n = 0;
  SEXP months = Rf_protect(Rf_coerceVector(num_months, INTSXP));
  int *p_months = INTEGER(months);

  int nmnths;

  switch (TYPEOF(date)){
  case INTSXP: {
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
    int *p_out = INTEGER(out);
    int *p_date = INTEGER(date);
    int ndates;

    R_xlen_t i, di, mi;
    for (i = di = mi = 0; i < n;
    di = (++di == dn) ? 0 : di,
      mi = (++mi == mn) ? 0 : mi, ++i){
      ndates = p_date[di];
      nmnths = p_months[mi];

      p_out[i] = (ndates == NA_INTEGER || nmnths == NA_INTEGER) ?
      NA_INTEGER : add_months(p_date[di], p_months[mi]);
    }
    SHALLOW_DUPLICATE_ATTRIB(out, date);
    Rf_unprotect(2);
    return out;
  }
  case REALSXP: {
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, n));
    double *p_out = REAL(out);
    double *p_date = REAL(date);
    double ndates;

    R_xlen_t i, di, mi;
    for (i = di = mi = 0; i < n;
    di = (++di == dn) ? 0 : di,
      mi = (++mi == mn) ? 0 : mi, ++i){
      ndates = p_date[di];
      nmnths = p_months[mi];

      p_out[i] = (ndates != ndates || nmnths == NA_INTEGER) ?
      NA_REAL : add_months(p_date[di], p_months[mi]);
    }
    SHALLOW_DUPLICATE_ATTRIB(out, date);
    Rf_unprotect(2);
    return out;
  }
  default: {
    Rf_unprotect(1);
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(date)));
  }
  }
}
