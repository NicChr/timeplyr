#include "timeplyr.h"
#include <chrono>

// C++20 solution

// int add_months(int date, int months_add){
//   using namespace std::chrono;
//   year_month_day ymd = year_month_day(sys_days(days(date)));
//   months m = months(months_add);
//   bool was_last = ymd == ymd.year()/ymd.month()/last;
//   ymd = ymd + m;
//   if (!ymd.ok() || was_last)
//     ymd = ymd.year()/ymd.month()/last;
//   sys_days days_since_epoch = sys_days(ymd);
//   return days_since_epoch.time_since_epoch().count();
// }

int add_months_roll_backward(int date, int months_add){
  using namespace std::chrono;
  year_month_day ymd = year_month_day(sys_days(days(date)));
  months m = months(months_add);
  ymd = ymd + m;
  if (!ymd.ok()){
    ymd = ymd.year()/ymd.month()/last;
  }
  sys_days days_since_epoch = sys_days(ymd);
  return days_since_epoch.time_since_epoch().count();
}

int add_months_roll_forward(int date, int months_add){
  using namespace std::chrono;
  year_month_day ymd = year_month_day(sys_days(days(date)));
  months m = months(months_add);
  ymd = ymd + m;
  if (!ymd.ok()){
    auto next_month = year_month(ymd.year(), ymd.month()) + months(1);
    ymd = next_month/day(1);
  }
  sys_days days_since_epoch = sys_days(ymd);
  return days_since_epoch.time_since_epoch().count();
}

int add_months_no_roll(int date, int months_add){
  using namespace std::chrono;
  year_month_day ymd = year_month_day(sys_days(days(date)));
  months m = months(months_add);
  ymd = ymd + m;
  if (!ymd.ok()) return NA_INTEGER;
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
      NA_INTEGER : add_months_roll_backward(p_date[di], p_months[mi]);
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
      NA_REAL : add_months_roll_backward(p_date[di], p_months[mi]);
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
