// #include <Rcpp.h>
// using namespace Rcpp;

// bool any_int_lt(IntegerVector x, int value) {
//   bool out = false;
//   int n = x.length();
//   for (int i = 0; i < n; ++i) {
//     if (x[i] < value && !IntegerVector::is_na(x[i])){
//       out = true;
//       break;
//     }
//   }
//   return out;
// }
//
// bool any_num_lt(NumericVector x, double value, Nullable<NumericVector> tol) {
//   double tolerance = sqrt(std::numeric_limits<double>::epsilon());
//   if (tol.isNotNull()){
//     tolerance = as<double>(tol);
//   }
//   tolerance = -tolerance;
//   bool out = false;
//   int n = x.length();
//   for (int i = 0; i < n; ++i) {
//     if ( ( (x[i] - value) < tolerance) && !NumericVector::is_na(x[i])){
//       out = true;
//       break;
//     }
//   }
//   return out;
// }
//
// bool any_int_lte(IntegerVector x, int value) {
//   bool out = false;
//   int n = x.length();
//   for (int i = 0; i < n; ++i) {
//     if (x[i] <= value && !IntegerVector::is_na(x[i])){
//       out = true;
//       break;
//     }
//   }
//   return out;
// }
//
// bool any_num_lte(NumericVector x, double value, Nullable<NumericVector> tol) {
//   double tolerance = sqrt(std::numeric_limits<double>::epsilon());
//   if (tol.isNotNull()){
//     tolerance = as<double>(tol);
//   }
//   bool out = false;
//   int n = x.length();
//   for (int i = 0; i < n; ++i) {
//     if ( ( (x[i] - value) < tolerance) && !NumericVector::is_na(x[i])){
//       out = true;
//       break;
//     }
//   }
//   return out;
// }
//
// bool any_int_gt(IntegerVector x, int value) {
//   bool out = false;
//   int n = x.length();
//   for (int i = 0; i < n; ++i) {
//     if (x[i] > value && !IntegerVector::is_na(x[i])){
//       out = true;
//       break;
//     }
//   }
//   return out;
// }
//
// bool any_num_gt(NumericVector x, double value, Nullable<NumericVector> tol) {
//   double tolerance = sqrt(std::numeric_limits<double>::epsilon());
//   if (tol.isNotNull()){
//     tolerance = as<double>(tol);
//   }
//   bool out = false;
//   int n = x.length();
//   for (int i = 0; i < n; ++i) {
//     if ( ( (x[i] - value) > tolerance) && !NumericVector::is_na(x[i])){
//       out = true;
//       break;
//     }
//   }
//   return out;
// }
//
// bool any_int_gte(IntegerVector x, int value) {
//   bool out = false;
//   int n = x.length();
//   for (int i = 0; i < n; ++i) {
//     if (x[i] >= value && !IntegerVector::is_na(x[i])){
//       out = true;
//       break;
//     }
//   }
//   return out;
// }
//
// bool any_num_gte(NumericVector x, double value, Nullable<NumericVector> tol) {
//   double tolerance = sqrt(std::numeric_limits<double>::epsilon());
//   if (tol.isNotNull()){
//     tolerance = as<double>(tol);
//   }
//   tolerance = -tolerance;
//   bool out = false;
//   int n = x.length();
//   for (int i = 0; i < n; ++i) {
//     if ( ( (x[i] - value) > tolerance) && !NumericVector::is_na(x[i])){
//       out = true;
//       break;
//     }
//   }
//   return out;
// }
//
// bool any_int_equal(IntegerVector x, int value) {
//   bool out = false;
//   int n = x.length();
//   for (int i = 0; i < n; ++i) {
//     if (x[i] == value){
//       out = true;
//       break;
//     }
//   }
//   return out;
// }
//
// bool any_num_equal(NumericVector x, double value, Nullable<NumericVector> tol) {
//   double tolerance = sqrt(std::numeric_limits<double>::epsilon());
//   if (tol.isNotNull()){
//     tolerance = as<double>(tol);
//   }
//   bool out = false;
//   int n = x.length();
//   for (int i = 0; i < n; ++i) {
//     if ( ( abs(x[i] - value) < tolerance) ){
//       out = true;
//       break;
//     }
//   }
//   return out;
// }
