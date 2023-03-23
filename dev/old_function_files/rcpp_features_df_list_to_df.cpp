// [[Rcpp::depends(RcppArmadillo)]]

#include <string>
#include <algorithm>
#include <vector>
#include <numeric>
#include <math.h>
#include <RcppArmadillo.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::DataFrame rcpp_features_df_list_to_df(Rcpp::List features) {

  int n_analyses = features.size();
  if (n_analyses == 0) return DataFrame();

  StringVector analyses = features.names();

  int ncols = 0;
  int nrows = 0;
  CharacterVector colnames;
  DataFrame res;

  StringVector must_have_cols = {
    "feature", "analysis",
    "rt", "rtmax", "rtmin",
    "mz", "mzmax", "mzmin", "mass",
    "filled", "adduct", "intensity", "group"
  };

  for (int i = 0; i < n_analyses; ++i) {
    DataFrame df_temp;
    df_temp = as<DataFrame>(features[i]);
    int df_nrows = df_temp.nrows();

    if (df_nrows > 0) {
      bool has_ana_col = df_temp.containsElementNamed("analysis");

      if (!has_ana_col) {
        vector<string> ana(1);
        ana[0] = analyses(i);
        StringVector ana_rcpp;
        ana_rcpp = analyses(i);
        int n_rows = df_temp.nrows();
        StringVector ana_col = rep(ana_rcpp, n_rows);
        df_temp.push_front(ana_col, "analysis");
      }

      if (i == 0 || nrows == 0) {
        nrows = df_nrows;
        ncols = df_temp.length();
        res = as<DataFrame>(df_temp);
        colnames = df_temp.names();

      } else {

        if (df_temp.length() != ncols) {
          Rcpp::Rcout << "!! Data frames must have the same number of columns! \n";
          return DataFrame();
        }
        nrows += df_nrows;
      }

      StringVector df_temp_cols = df_temp.names();

      for (int i=0; i<must_have_cols.size(); ++i) {
        auto str_position = std::find(df_temp_cols.begin(), df_temp_cols.end(), must_have_cols(i));

        if (str_position == df_temp_cols.end()) {
          Rcout << "!! The column '" << must_have_cols(i) << "' not found in features! \n";
          return DataFrame();
        }
      }

      features[i] = df_temp;
    }
  }

  List res_list(ncols);
  CharacterVector rownames(nrows);

  for (int j = 0; j < ncols; ++j) {
    SEXP res_col = res[j];
    Rcpp::Shield<SEXP> res_class(Rf_getAttrib(res_col, R_ClassSymbol));
    Rcpp::Shield<SEXP> res_attr(Rf_getAttrib(res_col, R_NamesSymbol));
    Rcpp::Function res_attr_assign("names<-");
    res_col = Rf_lengthgets(res_col, nrows);
    Rf_setAttrib(res_col, R_ClassSymbol, res_class);
    res_col = res_attr_assign(res_col, rownames);
    Rf_setAttrib(res_col, R_NamesSymbol, res_attr);
    int rowcount = 0;

    // concatenate input data frames vertically into result column vector
    for (int i = 0; i < n_analyses; ++i) {
      DataFrame df = as<DataFrame>(features[i]);
      SEXP df_col = df[j];
      Rf_copyMostAttrib(df_col, res_col);
      int type = TYPEOF(df_col);

      if (type == 14) {
        std::copy(REAL(df_col), REAL(df_col) + LENGTH(df_col), REAL(res_col) + rowcount);
      } else if (type == 13) {
        std::copy(INTEGER(df_col), INTEGER(df_col) + LENGTH(df_col), INTEGER(res_col) + rowcount);
      } else if (type == 16) {
        std::copy(STRING_PTR(df_col), STRING_PTR(df_col) + LENGTH(df_col), STRING_PTR(res_col) + rowcount);
      } else if (type == 10) {
        std::copy(LOGICAL(df_col), LOGICAL(df_col) + LENGTH(df_col), LOGICAL(res_col) + rowcount);
      } else {
        Rcpp::Rcout << type << " \n";
      }

      rowcount += df.nrows();
    }

    res_list[j] = res_col;
  }

  DataFrame res_df(res_list);
  res_df.attr("names") = colnames;
  res_df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

  // List list_out;
  //
  // list_out["size"] = n_analyses;
  // list_out["names"] = analyses;
  // list_out["df"] = features;
  // list_out["res"] = res_df;
  // list_out["nrows"] = nrows;
  // list_out["ncols"] = ncols;


  return(res_df);
}
