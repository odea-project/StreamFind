// assign_transformation_products.h
// C++ implementation of the AssignTransformationProducts algorithm.
// Accepts a flat suspects list (with feature_group pre-joined) and a
// transformation-products input table; returns an expanded output table
// with per-combination cosine-similarity and RT-plausibility metrics.

#ifndef ASSIGN_TRANSFORMATION_PRODUCTS_H
#define ASSIGN_TRANSFORMATION_PRODUCTS_H

#include <string>
#include <vector>

namespace nts::assign_transformation_products
{

  // ── Input: flat suspects (all analyses, feature_group already joined) ───────

  struct FlatSuspect
  {
    std::string SMILES;
    std::string feature_group;
    double      exp_rt;
    int         exp_ms2_size;
    std::string exp_ms2_mz;
    std::string exp_ms2_intensity;
  };

  // ── Input: one row of the transformation-products data.frame ───────────────

  struct TPInputRow
  {
    std::string name;
    std::string formula;
    double      mass;
    std::string SMILES;
    std::string InChI;
    std::string InChIKey;
    double      xLogP;
    std::string transformation;

    std::string precursor_name;
    std::string precursor_formula;
    double      precursor_mass;
    std::string precursor_SMILES;
    std::string precursor_InChI;
    std::string precursor_InChIKey;
    double      precursor_xLogP;

    std::string main_precursor_name;
    std::string main_precursor_formula;
    double      main_precursor_mass;
    std::string main_precursor_SMILES;
    std::string main_precursor_InChI;
    std::string main_precursor_InChIKey;
    double      main_precursor_xLogP;
  };

  // ── Output: column-parallel structure ──────────────────────────────────────

  struct TRANSFORMATION_PRODUCTS
  {
    std::vector<std::string> name;
    std::vector<std::string> formula;
    std::vector<double>      mass;
    std::vector<std::string> SMILES;
    std::vector<std::string> InChI;
    std::vector<std::string> InChIKey;
    std::vector<double>      xLogP;
    std::vector<std::string> transformation;

    std::vector<std::string> precursor_name;
    std::vector<std::string> precursor_formula;
    std::vector<double>      precursor_mass;
    std::vector<std::string> precursor_SMILES;
    std::vector<std::string> precursor_InChI;
    std::vector<std::string> precursor_InChIKey;
    std::vector<double>      precursor_xLogP;

    std::vector<std::string> main_precursor_name;
    std::vector<std::string> main_precursor_formula;
    std::vector<double>      main_precursor_mass;
    std::vector<std::string> main_precursor_SMILES;
    std::vector<std::string> main_precursor_InChI;
    std::vector<std::string> main_precursor_InChIKey;
    std::vector<double>      main_precursor_xLogP;

    std::vector<std::string> feature_group;
    std::vector<std::string> precursor_feature_group;
    std::vector<std::string> main_precursor_feature_group;
    std::vector<double>      cosine_similarity;
    std::vector<double>      main_precursor_cosine_similarity;
    std::vector<double>      rt_plausibility;
    std::vector<double>      main_precursor_rt_plausibility;

    int size() const { return static_cast<int>(name.size()); }

    void append_row(
        const TPInputRow  &src,
        const std::string &prod_fg,
        const std::string &prec_fg,
        const std::string &main_fg,
        double             cos_sim,
        double             main_cos_sim,
        double             rt_plaus,
        double             main_rt_plaus);
  };

  // ── Main entry point ────────────────────────────────────────────────────────

  TRANSFORMATION_PRODUCTS assign_transformation_products_impl(
      const std::vector<FlatSuspect> &suspects,
      const std::vector<TPInputRow>  &tp_rows,
      const std::string              &chromatographic_phase,
      double                          mzrMS2);

} // namespace nts::assign_transformation_products

#endif // ASSIGN_TRANSFORMATION_PRODUCTS_H
