#ifndef MASS_SPEC_CLUSTERING_H
#define MASS_SPEC_CLUSTERING_H

#include <vector>
#include <string>
#include <map>

namespace mass_spec {
namespace clustering {

/// Cluster spectra by m/z threshold and presence filter
/// @param spectra Input spectra data frame columns
/// @param mzClust m/z clustering tolerance (Da)
/// @param presence Minimum presence threshold (fraction)
/// @param verbose Enable verbose output
struct SpectraClusterInput {
    std::vector<std::string> unique_id;
    std::vector<std::string> analysis;
    std::vector<int> polarity;
    std::vector<std::string> id;
    std::vector<double> rt;
    std::vector<double> mz;
    std::vector<double> intensity;
    std::vector<double> pre_mz;      // optional, can be NaN
    std::vector<double> pre_ce;      // optional, can be NaN
    bool has_pre_mz = false;
    bool has_pre_ce = false;
};

/// Output clustered spectra
struct SpectraClusterOutput {
    std::vector<std::map<std::string, std::vector<double>>> spectra;
};

/// Cluster spectra with m/z threshold and presence filter
/// Returns list of clustered spectra
SpectraClusterOutput cluster_spectra(
    const SpectraClusterInput& input,
    double mzClust = 0.005,
    double presence = 0.8,
    bool verbose = false);

} // namespace clustering
} // namespace ms

#endif // MASS_SPEC_CLUSTERING_H
