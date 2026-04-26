// metfrag_runner.h — MetFragCL subprocess runner for NTS data
//
// Invokes MetFragCL (JAR or native executable) per feature, parses
// the CSV output, computes cosine similarity for explained peaks,
// and populates nts::NTS_DATA::suspects.

#ifndef NTS_METFRAG_RUNNER_H
#define NTS_METFRAG_RUNNER_H

#include <string>
#include <utility>
#include <vector>

namespace nts
{
  struct NTS_DATA;

  namespace metfrag_runner
  {
    struct MetFragParams
    {
      std::string metfrag_path;          ///< Full path to MetFragCL JAR or native executable.
      std::string database_type;         ///< MetFrag database type (e.g. "LocalCSV", "PubChem").
      std::string database_path;         ///< Path to local database file (LocalCSV / LocalSDF).
      double      ppm            = 5.0;  ///< MS1 precursor mass tolerance (ppm).
      double      sec            = 10.0; ///< RT tolerance for post-filtering (seconds).
      double      ppmMS2         = 10.0; ///< MS2 fragment tolerance (ppm).
      double      mzrMS2         = 0.008;///< MS2 minimum absolute m/z tolerance.
      int         top_n          = 1;    ///< Max candidates kept per feature (top-ranked by score).
      bool        filtered       = false;///< Include filtered features when true.
      std::string java_path      = "java";///< Path to Java executable (used in JAR mode only).
      std::string run_dir;               ///< Directory for temp files and logs; created if absent.
      bool        debug          = false;///< Write per-feature debug metadata when true.
      std::vector<std::pair<std::string, std::string>> extra_params; ///< Additional MetFrag parameters (override-last).
    };

    /**
     * Run MetFragCL screening for all (or selected) analyses in nts_data.
     * Results are written to nts_data.suspects[i] for each analysis index i.
     *
     * @param nts_data     NTS data with loaded features.
     * @param analyses     If non-empty, only these analysis names are processed.
     * @param params       MetFrag runner configuration.
     */
    void metfrag_screening_impl(
        NTS_DATA &nts_data,
        const std::vector<std::string> &analyses,
        const MetFragParams &params);

  } // namespace metfrag_runner
} // namespace nts

#endif // NTS_METFRAG_RUNNER_H
