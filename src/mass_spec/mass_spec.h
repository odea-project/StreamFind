#ifndef MASS_SPEC_H
#define MASS_SPEC_H

// mass_spec.h - Main API interface for mass spectrometry processing
// This header includes all submodules and provides the main API

#include "processing.h"
#include "clustering.h"
#include "reader.h"

namespace mass_spec
{

    // Re-export common types from submodules for convenience
    using clustering::SpectraClusterInput;
    using clustering::SpectraClusterOutput;
    using processing::OLSResult;
    using processing::Peak;

    // ============================================================================
    // MARK: Spectra charge-state calculation
    // ============================================================================

    struct SpectrumPoint
    {
        double mz;
        double intensity;
    };

    struct ChargeRow
    {
        double mz;
        double intensity;
        double cluster_mz; ///< rounded cluster representative
        int z;
        double mass;
        int polarity;
    };

    /// Calculate charge states for a single merged spectrum (sorted by mz).
    /// @param pts list of (mz, intensity) pairs, sorted ascending by mz
    /// @param polarity +1 or -1
    /// @param round_val m/z rounding denominator for clustering (e.g. 35)
    /// @param rel_low_cut relative intensity cut (fraction of base peak)
    /// @param abs_low_cut absolute intensity cut
    /// @param top_charges number of top charge candidates to consider
    std::vector<ChargeRow> calculate_spectra_charges(
        const std::vector<SpectrumPoint> &pts,
        int polarity,
        double round_val = 35.0,
        double rel_low_cut = 0.2,
        double abs_low_cut = 300.0,
        int top_charges = 5);

    // ============================================================================
    // MARK: Deconvolution
    // ============================================================================

    struct MassPoint
    {
        double mass;
        double intensity;
    };

    /// Cluster mass values within clust_val tolerance, weighted average of mass,
    /// summed intensity.
    std::vector<MassPoint> cluster_masses(
        const std::vector<MassPoint> &pts,
        double clust_val);

    /// Deconvolute one spectrum using its pre-computed charge rows.
    /// @param spectrum_pts raw (mz, intensity) pairs
    /// @param charges charge rows for this spectrum
    /// @param clust_val mass clustering tolerance (Da)
    /// @param window m/z window around each charge m/z to extract
    std::vector<MassPoint> deconvolute_spectrum(
        const std::vector<SpectrumPoint> &spectrum_pts,
        const std::vector<ChargeRow> &charges,
        double clust_val = 0.1,
        double window = 20.0);

} // namespace mass_spec

#endif // MASS_SPEC_H
