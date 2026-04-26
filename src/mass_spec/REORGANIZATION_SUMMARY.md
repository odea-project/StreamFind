# Mass Spec Library Reorganization - Summary

## Files Created (Phase 1: Scaffold)

### Core Structure
- src/mass_spec/reader.h - Abstract base class for MS file readers
- src/mass_spec/reader.cpp - Factory functions and format detection
- src/mass_spec/mzml.h - mzML reader interface
- src/mass_spec/mzml.cpp - mzML reader implementation (stub)
- src/mass_spec/mzxml.h - mzXML reader interface  
- src/mass_spec/mzxml.cpp - mzXML reader implementation (stub)

### Processing Module
- src/mass_spec/processing.h - Signal processing algorithms API
- src/mass_spec/processing.cpp - Implementation of smoothing, baseline correction, peak finding

### Clustering Module
- src/mass_spec/clustering.h - Spectral clustering API
- src/mass_spec/clustering.cpp - Implementation of m/z clustering with presence filter

### Main API
- src/mass_spec/mass_spec.h - Updated to include all submodules
- src/mass_spec/mass_spec.cpp - Existing implementation (to be updated)

## Next Steps

### Phase 2: Complete Implementations
1. Complete mzml.cpp with full mzML parsing logic from streamcraft.cpp
2. Complete mzxml.cpp with full mzXML parsing logic from streamcraft.cpp
3. Move charge state calculation and deconvolution to separate modules

### Phase 3: Export Bridge
1. Update mass_spec_export.cpp to use new module structure
2. Add exports for file I/O functions
3. Consolidate all ms:: namespace exports

### Phase 4: Cleanup
1. Delete streamcraft_export.cpp
2. Delete rcpp_ms_cluster_spectra.cpp
3. Update Makevars with new file wildcards
4. Run Rcpp::compileAttributes()
5. Test all exports

## Design Pattern

The new structure follows the same pattern as nts library:
- mass_spec.h/.cpp - Main API (like nts.h/.cpp)
- Submodules for specific functionality (reader, processing, clustering)
- mass_spec_export.cpp - Rcpp export bridge (like nts_export.cpp)

## Benefits

1. Unified library for all mass spec functionality
2. Clear separation: file I/O vs algorithms
3. Easier to test and maintain
4. Extensible for new file formats
5. Consistent with nts library structure
