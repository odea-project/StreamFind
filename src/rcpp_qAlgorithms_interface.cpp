#include <string>
#include <vector>
#include <Rcpp.h>

// #define STREAMCRAFT_HEADER_ONLY
// #include "StreamCraft/src/StreamCraft_lib.hpp"

#include "qAlgorithms/src/qalgorithms_measurement_data_tensor.cpp"
#include "qAlgorithms/src/qalgorithms_measurement_data.cpp"
#include "qAlgorithms/src/qalgorithms_utils.cpp"
#include "qAlgorithms/src/qalgorithms_qbin.cpp"
#include "qAlgorithms/src/qalgorithms_qpeaks.cpp"

#include "qAlgorithms/include/qalgorithms_console_output.h"
#include "qAlgorithms/include/qalgorithms_datatype_peak.h"
#include "qAlgorithms/include/qalgorithms_measurement_data_tensor.h"
#include "qAlgorithms/include/qalgorithms_measurement_data.h"
#include "qAlgorithms/include/qalgorithms_utils.h"
#include "qAlgorithms/include/qalgorithms_qbin.h"
#include "qAlgorithms/include/qalgorithms_qpeaks.h"

#include <iostream>
#include <chrono>
#include <fstream>
#include <filesystem>
#include <cstdlib>
#include <iomanip>


// [[Rcpp::export]]
Rcpp::List rcpp_centroid_spectra_qCentroids(std::vector<std::string> fileNames, int maxScale = 5, int mode = 2) {
  
  
  Rcpp::List list_out;
  
  PRINT_LOGO
  
  q::Algorithms::qPeaks::initialize();

  int i = 1;
  int i_total = fileNames.size();

  for (auto fileName : fileNames)
  {

    if (std::filesystem::path(fileName).extension() == ".mzML")
    {
      PRINT_FILE(fileName, i, i_total)
      std::vector<int> polarities = {1, -1};
      for (auto polarity : polarities)
      {
        PRINT_POLARITY(polarity)
        PRINT_READ_FILE
        sc::MZML data(fileName); // create mzML object
        PRINT_DONE
        PRINT_FIND_CENTROIDS
        q::Algorithms::qPeaks qpeaks;              // create qPeaks object
        q::MeasurementData::TensorData tensorData; // create tensorData object
        
        sc::MS_SPECTRA_HEADERS hd = data.get_spectra_headers();
        std::vector<int> spectrum_mode = hd.mode;
        std::vector<int> spectrum_polarity = hd.polarity;
        std::vector<int> indices = hd.index;
        std::vector<int> ms_levels = hd.level;
        std::vector<int> num_datapoints = hd.array_length;
        double expectedDifference = 0.0;
        
        int start_index = 10;
        
        indices.erase(std::remove_if(indices.begin(), indices.end(), [&ms_levels](int i) { return ms_levels[i] != 1; }), indices.end());
        
        indices.erase(std::remove_if(indices.begin(), indices.end(), [&spectrum_polarity, &num_datapoints, &polarity, &start_index](int i)
        { return spectrum_polarity[i] != polarity || i < start_index || num_datapoints[i] < 5; }), indices.end());
        
        std::cout << "Number of spectra: " << indices.size() << std::endl;
        
        int num_centroided_spectra = std::count(spectrum_mode.begin(), spectrum_mode.end(), 2);
        if (num_centroided_spectra != 0)
        {
          indices.erase(std::remove_if(indices.begin(), indices.end(), [&spectrum_mode](int i)
          { return spectrum_mode[i] != 1; }), indices.end());
        }
        
        std::vector<double> retention_times(indices.size());
        std::transform(indices.begin(), indices.end(), retention_times.begin(), [&](int idx) { return hd.rt[idx]; });
        float rt_diff = tensorData.calcRTDiff(retention_times);
        tensorData.rt_diff = rt_diff;
        
        if (indices.size() == 0) continue;
        
        // std::vector<std::vector<std::unique_ptr<q::DataType::Peak>>> centroids(indices.size());
        
        std::vector<std::vector<std::unique_ptr<q::DataType::Peak>>> centroids =
          std::vector<std::vector<std::unique_ptr<q::DataType::Peak>>>(indices.size()); // create vector of unique pointers to peaks
        
        std::vector<std::vector<double>> data_vec = data.get_spectrum(indices[start_index]).binary_data;
        expectedDifference = tensorData.calcExpectedDiff(data_vec[0]);
        
        std::cout << "Expected difference: " << expectedDifference << std::endl;
        
        for (size_t i = 0; i < 1; ++i) // loop over all indices
        {
            const int index = indices[i];
            std::vector<q::MeasurementData::MeasurementData::dataPoint> dataPoints = tensorData.mzmlToDataPoint(data, index);
            
            std::cout << "Number of data points: " << dataPoints.size() << std::endl;
            q::MeasurementData::MeasurementData::treatedData treatedData = tensorData.pretreatData(dataPoints, expectedDifference);
            
            
            std::cout << "Number of treated data points: " << treatedData.dataPoints.size() << std::endl;
            
            std::cout << "rt val" << retention_times[i] << std::endl;
            
            qpeaks.findCentroids(centroids[i], treatedData, index, retention_times[i]);
        } // for
        
        std::cout << "Number of centroids: " << centroids.size() << std::endl;
        
        continue;
        
        // auto start = std::chrono::high_resolution_clock::now();
        // std::vector<std::vector<std::unique_ptr<q::DataType::Peak>>> centroids =
        //   tensorData.findCentroids_MZML(qpeaks, data, true, polarity, 10); // read mzML file and find centroids via qPeaks
        // auto end = std::chrono::high_resolution_clock::now();
        // std::chrono::duration<double> duration = end - start;
        // if (centroids.size() == 0)
        // {
        //   std::cout << "no centroids found" << std::endl;
        //   continue;
        // }
        // size_t num_centroids = 0;
        // for (size_t i = 0; i < centroids.size(); ++i)
        // {
        //   for (size_t j = 0; j < centroids[i].size(); ++j)
        //   {
        //     num_centroids++;
        //   }
        // }
        // PRINT_DONE_TIME(duration, num_centroids)
        // 
        //   PRINT_FIND_EICS
        //   q::Algorithms::qBinning::CentroidedData unbinnedCentroids = qpeaks.passToBinning(centroids, centroids.size());
        // start = std::chrono::high_resolution_clock::now();
        // std::vector<q::Algorithms::qBinning::EIC> binnedData = q::Algorithms::qBinning::performQbinning(unbinnedCentroids, "", "", 3, true, false, false);
        // end = std::chrono::high_resolution_clock::now();
        // duration = end - start;
        // PRINT_DONE_TIME(duration, binnedData.size())
        // 
        //   PRINT_FIND_CHROMATROGRAPHIC_PEAKS
        //   start = std::chrono::high_resolution_clock::now();
        // std::vector<std::vector<std::unique_ptr<q::DataType::Peak>>> peaks =
        //   tensorData.findPeaks_QBIN(qpeaks, binnedData);
        // end = std::chrono::high_resolution_clock::now();
        // duration = end - start;
        // size_t num_peaks = 0;
        // for (size_t i = 0; i < peaks.size(); ++i)
        // {
        //   for (size_t j = 0; j < peaks[i].size(); ++j)
        //   {
        //     num_peaks++;
        //   }
        // }
        // PRINT_DONE_TIME(duration, num_peaks)
        // 
        // std::string polarity_str = polarity == 1 ? "positive" : "negative";
        //   
        //   // write peaks to csv file
        //   PRINT_WRITE_FEATURES_TO_FILE
        //   // use file input name to create output file name with polarity_features.csv extension
        //   std::string output_filename = fileName.substr(0, fileName.find_last_of(".")) + "_" + polarity_str + "_features.csv";
        // std::ofstream output_file(output_filename);
        // output_file << "mz,rt,int,mzUncertainty,rtUncertainty,intUncertainty,dqs_cen,dqs_bin,dqs_peak\n";
        // for (size_t i = 0; i < peaks.size(); ++i)
        // {
        //   for (size_t j = 0; j < peaks[i].size(); ++j)
        //   {
        //     output_file << peaks[i][j]->mz << "," << peaks[i][j]->retentionTime << "," << peaks[i][j]->area << ","
        //                 << peaks[i][j]->mzUncertainty << "," << peaks[i][j]->retentionTimeUncertainty << "," << peaks[i][j]->areaUncertainty << ","
        //                 << peaks[i][j]->dqsCen << "," << peaks[i][j]->dqsBin << "," << peaks[i][j]->dqsPeak << "\n";
        //   }
        // }
        // output_file.close();
        // PRINT_DONE
      } // for polarities
    } // if mzML
    else
    {
      // PRINT_FILE(fileName, i, i_total)
      std::cout << "Error: file extension not supported" << std::endl;
    }
    i++;
  } // for fileNames
  
  
  
  
  
  // std::vector<std::string> spectra_cols = spectra.names();
  // 
  // Rcpp::List list_out;
  // 
  // if (spectra.nrows() == 0 || spectra_cols.size() == 0) return list_out;
  // 
  // std::vector<std::string> must_have_names = {"scan", "mz", "intensity"};
  // 
  // std::vector<bool> has_must_have_names(3, false);
  // 
  // for (size_t i = 0; i < must_have_names.size(); ++i) {
  //   for (size_t j = 0; j < spectra_cols.size(); ++j) {
  //     if (must_have_names[i] == spectra_cols[j]) has_must_have_names[i] = true;
  //   }
  // }
  // 
  // for (bool value : has_must_have_names) {
  //   if (!value) {
  //     throw std::runtime_error("Spectra DataFrame does not have all required columns!");
  //   }
  // }
  // 
  // const std::vector<int>& all_scan = spectra["scan"];
  // const std::vector<double>& all_mz = spectra["mz"];
  // const std::vector<double>& all_intensity = spectra["intensity"];
  // 
  // int number_spectra = all_scan.size();
  // 
  // // Rcpp::Rcout << std::endl;
  // // Rcpp::Rcout << "Analyses with " << number_spectra << " spectra!" << std::endl;
  // // Rcpp::Rcout << std::endl;
  // 
  // q::Matrix xyData(number_spectra, 3);
  // 
  // for (size_t i = 0; i < all_scan.size(); i++) {
  //   xyData(i,0) = all_mz[i];
  //   xyData(i,1) = all_intensity[i];
  //   xyData(i,2) = all_scan[i];
  // }
  // 
  // q::tValues tVal;
  // 
  // q::Peakmodel model(maxScale);
  // 
  // switch (mode) {
  //   case 0:
  //     model.setMode(q::Mode::DEBUGGING);
  //     break;
  // 
  //   case 2:
  //     model.setMode(q::Mode::PROGRESS);
  //     break;
  // 
  //   default:
  //     break;
  // }
  // 
  // model.loadTValues(tVal);
  // 
  // model.addMultipleMeasurements(xyData);
  // 
  // // auto start = std::chrono::high_resolution_clock::now();
  // 
  // model.findPeaks();
  // 
  // // auto finish = std::chrono::high_resolution_clock::now();
  // //
  // // std::chrono::duration<double> elapsed = finish - start;
  // // Rcpp::Rcout << std::endl;
  // // Rcpp::Rcout << "Elapsed time: " << elapsed.count() << " s\n";
  // 
  // // model.runRegression();
  // 
  // list_out["scan"] = model.getPeakProperties(q::Peakproperties::SMPLID);
  // list_out["mz"] = model.getPeakProperties(q::Peakproperties::POSITION);
  // list_out["intensity"] = model.getPeakProperties(q::Peakproperties::HEIGHT);
  // list_out["area"] = model.getPeakProperties(q::Peakproperties::AREA);
  // list_out["DQS"] = model.getPeakProperties(q::Peakproperties::DQS);
  // 
  // // list_out["width"] = model.getPeakProperties(q::Peakproperties::WIDTH);
  // // list_out["SIGMAPOSITION"] = model.getPeakProperties(q::Peakproperties::SIGMAPOSITION);
  // // list_out["SIGMAHEIGHT"] = model.getPeakProperties(q::Peakproperties::SIGMAHEIGHT);
  // // list_out["SIGMAAREA"] = model.getPeakProperties(q::Peakproperties::SIGMAAREA);
  // // list_out["SIGMAWIDTH"] = model.getPeakProperties(q::Peakproperties::SIGMAWIDTH);
  // // list_out["COEFF_B0"] = model.getPeakProperties(q::Peakproperties::COEFF_B0);
  // // list_out["COEFF_B1"] = model.getPeakProperties(q::Peakproperties::COEFF_B1);
  // // list_out["COEFF_B2"] = model.getPeakProperties(q::Peakproperties::COEFF_B2);
  // // list_out["COEFF_B3"] = model.getPeakProperties(q::Peakproperties::COEFF_B3);
  // 
  // list_out.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

  return list_out;
}
