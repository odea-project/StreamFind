#include <Rcpp.h>
#include <H5Cpp.h>
#include <iostream>
#include <chrono>
#include <fstream>
#include <cstring>

// Helper function to create HDF5 string datatype
H5::StrType create_string_datatype(const std::vector<std::string>& strings) {
  size_t max_len = 0;
  for (const auto& str : strings) {
    max_len = std::max(max_len, str.length());
  }
  // Add some padding for safety
  max_len = std::max(max_len + 10, static_cast<size_t>(50));

  H5::StrType str_type(H5::PredType::C_S1, max_len);
  str_type.setCset(H5T_CSET_UTF8);
  str_type.setStrpad(H5T_STR_NULLTERM);
  return str_type;
}

//' Test HDF5 write performance with mixed data types (numeric and string)
//'
//' This function tests the performance of writing mixed data types (numeric and string)
//' to an HDF5 file. It handles each data type appropriately, storing numeric data as
//' double arrays and string data as variable-length string datasets.
//'
//' @param data_list List containing mixed numeric and string columns
//' @param file_name Name of the output HDF5 file (default: "mixed_performance_test.h5")
//' @param enable_compression Logical, enable gzip compression (default: false)
//' @param enable_chunking Logical, enable chunking for better I/O performance (default: false)
//' @return List containing performance metrics and file information
//' @export
// [[Rcpp::export]]
Rcpp::List test_hdf5_mixed_performance(Rcpp::List data_list, const std::string& file_name = "mixed_performance_test.h5",
                                      bool enable_compression = false, bool enable_chunking = false) {

  Rcpp::List result;

  try {
    // Start timing
    auto start_time = std::chrono::high_resolution_clock::now();

    if (data_list.size() == 0) {
      Rcpp::stop("Input list is empty");
    }

    // Analyze data types and get row count
    int n_rows = 0;
    int n_numeric_cols = 0;
    int n_string_cols = 0;
    std::vector<std::string> column_names = data_list.names();
    std::vector<bool> is_numeric(data_list.size());

    for (int i = 0; i < data_list.size(); ++i) {
      SEXP col = data_list[i];
      if (TYPEOF(col) == REALSXP) {
        is_numeric[i] = true;
        n_numeric_cols++;
        if (n_rows == 0) {
          Rcpp::NumericVector num_col = Rcpp::as<Rcpp::NumericVector>(col);
          n_rows = num_col.size();
        }
      } else if (TYPEOF(col) == STRSXP) {
        is_numeric[i] = false;
        n_string_cols++;
        if (n_rows == 0) {
          Rcpp::CharacterVector str_col = Rcpp::as<Rcpp::CharacterVector>(col);
          n_rows = str_col.size();
        }
      } else {
        Rcpp::stop("Unsupported data type in column " + std::to_string(i + 1));
      }
    }

    Rcpp::Rcout << "Processing " << n_numeric_cols << " numeric and " << n_string_cols << " string columns, " << n_rows << " rows" << std::endl;

    auto conversion_time = std::chrono::high_resolution_clock::now();

    // Create HDF5 file
    H5::Exception::dontPrint();
    H5::H5File file(file_name, H5F_ACC_TRUNC);

    // Create group for mixed data
    H5::Group mixed_group = file.createGroup("/mixed_data");

    // Process each column
    for (int i = 0; i < data_list.size(); ++i) {
      std::string col_name = column_names[i];
      if (col_name.empty()) {
        col_name = "col_" + std::to_string(i + 1);
      }

      if (is_numeric[i]) {
        // Handle numeric data
        Rcpp::NumericVector num_col = Rcpp::as<Rcpp::NumericVector>(data_list[i]);

        // Convert to std::vector for HDF5
        std::vector<double> data_vec(num_col.begin(), num_col.end());

        // Define dimensions
        hsize_t dims[1] = {static_cast<hsize_t>(n_rows)};
        H5::DataSpace dataspace(1, dims);

        // Create dataset creation property list
        H5::DSetCreatPropList create_plist;

        // Set up chunking if enabled
        if (enable_chunking) {
          hsize_t chunk_dims[1] = {std::min(static_cast<hsize_t>(1000), dims[0])};
          create_plist.setChunk(1, chunk_dims);
        }

        // Set up compression if enabled
        if (enable_compression && enable_chunking) {
          create_plist.setDeflate(6);
        }

        // Create and write dataset
        H5::DataSet dataset = mixed_group.createDataSet(col_name, H5::PredType::NATIVE_DOUBLE, dataspace, create_plist);
        dataset.write(data_vec.data(), H5::PredType::NATIVE_DOUBLE);

        // Add attribute to indicate data type
        H5::DataSpace attr_space(H5S_SCALAR);
        H5::Attribute attr = dataset.createAttribute("data_type", H5::StrType(H5::PredType::C_S1, 10), attr_space);
        attr.write(H5::StrType(H5::PredType::C_S1, 10), "numeric");

      } else {
        // Handle string data
        Rcpp::CharacterVector str_col = Rcpp::as<Rcpp::CharacterVector>(data_list[i]);

        // Convert to std::vector<std::string>
        std::vector<std::string> string_vec;
        for (int j = 0; j < str_col.size(); ++j) {
          string_vec.push_back(Rcpp::as<std::string>(str_col[j]));
        }

        // Create string datatype
        H5::StrType str_type = create_string_datatype(string_vec);

        // Define dimensions
        hsize_t dims[1] = {static_cast<hsize_t>(n_rows)};
        H5::DataSpace dataspace(1, dims);

        // Create dataset creation property list
        H5::DSetCreatPropList create_plist;

        // Set up chunking if enabled (required for compression)
        if (enable_chunking) {
          hsize_t chunk_dims[1] = {std::min(static_cast<hsize_t>(1000), dims[0])};
          create_plist.setChunk(1, chunk_dims);
        }

        // Set up compression if enabled
        if (enable_compression && enable_chunking) {
          create_plist.setDeflate(6);
        }

        // Create dataset
        H5::DataSet dataset = mixed_group.createDataSet(col_name, str_type, dataspace, create_plist);

        // Convert strings to char arrays for writing
        size_t str_len = str_type.getSize();
        std::vector<char> write_buffer(n_rows * str_len);

        for (int j = 0; j < n_rows; ++j) {
          std::string str = string_vec[j];
          str.resize(str_len - 1, '\0'); // Ensure null termination
          std::memcpy(&write_buffer[j * str_len], str.c_str(), str.length() + 1);
        }

        dataset.write(write_buffer.data(), str_type);

        // Add attribute to indicate data type
        H5::DataSpace attr_space(H5S_SCALAR);
        H5::Attribute attr = dataset.createAttribute("data_type", H5::StrType(H5::PredType::C_S1, 10), attr_space);
        attr.write(H5::StrType(H5::PredType::C_S1, 10), "string");
      }
    }

    auto write_time = std::chrono::high_resolution_clock::now();

    // Close file to ensure all data is written
    file.close();

    auto end_time = std::chrono::high_resolution_clock::now();

    // Calculate timings - only keep timing variables in C++
    auto conversion_duration = std::chrono::duration_cast<std::chrono::milliseconds>(conversion_time - start_time);
    auto write_duration = std::chrono::duration_cast<std::chrono::milliseconds>(write_time - conversion_time);
    auto total_duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);

    // Prepare minimal results - only timing and basic info
    result["conversion_time_ms"] = conversion_duration.count();
    result["write_time_ms"] = write_duration.count();
    result["total_time_ms"] = total_duration.count();
    result["n_numeric_cols"] = n_numeric_cols;
    result["n_string_cols"] = n_string_cols;
    result["n_rows"] = n_rows;
    result["compression_enabled"] = enable_compression;
    result["chunking_enabled"] = enable_chunking;
    result["success"] = true;

    Rcpp::Rcout << "Mixed data HDF5 write completed:" << std::endl;
    Rcpp::Rcout << "  Conversion time: " << conversion_duration.count() << " ms" << std::endl;
    Rcpp::Rcout << "  Write time: " << write_duration.count() << " ms" << std::endl;
    Rcpp::Rcout << "  Total time: " << total_duration.count() << " ms" << std::endl;

  } catch (const H5::Exception& e) {
    result["error"] = "HDF5 error: " + std::string(e.getCDetailMsg());
    Rcpp::Rcerr << "HDF5 Error: " << e.getCDetailMsg() << std::endl;
  } catch (const std::exception& e) {
    result["error"] = "Standard error: " + std::string(e.what());
    Rcpp::Rcerr << "Error: " << e.what() << std::endl;
  }

  return result;
}

//' Test HDF5 read performance for mixed data types
//'
//' This function tests the performance of reading mixed data types from an HDF5 file.
//' It reads both numeric and string datasets from the mixed_data group and converts
//' them back to an R list format.
//'
//' @param file_name Name of the HDF5 file to read
//' @param group_name Name of the group containing mixed data (default: "mixed_data")
//' @return List containing the mixed data and performance metrics
//' @export
// [[Rcpp::export]]
Rcpp::List test_hdf5_mixed_read_performance(const std::string& file_name, const std::string& group_name = "mixed_data") {

  Rcpp::List result;
  Rcpp::List performance;

  try {
    // Start timing
    auto start_time = std::chrono::high_resolution_clock::now();

    // Check if file exists
    std::ifstream file_check(file_name);
    if (!file_check.good()) {
      Rcpp::stop("File does not exist: " + file_name);
    }

    // Open HDF5 file
    H5::Exception::dontPrint();
    H5::H5File file(file_name, H5F_ACC_RDONLY);

    // Open the mixed_data group
    H5::Group mixed_group = file.openGroup("/" + group_name);

    // Get number of objects in the group
    hsize_t num_objs = mixed_group.getNumObjs();

    Rcpp::List data_list;
    int n_numeric_cols = 0;
    int n_string_cols = 0;
    int n_rows = 0;

    auto read_start = std::chrono::high_resolution_clock::now();

    // Read each dataset
    for (hsize_t i = 0; i < num_objs; ++i) {
      std::string obj_name = mixed_group.getObjnameByIdx(i);

      // Open dataset
      H5::DataSet dataset = mixed_group.openDataSet(obj_name);

      // Get dataspace and dimensions
      H5::DataSpace dataspace = dataset.getSpace();
      int rank = dataspace.getSimpleExtentNdims();
      std::vector<hsize_t> dims(rank);
      dataspace.getSimpleExtentDims(dims.data(), NULL);

      if (n_rows == 0) {
        n_rows = dims[0];
      }

      // Check data type attribute
      bool is_string_data = false;
      try {
        H5::Attribute attr = dataset.openAttribute("data_type");
        H5::StrType attr_type = attr.getStrType();
        size_t attr_size = attr_type.getSize();
        std::vector<char> attr_data(attr_size + 1, '\0');
        attr.read(attr_type, attr_data.data());
        std::string data_type_str(attr_data.data());
        is_string_data = (data_type_str == "string");
      } catch (...) {
        // If no attribute, try to determine from HDF5 datatype
        H5::DataType dtype = dataset.getDataType();
        is_string_data = (dtype.getClass() == H5T_STRING);
      }

      if (is_string_data) {
        // Read string data
        H5::StrType str_type = dataset.getStrType();
        size_t str_len = str_type.getSize();

        std::vector<char> read_buffer(dims[0] * str_len);
        dataset.read(read_buffer.data(), str_type);

        // Convert to R character vector
        Rcpp::CharacterVector str_col(dims[0]);
        for (hsize_t j = 0; j < dims[0]; ++j) {
          std::string str(&read_buffer[j * str_len]);
          str_col[j] = str;
        }

        data_list[obj_name] = str_col;
        n_string_cols++;

      } else {
        // Read numeric data
        std::vector<double> data_vec(dims[0]);
        dataset.read(data_vec.data(), H5::PredType::NATIVE_DOUBLE);

        // Convert to R numeric vector
        Rcpp::NumericVector num_col(data_vec.begin(), data_vec.end());
        data_list[obj_name] = num_col;
        n_numeric_cols++;
      }
    }

    auto read_end = std::chrono::high_resolution_clock::now();

    // Close file
    file.close();

    auto end_time = std::chrono::high_resolution_clock::now();

    // Calculate timings - only keep timing variables in C++
    auto read_duration = std::chrono::duration_cast<std::chrono::milliseconds>(read_end - read_start);
    auto total_duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);

    // Prepare minimal performance metrics - only timing and basic info
    performance["read_time_ms"] = read_duration.count();
    performance["total_time_ms"] = total_duration.count();
    performance["n_numeric_cols"] = n_numeric_cols;
    performance["n_string_cols"] = n_string_cols;
    performance["n_rows"] = n_rows;
    performance["success"] = true;

    result["data"] = data_list;
    result["performance"] = performance;

    Rcpp::Rcout << "Mixed data HDF5 read completed:" << std::endl;
    Rcpp::Rcout << "  Read time: " << read_duration.count() << " ms" << std::endl;
    Rcpp::Rcout << "  Total time: " << total_duration.count() << " ms" << std::endl;
    Rcpp::Rcout << "  Data composition: " << n_numeric_cols << " numeric + " << n_string_cols << " string columns" << std::endl;

  } catch (const H5::Exception& e) {
    result["error"] = "HDF5 error: " + std::string(e.getCDetailMsg());
    Rcpp::Rcerr << "HDF5 Error: " << e.getCDetailMsg() << std::endl;
  } catch (const std::exception& e) {
    result["error"] = "Standard error: " + std::string(e.what());
    Rcpp::Rcerr << "Error: " << e.what() << std::endl;
  }

  return result;
}