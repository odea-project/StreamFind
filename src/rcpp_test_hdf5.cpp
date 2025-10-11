#include <Rcpp.h>
#include <H5Cpp.h>
#include <iostream>
#include <chrono>
#include <fstream>

struct CVReference {
    char name[256]; // Assuming max length of 256 for the string
    char prefix[256]; // Assuming max length of 256 for the string
    int64_t accession;
};

// Function to read the CVReference dataset
Rcpp::List readCVReference(const H5::Group& group, const std::string& dataset_name) {
    Rcpp::List out;

    // Open the dataset
    H5::DataSet dataset = group.openDataSet(dataset_name);
    H5::DataType datatype = dataset.getDataType();

    if (datatype.getClass() == H5T_COMPOUND) {
        // Define the memory datatype
        H5::CompType mtype(sizeof(CVReference));
        mtype.insertMember("name", HOFFSET(CVReference, name), H5::StrType(H5::PredType::C_S1, 256));
        mtype.insertMember("prefix", HOFFSET(CVReference, prefix), H5::StrType(H5::PredType::C_S1, 256));
        mtype.insertMember("accession", HOFFSET(CVReference, accession), H5::PredType::NATIVE_INT64);

        // Get the dataspace and dimensions
        H5::DataSpace dataspace = dataset.getSpace();
        int rank = dataspace.getSimpleExtentNdims();
        std::vector<hsize_t> dims(rank);
        dataspace.getSimpleExtentDims(dims.data(), NULL);

        // Read the data
        std::vector<CVReference> data(dims[0]);
        dataset.read(data.data(), mtype);

        // Convert to Rcpp::List
        Rcpp::CharacterVector names(dims[0]);
        Rcpp::CharacterVector prefixes(dims[0]);
        Rcpp::IntegerVector accessions(dims[0]);
        for (hsize_t i = 0; i < dims[0]; ++i) {
            names[i] = std::string(data[i].name);
            prefixes[i] = std::string(data[i].prefix);
            accessions[i] = data[i].accession;
        }
        out["name"] = names;
        out["prefix"] = prefixes;
        out["accession"] = accessions;
    } else {
        Rcpp::Rcout << "Unsupported datatype for dataset: " << dataset_name << std::endl;
    }

    return out;
}

struct CVParam {
  char value[256]; // Assuming max length of 256 for the string
  int64_t cvRefID;
  int64_t uRefID;
};

// Function to recursively list names in the HDF5 file
Rcpp::List listNamesAndGetData(const H5::Group& group, const std::string& prefix = "") {

    Rcpp::List out;

    // Get the number of objects in the group
    hsize_t num_objs = group.getNumObjs();

    // Iterate over each object
    for (hsize_t i = 0; i < num_objs; ++i) {
        // Get the name of the object
        std::string obj_name = group.getObjnameByIdx(i);
        // Get the type of the object
        H5G_obj_t obj_type = group.getObjTypeByIdx(i);

        // Print the name with the prefix
        Rcpp::Rcout << prefix << obj_name << std::endl;

        // If the object is a group, recursively list its contents
        if (obj_type == H5G_GROUP) {
            H5::Group sub_group = group.openGroup(obj_name);
            out[obj_name] = listNamesAndGetData(sub_group, prefix + obj_name + "/");
        } else if (obj_type == H5G_DATASET) {
            // If the object is a dataset, read its data
            H5::DataSet dataset = group.openDataSet(obj_name);
            H5::DataType datatype = dataset.getDataType();

            if (datatype.getClass() == H5T_COMPOUND) {
                // Define the memory datatype
                H5::CompType mtype(sizeof(CVParam));
                mtype.insertMember("value", HOFFSET(CVParam, value), H5::StrType(H5::PredType::C_S1, 256));
                mtype.insertMember("cvRefID", HOFFSET(CVParam, cvRefID), H5::PredType::NATIVE_INT64);
                mtype.insertMember("uRefID", HOFFSET(CVParam, uRefID), H5::PredType::NATIVE_INT64);

                // Get the dataspace and dimensions
                H5::DataSpace dataspace = dataset.getSpace();
                int rank = dataspace.getSimpleExtentNdims();
                std::vector<hsize_t> dims(rank);
                dataspace.getSimpleExtentDims(dims.data(), NULL);

                // Read the data
                std::vector<CVParam> data(dims[0]);
                dataset.read(data.data(), mtype);

                // Convert to Rcpp::List
                Rcpp::List data_list;
                for (const auto& item : data) {
                    Rcpp::List item_list = Rcpp::List::create(
                        Rcpp::Named("value") = std::string(item.value),
                        Rcpp::Named("cvRefID") = Rcpp::wrap(item.cvRefID),
                        Rcpp::Named("uRefID") = Rcpp::wrap(item.uRefID)
                    );
                    data_list.push_back(item_list);
                }
                out[obj_name] = data_list;
            } else {
              Rcpp::Rcout << "Unsupported datatype for dataset: " << obj_name << std::endl;
            }
        }
    }

    return out;
}

// Function to recursively list names and get data in the HDF5 file
Rcpp::List listNamesAndGetData2(const H5::Group& group, const std::string& prefix = "") {
    Rcpp::List out;

    // Get the number of objects in the group
    hsize_t num_objs = group.getNumObjs();

    // Iterate over each object
    for (hsize_t i = 0; i < num_objs; ++i) {
        // Get the name of the object
        std::string obj_name = group.getObjnameByIdx(i);
        // Get the type of the object
        H5G_obj_t obj_type = group.getObjTypeByIdx(i);

        // Print the name with the prefix
        Rcpp::Rcout << prefix << obj_name << std::endl;

        // If the object is a group, recursively list its contents
        if (obj_type == H5G_GROUP) {
            H5::Group sub_group = group.openGroup(obj_name);
            out[obj_name] = listNamesAndGetData(sub_group, prefix + obj_name + "/");
        } else if (obj_type == H5G_DATASET) {
            // If the object is a dataset, read its data
            H5::DataSet dataset = group.openDataSet(obj_name);
            H5::DataType datatype = dataset.getDataType();

            if (datatype.getClass() == H5T_COMPOUND) {
                // Define the memory datatype
                H5::CompType mtype(sizeof(CVReference));
                mtype.insertMember("name", HOFFSET(CVReference, name), H5::StrType(H5::PredType::C_S1, 256));
                mtype.insertMember("prefix", HOFFSET(CVReference, prefix), H5::StrType(H5::PredType::C_S1, 256));
                mtype.insertMember("accession", HOFFSET(CVReference, accession), H5::PredType::NATIVE_INT64);

                // Get the dataspace and dimensions
                H5::DataSpace dataspace = dataset.getSpace();
                int rank = dataspace.getSimpleExtentNdims();
                std::vector<hsize_t> dims(rank);
                dataspace.getSimpleExtentDims(dims.data(), NULL);

                // Read the data
                std::vector<CVReference> data(dims[0]);
                dataset.read(data.data(), mtype);

                // Convert to Rcpp::List
                Rcpp::CharacterVector names(dims[0]);
                Rcpp::CharacterVector prefixes(dims[0]);
                Rcpp::IntegerVector accessions(dims[0]);
                for (hsize_t i = 0; i < dims[0]; ++i) {
                    names[i] = std::string(data[i].name);
                    prefixes[i] = std::string(data[i].prefix);
                    accessions[i] = data[i].accession;
                }
                out[obj_name] = Rcpp::List::create(
                    Rcpp::Named("name") = names,
                    Rcpp::Named("prefix") = prefixes,
                    Rcpp::Named("accession") = accessions
                );
            } else {
              Rcpp::Rcout << "Unsupported datatype for dataset: " << obj_name << std::endl;
            }
        }
    }

    return out;
}

// [[Rcpp::export]]
Rcpp::List test_read_hdf5(const std::string& file_name) {

  Rcpp::List out;

  try {
        // Open the HDF5 file
        H5::H5File file(file_name, H5F_ACC_RDONLY);

        // Open the root group
        H5::Group root_group = file.openGroup("/");

        return listNamesAndGetData2(root_group);
    } catch (H5::FileIException& e) {
        Rcpp::Rcerr << "FileIException: " << e.getCDetailMsg() << std::endl;
    } catch (H5::GroupIException& e) {
        Rcpp::Rcerr << "GroupIException: " << e.getCDetailMsg() << std::endl;
    } catch (H5::Exception& e) {
        Rcpp::Rcerr << "Exception: " << e.getCDetailMsg() << std::endl;
    }

  return out;
}

// [[Rcpp::export]]
Rcpp::List test_create_hdf5() {

  const H5std_string  FILE_NAME( "SDS.h5" );
  const H5std_string  DATASET_NAME( "IntArray" );
  const int   NX = 5;                    // dataset dimensions
  const int   NY = 6;
  const int   RANK = 2;

  Rcpp::List out;

  /*
   * Data initialization.
   */
  int i, j;
  int data[NX][NY];          // buffer for data to write
  for (j = 0; j < NX; j++)
  {
    for (i = 0; i < NY; i++)
      data[j][i] = i + j;
  }
  /*
   * 0 1 2 3 4 5
   * 1 2 3 4 5 6
   * 2 3 4 5 6 7
   * 3 4 5 6 7 8
   * 4 5 6 7 8 9
   */

  try
  {
    /*
     * Turn off the auto-printing when failure occurs so that we can
     * handle the errors appropriately
     */
    H5::Exception::dontPrint();
    /*
     * Create a new file using H5F_ACC_TRUNC access,
     * default file creation properties, and default file
     * access properties.
     */
    H5::H5File file( FILE_NAME, H5F_ACC_TRUNC );
    /*
     * Define the size of the array and create the data space for fixed
     * size dataset.
     */
    hsize_t     dimsf[2];              // dataset dimensions
    dimsf[0] = NX;
    dimsf[1] = NY;
    H5::DataSpace dataspace( RANK, dimsf );
    /*
     * Define datatype for the data in the file.
     * We will store little endian INT numbers.
     */
    H5::IntType datatype( H5::PredType::NATIVE_INT );
    datatype.setOrder( H5T_ORDER_LE );
    /*
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     */
    H5::DataSet dataset = file.createDataSet( DATASET_NAME, datatype, dataspace );
    /*
     * Write the data to the dataset using default memory space, file
     * space, and transfer properties.
     */
    dataset.write( data, H5::PredType::NATIVE_INT );
  }  // end of try block
  // catch failure caused by the H5File operations
  catch(const H5::FileIException& error)
  {
    Rcpp::Rcout << "HDF5 File Exception: " << error.getCDetailMsg() << std::endl;
    return -1;
  }
  // catch failure caused by the DataSet operations
  catch(const H5::DataSetIException& error)
  {
    Rcpp::Rcout << "Datset Exception: " << error.getCDetailMsg() << std::endl;
    return -1;
  }
  // catch failure caused by the DataSpace operations
  catch(const H5::DataSpaceIException& error)
  {
    Rcpp::Rcout << "Data space Exception: " << error.getCDetailMsg() << std::endl;
    return -1;
  }
  // catch failure caused by the DataSpace operations
  catch(const H5::DataTypeIException& error)
  {
    Rcpp::Rcout << "Data type Exception: " << error.getCDetailMsg() << std::endl;
    return -1;
  }

  return out;
}

//' Generate test data for HDF5 performance testing
//'
//' Creates a list of numeric vectors suitable for testing HDF5 write performance.
//' Each vector represents a column of data with normally distributed values.
//'
//' @param n_cols Number of columns (default: 100)
//' @param n_rows Number of rows per column (default: 100,000)
//' @param mean_base Base mean for normal distribution (default: 0.0)
//' @param sd Standard deviation for normal distribution (default: 1.0)
//' @return List with n_cols elements, each containing n_rows numeric values
//' @export
// [[Rcpp::export]]
Rcpp::List generate_test_data_cpp(int n_cols = 100, int n_rows = 100000, double mean_base = 0.0, double sd = 1.0) {
  Rcpp::List result(n_cols);

  // Set random seed for reproducibility
  Rcpp::Environment base_env("package:base");
  Rcpp::Function set_seed = base_env["set.seed"];
  set_seed(123);

  for (int col = 0; col < n_cols; ++col) {
    Rcpp::NumericVector col_data(n_rows);
    for (int row = 0; row < n_rows; ++row) {
      // Generate data with a slight trend per column
      col_data[row] = R::rnorm(mean_base + col * 0.1, sd);
    }
    result[col] = col_data;
  }

  return result;
}

//' Generate mixed test data with numeric and string columns
//'
//' This function generates test data that includes both numeric and string columns
//' for testing HDF5 mixed data type performance. It creates a specified number
//' of numeric columns with normal distribution and string columns with categorical
//' or ID-like data.
//'
//' @param n_cols_numeric Number of numeric columns to generate (default: 98)
//' @param n_cols_string Number of string columns to generate (default: 2)
//' @param n_rows Number of rows for each column (default: 100000)
//' @param mean_base Base mean for numeric data (default: 0.0)
//' @param sd Standard deviation for numeric data (default: 1.0)
//' @return List with mixed numeric and string columns
//' @export
// [[Rcpp::export]]
Rcpp::List generate_mixed_test_data_cpp(int n_cols_numeric = 98, int n_cols_string = 2,
                                       int n_rows = 100000, double mean_base = 0.0, double sd = 1.0) {
  Rcpp::List result;

  // Set random seed for reproducibility
  Rcpp::Environment base_env("package:base");
  Rcpp::Function set_seed = base_env["set.seed"];
  set_seed(123);

  // Generate numeric columns
  for (int col = 0; col < n_cols_numeric; ++col) {
    Rcpp::NumericVector col_data(n_rows);
    for (int row = 0; row < n_rows; ++row) {
      col_data[row] = R::rnorm(mean_base + col * 0.1, sd);
    }
    std::string col_name = "num_col_" + std::to_string(col + 1);
    result[col_name] = col_data;
  }

  // Generate string columns
  for (int col = 0; col < n_cols_string; ++col) {
    Rcpp::CharacterVector col_data(n_rows);

    if (col == 0) {
      // Category-like strings
      std::vector<std::string> categories = {"Type_A", "Type_B", "Type_C", "Type_D", "Type_E"};
      for (int row = 0; row < n_rows; ++row) {
        col_data[row] = categories[row % categories.size()];
      }
    } else {
      // ID-like strings
      for (int row = 0; row < n_rows; ++row) {
        std::string id_str = "ID_" + std::string(8 - std::to_string(row + 1).length(), '0') + std::to_string(row + 1);
        col_data[row] = id_str;
      }
    }

    std::string col_name = "str_col_" + std::to_string(col + 1);
    result[col_name] = col_data;
  }

  return result;
}

//' Test HDF5 write performance with large matrix data
//'
//' This function tests the performance of writing a large matrix (100 columns x 100,000 rows)
//' to an HDF5 file. It measures conversion time, write time, and file size, providing
//' comprehensive performance metrics for HDF5 operations.
//'
//' The function expects the input data as a list where each element represents a column
//' of the matrix. Each column should contain exactly 100,000 numeric values.
//'
//' Performance metrics returned:
//' - conversion_time_ms: Time to convert R list to C++ data structures
//' - write_time_ms: Time to write data to HDF5 file
//' - total_time_ms: Total execution time
//' - file_size_bytes/mb: Size of the created HDF5 file
//' - data_size_mb: Raw size of the data in memory
//'
//' @param data_list List containing 100 numeric vectors, each with 100,000 elements
//' @param file_name Name of the output HDF5 file (default: "performance_test.h5")
//' @param enable_compression Logical, enable gzip compression (default: false)
//' @param enable_chunking Logical, enable chunking for better I/O performance (default: false)
//' @return List containing performance metrics and file information
//' @export
//'
//' @examples
//' \dontrun{
//' # Generate test data
//' test_data <- generate_test_data_cpp()
//'
//' # Run performance test without compression
//' result1 <- test_hdf5_performance(test_data, "test_no_compress.h5")
//'
//' # Run performance test with compression
//' result2 <- test_hdf5_performance(test_data, "test_compressed.h5", TRUE, FALSE)
//'
//' # Run performance test with both compression and chunking
//' result3 <- test_hdf5_performance(test_data, "test_full.h5", TRUE, TRUE)
//'
//' # Compare file sizes
//' cat("No compression:", round(result1$file_size_mb, 2), "MB\n")
//' cat("With compression:", round(result2$file_size_mb, 2), "MB\n")
//' cat("Compression ratio:", round(result2$compression_ratio, 2), ":1\n")
//' }
// [[Rcpp::export]]
Rcpp::List test_hdf5_performance(Rcpp::List data_list, const std::string& file_name = "performance_test.h5",
                                bool enable_compression = false, bool enable_chunking = false) {

  Rcpp::List result;

  try {
    // Start timing
    auto start_time = std::chrono::high_resolution_clock::now();

    // Validate input: expecting a list with 100 elements, each being a numeric vector of length 100000
    if (data_list.size() != 100) {
      Rcpp::stop("Input list must have exactly 100 elements (columns)");
    }

    // Convert Rcpp::List to a 2D array (100 cols x 100000 rows)
    const int n_cols = data_list.size();
    if (n_cols == 0) {
      Rcpp::stop("Input list is empty");
    }
    const int n_rows = Rcpp::as<Rcpp::NumericVector>(data_list[0]).size();
    std::vector<std::vector<double>> data_matrix(n_cols, std::vector<double>(n_rows));

    for (int col = 0; col < n_cols; ++col) {
      Rcpp::NumericVector col_data = data_list[col];
      if (col_data.size() != n_rows) {
        Rcpp::stop("Each column must have exactly 100000 elements");
      }
      for (int row = 0; row < n_rows; ++row) {
        data_matrix[col][row] = col_data[row];
      }
    }

    auto conversion_time = std::chrono::high_resolution_clock::now();

    // Create HDF5 file
    H5::Exception::dontPrint();
    H5::H5File file(file_name, H5F_ACC_TRUNC);

    // Define dimensions: rows x cols in HDF5 convention
    hsize_t dims[2] = {static_cast<hsize_t>(n_rows), static_cast<hsize_t>(n_cols)};
    H5::DataSpace dataspace(2, dims);

    // Define datatype
    H5::FloatType datatype(H5::PredType::NATIVE_DOUBLE);
    datatype.setOrder(H5T_ORDER_LE);

    // Create dataset creation property list
    H5::DSetCreatPropList create_plist;

    // Set up chunking if enabled
    if (enable_chunking) {
      // Choose chunk size: use smaller chunks for better I/O performance
      // Chunk size should be balanced between memory usage and I/O efficiency
      hsize_t chunk_dims[2];
      chunk_dims[0] = std::min(static_cast<hsize_t>(1000), static_cast<hsize_t>(n_rows)); // chunk rows
      chunk_dims[1] = std::min(static_cast<hsize_t>(10), static_cast<hsize_t>(n_cols));   // chunk cols
      create_plist.setChunk(2, chunk_dims);

      Rcpp::Rcout << "  Chunking enabled: " << chunk_dims[0] << " x " << chunk_dims[1] << std::endl;
    }

    // Set up compression if enabled (requires chunking)
    if (enable_compression) {
      if (!enable_chunking) {
        // Enable chunking automatically if compression is requested
        hsize_t chunk_dims[2];
        chunk_dims[0] = std::min(static_cast<hsize_t>(1000), static_cast<hsize_t>(n_rows));
        chunk_dims[1] = std::min(static_cast<hsize_t>(10), static_cast<hsize_t>(n_cols));
        create_plist.setChunk(2, chunk_dims);
        Rcpp::Rcout << "  Auto-chunking for compression: " << chunk_dims[0] << " x " << chunk_dims[1] << std::endl;
      }

      // Set gzip compression level (0-9, where 9 is maximum compression)
      create_plist.setDeflate(6); // Good balance between compression ratio and speed
      Rcpp::Rcout << "  Compression enabled: gzip level 6" << std::endl;
    }

    // Create dataset with or without special properties
    H5::DataSet dataset;
    if (enable_compression || enable_chunking) {
      dataset = file.createDataSet("matrix_data", datatype, dataspace, create_plist);
    } else {
      dataset = file.createDataSet("matrix_data", datatype, dataspace);
    }

    // Prepare data for writing (HDF5 expects row-major order)
    std::vector<double> write_buffer(n_rows * n_cols);
    for (int row = 0; row < n_rows; ++row) {
      for (int col = 0; col < n_cols; ++col) {
        write_buffer[row * n_cols + col] = data_matrix[col][row];
      }
    }

    auto write_start_time = std::chrono::high_resolution_clock::now();

    // Write data to dataset
    dataset.write(write_buffer.data(), H5::PredType::NATIVE_DOUBLE);

    auto end_time = std::chrono::high_resolution_clock::now();

    // Calculate timing
    auto conversion_duration = std::chrono::duration_cast<std::chrono::milliseconds>(conversion_time - start_time);
    auto write_duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - write_start_time);
    auto total_duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);

    // Get file size
    std::ifstream file_stream(file_name, std::ifstream::ate | std::ifstream::binary);
    std::streamsize file_size = file_stream.tellg();
    file_stream.close();

    // Create result list
    result["file_name"] = file_name;
    result["dimensions"] = Rcpp::IntegerVector::create(n_rows, n_cols);
    result["conversion_time_ms"] = static_cast<int>(conversion_duration.count());
    result["write_time_ms"] = static_cast<int>(write_duration.count());
    result["total_time_ms"] = static_cast<int>(total_duration.count());
    result["file_size_bytes"] = static_cast<double>(file_size);
    result["file_size_mb"] = static_cast<double>(file_size) / (1024.0 * 1024.0);
    result["data_size_mb"] = static_cast<double>(n_rows * n_cols * sizeof(double)) / (1024.0 * 1024.0);
    result["compression_enabled"] = enable_compression;
    result["chunking_enabled"] = enable_chunking;

    // Calculate compression ratio if compression was used
    if (enable_compression) {
      double compression_ratio = static_cast<double>(n_rows * n_cols * sizeof(double)) / static_cast<double>(file_size);
      result["compression_ratio"] = compression_ratio;
    }

    Rcpp::Rcout << "HDF5 Performance Test Results:" << std::endl;
    Rcpp::Rcout << "  File: " << file_name << std::endl;
    Rcpp::Rcout << "  Dimensions: " << n_rows << " x " << n_cols << std::endl;
    Rcpp::Rcout << "  Compression: " << (enable_compression ? "enabled" : "disabled") << std::endl;
    Rcpp::Rcout << "  Chunking: " << (enable_chunking ? "enabled" : "disabled") << std::endl;
    Rcpp::Rcout << "  Conversion time: " << conversion_duration.count() << " ms" << std::endl;
    Rcpp::Rcout << "  Write time: " << write_duration.count() << " ms" << std::endl;
    Rcpp::Rcout << "  Total time: " << total_duration.count() << " ms" << std::endl;
    Rcpp::Rcout << "  File size: " << static_cast<double>(file_size) / (1024.0 * 1024.0) << " MB" << std::endl;
    Rcpp::Rcout << "  Raw data size: " << static_cast<double>(n_rows * n_cols * sizeof(double)) / (1024.0 * 1024.0) << " MB" << std::endl;
    if (enable_compression) {
      double compression_ratio = static_cast<double>(n_rows * n_cols * sizeof(double)) / static_cast<double>(file_size);
      Rcpp::Rcout << "  Compression ratio: " << compression_ratio << ":1" << std::endl;
    }

  } catch (const H5::FileIException& error) {
    Rcpp::Rcerr << "HDF5 File Exception: " << error.getCDetailMsg() << std::endl;
    result["error"] = std::string("File exception: ") + error.getCDetailMsg();
  } catch (const H5::DataSetIException& error) {
    Rcpp::Rcerr << "HDF5 Dataset Exception: " << error.getCDetailMsg() << std::endl;
    result["error"] = std::string("Dataset exception: ") + error.getCDetailMsg();
  } catch (const H5::DataSpaceIException& error) {
    Rcpp::Rcerr << "HDF5 DataSpace Exception: " << error.getCDetailMsg() << std::endl;
    result["error"] = std::string("DataSpace exception: ") + error.getCDetailMsg();
  } catch (const H5::DataTypeIException& error) {
    Rcpp::Rcerr << "HDF5 DataType Exception: " << error.getCDetailMsg() << std::endl;
    result["error"] = std::string("DataType exception: ") + error.getCDetailMsg();
  } catch (const std::exception& e) {
    Rcpp::Rcerr << "Standard Exception: " << e.what() << std::endl;
    result["error"] = std::string("Standard exception: ") + e.what();
  }

  return result;
}

//' Test HDF5 read performance for matrix data
//'
//' This function tests the performance of reading a large matrix from an HDF5 file.
//' It looks for the "matrix_data" dataset, reads it, and converts it back to an R list
//' format (list of columns). It measures read time, conversion time, and provides
//' comprehensive performance metrics.
//'
//' The function expects an HDF5 file with a "matrix_data" dataset containing a 2D matrix
//' of double-precision floating point numbers.
//'
//' Performance metrics returned:
//' - read_time_ms: Time to read data from HDF5 file
//' - conversion_time_ms: Time to convert HDF5 data to R list format
//' - total_time_ms: Total execution time
//' - file_size_bytes/mb: Size of the HDF5 file
//' - data_size_mb: Size of the data in memory
//' - dataset_info: Information about the dataset (dimensions, type, etc.)
//'
//' @param file_name Name of the HDF5 file to read
//' @param dataset_name Name of the dataset to read (default: "matrix_data")
//' @return List containing the data as columns and performance metrics
//' @export
//'
//' @examples
//' \dontrun{
//' # First create a test file
//' test_data <- generate_test_data_cpp()
//' test_hdf5_performance(test_data, "test.h5")
//'
//' # Then read it back
//' result <- test_hdf5_read_performance("test.h5")
//'
//' # Check results
//' print(paste("Read time:", result$performance$read_time_ms, "ms"))
//' print(paste("Data dimensions:", paste(result$performance$dimensions, collapse=" x ")))
//' }
// [[Rcpp::export]]
Rcpp::List test_hdf5_read_performance(const std::string& file_name, const std::string& dataset_name = "matrix_data") {

  Rcpp::List result;
  Rcpp::List performance;
  Rcpp::List dataset_info;

  try {
    // Start timing
    auto start_time = std::chrono::high_resolution_clock::now();

    // Check if file exists
    std::ifstream file_check(file_name);
    if (!file_check.good()) {
      Rcpp::stop("File does not exist: " + file_name);
    }
    file_check.close();

    // Get file size
    std::ifstream file_stream(file_name, std::ifstream::ate | std::ifstream::binary);
    std::streamsize file_size = file_stream.tellg();
    file_stream.close();

    // Open HDF5 file
    H5::Exception::dontPrint();
    H5::H5File file(file_name, H5F_ACC_RDONLY);

    // Check if dataset exists
    bool dataset_exists = false;
    try {
      H5::DataSet temp_dataset = file.openDataSet(dataset_name);
      dataset_exists = true;
      temp_dataset.close();
    } catch (const H5::Exception& e) {
      dataset_exists = false;
    }

    if (!dataset_exists) {
      Rcpp::stop("Dataset '" + dataset_name + "' not found in file: " + file_name);
    }

    // Open the dataset
    H5::DataSet dataset = file.openDataSet(dataset_name);
    H5::DataType datatype = dataset.getDataType();
    H5::DataSpace dataspace = dataset.getSpace();

    // Get dataset information
    int rank = dataspace.getSimpleExtentNdims();
    std::vector<hsize_t> dims(rank);
    dataspace.getSimpleExtentDims(dims.data(), NULL);

    // Store dataset info
    dataset_info["name"] = dataset_name;
    dataset_info["rank"] = rank;
    dataset_info["type_class"] = static_cast<int>(datatype.getClass());
    dataset_info["type_size"] = static_cast<int>(datatype.getSize());

    if (rank != 2) {
      Rcpp::stop("Expected 2D dataset, but found " + std::to_string(rank) + "D dataset");
    }

    // Verify datatype is floating point
    if (datatype.getClass() != H5T_FLOAT) {
      Rcpp::stop("Expected floating point dataset, but found different type");
    }

    int n_rows = static_cast<int>(dims[0]);
    int n_cols = static_cast<int>(dims[1]);

    dataset_info["dimensions"] = Rcpp::IntegerVector::create(n_rows, n_cols);
    dataset_info["total_elements"] = n_rows * n_cols;

    Rcpp::Rcout << "Reading HDF5 dataset:" << std::endl;
    Rcpp::Rcout << "  Dataset: " << dataset_name << std::endl;
    Rcpp::Rcout << "  Dimensions: " << n_rows << " x " << n_cols << std::endl;
    Rcpp::Rcout << "  Total elements: " << (n_rows * n_cols) << std::endl;
    Rcpp::Rcout << "  Data type size: " << datatype.getSize() << " bytes" << std::endl;

    auto read_start_time = std::chrono::high_resolution_clock::now();

    // Read the data
    std::vector<double> read_buffer(n_rows * n_cols);
    dataset.read(read_buffer.data(), H5::PredType::NATIVE_DOUBLE);

    auto read_end_time = std::chrono::high_resolution_clock::now();

    // Convert to R list format (column-wise)
    auto conversion_start_time = std::chrono::high_resolution_clock::now();

    Rcpp::List data_list(n_cols);
    for (int col = 0; col < n_cols; ++col) {
      Rcpp::NumericVector col_data(n_rows);
      for (int row = 0; row < n_rows; ++row) {
        // HDF5 stores in row-major order
        col_data[row] = read_buffer[row * n_cols + col];
      }
      data_list[col] = col_data;
    }

    auto end_time = std::chrono::high_resolution_clock::now();

    // Calculate timing
    auto read_duration = std::chrono::duration_cast<std::chrono::milliseconds>(read_end_time - read_start_time);
    auto conversion_duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - conversion_start_time);
    auto total_duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);

    // Store performance metrics
    performance["file_name"] = file_name;
    performance["dataset_name"] = dataset_name;
    performance["dimensions"] = Rcpp::IntegerVector::create(n_rows, n_cols);
    performance["read_time_ms"] = static_cast<int>(read_duration.count());
    performance["conversion_time_ms"] = static_cast<int>(conversion_duration.count());
    performance["total_time_ms"] = static_cast<int>(total_duration.count());
    performance["file_size_bytes"] = static_cast<double>(file_size);
    performance["file_size_mb"] = static_cast<double>(file_size) / (1024.0 * 1024.0);
    performance["data_size_mb"] = static_cast<double>(n_rows * n_cols * sizeof(double)) / (1024.0 * 1024.0);

    // Calculate throughput
    double read_throughput = performance["data_size_mb"];
    read_throughput = read_throughput / (read_duration.count() / 1000.0);
    performance["read_throughput_mbps"] = read_throughput;

    // Print results
    Rcpp::Rcout << "\nHDF5 Read Performance Results:" << std::endl;
    Rcpp::Rcout << "  Read time: " << read_duration.count() << " ms" << std::endl;
    Rcpp::Rcout << "  Conversion time: " << conversion_duration.count() << " ms" << std::endl;
    Rcpp::Rcout << "  Total time: " << total_duration.count() << " ms" << std::endl;
    Rcpp::Rcout << "  File size: " << static_cast<double>(file_size) / (1024.0 * 1024.0) << " MB" << std::endl;
    Rcpp::Rcout << "  Data size: " << static_cast<double>(n_rows * n_cols * sizeof(double)) / (1024.0 * 1024.0) << " MB" << std::endl;
    Rcpp::Rcout << "  Read throughput: " << read_throughput << " MB/s" << std::endl;

    // Prepare final result
    result["data"] = data_list;
    result["performance"] = performance;
    result["dataset_info"] = dataset_info;

    // Add some basic statistics about the data
    Rcpp::List data_stats;
    if (n_cols > 0 && n_rows > 0) {
      Rcpp::NumericVector first_col = data_list[0];
      double sum = 0.0;
      double sum_sq = 0.0;
      double min_val = first_col[0];
      double max_val = first_col[0];

      // Calculate stats for first column as sample
      for (int i = 0; i < std::min(1000, n_rows); ++i) {
        double val = first_col[i];
        sum += val;
        sum_sq += val * val;
        if (val < min_val) min_val = val;
        if (val > max_val) max_val = val;
      }

      int sample_size = std::min(1000, n_rows);
      double mean = sum / sample_size;
      double variance = (sum_sq / sample_size) - (mean * mean);

      data_stats["sample_mean_first_col"] = mean;
      data_stats["sample_var_first_col"] = variance;
      data_stats["sample_min_first_col"] = min_val;
      data_stats["sample_max_first_col"] = max_val;
      data_stats["sample_size"] = sample_size;
    }
    result["data_stats"] = data_stats;

  } catch (const H5::FileIException& error) {
    Rcpp::Rcerr << "HDF5 File Exception: " << error.getCDetailMsg() << std::endl;
    result["error"] = std::string("File exception: ") + error.getCDetailMsg();
  } catch (const H5::DataSetIException& error) {
    Rcpp::Rcerr << "HDF5 Dataset Exception: " << error.getCDetailMsg() << std::endl;
    result["error"] = std::string("Dataset exception: ") + error.getCDetailMsg();
  } catch (const H5::DataSpaceIException& error) {
    Rcpp::Rcerr << "HDF5 DataSpace Exception: " << error.getCDetailMsg() << std::endl;
    result["error"] = std::string("DataSpace exception: ") + error.getCDetailMsg();
  } catch (const H5::DataTypeIException& error) {
    Rcpp::Rcerr << "HDF5 DataType Exception: " << error.getCDetailMsg() << std::endl;
    result["error"] = std::string("DataType exception: ") + error.getCDetailMsg();
  } catch (const std::exception& e) {
    Rcpp::Rcerr << "Standard Exception: " << e.what() << std::endl;
    result["error"] = std::string("Standard exception: ") + e.what();
  }

  return result;
}

