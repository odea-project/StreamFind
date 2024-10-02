#include <Rcpp.h>
#include <H5Cpp.h>
#include <iostream>

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
        std::cerr << "Unsupported datatype for dataset: " << dataset_name << std::endl;
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
        std::cout << prefix << obj_name << std::endl;

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
                std::cerr << "Unsupported datatype for dataset: " << obj_name << std::endl;
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
        std::cout << prefix << obj_name << std::endl;

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
                std::cerr << "Unsupported datatype for dataset: " << obj_name << std::endl;
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
    std::cerr << "HDF5 File Exception: " << error.getCDetailMsg() << std::endl;
    return -1;
  }
  // catch failure caused by the DataSet operations
  catch(const H5::DataSetIException& error)
  {
    std::cerr << "Datset Exception: " << error.getCDetailMsg() << std::endl;
    return -1;
  }
  // catch failure caused by the DataSpace operations
  catch(const H5::DataSpaceIException& error)
  {
    std::cerr << "Data space Exception: " << error.getCDetailMsg() << std::endl;
    return -1;
  }
  // catch failure caused by the DataSpace operations
  catch(const H5::DataTypeIException& error)
  {
    std::cerr << "Data type Exception: " << error.getCDetailMsg() << std::endl;
    return -1;
  }
  
  return out;
}