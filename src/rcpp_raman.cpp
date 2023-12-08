#include <Rcpp.h>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm>
#include <cctype>
#include <map>
#include <iterator>


// Function to trim whitespace from both ends of a string
std::string trim_whitespaces_before_and_after(const std::string& str) {
  size_t first = str.find_first_not_of(" \t\n\r");
  size_t last = str.find_last_not_of(" \t\n\r");
  return str.substr(first, (last - first + 1));
}

std::string extractFileName(const std::string& filePath) {
  size_t lastSlash = filePath.find_last_of("/");
  std::string baseName = (lastSlash != std::string::npos) ? filePath.substr(lastSlash + 1) : filePath;
  size_t lastDot = baseName.find_last_of(".");
  std::string extension = (lastDot != std::string::npos) ? baseName.substr(lastDot + 1) : "";
  if (!extension.empty()) baseName = baseName.substr(0, lastDot);
  return baseName;
}

std::string extractFileDir(const std::string& filePath) {
  size_t lastSlash = filePath.find_last_of("/");
  std::string dirName = (lastSlash != std::string::npos) ? filePath.substr(0, lastSlash - 1) : "";
  return dirName;
}

std::string extractFileExtension(const std::string& filePath) {
  size_t lastSlash = filePath.find_last_of("/");
  std::string baseName = (lastSlash != std::string::npos) ? filePath.substr(lastSlash + 1) : filePath;
  size_t lastDot = baseName.find_last_of(".");
  std::string extension = (lastDot != std::string::npos) ? baseName.substr(lastDot + 1) : "";
  return extension;
}

// [[Rcpp::export]]
Rcpp::List rcpp_parse_asc_file(std::string file_path) {
  
  std::string file_info = extractFileName(file_path);
  
  std::ifstream file(file_path);
  
  std::string line;
  
  Rcpp::List metadata_list;
  
  std::vector<std::vector<double>> data_values;
  
  while (std::getline(file, line)) {
    
    if (line.find(":") != std::string::npos) {
      std::istringstream iss(line);
      std::string key, value;
      std::getline(iss, key, ':');
      std::getline(iss, value);
      metadata_list[key] = trim_whitespaces_before_and_after(value);
      
    } else if (line.find(";") != std::string::npos) {
      std::istringstream iss(line);
      std::string token;
      std::vector<double> row;
      while (std::getline(iss, token, ';')) {
        if (std::none_of(token.begin(), token.end(), ::isdigit)) break;
        row.push_back(std::stod(token));
      }
      data_values.push_back(row);
    }
  }
  
  Rcpp::NumericMatrix data_matrix(data_values.size(), data_values[0].size());
  for (size_t i = 0; i < data_values.size(); ++i) {
    for (size_t j = 0; j < data_values[i].size(); ++j) {
      data_matrix(i, j) = data_values[i][j];
    }
  }
  
  Rcpp::List data_list(data_matrix.ncol());
  for (int i = 0; i < data_matrix.ncol(); ++i) {
    data_list[i] = data_matrix(Rcpp::_, i);
  }
  
  if (data_list.size() == 3) {
    data_list.names() = Rcpp::CharacterVector({"rt","shift","intensity"});
  } else if (data_list.size() == 2) {
    data_list.names() = Rcpp::CharacterVector({"shift","intensity"});
  }
  
  data_list.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");
  
  return Rcpp::List::create(
    Rcpp::Named("name") = file_info,
    Rcpp::Named("replicate") = file_info,
    Rcpp::Named("blank") = "",
    Rcpp::Named("file") = file_path,
    Rcpp::Named("metadata") = metadata_list,
    Rcpp::Named("spectra") = data_list
  );
}

// [[Rcpp::export]]
void rcpp_write_asc_file(const std::string& file,
                         Rcpp::List metadata_list,
                         Rcpp::NumericMatrix spectra) {
  
  std::map<std::string, std::string> metadata_map;
  
  Rcpp::CharacterVector keys = metadata_list.names();
  
  for (int i = 0; i < keys.size(); ++i) {
    metadata_map[std::string(keys[i])] = Rcpp::as<std::string>(metadata_list[i]);
  }
  
  std::vector<std::string> metadata_lines;
  std::transform(metadata_map.begin(), metadata_map.end(), std::back_inserter(metadata_lines),
    [](const auto& kv) { return kv.first + ": " + kv.second; });
  
  std::ofstream metadata_stream(file);
  for (const auto& line : metadata_lines) {
    metadata_stream << line << std::endl;
  }
  metadata_stream.close();
  
  std::ofstream append_stream(file, std::ios::app);
  append_stream << std::endl << std::endl;
  append_stream.close();
  
  std::ofstream table_stream(file, std::ios::app);
  for (int i = 0; i < spectra.nrow(); ++i) {
    for (int j = 0; j < spectra.ncol(); ++j) {
      table_stream << spectra(i, j);
      if (j < spectra.ncol() - 1) {
        table_stream << ";";
      }
    }
    table_stream << std::endl;
  }
  table_stream.close();
}
