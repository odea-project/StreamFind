import React, { useEffect, useState } from "react";
import axios from "axios";
import Button from "@mui/material/Button";

const SelectMzml = ({ onFileSelect }) => {
  const [files, setFiles] = useState([]);
  const [selectedFiles, setSelectedFiles] = useState([]);

  useEffect(() => {
    axios
      .get("http://127.0.0.1:4568/files_project")
      .then((response) => {
        console.log(response);
        setFiles(response.data);
      })
      .catch((error) => {
        console.log(error);
      });
  }, []);

  const handleFileClick = (file) => {
    const isSelected = selectedFiles.includes(file);
    if (isSelected) {
      setSelectedFiles(
        selectedFiles.filter((selectedFile) => selectedFile !== file)
      );
    } else {
      setSelectedFiles([...selectedFiles, file]);
    }
  };

  const handleSelectFiles = () => {
    onFileSelect(selectedFiles);
  };

  return (
    <div>
      SelectMzml
      {files.map((file, index) => (
        <li
          key={index}
          style={{
            cursor: "pointer",
            fontWeight: selectedFiles.includes(file) ? "bold" : "normal",
          }}
          onClick={() => handleFileClick(file)}
        >
          {file}
        </li>
      ))}
      {selectedFiles.length > 0 && (
        <div>
          <p>Selected Files:</p>
          {selectedFiles.map((file, index) => (
            <p key={index}>{file}</p>
          ))}
          <Button onClick={handleSelectFiles}>Confirm Selection</Button>
        </div>
      )}
    </div>
  );
};

export default SelectMzml;
