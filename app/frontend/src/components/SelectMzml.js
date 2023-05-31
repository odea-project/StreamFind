import React, { useEffect, useState } from "react";
import axios from "axios";
import ArrowBackIcon from "@mui/icons-material/ArrowBack";
import FolderIcon from "@mui/icons-material/Folder";
import InsertDriveFileIcon from '@mui/icons-material/InsertDriveFile';
import { Button } from "@mui/material";

const SelectMzml = ({ onFolderSelect }) => {
  const [folders, setFolders] = useState([]);
  const [selectedFolder, setSelectedFolder] = useState("");
  const [previousFolders, setPreviousFolders] = useState([]);
  const [selectedFiles, setSelectedFiles] = useState([]);

  useEffect(() => {
    axios
      .get("http://127.0.0.1:8000/files_project")
      .then((response) => {
        console.log(response);
        setFolders(response.data);
      })
      .catch((error) => {
        console.log(error);
      });
  }, []);

  const handleSendFiles = () => {
    onFolderSelect(selectedFiles);
  };

  const handleFolderClick = (item) => {
    if (item.endsWith(".mzML")) {
      // Clicked item is a ".mzML" file
      const fileName = item.split("/").pop(); // Extract the file name
      setSelectedFiles((prevSelectedFiles) => [
        ...prevSelectedFiles,
        "///",
        fileName,
      ]);
    } else {
      // Clicked item is a folder
      setSelectedFolder(selectedFolder + "/" + item);
      axios
        .post("http://127.0.0.1:8000/open_folder", {
          name: selectedFolder + "/" + item,
        })
        .then((response) => {
          console.log(response);
          console.log(response.data);
          setPreviousFolders((prevFolders) => [...prevFolders, selectedFolder]);
          setFolders(response.data);
        })
        .catch((error) => {
          console.log(error);
        });
    }
  };

  const handleBackClick = () => {
    const prevFolder = previousFolders.pop();
    setSelectedFolder(prevFolder);
    axios
      .post("http://127.0.0.1:8000/open_folder", {
        name: prevFolder,
      })
      .then((response) => {
        console.log(response); // Log the full response object for debugging
        console.log(response.data); // Array of file names within the folder
        setFolders(response.data);
      })
      .catch((error) => {
        console.log(error);
        // Handle errors if needed
      });
  };

  return (
    <div>
      {selectedFolder !== "" && (
        <div>
          <p>Current Path: {selectedFolder}</p>
          <ArrowBackIcon
            style={{ cursor: "pointer" }}
            onClick={handleBackClick}
          ></ArrowBackIcon>
        </div>
      )}
      {folders.map((item, index) => (
        <li
          key={index}
          style={{
            cursor: "pointer",
            fontWeight: selectedFolder === item ? "bold" : "normal",
          }}
          onClick={() => handleFolderClick(item)}
        >
          {item.endsWith(".mzML") ? (
            <InsertDriveFileIcon onClick={(e) => {
              e.stopPropagation();
              handleFolderClick(item)}}/>
          ) : (
            <FolderIcon fontSize="small" />
          )}
          {item}
        </li>
      ))}
      {selectedFiles}
      {selectedFiles.length > 0 && (
        <Button onClick={handleSendFiles}>Confirm Selected</Button>
      )}
    </div>
  );
};

export default SelectMzml;
