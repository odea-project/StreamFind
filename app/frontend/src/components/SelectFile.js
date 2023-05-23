import React, { useEffect, useState } from "react";
import axios from "axios";

function SelectFile() {
  const [selectedFiles, setSelectedFiles] = useState([]);
  const [files, setFiles] = useState([]);
  const [currentDirectory, setCurrentDirectory] = useState([]);
  const [combined, setCombined] = useState([]);

  useEffect(() => {
    // Fetch the initial list of files and folders
    fetchFiles();
  }, []);

  const fetchFiles = async () => {
    try {
      const response = await fetch("http://127.0.0.1:8374/files_project");
      console.log(response);
      const data = await response.json();
      setFiles(data);
    } catch (error) {
      console.error("Error fetching files:", error);
    }
  };

  const handleFolderClick = async (folder) => {
    try {
      // Send a POST request to select the clicked folder
      await fetch("http://127.0.0.1:8374/select_folder", {
        method: "POST",
        body: folder,
      });
      setCurrentDirectory((prevDir) =>
        prevDir ? `${prevDir}/${folder}` : folder
      );
      fetchFiles();
    } catch (error) {
      console.error("Error selecting folder:", error);
    }
  };

  const handleParentFolderClick = async () => {
    try {
      // Send a POST request to return to the parent folder
      await fetch("http://127.0.0.1:8374/parent_folder", { method: "POST" });
      setCurrentDirectory((prevDir) =>
        prevDir.split("/").slice(0, -1).join("/")
      );
      fetchFiles();
    } catch (error) {
      console.error("Error returning to parent folder:", error);
    }
  };
  return (
    <div>
      <h1>File Explorer</h1>
      <p>Current Directory: {currentDirectory || "/"}</p>
      <button onClick={handleParentFolderClick}>Parent Folder</button>
      <ul>
        {files.map((file) => (
          <li key={file} onClick={() => handleFolderClick(file)}>
            {file}
          </li>
        ))}
      </ul>
    </div>
  );
}

export default SelectFile;
