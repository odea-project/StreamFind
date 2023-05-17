import { useState, useRef } from "react";
import Upload from "@mui/icons-material/UploadFile";
import toast, { Toaster } from "react-hot-toast";

export const SelectFile = () => {
  const [fileName, setFileName] = useState("");
  const inputFile = useRef(null);
  const onButtonClick = () => {
    inputFile.current.click();
  };
  const onFileSelect = () => {
    const file = inputFile.current.files[0];
    setFileName(file.name);
    toast.success(file.name);
    console.log(file.name);
  };
  return (
    <div>
      <input
        type="file"
        accept=".mZmL"
        id="file"
        ref={inputFile}
        style={{ display: "none" }}
        directory="D:\work\streamFind\app\backend\sample mzml"
        onChange={onFileSelect}
      />
      <button onClick={onButtonClick} className="instructions_btn">
        Select mZmL file <Upload fontSize="medium" />
      </button>
      <Toaster />
      {fileName && <p className="instructions">Send: {fileName}</p>}
      {fileName && <button className="instructions_btn">Input</button>}
    </div>
  );
};

export default SelectFile;
