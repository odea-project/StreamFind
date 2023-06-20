import React, { useState } from "react";
import InsertDriveFileIcon from "@mui/icons-material/InsertDriveFile";
import SettingsIcon from "@mui/icons-material/Settings";
import Modal from "@mui/material/Modal";
import Box from "@mui/material/Box";
import Typography from "@mui/material/Typography";
import Button from "@mui/material/Button";
import SelectMzml from "./SelectMzml";
import IconButton from "@mui/material/IconButton";
import CloseIcon from "@mui/icons-material/Close";
import MsData from "./MsData";

const MsAnalysis = ({ id, content, data, outputs, inputs }) => {
  const [open, setOpen] = useState(false);
  const handleOpen = () => {
    setOpen(true);
    console.log("ok");
  };
  const handleClose = () => {
    setOpen(false);
    setIsChildOpen(false);
  };
  const style = {
    position: "absolute",
    top: "50%",
    left: "50%",
    transform: "translate(-50%, -50%)",
    width: 500,
    height: 500,
    bgcolor: "#c1eff9",
    border: "2px solid white",
    borderRadius: "25px",
    p: 5,
  };
  const style1 = {
    position: "absolute",
    top: "50%",
    left: "50%",
    transform: "translate(-50%, -50%)",
    width: 300,
    height: "auto",
    bgcolor: "#c1eff9",
    border: "2px solid white",
    borderRadius: "25px",
    p: 5,
  };

  const [isChildOpen, setIsChildOpen] = useState(false);
  const handleFolderSelect = (file) => {
    setSelectedFile(file);
    setIsChildOpen(false);
    setfilSelected(true);
  };

  const openFileSelect = () => {
    setSelectedFile(""); // Reset selected file
    setfilSelected(false); // Reset file selection state
    setIsChildOpen(true);
  };

  const [selectedFile, setSelectedFile] = useState("");
  const [filSelected, setfilSelected] = useState(false);
  const [arrow, setArrow] = useState("false");

  return (
    <div>
      <p style={{ position: "absolute", top: 77.7, left: 42 }}>send </p>
      <div
        style={{
          position: "absolute",
          top: -3,
          left: 5,
        }}
      >
        <SettingsIcon style={{ cursor: "pointer" }} onClick={handleOpen} />
      </div>
      <Modal
        open={open}
        onClose={handleClose}
        aria-labelledby="modal-modal-title"
        aria-describedby="modal-modal-description"
      >
        <Box sx={style1}>
          <IconButton
            aria-label="close"
            onClick={handleClose}
            sx={{
              position: "absolute",
              right: 8,
              top: 8,
            }}
          >
            <CloseIcon />
          </IconButton>
          <Typography id="modal-modal-title" variant="h6" component="h2">
            Select mZmL file
          </Typography>
          <Button onClick={openFileSelect}>See Files</Button>
          {isChildOpen && (
            <Box sx={style}>
              {" "}
              <IconButton
                aria-label="close"
                onClick={handleClose}
                sx={{
                  position: "absolute",
                  right: 8,
                  top: 8,
                }}
              >
                <CloseIcon />
              </IconButton>
              <SelectMzml onFolderSelect={handleFolderSelect} />
            </Box>
          )}
          {filSelected && (
            <div>
              <h4>Selected:{selectedFile}</h4>
              <Button onClick={handleClose}>OK</Button>
            </div>
          )}
        </Box>
      </Modal>
      <InsertDriveFileIcon
        style={{ fontSize: "6em", color: "green" }}
      ></InsertDriveFileIcon>
      <div
        style={{
          position: "absolute",
          top: 100,
          left: 30,
        }}
      >
        {inputs.map((port) =>
          React.cloneElement(port, {
            style: {
              width: "10px",
              height: "10px",
              borderRadius: "50px",
              background: "blue",
            },
          })
        )}
      </div>
    </div>
  );
};

export default MsAnalysis;
