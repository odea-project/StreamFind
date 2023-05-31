import React, { memo, useRef } from "react";
import { useDrag, useDrop } from "react-dnd";
import SettingsIcon from "@mui/icons-material/Settings";
import { ItemTypes } from "./ItemTypes";
import { useState } from "react";
import { MsAnalysis } from "./MsAnalysis";
import toast, { Toaster } from "react-hot-toast";
import PlayCircleIcon from "@mui/icons-material/PlayCircle";
import Modal from "@mui/material/Modal";
import Box from "@mui/material/Box";
import Typography from "@mui/material/Typography";
import Button from "@mui/material/Button";
import SelectMzml from "./SelectMzml";
import IconButton from "@mui/material/IconButton";
import CloseIcon from "@mui/icons-material/Close";
import { blueGrey } from "@mui/material/colors";
import ArrowDownwardIcon from "@mui/icons-material/ArrowDownward";

function getStyles(left, top, isDragging) {
  const transform = `translate3d(${left}px, ${top}px, 0)`;
  return {
    position: "absolute",
    transform,
    WebkitTransform: transform,
    opacity: isDragging ? 0.4 : 1,
  };
}

export const DraggableElement = memo(function DraggableBox(props) {
  const {
    id,
    title,
    left,
    top,
    source,
    type,
    children,
    settingIcon,
    whichObj,
  } = props;

  const [isDroppedOnDropbox, setIsDroppedOnDropbox] = useState(false);

  const [{ isDragging }, drag] = useDrag(
    () => ({
      type,
      item: {
        id,
        left,
        top,
        title,
        source,
        type,
        children,
        settingIcon,
        whichObj,
      },
      collect: (monitor) => ({
        isDragging: monitor.isDragging(),
      }),
    }),
    [id, left, top, title, source, type, children, settingIcon, whichObj]
  );

  const [{ canDrop, isOver }, drop] = useDrop(
    () => ({
      accept: ItemTypes.MS_ANALYSIS,
      drop: (item, monitor) => {
        console.log("Dropped Item:", item);
        setIsDroppedOnDropbox(true);
        toast.success(item.title);
        setArrow(true);
      },
      collect: (monitor) => ({
        canDrop: monitor.canDrop(),
        isOver: monitor.isOver(),
      }),
    }),
    [type]
  );

  const isActive = canDrop && isOver;

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
    <div
      ref={(node) => drag(drop(node))}
      style={getStyles(left, top, isDragging)}
    >
      {settingIcon && (
        <div style={{ position: "absolute", top: 13, left: 7 }}>
          <SettingsIcon sx={{ color: blueGrey[700] }} />
        </div>
      )}
      <div
        style={{
          display: "flex",
          alignItems: "center",
          borderRadius: "0px",
          border: isOver ? "2px solid black" : "0px solid black",
        }}
      >
        {whichObj && (
          <>
            <p style={{ position: "absolute", top: 31, left: -15 }}>in </p>
            <div
              style={{
                width: "10px",
                height: "10px",
                borderRadius: "50%",
                backgroundColor: "green",
                marginRight: "10px",
                position: "absolute",
                top: 53,
                left: 2,
              }}
            ></div>
            <div
              style={{
                width: "10px",
                height: "10px",
                borderRadius: "50%",
                backgroundColor: "blue",
                marginRight: "10px",
                position: "absolute",
                top: 53,
                left: 99,
              }}
            ></div>
            <p style={{ position: "absolute", top: 31, left: 113 }}>out</p>
          </>
        )}
        {children}
        {whichObj && isDroppedOnDropbox && (
          <>
            <div style={{ position: "absolute", top: -33, left: 45 }}>
              <ArrowDownwardIcon></ArrowDownwardIcon>
            </div>
            <svg
              height="100"
              width="100"
              style={{ position: "absolute", top: -70, left: 56 }}
            >
              <path
                d="M0 43 C22 -10, 190 0, 250 90"
                stroke="black"
                strokeWidth="1.7"
                fill="transparent"
              />
            </svg>
            <div style={{ position: "absolute", top: -7, left: 45 }}>
              {filSelected ? (
                <PlayCircleIcon style={{ color: "green" }}></PlayCircleIcon>
              ) : (
                <PlayCircleIcon style={{ color: "red" }}></PlayCircleIcon>
              )}
            </div>
            <p style={{ position: "absolute", top: -21, left: 72 }}>input</p>
            {isDroppedOnDropbox ? (
              <div
                style={{
                  position: "absolute",
                  zIndex: 1,
                  cursor: "pointer",
                  top: -200,
                  left: 130,
                }}
              >
                <div
                  style={{
                    position: "absolute",
                    zIndex: 1,
                    cursor: "pointer",
                    top: 7,
                    left: 12,
                  }}
                >
                  <SettingsIcon
                    sx={{ color: blueGrey[700] }}
                    onClick={handleOpen}
                  ></SettingsIcon>
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
                    <Typography
                      id="modal-modal-title"
                      variant="h6"
                      component="h2"
                    >
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
                <MsAnalysis></MsAnalysis>
                <div
                  style={{
                    width: "10px",
                    height: "10px",
                    borderRadius: "50%",
                    backgroundColor: "blue",
                    marginRight: "10px",
                    position: "absolute",
                    top: 135,
                    left: 25,
                  }}
                ></div>
                <p style={{ position: "absolute", top: 113, left: 39 }}>send</p>
              </div>
            ) : null}
          </>
        )}
      </div>
      {isActive && <div>Can drop here</div>}
    </div>
  );
});
