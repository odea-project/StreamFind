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
import SelectFile from "./SelectFile";

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
  const handleOpen = () => setOpen(true);
  const handleClose = () => setOpen(false);
  const style = {
    position: "absolute",
    top: "50%",
    left: "50%",
    transform: "translate(-50%, -50%)",
    width: 400,
    height: 400,
    bgcolor: "#8d98aa",
    border: "2px solid white",
    borderRadius: "25px",
    p: 5,
  };
  const style1 = {
    position: "absolute",
    top: "50%",
    left: "50%",
    transform: "translate(-50%, -50%)",
    width: 400,
    height: 150,
    bgcolor: "#8d98aa",
    border: "2px solid white",
    borderRadius: "25px",
    p: 5,
  };

  const [isChildOpen, setIsChildOpen] = useState(false);
  const handleFileSelect = (file) => {
    setSelectedFile(file);

    setIsChildOpen(false);
    setfilSelected(true);
  };

  const openFileSelect = () => {
    setIsChildOpen(true);
  };

  const [selectedFile, setSelectedFile] = useState("");
  const [filSelected, setfilSelected] = useState(false);

  return (
    <div
      ref={(node) => drag(drop(node))}
      style={getStyles(left, top, isDragging)}
    >
      {settingIcon && (
        <div>
          <SettingsIcon />
        </div>
      )}
      <div
        style={{
          display: "flex",
          alignItems: "center",
          borderRadius: "0px",
          border: isOver ? "2px solid white" : "0px solid white",
        }}
      >
        {whichObj && (
          <>
            <p>in </p>
            <div
              style={{
                width: "10px",
                height: "10px",
                borderRadius: "50%",
                backgroundColor: "green",
                marginRight: "10px",
              }}
            ></div>
          </>
        )}
        <div style={{ flexGrow: 1 }}>
          {children}
          {whichObj && isDroppedOnDropbox && (
            <svg width="100" height="50">
              <marker
                id="arrowhead"
                markerWidth="10"
                markerHeight="7"
                refX="50"
                refY="8"
                orient="auto-start-reverse"
              >
                <polygon points="0 0, 10 0, 0 7" />
              </marker>
              <path
                d="M 20 10 Q 0 20, 100 50"
                fill="none"
                stroke="black"
                strokeWidth="2"
                markerEnd="url(#arrowhead)"
              />
            </svg>
          )}
        </div>
        {whichObj && (
          <>
            <div
              style={{
                width: "10px",
                height: "10px",
                borderRadius: "50%",
                backgroundColor: "blue",
                marginRight: "10px",
              }}
            ></div>
            <p>out</p>
            {isDroppedOnDropbox ? (
              <div>
                <SettingsIcon
                  style={{
                    cursor: "pointer",
                  }}
                  onClick={handleOpen}
                ></SettingsIcon>
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
                        <SelectMzml onFileSelect={handleFileSelect} />
                      </Box>
                    )}
                    {filSelected && (
                      <div>
                        <h4>Selected:{selectedFile}</h4>
                        <Button onClick={handleClose}>Upload</Button>
                      </div>
                    )}
                  </Box>
                </Modal>
                <MsAnalysis></MsAnalysis>
                {filSelected && (
                  <>
                    <PlayCircleIcon></PlayCircleIcon>
                  </>
                )}
              </div>
            ) : null}
          </>
        )}
      </div>
      {isActive && <div>Can drop here</div>}
    </div>
  );
});
