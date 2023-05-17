import React, { memo, useRef } from "react";
import { useDrag, useDrop } from "react-dnd";
import SettingsIcon from "@mui/icons-material/Settings";
import { ItemTypes } from "./ItemTypes";
import { useState } from "react";
import { MsAnalysis } from "./MsAnalysis";
import toast, { Toaster } from "react-hot-toast";
import PlayCircleIcon from "@mui/icons-material/PlayCircle";

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
          borderRadius: "25px",
          border: isOver ? "2px solid brown" : "0px solid brown",
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
                refX="10"
                refY="4"
                orient="auto-start-reverse"
              >
                <polygon points="0 0, 10 3.5, 0 7" />
              </marker>
              <path
                d="M 10 10 Q 0 20, 100 50"
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
                <input
                  type="file"
                  accept=".mZmL"
                  id="file"
                  ref={inputFile}
                  style={{ display: "none" }}
                  directory="D:\work\streamFind\app\backend\sample mzml"
                  onChange={onFileSelect}
                />
                <SettingsIcon
                  style={{
                    cursor: "pointer",
                  }}
                  onClick={onButtonClick}
                ></SettingsIcon>
                <MsAnalysis></MsAnalysis>
                {fileName && <PlayCircleIcon />}
              </div>
            ) : null}
          </>
        )}
      </div>
      {isActive && <div>Can drop here</div>}
    </div>
  );
});
