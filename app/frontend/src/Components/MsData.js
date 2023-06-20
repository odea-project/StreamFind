import React from "react";
import FolderIcon from "@mui/icons-material/Folder";
import PlayCircleIcon from "@mui/icons-material/PlayCircle";
import SettingsIcon from "@mui/icons-material/Settings";

const MsData = ({ id, content, data, inputs, outputs }) => (
  <div>
    <p style={{ position: "absolute", top: 31, left: -28 }}>in </p>
    <p style={{ position: "absolute", top: 31, left: 113 }}>out</p>
    <div
      style={{
        position: "absolute",
        top: 0,
        left: 0,
      }}
    >
      <SettingsIcon />
    </div>
    <FolderIcon style={{ fontSize: "6em", color: "orange" }}></FolderIcon>
    {outputs.map((port) => {
      return (
        <div
          style={{
            position: "absolute",
            top: port.key.startsWith("portMsAnalysisgreen")
              ? 49
              : port.key.startsWith("portMsAnalysisblue")
              ? 50
              : -15,
            left: port.key.startsWith("portMsAnalysisgreen")
              ? -8
              : port.key.startsWith("portMsAnalysisblue")
              ? 95
              : 40,
          }}
        >
          {port.key.startsWith("portMsAnalysisplay")
            ? React.cloneElement(port, {
                style: {
                  position: "absolute",
                  color: "red",
                },
                children: <PlayCircleIcon />,
              })
            : React.cloneElement(port, {
                style: {
                  width: "10px",
                  height: "10px",
                  borderRadius: "50px",
                  background: port.key.startsWith("portMsAnalysisgreen")
                    ? "green"
                    : "blue",
                },
              })}
        </div>
      );
    })}
  </div>
);

export default MsData;
