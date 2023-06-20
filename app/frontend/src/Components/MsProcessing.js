import React from "react";
import QueryStatsIcon from "@mui/icons-material/QueryStats";
import PlayCircleIcon from "@mui/icons-material/PlayCircle";
import SettingsIcon from "@mui/icons-material/Settings";

const MsProcessing = ({ id, content, data, outputs, inputs }) => (
  <div>
    <p style={{ position: "absolute", top: 29, left: -32 }}>in </p>
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
    <QueryStatsIcon style={{ fontSize: "6em" }}></QueryStatsIcon>
    <p style={{ position: "absolute", top: -35, left: 47 }}>input </p>
    <div
      style={{
        width: "10px",
        height: "10px",
        borderRadius: "50%",
        backgroundColor: "grey",
        marginRight: "10px",
        position: "absolute",
        top: -15,
        left: 35,
      }}
    ></div>
    {inputs.map((port) => {
      return (
        <div
          style={{
            position: "absolute",
            top: port.key.startsWith("portMsPreProcessingin") ? 43 : 45,
            left: port.key.startsWith("portMsPreProcessingout") ? -15 : 85,
          }}
        >
          {port.key.startsWith("portMsPreProcessing")
            ? React.cloneElement(port, {
                style: {
                  position: "absolute",
                  color: "green",
                },
                children: <PlayCircleIcon />,
              })
            : null}
        </div>
      );
    })}
  </div>
);

export default MsProcessing;
