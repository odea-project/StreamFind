import FolderTwoToneIcon from "@mui/icons-material/FolderTwoTone";
import { Paper } from "@mui/material";
import { DraggableElement } from "./DraggableElement";
import Grid from "@mui/material/Unstable_Grid2";
import { ItemTypes } from "./ItemTypes";
import { SourceTypes } from "./SourceTypes";
import { useState } from "react";
import SettingsIcon from "@mui/icons-material/Settings";

export const MsData = () => {
  return (
    <DraggableElement
      id="msData"
      title="ms_data"
      source={SourceTypes.OBJECTS}
      type={ItemTypes.BOX}
    >
      <Paper>
        <Grid
          container
          spacing={0}
          direction="column"
          alignItems="center"
          justifyContent="center"
        >
          <Grid xs={12}>
            <FolderTwoToneIcon style={{ fontSize: "5em" }} />
          </Grid>
          <Grid xs={12} style={{ textAlign: "center" }}>
            <div>msData</div>
          </Grid>
        </Grid>
      </Paper>
    </DraggableElement>
  );
};
