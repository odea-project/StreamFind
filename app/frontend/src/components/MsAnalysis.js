import { Paper } from "@mui/material";
import { DraggableElement } from "./DraggableElement";
import Grid from "@mui/material/Unstable_Grid2";
import InsertDriveFileIcon from '@mui/icons-material/InsertDriveFile';
import { ItemTypes } from "./ItemTypes";
import { SourceTypes } from "./SourceTypes";

export const MsAnalysis = () => {
  return (
    <>
      <DraggableElement
        id="msAnalysis"
        title="msAnalysis"
        source={SourceTypes.OBJECTS}
        type={ItemTypes.MS_ANALYSIS}
      >
          <Grid
            container
            spacing={0}
            direction="column"
            alignItems="center"
            justifyContent="center"
          >
            <Grid xs={12}>
              <InsertDriveFileIcon style={{fontSize:'6em', color:'green'}} />
            </Grid>
            <Grid xs={12} style={{ textAlign: "center" }}>
              <div>mzML</div>
            </Grid>
          </Grid>
      </DraggableElement>
    </>
  );
};
