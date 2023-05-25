import { Paper } from "@mui/material";
import { DraggableElement } from "./DraggableElement";
import Grid from "@mui/material/Unstable_Grid2";
import mzmlIcon from "D:/work/streamFind/app/frontend/src/mzml.PNG";
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
        <Paper style={{ backgroundColor: "transparent" }}>
          <Grid
            container
            spacing={0}
            direction="column"
            alignItems="center"
            justifyContent="center"
          >
            <Grid xs={12}>
              <img
                style={{
                  width: "78px",
                  height: "70px",
                }}
                src={mzmlIcon}
                alt="Custom Icon"
              />
            </Grid>
            <Grid xs={12} style={{ textAlign: "center" }}>
              <div>mzML</div>
            </Grid>
          </Grid>
        </Paper>
      </DraggableElement>
    </>
  );
};
