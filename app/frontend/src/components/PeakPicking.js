import { Paper } from "@mui/material";
import { DraggableElement } from "./DraggableElement";
import Grid from "@mui/material/Unstable_Grid2";
import peakIcon from "D:/work/streamFind/app/frontend/src/peak.PNG";
import { ItemTypes } from "./ItemTypes";
import { SourceTypes } from "./SourceTypes";

export const PeakPicking = () => {
  return (
    <>
      <DraggableElement
        id="peakPicking"
        title="peakpicking"
        source={SourceTypes.OBJECTS}
        type={ItemTypes.PEAK}
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
                  width: "80px",
                  height: "70px",
                }}
                src={peakIcon}
                alt="Custom Icon"
              />
            </Grid>
            <Grid xs={12} style={{ textAlign: "center" }}>
              <div>Peak Picking</div>
            </Grid>
          </Grid>
        </Paper>
      </DraggableElement>
    </>
  );
};
