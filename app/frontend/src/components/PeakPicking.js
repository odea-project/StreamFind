import { Paper } from "@mui/material";
import { DraggableElement } from "./DraggableElement";
import Grid from "@mui/material/Unstable_Grid2";
import QueryStatsIcon from '@mui/icons-material/QueryStats';
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
          <Grid
            container
            spacing={0}
            direction="column"
            alignItems="center"
            justifyContent="center"
          >
            <Grid xs={12}>
            <QueryStatsIcon style={{fontSize:'6em'}} />
            </Grid>
            <Grid xs={12} style={{ textAlign: "center" }}>
              <div>Peak Picking</div>
            </Grid>
          </Grid>
      </DraggableElement>
    </>
  );
};
