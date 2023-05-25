import msdataIcon from "D:/work/streamFind/app/frontend/src/msdata.PNG";
import { Paper } from "@mui/material";
import { DraggableElement } from "./DraggableElement";
import Grid from "@mui/material/Unstable_Grid2";
import { ItemTypes } from "./ItemTypes";
import { SourceTypes } from "./SourceTypes";

export const MsData = () => {
  return (
    <DraggableElement
      id="msData"
      title="ms_data"
      source={SourceTypes.OBJECTS}
      type={ItemTypes.BOX}
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
              src={msdataIcon}
              alt="Custom Icon"
            />
          </Grid>
          <Grid xs={12} style={{ textAlign: "center" }}>
            <div>msData</div>
          </Grid>
        </Grid>
      </Paper>
    </DraggableElement>
  );
};
