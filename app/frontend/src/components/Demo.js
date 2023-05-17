import { Container } from "./Container.js";
import Grid from "@mui/material/Unstable_Grid2";
import { Objects } from "./Objects";
import { Input } from "./Input.js";

export default function Demo() {
  return (
    <div>
      <Grid container spacing={2}>
        <Grid container justifyContent="center" xs={15}>
          <Container />
        </Grid>
        <Grid xs={4}>
          <div>
            <h4>Objects</h4>
          </div>
          <Objects />
        </Grid>
        <Grid xs={4}>
          <div>
            <h4>Input</h4>
          </div>
          <Input />
        </Grid>
        <Grid xs={4}>
          <div>
            <h4>Ms-Preprocessing</h4>
          </div>
        </Grid>
      </Grid>
    </div>
  );
}
