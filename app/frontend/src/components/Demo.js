import { Container } from "./Container.js";
import Grid from "@mui/material/Unstable_Grid2";
import { Objects } from "./Objects";
import { Input } from "./Input.js";
import { PreProcessing } from "./PreProcessing";
import MenuIcon from "@mui/icons-material/Menu";

export default function Demo() {
  return (
    <div>
      <Grid container spacing={2}>
        <Grid xs={12}>
          <Container />
        </Grid>
        <div className="demo_box">
          <div className="upper_div">
            <MenuIcon></MenuIcon>
            <h4>Objects</h4>
          </div>
          <Objects />
        </div>
        <div className="demo_box">
          <div className="upper_div">
            <MenuIcon></MenuIcon>
            <h4>Input</h4>
          </div>
          <Input />
        </div>
        <div className="demo_box">
          <div className="upper_div">
            <MenuIcon></MenuIcon>
            <h4>Ms-Preprocessing</h4>
          </div>
          <PreProcessing />
        </div>
      </Grid>
    </div>
  );
}
