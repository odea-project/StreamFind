import { Container } from './Container.js'
import Grid from '@mui/material/Unstable_Grid2';
import {Objects} from "./Objects";

export default function Demo () {
  return (
    <div>
      <Grid container spacing={2}>
        <Grid xs={12}>
            <Container />
        </Grid>
        <Grid xs={4}>
            <div><h2>Objects</h2></div>
            <Objects />
        </Grid>
        <Grid xs={4}>
            Input
        </Grid>
        <Grid xs={4}>
            Ms-preprocessing
        </Grid>
      </Grid>
    </div>
  )
}
