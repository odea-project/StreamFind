import {Paper} from "@mui/material";
import {DraggableElement} from "./DraggableElement";
import Grid from "@mui/material/Unstable_Grid2";
import InsertDriveFileOutlinedIcon from '@mui/icons-material/InsertDriveFileOutlined';
import {ItemTypes} from "./ItemTypes";
import {SourceTypes} from "./SourceTypes";

export const MsAnalysis = () => {
  return (
      <>
        <DraggableElement id="msAnalysis" title="msAnalysis" source={SourceTypes.OBJECTS} type={ItemTypes.MS_ANALYSIS}>
            <Paper>
              <Grid  container
                     spacing={0}
                     direction="column"
                     alignItems="center"
                     justifyContent="center">
                <Grid xs={12}>
                    <InsertDriveFileOutlinedIcon style={{fontSize: "10em"}} />
                </Grid>
                <Grid xs={12} style={{textAlign: "center"}}>
                    <div>msAnalysis</div>
                </Grid>
              </Grid>
            </Paper>
        </DraggableElement>
      </>
  )
}
