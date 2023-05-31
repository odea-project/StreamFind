import FolderIcon from '@mui/icons-material/Folder';
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
        <Grid
          container
          spacing={0}
          direction="column"
          alignItems="center"
          justifyContent="center"
        >
          <Grid xs={12}>
            <FolderIcon style={{fontSize:'6em', color:'orange'}} />
          </Grid>
          <Grid xs={12} style={{ textAlign: "center" }}>
            <div>msData</div>
          </Grid>
        </Grid>
    </DraggableElement>
  );
};
