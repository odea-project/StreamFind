import FolderTwoToneIcon from '@mui/icons-material/FolderTwoTone';
import SnippetFolderTwoToneIcon from '@mui/icons-material/SnippetFolderTwoTone';
import {Paper} from "@mui/material";
import {DraggableElement} from "./DraggableElement";
import Grid from "@mui/material/Unstable_Grid2";
import {useDrop} from "react-dnd";
import {ItemTypes} from "./ItemTypes";
import {SourceTypes} from "./SourceTypes";
import {useState} from "react";

export const MsData = () => {
    const [msAnalysisItems, setMsAnalysisItems] = useState({})
    const [{isOver, isOverCurrent}, drop] = useDrop(() => ({
          accept: ItemTypes.MS_ANALYSIS,
          /*drop: (item, monitor) => {
              console.log("item", item)
              if (item.source !== SourceTypes.CONTAINER) {
                  // add new item
                  const updatedItems = Object.assign(msAnalysisItems, {
                      [item.id + `_${Date.now()}`]: {
                          title: item.title,
                          // data: item.data,
                          source: SourceTypes.CONTAINER,
                          children: item.children
                      }
                  });
                  setMsAnalysisItems(updatedItems);
                  console.log("updatedItems", updatedItems)
              }
          },
          collect: monitor => ({
              isOver: monitor.isOver(),
          })*/
    }),
    [],
  )

  console.log("Object.keys(msAnalysisItems).length", Object.keys(msAnalysisItems).length)
  return (
          <DraggableElement id="msData" title="ms_data" source={SourceTypes.OBJECTS} type={ItemTypes.BOX}>
            <Paper ref={drop} /*style={{border: isOver? "5px solid red" : "5px solid blue"}}*/ >
              <Grid  container
                     spacing={0}
                     direction="column"
                     alignItems="center"
                     justifyContent="center">
                <Grid xs={12}>
                    {
                        (msAnalysisItems && Object.keys(msAnalysisItems).length === 0) ?
                            <FolderTwoToneIcon style={{fontSize: "10em"}} /> :
                            <SnippetFolderTwoToneIcon style={{fontSize: "10em"}} />
                    }
                </Grid>
                <Grid xs={12} style={{textAlign: "center"}}>
                    <div>msData</div>
                </Grid>
              </Grid>
            </Paper>
        </DraggableElement>

  )
}
