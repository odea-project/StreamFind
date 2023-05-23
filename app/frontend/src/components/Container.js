import update from "immutability-helper";
import { useCallback, useState, useRef } from "react";
import { useDrop, useDrag } from "react-dnd";
import { DraggableElement } from "./DraggableElement.js";
import { ItemTypes } from "./ItemTypes.js";
import { SourceTypes } from "./SourceTypes";
import toast, { Toaster } from "react-hot-toast";
import MenuIcon from "@mui/icons-material/Menu";

export const Container = () => {
  const [items, setItems] = useState({});
  const moveBox = useCallback(
    (id, left, top) => {
      setItems(
        update(items, {
          [id]: {
            $merge: { left, top },
          },
        })
      );
    },
    [items]
  );

  const [{ isOver }, drop] = useDrop(
    () => ({
      accept: [ItemTypes.BOX],
      drop: (item, monitor) => {
        console.log("item", item);

        if (!item) {
          return;
        }

        if (item.source !== SourceTypes.CONTAINER) {
          // add new item
          const updatedItems = Object.assign(items, {
            [item.id + `_${Date.now()}`]: {
              top: Math.round(monitor.getClientOffset().y),
              left: Math.round(monitor.getClientOffset().x),
              title: item.title,
              source: SourceTypes.CONTAINER,
              type: item.type,
              children: item.children,
              settingIcon: true,
              whichObj: true,
            },
          });

          setItems(updatedItems);
          toast.success(item.title);
          console.log("updatedItems", updatedItems);
        } else {
          // move item
          const delta = monitor.getDifferenceFromInitialOffset();
          let left = Math.round(item.left + delta.x);
          let top = Math.round(item.top + delta.y);
          moveBox(item.id, left, top);
          return undefined;
        }
      },
      collect: (monitor) => ({
        isOver: !!monitor.isOver(),
      }),
    }),
    [moveBox]
  );

  return (
    <div
      className="drop"
      ref={drop}
      style={{
        width: "100%",
        height: "65vh",
        position: "relative",
        backgroundColor: "#F9EBC7",
        borderRadius: "0px",
        border: isOver ? "5px solid white" : "1px solid white",
      }}
    >
      <MenuIcon fontSize="large"></MenuIcon>
      {Object.keys(items).map((key) => (
        <DraggableElement
          key={key}
          id={key}
          style={{ Color: "red" }}
          {...items[key]}
        ></DraggableElement>
      ))}
      <Toaster />;
    </div>
  );
};
