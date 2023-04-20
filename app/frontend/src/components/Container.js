import update from 'immutability-helper'
import { useCallback, useState } from 'react'
import { useDrop } from 'react-dnd'
import { DraggableElement } from './DraggableElement.js'
import { ItemTypes } from './ItemTypes.js'
import {SourceTypes} from "./SourceTypes";


export const Container = () => {
  const [items, setItems] = useState({})
  const moveBox = useCallback(
    (id, left, top) => {
      setItems(
        update(items, {
          [id]: {
            $merge: { left, top }
          },
        }),
      )
    },
    [items],
  )
  const [{isOver}, drop] = useDrop(
    () => ({
      accept: [ItemTypes.BOX, ItemTypes.MS_ANALYSIS],
      drop: (item, monitor) => {

          console.log("item", item)

          if (item.source !== SourceTypes.CONTAINER) {
              // add new item
              const updatedItems = Object.assign(items, {
                  [item.id + `_${Date.now()}`]: {
                      top: Math.round(monitor.getClientOffset().y),
                      left: Math.round(monitor.getClientOffset().x),
                      title: item.title,
                      source: SourceTypes.CONTAINER,
                      type: item.type,
                      children: item.children
                  }
              });
              setItems(updatedItems);
              console.log("updatedItems", updatedItems)
          } else {
              // move item
              const delta = monitor.getDifferenceFromInitialOffset()
              let left = Math.round(item.left + delta.x)
              let top = Math.round(item.top + delta.y)
              moveBox(item.id, left, top)
              return undefined
          }
      },
      collect: monitor => ({
          isOver: !!monitor.isOver()
      })
    }),
    [moveBox],
  )
  return (
    <div ref={drop} style={{width: "100%", height: "50vh", position: 'relative', border: isOver? "5px solid red" : "1px solid blue"}}>
      {Object.keys(items).map((key) => (
        <DraggableElement key={key} id={key} {...items[key]} />
      ))}
    </div>
  )
}
