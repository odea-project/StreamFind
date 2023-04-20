import { memo } from 'react'
import { useDrag } from 'react-dnd'

function getStyles(left, top, isDragging) {
  const transform = `translate3d(${left}px, ${top}px, 0)`
  return {
    position: 'absolute',
    transform,
    WebkitTransform: transform,
    opacity: isDragging ? 0.4 : 1,
  }
}

export const DraggableElement = memo(function DraggableBox(props) {
  const { id, title, left, top, source, type, children} = props

  const [{ isDragging }, drag] = useDrag(
    () => ({
      type,
      item: { id, left, top, title, source, type, children},
      collect: (monitor) => ({
        isDragging: monitor.isDragging(),
      }),
    }),
    [id, left, top, title, source, type, children],
  )

  return (
    <div
      ref={drag}
      style={getStyles(left, top, isDragging)}
    >
      <div>{children}</div>
    </div>
  )
})
