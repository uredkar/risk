import React, { forwardRef } from 'react';

import { useDrag } from 'react-dnd';
import dynamic from 'next/dynamic';
import { Item } from './type'; // Import the Item type

interface DraggableItemProps {
  id: string;
  name: string;
  type: string;
}

const DraggableItem = forwardRef<HTMLDivElement, DraggableItemProps>(({ id, name, type }, ref) => {
  const [{ isDragging }, drag] = useDrag(() => ({
    type: 'ITEM',
    item: { id,type } as Item,
    collect: (monitor) => ({
      isDragging: monitor.isDragging(),
    }),
  }));

  return (
    <div
    ref={(node) => {
        drag(node); // Connect the drag source to the DOM node
        if (ref && node) {
          ref(node); // Call the ref with the node (for forwardRef)
        }
      }}
      style={{
        opacity: isDragging ? 0.5 : 1,
        cursor: 'move',
        padding: '10px',
        border: '1px solid #ccc',
        margin: '5px',
      }}
    >
      
      <div>{name}</div>
      
    </div>
  );
});

export default DraggableItem;