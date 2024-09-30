import { useDrop } from 'react-dnd';
import React, { useState,forwardRef  } from 'react';
import ChartComponent from './chartcomponent';
import { Item } from './type';

const DroppableArea: React.FC = () => {
  const [droppedItems, setDroppedItems] = useState<Item[]>([]);

  const [{ isOver }, drop] = useDrop(() => ({
    accept: 'ITEM',
    drop: (item: Item) => {
      setDroppedItems((prev) => [...prev, item]);
    },
    collect: (monitor) => ({
      isOver: monitor.isOver(),
    }),
  }));

  return (
    <div
      ref={drop}
      style={{
        height: '800px',
        width: '800px',
        border: '2px dashed #ccc',
        backgroundColor: isOver ? '#f0f0f0' : 'white',
        padding: '10px',
      }}
    >
      {isOver ? 'Release to drop' : 'Drag item here'}
       {/* Render dropped items */}
       {droppedItems.map((item) => {
        
        if (item.type === 'chart') {
          return <ChartComponent key={item.id} />; // Render ChartComponent when dropped
        }
        return null; // You can add more types here
      })}
    </div>
  );
};

export default DroppableArea;
