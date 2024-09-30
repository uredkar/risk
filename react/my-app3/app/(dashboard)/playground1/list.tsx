import React, { useEffect, memo } from 'react'
import { FC, useState } from 'react';
import Task from './task';



// Types
export type Todo = {
    id: number
    task: string
  }
  
  interface Props {
    todoList: Todo[]
    handleDelete: any
  }

  

const List: FC<Props> = ({ todoList, handleDelete }) => {
  const [items, setItems] = useState(["foo", "bar"]);

  const handleClick = () => {
    const newItems = items.slice();
    newItems.unshift("baz");

    setItems(newItems);
  };

  return (
    <>
    <div>
        <h3>List 1</h3>
      <ul>
        {items.map((item, index) => (
          <li key={index}>
            {item}
            <input type="text" />
          </li>
        ))}
      </ul>
      <button onClick={handleClick}>+List</button>
    </div>
    <div>
        <h3>Todo List</h3>
         <ul>
      {todoList.map((todo: Todo) => (
        <Task 
          key={todo.id} 
          id={todo.id}
          task={todo.task} 
          handleDelete={handleDelete}
        />
      ))}
    </ul>
    </div>
    </>
  );
};

export default memo(List)