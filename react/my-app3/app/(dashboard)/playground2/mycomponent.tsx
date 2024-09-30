// MyComponent.tsx
import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { RootState, AppDispatch } from './store';
import { setCount, setText, setVisibility, setUser,  setNewItemInput, addItem } from './appslice';

const MyComponent = () => {
  const dispatch: AppDispatch = useDispatch();
  const { count, text, isVisible, user, items,newItemInput } = useSelector((state: RootState) => state.app);



  const handleSetCount = (newCount: number) => {
    dispatch(setCount(newCount));
  };

  const handleSetText = (newText: string) => {
    dispatch(setText(newText));
  };

  const handleToggleVisibility = (visibility: boolean) => {
    dispatch(setVisibility(visibility));
  };

  const handleSetUser = (name: string, age: number) => {
    dispatch(setUser({ name, age }));
  };


  const handleNewItemInputChange = (value: string) => {
    dispatch(setNewItemInput(value));
  };

  const handleAddItem = () => {
    dispatch(addItem());
  }

  return (
    <div>
      <div>
        <h1>Count: {count}</h1>
        <button onClick={() => handleSetCount(count + 1)}>Increment Count</button>
      </div>

      <div>
        <h2>Text: {text}</h2>
        <input
          value={text}
          onChange={(e) => handleSetText(e.target.value)}
          placeholder="Set Text"
        />
      </div>

      <div>
        <h3>Visibility: {isVisible ? 'Visible' : 'Hidden'}</h3>
        <button onClick={() => handleToggleVisibility(!isVisible)}>
          Toggle Visibility
        </button>
      </div>

      <div>
        <h4>User Name: {user.name}</h4>
        <input
          value={user.name}
          onChange={(e) => dispatch(setUser({ name: e.target.value, age: user.age }))}
          placeholder="User Name"
        />
      </div>

      <div>
        <h5>Items: {items.join(', ')}</h5>
        <input
          value={newItemInput}
          onChange={(e) => handleNewItemInputChange(e.target.value)}
          placeholder="Add New Item"
        />
        <button onClick={handleAddItem}>Add Item</button>
      </div>
    </div>
  );
};

export default MyComponent;
