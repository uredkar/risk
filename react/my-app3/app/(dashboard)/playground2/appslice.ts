// appSlice.ts (Create a slice for your state)
import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface User {
  name: string;
  age: number;
}

interface AppState {
  count: number;
  text: string;
  isVisible: boolean;
  user: User;
  items: string[];
  newItemInput: string;
}

const initialState: AppState = {
  count: 0,
  text: '',
  isVisible: true,
  user: { name: '', age: 0 },
  items: [],
  newItemInput: '',
};

const appSlice = createSlice({
  name: 'app',
  initialState,
  reducers: {
    setCount: (state, action: PayloadAction<number>) => {
      state.count = action.payload;
    },
    setText: (state, action: PayloadAction<string>) => {
      state.text = action.payload;
    },
    setVisibility: (state, action: PayloadAction<boolean>) => {
      state.isVisible = action.payload;
    },
    setUser: (state, action: PayloadAction<User>) => {
      state.user = action.payload;
    },
    setItems: (state, action: PayloadAction<string[]>) => {
      state.items = action.payload;
    },
    setNewItemInput: (state, action: PayloadAction<string>) => {
        state.newItemInput = action.payload;
    },
    addItem: (state) => {
        if (state.newItemInput.trim()) {
          state.items.push(state.newItemInput);
          state.newItemInput = ''; // Clear the input after adding
        }
      },
  },
});

export const { setCount, setText, setVisibility, setUser, setItems, setNewItemInput, addItem } = appSlice.actions;
export default appSlice.reducer;