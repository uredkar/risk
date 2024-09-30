// store.ts (Setting up Redux store)
import { configureStore } from '@reduxjs/toolkit';
import appReducer from './appslice';

export const store = configureStore({
  reducer: {
    app: appReducer,
  },
});

// TypeScript types for dispatch and state
export type RootState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;
