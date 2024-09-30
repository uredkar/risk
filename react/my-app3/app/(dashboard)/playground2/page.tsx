"use client"; // This is a client component
import './playground.css';
import * as React from 'react';
import { useMemo, useCallback } from 'react'
import { useEffect, useState } from 'react';

import Box from '@mui/material/Box';
import Switch from '@mui/material/Switch';
import Paper from '@mui/material/Paper';
import Fade from '@mui/material/Fade';
import FormControlLabel from '@mui/material/FormControlLabel';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { store } from './store';
import MyComponent from './mycomponent';
import ChartComponent from './chartcomponent';


export default function Playground2Page() {

    return(
      <Provider store={store}>
        <MyComponent />
        <ChartComponent></ChartComponent>
      </Provider>
    )
  };
  