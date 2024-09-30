"use client"; // This is a client component
import './playground.css';
import * as React from 'react';
import { useState } from 'react';
import dynamic from 'next/dynamic';
import Box from '@mui/material/Box';
import Switch from '@mui/material/Switch';
import Paper from '@mui/material/Paper';
import Fade from '@mui/material/Fade';
import FormControlLabel from '@mui/material/FormControlLabel';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
//import { store } from './store';
//import MyComponent from './mycomponent';
//import ChartComponent from './chartcomponent';
import { DndProvider } from "react-dnd";
import { HTML5Backend } from "react-dnd-html5-backend";


//import Card from '@mui/toolpad-core/react'; //'@mui/joy/Card';
//import Grid from '@mui/toolpad-core/react'; //'@mui/joy/Grid';
//import Container from '@mui/toolpad-core/react'; //'@mui/joy/Container';
import { Grid2, Container, Card, Typography } from '@mui/material';

import { useDrop } from 'react-dnd';

import DraggableItem from './draggableitem';
import DroppableArea from './droppablearea';
  

  
interface DroppedComponent {
  id: string;
  type: string;
}
 



const Dashboard: React.FC = () => {

  
  return (
    <DndProvider backend={HTML5Backend}>
      <Container>
        <h2>Portfolio Risk Dashboard</h2>
        <Grid2 container spacing={2}>
          {/* Draggable Items */}
          <Grid2>
            <Card>
              <DraggableItem id="1" name="Stock Chart 1"  type="chart"/>
              <DraggableItem id="2" name="Risk Grid"  type="grid"/>
            </Card>
          </Grid2>

          {/* Droppable Area */}
          <Grid2>
            <Card>
              <DroppableArea />
            </Card>
          </Grid2>
        </Grid2>
      </Container>
    </DndProvider>
 
  );
};


export default Dashboard;
  