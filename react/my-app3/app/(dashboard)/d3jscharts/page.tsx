"use client"; // This is a client component
import './d3jscharts.css';
import * as React from 'react';
import { useMemo, useCallback } from 'react'
import { useEffect, useState } from 'react';
import Controlled from './controlled';
import Box from '@mui/material/Box';
import Switch from '@mui/material/Switch';
import Paper from '@mui/material/Paper';
import Fade from '@mui/material/Fade';
import FormControlLabel from '@mui/material/FormControlLabel';
import BarChart from './barchart';

type Props = {
    innerWidth: number
}

const initialTodos = [
    { id: 1, task: 'Go shopping' },
    { id: 2, task: 'Pay the electricity bill'}
  ]


const icon = (
<Paper sx={{ m: 1, width: 100, height: 100 }} elevation={4}>
    <svg>
    <Box
        component="polygon"
        points="0,100 50,00, 100,100"
        sx={(theme) => ({
        fill: theme.palette.common.white,
        stroke: theme.palette.divider,
        strokeWidth: 1,
        })}
    />
    </svg>
</Paper>
);

function SimpleFade () {
    const [checked, setChecked] = React.useState(false);

    const handleChange = () => {
      setChecked((prev) => !prev);
    };

    return ( 
        <Box sx={{ height: 180 }}>
    <FormControlLabel
    control={<Switch checked={checked} onChange={handleChange} />}
    label="Show"
    />
    <Box sx={{ display: 'flex' }}>
    <Fade in={checked}>{icon}</Fade>
    </Box>
    </Box>
    );
}

export default function D3jsChartsPage() {

    const [todoList, setTodoList] = useState(initialTodos)
    const [task, setTask] = useState('')
    const [term, setTerm] = useState('')

    const printTodoList = useCallback(() => {
        console.log('Changing todoList', todoList)
    }, [todoList])

    useEffect(() => {
        // console.log('Rendering <App />')
    })

    useEffect(() => {
        printTodoList()
    }, [todoList, printTodoList])

    const handleCreate = () => {
        const newTodo = {
        id: Date.now(), 
        task
        }

        setTodoList([...todoList, newTodo])
        setTask('')
    }

    const handleSearch = () => {
        setTerm(task)
    }

    
    const handleSuccess = ({
        coords: { latitude, longitude },
    }: {
        coords: { latitude: number; longitude: number }
    }) => {
    
  }

  useEffect(() => {
    if (navigator.geolocation) {
      navigator.geolocation.getCurrentPosition(handleSuccess)
    }
  }, [])
  return (
    <div className="App">
      <BarChart/>
      <Controlled></Controlled>
      
      <input 
        type="text" 
        value={task} 
        onChange={(e) => setTask(e.target.value)} 
      />
        <button onClick={handleCreate}>Create</button>
        <button onClick={handleSearch}>Search</button>
      
    </div>
  )
  /*return <Geolocation latitude={latitude} longitude={longitude}>
    
  </Geolocation>*/
  };
  