"use client"; // This is a client component
import './playground.css';
import * as React from 'react';
import { useMemo, useCallback } from 'react'
import { useEffect, useState } from 'react';
import Geolocation from './geolocation';
import MyComponent from './withinnerwidth';
import Controlled from './controlled';
import Box from '@mui/material/Box';
import Switch from '@mui/material/Switch';
import Paper from '@mui/material/Paper';
import Fade from '@mui/material/Fade';
import FormControlLabel from '@mui/material/FormControlLabel';
import Counter from './counter';
import List, {Todo } from './list';
import Notes from './notes';
import Issues2 from './issues2';

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

export default function Playground1Page() {

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

    const handleDelete = useCallback((taskId: number) => {
        const newTodoList = todoList.filter((todo: Todo) => todo.id !== taskId)
        setTodoList(newTodoList)
    }, [todoList])

    const filteredTodoList = useMemo(() => todoList.filter((todo: Todo) => {
        // console.log('Filtering...')
        return todo.task.toLowerCase().includes(term.toLowerCase())
    }), [term, todoList])
  

    const [latitude, setLatitude] = useState<number | null>(null)
    const [longitude, setLongitude] = useState<number | null>(null)

    const handleSuccess = ({
        coords: { latitude, longitude },
    }: {
        coords: { latitude: number; longitude: number }
    }) => {
    setLatitude(latitude)
    setLongitude(longitude)
  }

  useEffect(() => {
    if (navigator.geolocation) {
      navigator.geolocation.getCurrentPosition(handleSuccess)
    }
  }, [])
  return (
    <div className="App">
      <MyComponent />
      <Geolocation latitude={latitude} longitude={longitude}>
      </Geolocation>
      <Notes />
      <SimpleFade />
      <Controlled></Controlled>
      <Counter initialCount={1} />
      <input 
        type="text" 
        value={task} 
        onChange={(e) => setTask(e.target.value)} 
      />
        <button onClick={handleCreate}>Create</button>
        <button onClick={handleSearch}>Search</button>
      <List todoList={filteredTodoList} handleDelete={handleDelete}/>
      <Issues2 />
    </div>
  )
  /*return <Geolocation latitude={latitude} longitude={longitude}>
    
  </Geolocation>*/
  };
  