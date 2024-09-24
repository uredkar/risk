import { FC,  useState } from 'react'

type Props = {
  initialCount: number
}

type Count = {
  count: number
}

const Counter: FC<Props> = (props) => {
  const [state, setState] = useState<Count>({ count: props.initialCount })

  const handleClick = () => {
    setState({
      count: state.count + 1
    })
  }

  const handleCounter = (operation = 'add') => {
    if (operation === 'add') {
        return setState({
            count: state.count + 1
          })
    }
    
    return setState({
        count: state.count - 1
      })
  }


  return (
    <div>
        <p>{state.count}</p>
    <div>      
      <button onClick={handleClick}>+</button>
      <button onClick={() => handleCounter('add')}>+ Add</button>
      <button onClick={() => handleCounter('substract')}>- Subtract</button>
    </div>
    
    </div>
  )
}

export default Counter
