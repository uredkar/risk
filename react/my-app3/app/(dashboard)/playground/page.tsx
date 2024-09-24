"use client"; // This is a client component
import { useEffect, useState } from 'react'
import Geolocation from './geolocation'
import MyComponent from './withinnerwidth'

type Props = {
    innerWidth: number
  }
export default function PlaygroundPage() {

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
    </div>
  )
  /*return <Geolocation latitude={latitude} longitude={longitude}>
    
  </Geolocation>*/
  };
  