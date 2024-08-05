import { useState } from 'react'
import { useSprings, animated, to as interpolate } from '@react-spring/web'
import { useDrag } from '@use-gesture/react'

import styles from './style.module.css'

const allCards = [
  'https://upload.wikimedia.org/wikipedia/commons/f/f5/RWS_Tarot_08_Strength.jpg',
  'https://upload.wikimedia.org/wikipedia/commons/5/53/RWS_Tarot_16_Tower.jpg',
  'https://upload.wikimedia.org/wikipedia/commons/9/9b/RWS_Tarot_07_Chariot.jpg',
  'https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/RWS_Tarot_06_Lovers.jpg/640px-RWS_Tarot_06_Lovers.jpg',
  'https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/RWS_Tarot_02_High_Priestess.jpg/690px-RWS_Tarot_02_High_Priestess.jpg',
]

const randomFrom = (xs: unknown[]) => {
  const index = Math.floor(Math.random() * xs.length)
  return xs[index]
}

function Dragich() {

  return (
    <>
      {allCards.map((url, i) => (
        <animated.div className={styles.deck} key={i}>
          <animated.div >
            <img src={url} />
          </animated.div>
        </animated.div>
      ))}
    </>
  )
}

export default function App() {
  return (
    <div className={`flex fill center ${styles.container}`}>
      <Dragich />
    </div>
  )
}
