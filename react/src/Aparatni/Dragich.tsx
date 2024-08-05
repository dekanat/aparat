import { useState } from 'react'

import styles from './style.module.css'

const allCards = [
  'https://upload.wikimedia.org/wikipedia/commons/f/f5/RWS_Tarot_08_Strength.jpg',
  'https://upload.wikimedia.org/wikipedia/commons/5/53/RWS_Tarot_16_Tower.jpg',
  'https://upload.wikimedia.org/wikipedia/commons/9/9b/RWS_Tarot_07_Chariot.jpg',
  'https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/RWS_Tarot_06_Lovers.jpg/640px-RWS_Tarot_06_Lovers.jpg',
  'https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/RWS_Tarot_02_High_Priestess.jpg/690px-RWS_Tarot_02_High_Priestess.jpg',
]

type CardProps = {
  url: string
}

function Card({ url }: CardProps) {
  return (
    <>
      <div className={styles.tarotCard}>
        <img src={url} />
      </div>
    </>
  )
}

const positionOf = (i:number) => {
  const dx = 10
  const dy = 10

  return {
    top: i * dx,
    left: i * dy
  }
}

function Dragich() {

  return (
    <>
      {allCards.map((url, i) => (
        <div className={styles.deck} key={i}>
          <div style={{position: 'absolute', ...positionOf(i) }}>
            <Card url={url} />
          </div>
        </div>
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
