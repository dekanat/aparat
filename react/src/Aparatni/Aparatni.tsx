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

const to = (i: number) => ({
  x: 0,
  y: i * -4,
  skew: 10,
  scale: 1,
  delay: i * 100,
})

const trans = (skew: number, scale: number) =>
  `perspective(1500px) rotateX(${skew}deg) scale(${scale})`

function Deck() {
  const [goners, setGoners] = useState(() => new Set())
  const [cards, setCards] = useState(() => [
    randomFrom(allCards)
  ])

  const [props, api] = useSprings(cards.length, i => ({
    to: { x: 0, y: i * -4, skew: 10, scale: 1 },
    from: { x: 0, scale: 1.5, skew: 20, y: -1000 }
  }))

  const bind = 
    useDrag(({ args: [index], active: isActive, movement: [mx], direction: [xDir], velocity: [vx] }) => {
      const isGestureConcluded = !isActive;

      if (isGestureConcluded) {
        const concludedGesture = vx > 0.2 ? "swipe" : "nothing"

        if (concludedGesture === 'swipe') {
          goners.add(index)
        }
      }

    const isCurrentSpring = (i: number) => i === index

    const animateCurrentSpring = (i: number) => {
      if (isCurrentSpring(i)) {
        const isGone = goners.has(index)
        const x = isGone ? (200 + window.innerWidth) * xDir : isActive ? mx : 0
        const scale = isActive ? 1.1 : 1
        const skew = isActive ? 30 : 10
        return {
          x,
          scale,
          skew,
          delay: undefined,
          config: { friction: 200, tension: 500 },
        }
      } else {
        return {
          skew: isActive ? 20 : 10,
          delay: undefined,
          config: { friction: 200, tension: 500 },
        }
      }
    }

    api.start(animateCurrentSpring)

    if (!isActive && goners.size === cards.length) {
      setTimeout(() => {
        setCards([
          randomFrom(allCards),
        ])
        setGoners(new Set())
        api.start(i => to(i))
      }, 600)
    }
  })

  const getCard = (i: number) => `url(${cards[i]})`

  return (
    <>
      {props.map(({ x, y, skew, scale }, i) => (
        <animated.div className={styles.deck} key={i} style={{ x, y }}>
          <animated.div
            {...bind(i)}
            style={{
              transform: interpolate([skew, scale], trans),
              backgroundImage: interpolate([i], getCard),
            }}
          />
        </animated.div>
      ))}
    </>
  )
}

export default function App() {
  return (
    <div className={`flex fill center ${styles.container}`}>
      <Deck />
    </div>
  )
}
