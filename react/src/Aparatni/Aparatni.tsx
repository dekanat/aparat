import Frst from "./Frst";
import Seknd from "./Seknd";

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

// These two are just helpers, they curate spring data, values that are later being interpolated into css
const to = (i: number) => ({
  x: 0,
  y: i * -4,
  skew: 10,
  scale: 1,
  delay: i * 100,
})
const from = (_i: number) => ({ x: 0, scale: 1.5, skew: 20, y: -1000 })
// This is being used down there in the view, it interpolates rotation and scale into a css transform
const trans = (skew: number, scale: number) =>
  `perspective(1500px) rotateX(${skew}deg) scale(${scale})`

function Deck() {
  const [goners, setGoners] = useState(() => new Set()) // The set flags all the cards that are flicked out
  const [cards, setCards] = useState(() => [
    randomFrom(allCards),
    // randomFrom(allCards),
    // randomFrom(allCards),
  ])

  const [props, api] = useSprings(cards.length, i => ({
    ...to(i),
    from: from(i),
  })) // Create a bunch of springs using the helpers above
  // Create a gesture, we're interested in down-state, delta (current-pos - click-pos), direction and velocity
  const bind = useDrag(({ args: [index], active: isActive, movement: [mx], direction: [xDir], velocity: [vx] }) => {
    const isGestureConcluded = !isActive;

    if (isGestureConcluded) {
      // If you flick hard enough it should trigger the card to fly out
      const concludedGesture = vx > 0.2 ? "swipe" : "nothing"

      if (concludedGesture === 'swipe') {
        goners.add(index)
        // cards.unshift(randomFrom(allCards))
        // console.log(goners, cards)
      }
    }

    // const trigger = vx > 0.2 
    // console.log(isActive)
    // if (!isActive && trigger) gone.add(index) // If button/finger's up and trigger velocity is reached, we flag the card ready to fly out

    const isCurrentSpring = (i: number) => i === index

    const animateCurrentSpring = (i: number) => {
      if (isCurrentSpring(i)) {
        const isGone = goners.has(index)
        const x = isGone ? (200 + window.innerWidth) * xDir : isActive ? mx : 0 // When a card is gone it flys out left or right, otherwise goes back to zero
        const scale = isActive ? 1.1 : 1 // Active cards lift up a bit
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

  // Now we're just mapping the animated values to our view, that's it. Btw, this component only renders once. :-)
  return (
    <>
      {props.map(({ x, y, skew, scale }, i) => (
        <animated.div className={styles.deck} key={i} style={{ x, y }}>
          {/* This is the card itself, we're binding our gesture to it (and inject its index so we know which is which) */}
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
