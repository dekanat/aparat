import { useState } from "react";

import styles from "./style.module.css";
import { Vector2, useDrag } from "@use-gesture/react";
import { animated, useSpringValue } from "@react-spring/web";

const allCards = [
  "https://upload.wikimedia.org/wikipedia/commons/f/f5/RWS_Tarot_08_Strength.jpg",
  "https://upload.wikimedia.org/wikipedia/commons/5/53/RWS_Tarot_16_Tower.jpg",
  "https://upload.wikimedia.org/wikipedia/commons/9/9b/RWS_Tarot_07_Chariot.jpg",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/RWS_Tarot_06_Lovers.jpg/640px-RWS_Tarot_06_Lovers.jpg",
  // "https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/RWS_Tarot_02_High_Priestess.jpg/690px-RWS_Tarot_02_High_Priestess.jpg",
];

type CardProps = {
  url: string;
  position: Position;
};

function Card({ url }: CardProps) {
  return (
    <>
      <div className={styles.tarotCard}>
        <img src={url} />
      </div>
    </>
  );
}

const positionOf = (i: number) => {
  const dx = 10;
  const dy = 10;

  return {
    top: i * dx,
    left: i * dy,
  };
};

type TarotCard = {
  url: string;
  position: Position;
};

type Position = {
  top: number;
  left: number;
};

function Dragich() {
  const options = {
    axis: "y" as const,
  };

  const left = useSpringValue(0);

  const initialTarotCards = allCards.map((url, i) => {
    return {
      url,
      position: positionOf(i),
    };
  });

  const [givenCards, setGivenCards] = useState(() => initialTarotCards);

  const moveTopCard = (
    cards: TarotCard[],
    movedCards: TarotCard[],
    newPosition: Position
  ): TarotCard[] => {
    if (cards.length === 1) {
      return [...movedCards, { ...cards[0], position: newPosition }];
    } else {
      const [head, ...tail] = cards;
      return moveTopCard(tail, [...movedCards, head], newPosition);
    }
  };

  const onDragMoves = (position: Position) => {
    const movedCards = moveTopCard(givenCards, [], position);

    setGivenCards(movedCards);
  };

  const cardDrags = useDrag((state) => {
    const atRestWhen = ([vx, vy]: Vector2) => vx === 0 && vy === 0;

    if (state.active && state.first) {
      console.log("Drag started");
      console.log(state.target);
      console.log(
        `current position: ${state.xy} | of element`,
        state.currentTarget
      );
    } else if (state.active) {
      if (atRestWhen(state.velocity)) {
        console.log("Dragging paused");
      } else {
        console.log(`Dragging continues at ${state.velocity} px/ms`);
        console.log(
          `current position: ${state.xy} | of element`,
          state.currentTarget
        );

        const [x, y] = state.xy;
        onDragMoves({ left: x, top: y });
        left.start(x);
      }
    } else {
      console.log("Drag ended");

      const movedCards = moveTopCard(
        givenCards,
        [],
        positionOf(givenCards.length - 1)
      );

      setGivenCards(movedCards);
      left.start(0);
    }
  }, options);

  return (
    <>
      {givenCards.map(({ url, position }, i) => (
        <div className={styles.deck} key={i}>
          <animated.div
            {...cardDrags()}
            style={{ position: "absolute", left, top: position.top }}
          >
            <Card url={url} position={position} />x
          </animated.div>
        </div>
      ))}
    </>
  );
}

export default function App() {
  return (
    <div className={`flex fill center ${styles.container}`}>
      <Dragich />
    </div>
  );
}