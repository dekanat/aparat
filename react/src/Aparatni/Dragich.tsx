import { useState } from "react";

import styles from "./style.module.css";
import { Vector2, useDrag } from "@use-gesture/react";

const allCards = [
  "https://upload.wikimedia.org/wikipedia/commons/f/f5/RWS_Tarot_08_Strength.jpg",
  "https://upload.wikimedia.org/wikipedia/commons/5/53/RWS_Tarot_16_Tower.jpg",
  "https://upload.wikimedia.org/wikipedia/commons/9/9b/RWS_Tarot_07_Chariot.jpg",
  // "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/RWS_Tarot_06_Lovers.jpg/640px-RWS_Tarot_06_Lovers.jpg",
  // "https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/RWS_Tarot_02_High_Priestess.jpg/690px-RWS_Tarot_02_High_Priestess.jpg",
];

type CardProps = {
  url: string;
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

  const initialTarotCards = allCards.map((url, i) => {
    return {
      url,
      position: positionOf(i),
    };
  });

  const [givenCards, setGivenCards] = useState(() => initialTarotCards);

  const moveActiveCard = (activeUrl: string, newPosition: Position) => {
    const activeIndex = givenCards.findIndex(({ url }) => url === activeUrl);
    givenCards[activeIndex] = {
      ...givenCards[activeIndex],
      position: newPosition,
    };
    setGivenCards(givenCards);
  };

  const cardDrags = useDrag((state) => {
    const atRestWhen = ([vx, vy]: Vector2) => vx === 0 && vy === 0;

    const activeCardUrl =
      "https://upload.wikimedia.org/wikipedia/commons/9/9b/RWS_Tarot_07_Chariot.jpg";

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

        const [left, top] = state.xy;
        const [head, ...tail] = givenCards.reverse();
        const movedHead = {
          ...head,
          position: { left, top },
        };

        setGivenCards([movedHead, ...tail].reverse());
      }
    } else {
      console.log("Drag ended");

      const [head, ...tail] = givenCards.reverse();
      const movedHead = {
        ...head,
        position: positionOf(givenCards.length - 1),
      };

      setGivenCards([movedHead, ...tail].reverse());
      // moveActiveCard(activeCardUrl, positionOf(givenCards.length - 1));
    }

    console.log(state);
  }, options);

  const useCardDrags = (givenCard: TarotCard) => {
    const revealAttributes = useDrag(() => {
      console.log(givenCard);
    });

    return revealAttributes();
  };

  return (
    <>
      {givenCards.map(({ url, position }, i) => (
        <div className={styles.deck} key={i}>
          <div
            {...cardDrags()}
            // {...useCardDrags({ url, position })}
            style={{ position: "absolute", ...position }}
          >
            <Card url={url} />x
          </div>
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
