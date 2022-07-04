# Adaptation to Online Pong Game (former *SquashCatch*)

Analyses for the online Pong task

---

## Description of the experiment

Participants played an online version of the Pong game on their computer. They had to intercept the pong ball after it bounced back from the upper wall using a paddle controlled by their cursor.
A 9° rotation was applied to the post-bounce trajectory of the ball. In addition, the visual tilt of the bouncing wall was manipulated to be consistent or inconsistent with the ball trajectory.

- No perturbation trials
- Perturbation trials, tilted wall (consistent condition)
- Perturbation trials, horizontal wall (inconsistent condition)

### Session 1

- No perturbation *(x 4 blocks)*
- Trained perturbation *(x 4 blocks)*
  - **Tilted** wall for the **trained tilt** group
  - **Horizontal** wall for the **trained horizontal** group

### Session 2

- Trained perturbation *(x 2 blocks)*
- Untrained perturbation *(x 1 block)*
  - **Horizontal** wall for the **trained tilt** group
  - **Tilted** wall for the **trained horizontal** group
- No perturbation *(x 1 block)*


---

## Versions of the task

### bounceV1

Speed of the ball:
- Block #1: 0.03 a.u./frame
- Block #2: 0.04 a.u./frame
- Blocks #3-8: 0.05 a.u./frame

### bounceV2

The maximum speed of the ball was reduced to 0.04 a.u.

Added 2 blocks of trained perturbation during session 1.

### bounceV3 *(data used to publish results)*

Made the paddle shorter than in previous versions (0.05 a.u. instead of 0.075 a.u.).

Removed the 2 additional blocks of trained perturbation during session 1.
