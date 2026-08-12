Best way to visualize a fairness check. Thanks, Gemini!

```ts
/**
 * Represents a step in a cycle.
 * @typedef {Object} CycleStep
 * @property {string} state - The current state description
 * @property {boolean} isEnabled - Is Action A enabled in this state?
 * @property {boolean} wasTaken - Was Action A taken to get to the next state?
 */

/**
 * WEAK FAIRNESS CHECK
 * Rule: If Action A is CONTINUOUSLY enabled across the ENTIRE cycle, 
 * it MUST eventually be taken.
 * 
 * @param {CycleStep[]} cycle 
 * @returns {boolean} true if the cycle is valid under WF, false if it violates WF
 */
function isFairUnderWeakFairness(cycle) {
  const actionWasTaken = cycle.some(step => step.wasTaken);
  if (actionWasTaken) return true; // Action happened, condition satisfied

  // Action was never taken. Was it enabled CONTINUOUSLY in every state of the loop?
  const isContinuouslyEnabled = cycle.every(step => step.isEnabled);

  // If enabled everywhere and never taken -> VIOLATION (Unfair cycle)
  return !isContinuouslyEnabled;
}

/**
 * STRONG FAIRNESS CHECK
 * Rule: If Action A is REPEATEDLY enabled (enabled in AT LEAST ONE state of the loop), 
 * it MUST eventually be taken.
 * 
 * @param {CycleStep[]} cycle 
 * @returns {boolean} true if the cycle is valid under SF, false if it violates SF
 */
function isFairUnderStrongFairness(cycle) {
  const actionWasTaken = cycle.some(step => step.wasTaken);
  if (actionWasTaken) return true; // Action happened, condition satisfied

  // Action was never taken. Was it enabled INFINITELY OFTEN (at least once in the loop)?
  const isRepeatedlyEnabled = cycle.some(step => step.isEnabled);

  // If enabled anywhere in the loop and never taken -> VIOLATION (Unfair cycle)
  return !isRepeatedlyEnabled;
}
```