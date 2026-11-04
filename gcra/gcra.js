// This scirpt shows three similar concepts expressed differently: token bucket, leaky bucket as meter, and GCRA (generic cell rate algorithm).

{
  // token bucket -- "restore" the tokens!
  function restore(
    remainingUsageBytes,
    previousUsageMs,
    regenRateMs,
    maxBytes,
    now,
  ) {
    const restored = Math.min(
      remainingUsageBytes + (now - previousUsageMs) / regenRateMs,
      maxBytes,
    );
    return {
      availableUsageBytes: restored,
    };
  }

  const now = Date.now();
  const rate = 1000; // 1 byte every second
  const maxBytes = 50;

  // scenario, remaining 10 bytes, previous was 5000 ms ago, maxBytes is 50, with rate at 1 every sec,
  // result is 15
  const remaining = 10;
  console.log(
    `Token bucket: ${JSON.stringify(restore(remaining, now - 5000, rate, maxBytes, now))}`,
  );
}

{
  // leaky bucket as meter -- drains the bucket so we can fill more, same concept!
  // an opposite of token bucket. When checking, instead of checking for available token to use,
  // we check if bucket still has capacity to hold more items.
  // instead of tracking the available space, we track the used up space (usedBytes)
  function drain(previousUsageBytes, previousUsageMs, regenRateMs, now) {
    const currentUsage = Math.max(
      0,
      previousUsageBytes - (now - previousUsageMs) / regenRateMs,
    );
    return {
      currentUsage,
    };
  }

  // scenario, usage 40 bytes (remaining 10), previous was 5000ms ago, maxBytes is 50, with rate at 1 every sec
  // currentUsage result should be 35 bytes
  const now = Date.now();
  const rate = 1000; // 1 byte every second
  const maxBytes = 50;
  const currentUsage = 40;
  console.log(
    `Leaky bucket as meter: ${JSON.stringify(drain(currentUsage, now - 5000, rate, now))}`,
  );

  // max bytes is not used in the equation but used when we wanna check the usage. Don't allow if cost + currentUsage > maxBytes
}

{
  // GCRA, same algorithm, but one variable, the TAT (theoretical arrival time)
  // tau is how far ahead tat may run, it's regenRateMs * maxBytes, for example if it's 1 byte every second and max is 50,
  // tau is 50,000 ms.
  //
  // you can think of tat as the time the availability will be back at max (if it was in the past then it's max already)
  function getAvailableUsage(storedTat, tau, regenRate, now) {
    // in this case "cell" is 1 byte
    const tat = Math.max(now, storedTat);
    return {
      availableUsageBytes: (tau - (tat - now)) / regenRate,
      // this + cost * regenRate will be stored
      // it's the time token/stamina/cell is fully reset
      tat,
    };
  }

  // GCRA tracks the used up space similar to leaky bucket (as time), available slots are derived from it.
  // scenario, remaining 10, previous was 5000ms ago, max bytes 50, with rate at 1 every sec,
  // remaining cells should be 15, with newTat be now + 35 seconds.
  const now = Date.now();
  const maxBytes = 50;
  const rate = 1000; // 1 byte every second
  const tau = maxBytes * rate;

  // the new idea here is that if last usage was 40, last usage was 5 seconds ago, this means the tat then
  // would have resulted in timeThen + (40 * regenRate) which is 35 seconds away from now
  // The optimization made here is the "flattening" of dimension. Instead of using time to only store
  // when it was last used, we also use time to store the usage. So whenItWasLastUsed (time) and (lastUsedValue) combined is
  // basically whenItWillBeFull. One can also arrive at GCRA algebraically from toekn bucket / leaky bucket math too.
  const lastTat = now + 35 * 1000;
  const out = getAvailableUsage(lastTat, tau, rate, now);
  console.log(`GRCA ${JSON.stringify(out)}`);

  {
    const costBytes = 10;
    // usage without debt mode
    if (out.availableUsageBytes < costBytes) {
      // log something
    } else {
      const nextTat = out.tat + costBytes * rate;
      console.log(
        `After using ${costBytes} bytes, nextTat is ${(nextTat - now) / 1000} seconds from now`,
      );
    }
  }

  {
    // usage WITH debt mode (allow overshooting)
    const costBytes = 100;
    if (out.availableUsageBytes <= 0) {
      // log something
    } else {
      const nextTat = out.tat + costBytes * rate;
      console.log(
        `After using ${costBytes} (overshoot) bytes, nextTat is ${(nextTat - now) / 1000} seconds from now`,
      );
    }
  }
}
