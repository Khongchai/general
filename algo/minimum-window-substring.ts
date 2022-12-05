// First try (not working)
export function minWindow(s: string, t: string): string {
  /**
   * 0. Initialize hashMap with all strings from t.
   * 1. Let start and end pointer = 0.
   * 2. Let findCounter = 0;
   * 3. Move end pointer until findCounter = length o t
   * 4. Compare the start and end with the smallestWindow.
   * 4. Move start counter until we find the next character from t.
   * 5. Derement the value of findCounter by one.
   * 6. Repeat step 4.
   * 7. Return substring at start and end.
   */

  let start = 0,
    end = 0;
  const map: Record<string, string> = {};
  const smallestWindow: (number | null)[] = [null, null];
  let findCounter = 0;

  for (let i = 0; i < t.length; i++) {
    map[t[i]] = t[i];
  }

  for (let _ = 0; _ < s.length; _++) {
    if (map[s[end]]) {
      findCounter++;
    }

    // When all found
    if (findCounter === t.length) {
      if (
        smallestWindow[0] != null &&
        smallestWindow[1]! - smallestWindow[0] > end - start
      ) {
        smallestWindow[0] = start;
        smallestWindow[1] = end;
      }
    }
    while (findCounter === t.length) {
      start++;

      if (map[s[start]]) {
        findCounter = t.length - 1;
      }
    }

    end++;
  }

  return s.substring(smallestWindow[0]!, smallestWindow[1]! + 1);
}
