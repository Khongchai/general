// For grabbing all those sweet deals on FB :)

const buttonToClick = document.getElementById("send-button");
runCallbackAt("20:45:00", () => {
  buttonToClick.click();
});

/**
 *
 * @param {string} time
 * @param {VoidFunction} callback
 */
function runCallbackAt(time, callback) {
  let currentTime = new Date();
  let targetTime = new Date(
    currentTime.toISOString().split("T")[0] + "T" + time
  );

  if (targetTime < currentTime) {
    targetTime.setDate(targetTime.getDate() + 1); // if the time has already passed today, run it tomorrow
  }

  let timeout = targetTime - currentTime;

  setTimeout(callback, timeout);
}
