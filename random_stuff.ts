type TimeUnit = "ms" | "s" | "m" | "h" | "d" | "w";
type Time = `${number}${TimeUnit}`;

const something: Time = `5s`;
