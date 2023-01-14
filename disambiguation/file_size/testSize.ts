function main() {
  const file = Deno.statSync("testSize.json");
  const fileSizeInBytes = file.size;
  console.log(`Deno, file size: ${fileSizeInBytes}`);

  const fileContent = Deno.readTextFileSync("testSize.json");
  const encoder = new TextEncoder();
  const bytes = encoder.encode(fileContent);
  console.log(`Deno, string size: ${bytes.byteLength}`);
}

main();
