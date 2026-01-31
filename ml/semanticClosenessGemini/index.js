
// @ts-check

import { GoogleGenAI } from "@google/genai";
import cosineSimilarity from "compute-cosine-similarity";

// Initialize the API with your key
// Best practice: Use process.env.GEMINI_API_KEY
const genAI = new GoogleGenAI({apiKey:''});

async function rankWidgets(userQuery, widgets) {

  // 1. Prepare tag strings
  const widgetTags = widgets.map(w => w.tags.join(", "));

  // 2. Generate Embeddings
  // We use batchEmbedContents to get all widget embeddings in one network call
  const [queryBatch, documentBatch] = await Promise.all([
    genAI.models.embedContent({
         model: 'gemini-embedding-001',
        contents: userQuery,
        config: {
            taskType: "RETRIEVAL_QUERY"
        }
    }),
    genAI.models.embedContent({
        model: 'gemini-embedding-001',
        contents: widgetTags,
        config: {
            taskType: "RETRIEVAL_DOCUMENT",
        }
    })
  ]);

  const queryVector = queryBatch.embeddings ?? [];
  if (!queryVector) {
    throw new Error("Query vector undefined");
  }
  const widgetEmbeddings = documentBatch.embeddings?.map(e => e.values);

  // 3. Calculate Scores
  const rankedResults = widgets.map((widget, index) => {
    const score = cosineSimilarity(queryVector[0].values, widgetEmbeddings[index]);
    return { ...widget, score };
  });

  // 4. Sort by score descending
  return rankedResults.sort((a, b) => b.score - a.score);
}

// --- DATA ---
const widgets = [
  { id: "TVLLeaderboard", tags: ["TVL", "leaderboard", "All time"] },
  { id: "CryptoProjectMindshareLeaderboard", tags: ["crypto", "mindshare", "leaderboard"] },
  { id: "CryptoProjectCard", tags: ["crypto", "detail"] }
];

// --- RUN ---
const userInput = "Show me the TVL trend of lending protocols this month.";

rankWidgets(userInput, widgets)
  .then(results => {
    console.log(`Query: "${userInput}"\n`);
    console.log(`Rank | Widget ID           | Score`);
    console.log(`-----------------------------------`);
    results.forEach((res, i) => {
      console.log(`${i + 1}    | ${res.id.padEnd(19)} | ${res.score.toFixed(4)}`);
    });
  })
  .catch(err => console.error("Error:", err));